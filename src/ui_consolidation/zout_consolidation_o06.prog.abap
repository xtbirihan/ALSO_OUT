**********************************************************************
*& Key           : AMOGYORODI-230814
*& Request No.   :
**********************************************************************
*& Description (short)
*&
*&
**********************************************************************
*----------------------------------------------------------------------*
***INCLUDE ZOUT_CONSOLIDATION_O02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module PBO_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0210 OUTPUT.
  SET PF-STATUS 'STANDARD'.
  SET TITLEBAR 'TITLE_0100'.
ENDMODULE.

MODULE pbo_0210 OUTPUT.
  DATA(lo_container_openpick) = NEW cl_gui_custom_container( container_name = 'CONT_PICK_POS' ).
  lo_view = lcl_view=>get_instance( ).
  lo_view->show_alv_open_picking( lo_container_openpick ).
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module PBO_0600 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_0600 OUTPUT.

  DATA: lv_ident    TYPE zde_param_low,
        ls_dlv_item TYPE /scwm/dlv_item_out_prd_str,
        ls_huitem   TYPE  /scwm/s_huitm_int.

  CLEAR: ls_huitem,
         ls_dlv_item.

  IF zstr_consolidation_scr_0600-packhu IS INITIAL.
    CLEAR zstr_consolidation_scr_0600.
    MOVE-CORRESPONDING zstr_consolidation_scr_0300 TO zstr_consolidation_scr_0600.
  ENDIF.

  DATA(lo_container_items) = NEW cl_gui_custom_container( container_name = 'CONT_ITEMS' ).
  DATA(lo_container_scan) = NEW cl_gui_custom_container( container_name = 'CONT_SCAN' ).
  DATA(lo_container_captured) = NEW cl_gui_custom_container( container_name = 'CONT_CAPTURED' ).

  lo_view = lcl_view=>get_instance( ).

  lo_view->show_alv_serial_hu_content( lo_container_items ).
  lo_view->show_alv_serial( io_container = lo_container_captured ).

  lo_model = lcl_model=>get_instance( ).
  READ TABLE lo_model->mt_cons_huhdr INTO DATA(ls_huhdr) WITH KEY huident = zstr_consolidation_scr_0600-packhu.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  READ TABLE lo_model->mt_cons_huitm INTO ls_huitem
      WITH KEY guid_parent = ls_huhdr-guid_hu
               matid       = lo_view->ms_cons_hu_serial-matid.

  IF sy-subrc <> 0.

    LOOP AT lo_model->mt_cons_huhdr INTO DATA(ls_huhdr_subhu)
                                 WHERE higher_guid = ls_huhdr-guid_hu AND
                                       guid_hu     = lo_view->ms_cons_hu_serial-guid_hu. " Andriyan Yordanov

      READ TABLE lo_model->mt_cons_huitm INTO ls_huitem
        WITH KEY guid_parent = ls_huhdr_subhu-guid_hu
                 matid       = lo_view->ms_cons_hu_serial-matid. " Andriyan Yordanov

      IF sy-subrc = 0.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

  " get main ident type of the SN from the param.
  IF lv_ident IS INITIAL.
    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = lcl_controller=>sv_lgnum
        iv_process   = zif_param_const=>c_zout_0008
        iv_parameter = zif_param_const=>c_indenttab01
      IMPORTING
        ev_constant  = lv_ident ).
  ENDIF.

  CLEAR: zstr_consolidation_scr_0600-huident_curr,
         zstr_consolidation_scr_0600-sn_matnr,
         zstr_consolidation_scr_0600-sn_maktx,
         zstr_consolidation_scr_0600-selsn,
         zstr_consolidation_scr_0600-selno.

  IF lo_view->ms_cons_hu_serial-matid IS NOT INITIAL.

    zstr_consolidation_scr_0600-huident_curr = VALUE #( lo_model->mt_cons_huhdr[ guid_hu = lo_view->ms_cons_hu_serial-guid_hu ]-huident OPTIONAL ).

    NEW /scwm/cl_ui_stock_fields( )->get_matkey_by_id(
      EXPORTING
        iv_matid = lo_view->ms_cons_hu_serial-matid          " Material GUIDs with Conversion Exit
      IMPORTING
        ev_matnr = zstr_consolidation_scr_0600-sn_matnr      " Material Number
        ev_maktx = zstr_consolidation_scr_0600-sn_maktx ).   " Material Description

    " all SN numbers which should be scanned
    zstr_consolidation_scr_0600-selno = VALUE #(  lo_view->mt_cons_sn_reques_cont[ guid_hu = lo_view->ms_cons_hu_serial-guid_hu
                                                                                   matid   = lo_view->ms_cons_hu_serial-matid ]-quan OPTIONAL ).

    " count what is the amount of the current scanned SN
    DATA(lt_scanned_nums) = zcl_crud_ztcross_cap_nums=>select_multi_by_huid(
                                            iv_lgnum = lcl_controller=>sv_lgnum
                                            iv_huid  = lo_view->ms_cons_hu_serial-guid_hu ).

    DELETE lt_scanned_nums WHERE group_mc <> lo_view->ms_cons_hu_serial-group_mc.
    DELETE lt_scanned_nums WHERE id_type <> lv_ident.
    zstr_consolidation_scr_0600-selsn = lines( lt_scanned_nums ).

  ENDIF.

  IF ls_huitem IS INITIAL AND
     lo_view->ms_cons_hu_serial-matid IS NOT INITIAL. " the user pressed capture
    RETURN.
  ENDIF.

  READ TABLE lo_model->mt_dlv_items INTO ls_dlv_item
    WITH KEY docid  = ls_huitem-qdocid
             itemid = ls_huitem-qitmid.

  IF sy-subrc <> 0 AND
     lo_view->ms_cons_hu_serial-matid IS NOT INITIAL. " the user pressed capture
    RETURN.
  ENDIF.

  IF ls_dlv_item-eew-zzindenttab01 = lv_ident AND
     zstr_consolidation_scr_0600-selsn <> zstr_consolidation_scr_0600-selno AND
     lo_model->is_shipping_carton( is_huhdr = ls_huhdr ) = abap_false. " if on the top we have shipHu then we should do nothing with SN scan just display

    IF lo_model->mt_cross_numbers IS INITIAL.
      SELECT * FROM ztcross_numbers INTO TABLE @DATA(lt_cross_num_cust)
        WHERE spras	 = @sy-langu AND
              lgnum  = @lcl_controller=>sv_lgnum.
      IF sy-subrc = 0.
        lo_model->mt_cross_numbers = lt_cross_num_cust.
      ENDIF.
    ENDIF.

    ASSIGN lo_model->mt_cross_numbers[ selnum = ls_dlv_item-eew-zzindenttab01 ] TO FIELD-SYMBOL(<ls_eew_zzindenttab01>).

    IF sy-subrc = 0.
      DATA(lv_field_01_selnum) = 'ZSTR_CONSOLIDATION_SCR_0600-SN_SELNUM_KEY'.
      zstr_consolidation_scr_0600-zzindenttab01_txt01 = <ls_eew_zzindenttab01>-number_description.
      zstr_consolidation_scr_0600-sn_selnum_key       = <ls_eew_zzindenttab01>-selnum.
    ENDIF.

    ASSIGN lo_model->mt_cross_numbers[ selnum = ls_dlv_item-eew-zzindenttab02 ] TO FIELD-SYMBOL(<ls_eew_zzindenttab02>).

    IF sy-subrc = 0.
      DATA(lv_field_02_selnum) = 'ZSTR_CONSOLIDATION_SCR_0600-SN_SELNUM_02'.
      zstr_consolidation_scr_0600-zzindenttab02_txt02 = <ls_eew_zzindenttab02>-number_description.
      zstr_consolidation_scr_0600-sn_selnum_02        = <ls_eew_zzindenttab02>-selnum.
    ENDIF.

    ASSIGN lo_model->mt_cross_numbers[ selnum = ls_dlv_item-eew-zzindenttab03 ] TO FIELD-SYMBOL(<ls_eew_zzindenttab03>).

    IF sy-subrc = 0.
      DATA(lv_field_03_selnum) = 'ZSTR_CONSOLIDATION_SCR_0600-SN_SELNUM_03'.
      zstr_consolidation_scr_0600-zzindenttab03_txt03 = <ls_eew_zzindenttab03>-number_description.
      zstr_consolidation_scr_0600-sn_selnum_03        = <ls_eew_zzindenttab03>-selnum.
    ENDIF.

    ASSIGN lo_model->mt_cross_numbers[ selnum = ls_dlv_item-eew-zzindenttab04 ] TO FIELD-SYMBOL(<ls_eew_zzindenttab4>).

    IF sy-subrc = 0.
      DATA(lv_field_04_selnum) = 'ZSTR_CONSOLIDATION_SCR_0600-SN_SELNUM_04'.
      zstr_consolidation_scr_0600-zzindenttab04_txt04 = <ls_eew_zzindenttab4>-number_description.
      zstr_consolidation_scr_0600-sn_selnum_04        = <ls_eew_zzindenttab4>-selnum.
    ENDIF.

    ASSIGN lo_model->mt_cross_numbers[ selnum = ls_dlv_item-eew-zzindenttab05 ] TO FIELD-SYMBOL(<ls_eew_zzindenttab05>).

    IF sy-subrc = 0.
      DATA(lv_field_05_selnum) = 'ZSTR_CONSOLIDATION_SCR_0600-SN_SELNUM_05'.
      zstr_consolidation_scr_0600-zzindenttab05_txt05 = <ls_eew_zzindenttab05>-number_description.
      zstr_consolidation_scr_0600-sn_selnum_05        = <ls_eew_zzindenttab05>-selnum.
    ENDIF.
  ELSE.

    CLEAR: lv_field_05_selnum,
           lv_field_04_selnum,
           lv_field_03_selnum,
           lv_field_02_selnum,
           lv_field_01_selnum.

  ENDIF.

  " additional - requirment control the cursor
  IF lv_field_01_selnum IS NOT INITIAL AND
     zstr_consolidation_scr_0600-zzindenttab01_scan IS INITIAL.
    DATA(lv_new_cursorfield) =  'ZSTR_CONSOLIDATION_SCR_0600-ZZINDENTTAB01_SCAN'.
  ELSEIF lv_field_02_selnum IS NOT INITIAL AND
         zstr_consolidation_scr_0600-zzindenttab02_scan IS INITIAL.
    lv_new_cursorfield =  'ZSTR_CONSOLIDATION_SCR_0600-ZZINDENTTAB02_SCAN'.
  ELSEIF lv_field_03_selnum IS NOT INITIAL AND
         zstr_consolidation_scr_0600-zzindenttab03_scan IS INITIAL.
    lv_new_cursorfield =  'ZSTR_CONSOLIDATION_SCR_0600-ZZINDENTTAB03_SCAN'.
  ELSEIF lv_field_04_selnum IS NOT INITIAL AND
         zstr_consolidation_scr_0600-zzindenttab04_scan IS INITIAL.
    lv_new_cursorfield =  'ZSTR_CONSOLIDATION_SCR_0600-ZZINDENTTAB04_SCAN'.
  ELSEIF lv_field_05_selnum IS NOT INITIAL AND
         zstr_consolidation_scr_0600-zzindenttab05_scan IS INITIAL.
    lv_new_cursorfield =  'ZSTR_CONSOLIDATION_SCR_0600-ZZINDENTTAB05_SCAN'.
  ENDIF.

  IF lv_new_cursorfield IS NOT INITIAL.
    SET CURSOR FIELD lv_new_cursorfield DISPLAY OFFSET 0.
  ENDIF.

  LOOP AT SCREEN.

    " General Prod. MC. Scanned Info
    IF screen-name = gc_sn_hu    OR
       screen-name = gc_sn_matnr OR
       screen-name = gc_sn_matx  OR
       screen-name = gc_sn_selno OR
       screen-name = gc_sn_selsn.

      IF lv_field_01_selnum IS NOT INITIAL.
        screen-invisible = 0.
        screen-output    = 1.
        screen-active    = 1.
      ELSE.
        screen-invisible = 1.
        screen-output    = 0.
        screen-active    = 0.
      ENDIF.

    ENDIF.

    " IND1 - MAIN
    IF screen-name = gc_sn_field_ind1   OR
       screen-name = gc_sn_field_indtxt1 .

      IF lv_field_01_selnum IS NOT INITIAL.
        screen-invisible = 0.
        screen-output    = 1.
        screen-active    = 1.
      ELSE.
        screen-invisible = 1.
        screen-output    = 0.
        screen-active    = 0.
      ENDIF.

    ENDIF.

    IF screen-name = gc_sn_field_indscan1 AND
       lv_field_01_selnum IS NOT INITIAL.
      screen-invisible = 0.
      screen-output    = 1.
      screen-active    = 1.
    ELSEIF screen-name = gc_sn_field_indscan1  AND
       lv_field_01_selnum IS INITIAL.
      screen-invisible = 1.
      screen-output    = 0.
      screen-active    = 0.
    ENDIF.

    " IND2
    IF screen-name = gc_sn_field_ind2   OR
       screen-name = gc_sn_field_indtxt2 .

      IF lv_field_02_selnum IS NOT INITIAL.
        screen-invisible = 0.
        screen-output    = 1.
        screen-active    = 1.
      ELSE.
        screen-invisible = 1.
        screen-output    = 0.
        screen-active    = 0.
      ENDIF.

    ENDIF.

    IF screen-name = gc_sn_field_indscan2 AND
       lv_field_02_selnum IS NOT INITIAL.
      screen-invisible = 0.
      screen-output    = 1.
      screen-active    = 1.
    ELSEIF screen-name = gc_sn_field_indscan2  AND
       lv_field_02_selnum IS INITIAL.
      screen-invisible = 1.
      screen-output    = 0.
      screen-active    = 0.
    ENDIF.

    " IND3
    IF screen-name = gc_sn_field_ind3   OR
       screen-name = gc_sn_field_indtxt3 .

      IF lv_field_03_selnum IS NOT INITIAL.
        screen-invisible = 0.
        screen-output    = 1.
        screen-active    = 1.
      ELSE.
        screen-invisible = 1.
        screen-output    = 0.
        screen-active    = 0.
      ENDIF.

    ENDIF.

    IF screen-name = gc_sn_field_indscan3 AND
       lv_field_03_selnum IS NOT INITIAL.
      screen-invisible = 0.
      screen-output    = 1.
      screen-active    = 1.
    ELSEIF screen-name = gc_sn_field_indscan3 AND
           lv_field_03_selnum IS INITIAL.
      screen-invisible = 1.
      screen-output    = 0.
      screen-active    = 0.
    ENDIF.

    " IND4
    IF screen-name = gc_sn_field_ind4   OR
       screen-name = gc_sn_field_indtxt4 .

      IF lv_field_04_selnum IS NOT INITIAL.
        screen-invisible = 0.
        screen-output    = 1.
        screen-active    = 1.
      ELSE.
        screen-invisible = 1.
        screen-output    = 0.
        screen-active    = 0.
      ENDIF.

    ENDIF.

    IF screen-name = gc_sn_field_indscan4 AND
       lv_field_04_selnum IS NOT INITIAL.
      screen-invisible = 0.
      screen-output    = 1.
      screen-active    = 1.
    ELSEIF screen-name = gc_sn_field_indscan4 AND
           lv_field_04_selnum IS INITIAL.
      screen-invisible = 1.
      screen-output    = 0.
      screen-active    = 0.
    ENDIF.

    " IND5
    IF screen-name = gc_sn_field_ind5   OR
       screen-name = gc_sn_field_indtxt5 .

      IF lv_field_05_selnum IS NOT INITIAL.
        screen-invisible = 0.
        screen-output    = 1.
        screen-active    = 1.
      ELSE.
        screen-invisible = 1.
        screen-output    = 0.
        screen-active    = 0.
      ENDIF.

    ENDIF.

    IF screen-name = gc_sn_field_indscan5 AND
       lv_field_05_selnum IS NOT INITIAL.
      screen-invisible = 0.
      screen-output    = 1.
      screen-active    = 1.
    ELSEIF  screen-name = gc_sn_field_indscan5 AND
       lv_field_05_selnum IS  INITIAL.
      screen-invisible = 1.
      screen-output    = 0.
      screen-active    = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.
