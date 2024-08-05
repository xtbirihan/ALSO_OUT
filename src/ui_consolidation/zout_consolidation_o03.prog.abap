*----------------------------------------------------------------------*
***INCLUDE ZOUT_CONSOLIDATION_O03.
*----------------------------------------------------------------------*
**********************************************************************
*& Key           : AMOGYORODI-230814
*& Request No.   :
**********************************************************************
*& Description (short)
*&
*&
**********************************************************************
*&---------------------------------------------------------------------*
*& Module STATUS_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'STANDARD'.
  SET TITLEBAR 'TITLE_0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module PBO_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_0300 OUTPUT.
  DATA(lo_container_huitem) = NEW cl_gui_custom_container( container_name = 'CONT_HU_ITEMS' ).
  lo_view = lcl_view=>get_instance( ).
  lo_view->show_alv_cons_hu_content( lo_container_huitem ).

  DATA(lo_container_mat_pack_inst) = NEW cl_gui_custom_container( container_name = 'CTRL_PACKING_INST').
  lo_view->show_pack_inst( iv_dlv_level     = abap_true
                           iv_product_level = abap_true
                           io_container     = lo_container_mat_pack_inst ).

ENDMODULE.

MODULE status_popup OUTPUT.
  SET PF-STATUS 'POPUP'.
  SET TITLEBAR 'TITLE_0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module PBO_0340 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_0340 OUTPUT.

  DATA:
    lv_hutyp_txt TYPE /scwm/de_desc40,
    lt_thutyp    TYPE STANDARD TABLE OF /scwm/t307,
    lt_thutypt   TYPE STANDARD TABLE OF /scwm/thutypt.

  IF zstr_consolidation_scr_0300-newhu_pmat IS NOT INITIAL AND
     zstr_consolidation_scr_0300-desthu     IS NOT INITIAL.

    DATA(lo_controller) = lcl_controller=>get_instance( ).

    IF zstr_consolidation_scr_0300-hutype IS INITIAL.

      DATA(lv_new_pmatid) = NEW /scwm/cl_ui_stock_fields( )->get_matid_by_no(
         EXPORTING
           iv_matnr = zstr_consolidation_scr_0300-newhu_pmat  ). " Material Number

      DATA(lv_matid22) = /scmtms/cl_guid_convert=>x16_to_c22( lv_new_pmatid ).

      SELECT SINGLE FROM /sapapo/matpack
          FIELDS hutyp
        WHERE matid =  @lv_matid22
        INTO @DATA(lv_hutyp_new_hu).

      IF sy-subrc = 0.
        zstr_consolidation_scr_0300-hutype = lv_hutyp_new_hu.
      ENDIF.

    ENDIF.

    lo_controller->handle_popup_conc_mathut(
      EXPORTING
        iv_humat       = zstr_consolidation_scr_0300-newhu_pmat
        iv_hutype      = zstr_consolidation_scr_0300-hutype
      IMPORTING
        et_lbox_values = lt_lbox_values ).

  ELSEIF zstr_consolidation_scr_0300-newhu_pmat IS INITIAL AND
         zstr_consolidation_scr_0300-desthu     IS NOT INITIAL.

    lo_model = lcl_model=>get_instance( ).
    ASSIGN lo_model->mt_cons_huhdr[ huident =  zstr_consolidation_scr_0300-packhu ] TO FIELD-SYMBOL(<ls_src_hu_repack>).

    IF sy-subrc <> 0.
      " this will not happen error in a process
      RETURN.
    ENDIF.

    lo_controller = lcl_controller=>get_instance( ).

    IF lo_model->is_pallet( is_huhdr = <ls_src_hu_repack> ) = abap_true.

      " if source HU is a pallet then we should display only pallet types
      lo_controller->handle_call_repack_scr(
        EXPORTING
          iv_hutype_pall   = zcl_crud_ztcross_cart_type=>c_pall_type
        IMPORTING
          et_repack_matid     = DATA(lt_repack_matid)
          et_mapp_hutyp_matid = DATA(lt_mapp_hutyp_matid) ).

      " add info to pop-up display - TODO add this to static method
      LOOP AT lt_repack_matid ASSIGNING FIELD-SYMBOL(<ls_list_repackmatid>).
        ASSIGN lt_mapp_hutyp_matid[ matnr = <ls_list_repackmatid>-matnr ] TO FIELD-SYMBOL(<lv_hutype>).
        IF sy-subrc = 0.
          lv_hutyp_txt = <lv_hutype>-hutyp_txt.
        ENDIF.

        CLEAR ls_lbox_value.

        ls_lbox_value-key  = |{ <ls_list_repackmatid>-matnr ALPHA = OUT }|.
        ls_lbox_value-text = |{ lv_hutyp_txt }/{ <ls_list_repackmatid>-txt[ langu = sy-langu ]-maktx ALPHA = OUT }|.

        CLEAR lv_hutyp_txt.
        APPEND ls_lbox_value TO lt_lbox_values.
      ENDLOOP.

    ELSEIF lo_model->is_shipping_carton( is_huhdr = <ls_src_hu_repack> ) = abap_true.

      " if source HU is a shipHU then we should display  pallet types + ShipHu types
      lo_controller->handle_call_repack_scr(
        EXPORTING
           iv_hutype_pall   = zcl_crud_ztcross_cart_type=>c_pall_type
        IMPORTING
            et_repack_matid     = lt_repack_matid
            et_mapp_hutyp_matid = lt_mapp_hutyp_matid ).

      " add info to pop-up display - TODO add this to static method
      LOOP AT lt_repack_matid ASSIGNING <ls_list_repackmatid>.
        ASSIGN lt_mapp_hutyp_matid[ matnr = <ls_list_repackmatid>-matnr ] TO <lv_hutype>.
        IF sy-subrc = 0.
          lv_hutyp_txt = <lv_hutype>-hutyp_txt.
        ENDIF.

        CLEAR ls_lbox_value.

        ls_lbox_value-key  = |{ <ls_list_repackmatid>-matnr ALPHA = OUT }|.
        ls_lbox_value-text = |{ lv_hutyp_txt }/{ <ls_list_repackmatid>-txt[ langu = sy-langu ]-maktx ALPHA = OUT }|.

        CLEAR lv_hutyp_txt.
        APPEND ls_lbox_value TO lt_lbox_values.
      ENDLOOP.

      lo_controller->handle_call_repack_scr(
        EXPORTING
          iv_hutype_pall    = zcl_crud_ztcross_cart_type=>c_shipping_carton_type
        IMPORTING
          et_repack_matid     = lt_repack_matid
          et_mapp_hutyp_matid = lt_mapp_hutyp_matid ).

      " add info to pop-up display - TODO add this to static method
      LOOP AT lt_repack_matid ASSIGNING <ls_list_repackmatid>.
        ASSIGN lt_mapp_hutyp_matid[ matnr = <ls_list_repackmatid>-matnr ] TO <lv_hutype>.
        IF sy-subrc = 0.
          lv_hutyp_txt = <lv_hutype>-hutyp_txt.
        ENDIF.

        CLEAR ls_lbox_value.

        ls_lbox_value-key  = |{ <ls_list_repackmatid>-matnr ALPHA = OUT }|.
        ls_lbox_value-text = |{ lv_hutyp_txt }/{ <ls_list_repackmatid>-txt[ langu = sy-langu ]-maktx ALPHA = OUT }|.

        CLEAR lv_hutyp_txt.
        APPEND ls_lbox_value TO lt_lbox_values.
      ENDLOOP.

    ENDIF.

  ELSEIF  zstr_consolidation_scr_0300-newhu_pmat IS NOT INITIAL AND
          zstr_consolidation_scr_0300-desthu     IS  INITIAL.
    " user deleted the HU
    CLEAR lt_lbox_values.
    CLEAR zstr_consolidation_scr_0300-newhu_pmat.

  ENDIF.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = gc_lbox_pmat "  gc_lbox_hutyp
      values = lt_lbox_values.

  LOOP AT SCREEN.
    IF screen-name CS gc_lbox_pmat.
      IF zstr_consolidation_scr_0300-desthu IS INITIAL.
        screen-input = 0.
      ELSEIF zstr_consolidation_scr_0300-hutype IS INITIAL.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module PBO_0350 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_0350 OUTPUT.

****  DATA:
****    lv_hutyp_txt TYPE /scwm/de_desc40,
***    lt_thutyp    TYPE STANDARD TABLE OF /scwm/t307,
***    lt_thutypt   TYPE STANDARD TABLE OF /scwm/thutypt.

  CLEAR lt_lbox_values.
  " Andriyan Yordanov ---- start change
  IF zstr_consolidation_scr_0300-newhu_pmat IS NOT INITIAL.

    lo_controller = lcl_controller=>get_instance( ).

    lo_controller->handle_popup_conc_mathut(
      EXPORTING
        iv_humat       = zstr_consolidation_scr_0300-newhu_pmat
        iv_hutype      = zstr_consolidation_scr_0300-hutype
      IMPORTING
        et_lbox_values = lt_lbox_values ).

  ELSE.

    lo_controller = lcl_controller=>get_instance( ).

    lo_controller->handle_call_repack_scr(
      EXPORTING
        iv_packhu   = zstr_consolidation_scr_0300-packhu
      IMPORTING
        et_repack_matid     = lt_repack_matid
        et_mapp_hutyp_matid = lt_mapp_hutyp_matid ).

    CLEAR lv_hutyp_txt.

    LOOP AT lt_repack_matid ASSIGNING <ls_list_repackmatid>.
      ASSIGN lt_mapp_hutyp_matid[ matnr = <ls_list_repackmatid>-matnr ] TO <lv_hutype>.
      IF sy-subrc = 0.
        lv_hutyp_txt = <lv_hutype>-hutyp_txt.
      ENDIF.

      CLEAR ls_lbox_value.

      ls_lbox_value-key  = |{ <ls_list_repackmatid>-matnr ALPHA = OUT }|.
      ls_lbox_value-text = |{ lv_hutyp_txt }/{ <ls_list_repackmatid>-txt[ langu = sy-langu ]-maktx ALPHA = OUT }|.

      CLEAR lv_hutyp_txt.
      APPEND ls_lbox_value TO lt_lbox_values.
    ENDLOOP.

  ENDIF.

***  SELECT * FROM /scwm/t307 INTO TABLE lt_thutyp
***    WHERE hutypgrp = 'HGE1' OR hutypgrp = 'HGI1' OR hutypgrp = 'HGX1' OR hutypgrp = 'HGX2'.
***  IF sy-subrc <> 0.
***    RETURN.
***  ENDIF.

***  IF lt_thutyp IS NOT INITIAL.
***    SELECT * FROM /scwm/thutypt INTO TABLE lt_thutypt
***      FOR ALL ENTRIES IN lt_thutyp
***      WHERE hutyp = lt_thutyp-letyp.
***  ENDIF.
***
***  LOOP AT lt_thutyp INTO DATA(ls_thutyp).
***    CLEAR ls_lbox_value.
***    ls_lbox_value-key = |{ ls_thutyp-letyp ALPHA = OUT }|.
***
***    READ TABLE lt_thutypt INTO DATA(ls_thutypt)
***      WITH KEY hutyp = ls_thutyp-letyp.
***    IF sy-subrc <> 0.
***      CLEAR ls_thutypt-hutyptext.
***    ENDIF.
***
***    ls_lbox_value-text = ls_thutypt-hutyptext.
***    APPEND ls_lbox_value TO lt_lbox_values.
***  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = gc_lbox_pmat "  gc_lbox_hutyp
      values = lt_lbox_values.

  LOOP AT SCREEN.
    IF screen-name CS gc_lbox_pmat." 'HUTYP'.
      IF zstr_consolidation_scr_0300-desthu IS INITIAL.
        screen-input = 0.
      ELSEIF zstr_consolidation_scr_0300-hutype IS INITIAL.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.
