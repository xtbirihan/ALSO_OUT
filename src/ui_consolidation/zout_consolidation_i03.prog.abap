**********************************************************************
*& Key           : AMOGYORODI-230814
*& Request No.   :
**********************************************************************
*& Description (short)
*&
*&
**********************************************************************
*----------------------------------------------------------------------*
***INCLUDE ZOUT_CONSOLIDATION_I03.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  lcl_controller=>get_instance( )->user_command_0300( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0310  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0310 INPUT.
  CASE sy-ucomm.
    WHEN 'OK' OR 'ENTER' OR 'SKIP' OR 'CANC' OR 'CANCEL' OR 'LOGOUT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

MODULE user_command_0320 INPUT.
  CASE sy-ucomm.
    WHEN 'OK' OR 'ENTER' OR 'CANC' OR 'CANCEL' OR 'LOGOUT'.
      LEAVE TO SCREEN 0.
    WHEN 'REFRESH'.
      SELECT SINGLE * FROM /scwm/huhdr INTO @DATA(ls_huhdr_db)
        WHERE huident = @zstr_consolidation_scr_0300-packhu.
      IF sy-subrc = 0.
        zstr_consolidation_scr_0300-weight = ls_huhdr_db-g_weight.
        zstr_consolidation_scr_0300-length_uom = ls_huhdr_db-unit_lwh.
        zstr_consolidation_scr_0300-height_uom = ls_huhdr_db-unit_lwh.
        zstr_consolidation_scr_0300-widht_uom = ls_huhdr_db-unit_lwh.
        zstr_consolidation_scr_0300-weight_uom = ls_huhdr_db-unit_gw.
      ENDIF.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0330  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0330 INPUT.
  CASE sy-ucomm.
    WHEN 'OK' OR 'ENTER' OR 'CANC' OR 'CANCEL' OR 'LOGOUT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0340  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0340 INPUT.

  DATA:
    lv_huident     TYPE /scwm/de_huident,
    ls_huheader    TYPE /scwm/s_huhdr_int,
***    ls_huheader_src TYPE /scwm/s_huhdr_int,
    lt_huref_merge TYPE /scwm/tt_huref_int.

  CASE sy-ucomm.
    WHEN 'OK' OR 'ENTER'.

      IF zstr_consolidation_scr_0300-hutype IS NOT INITIAL AND
         zstr_consolidation_scr_0300-desthu IS NOT INITIAL.
        LEAVE TO SCREEN 0.
      ENDIF.

      IF zstr_consolidation_scr_0300-desthu IS NOT INITIAL AND
         zstr_consolidation_scr_0300-hutype IS INITIAL.

        CLEAR: lt_huref_merge,
               ls_huheader.

        lv_huident = |{ zstr_consolidation_scr_0300-desthu ALPHA = IN }|.

        CALL FUNCTION '/SCWM/HUHEADER_READ'
          EXPORTING
            iv_appl     = wmegc_huappl_wme
            iv_huident  = lv_huident
          IMPORTING
            es_huheader = ls_huheader "ls_huheader_src
            et_huref    = lt_huref_merge
          EXCEPTIONS
            OTHERS      = 1.

        IF sy-subrc <> 0 OR
           ls_huheader IS INITIAL.
          RETURN.
        ELSE.

          " user scanned the existing HU start validation
          IF NOT line_exists( lt_huref_merge[ docid = zstr_consolidation_scr_0300-docid ] ). " Andriyan Yordanov check if HU is part of the doc.

            CLEAR: zstr_consolidation_scr_0300-desthu.

            LOOP AT SCREEN.
              IF screen-name CS 'DESTHU'.
                screen-input = 1.
                MODIFY SCREEN.
              ENDIF.
            ENDLOOP.

            "Scanned HU exists and is not part of current document &1
            MESSAGE e013(zewm_cl_msg_con) WITH   zstr_consolidation_scr_0300-docno.
          ENDIF.

          lo_model = lcl_model=>get_instance( ).

          ASSIGN lo_model->mt_cons_huhdr[ huident = zstr_consolidation_scr_0300-packhu ] TO FIELD-SYMBOL(<ls_src_hu>).

          IF sy-subrc = 0.
            " check destination type
            IF lo_model->is_shipping_carton( is_huhdr = ls_huheader ) = abap_true AND
               lo_model->is_pallet( is_huhdr = <ls_src_hu> ) = abap_true.

              LOOP AT SCREEN.
                IF screen-name CS 'DESTHU'.
                  screen-input = 1.
                  MODIFY SCREEN.
                ENDIF.
              ENDLOOP.

              CLEAR: zstr_consolidation_scr_0300-desthu.

              MESSAGE e016(zewm_cl_msg_con) WITH |{ <ls_src_hu>-huident ALPHA = OUT }|
                                                 |{ ls_huheader-huident ALPHA = OUT }|.
            ENDIF.
          ENDIF.

          " all good of the existing HU and we can do merge
          LOOP AT SCREEN.
            IF screen-name CS 'NEWHU_PMAT'.
              screen-input = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.

          zstr_consolidation_scr_0300-hutype = ls_huheader-letyp.

          NEW /scwm/cl_ui_stock_fields( )->get_matkey_by_id(
            EXPORTING
              iv_matid = ls_huheader-pmat_guid                 " Material GUIDs with Conversion Exit
            IMPORTING
              ev_matnr = zstr_consolidation_scr_0300-newhu_pmat ).                " Material Number

        ENDIF.
      ENDIF.

    WHEN 'CANC' OR 'CANCEL' OR 'LOGOUT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0350  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0350 INPUT.

  DATA: lt_huref        TYPE  /scwm/tt_huref_int,
        lv_src_hu       TYPE /scwm/de_huident,
        ls_huheader_src TYPE /scwm/s_huhdr_int,
        lv_huident_dest TYPE /scwm/de_huident,
        lt_huhdr_int    TYPE /scwm/tt_huhdr_int.

  CASE sy-ucomm.
    WHEN 'OK' OR 'ENTER'.

      IF zstr_consolidation_scr_0300-desthu IS INITIAL.
        RETURN.
      ENDIF.

      CLEAR: lv_huident,
             lv_src_hu,
             ls_huheader_src,
             ls_huheader,
             lt_huref.

      lv_huident_dest = |{ zstr_consolidation_scr_0300-desthu ALPHA = IN }|.
      lv_src_hu  = |{ zstr_consolidation_scr_0300-packhu ALPHA = IN }|.

      CALL FUNCTION '/SCWM/HUHEADER_READ'
        EXPORTING
          iv_appl     = wmegc_huappl_wme
          iv_huident  = lv_src_hu
        IMPORTING
          es_huheader = ls_huheader_src
          et_huref    = lt_huref
        EXCEPTIONS
          OTHERS      = 1.

      CALL FUNCTION '/SCWM/HUMAIN_REFRESH'.

      CALL FUNCTION '/SCWM/HU_READ'
        EXPORTING
          iv_appl    = wmegc_huappl_wme
          iv_huident = lv_huident_dest
        IMPORTING
          es_huhdr   = ls_huheader
          et_huhdr   = lt_huhdr_int
          et_huref   = lt_huref
        EXCEPTIONS
          error      = 1
          OTHERS     = 2.

      IF sy-subrc <> 0 OR
         ls_huheader IS INITIAL.

        IF zstr_consolidation_scr_0300-newhu_pmat IS INITIAL.
          RETURN.
        ENDIF.

      ELSE.

        " Andriyan Yordanov
        IF ls_huheader-copst = abap_true OR
           ls_huheader-procs IS INITIAL.
          LOOP AT SCREEN.
            IF screen-name CS 'DESTHU'.
              screen-input = 1.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.

          CLEAR zstr_consolidation_scr_0300-desthu.
          "HU already closed. Action not possible
          MESSAGE s011(zewm_cl_msg_con) DISPLAY LIKE wmegc_severity_err.
          RETURN.
        ENDIF.

        lo_model = lcl_model=>get_instance( ).

        IF NOT line_exists( lt_huref[ docid = zstr_consolidation_scr_0300-docid ] ) AND
              lo_model->is_pallet( is_huhdr = ls_huheader ) = abap_true. " Andriyan Yordanov check if HU is part of the doc.

          LOOP AT SCREEN.
            IF screen-name CS 'DESTHU'.
              screen-input = 1.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.

          CLEAR zstr_consolidation_scr_0300-desthu.
          "Scanned HU exists and is not part of current document &1
          MESSAGE s013(zewm_cl_msg_con) WITH   zstr_consolidation_scr_0300-docno  DISPLAY LIKE wmegc_severity_err.
          RETURN.
        ENDIF.

        IF lo_model->is_shipping_carton( is_huhdr = ls_huheader ) AND
           lo_model->is_pallet( is_huhdr = ls_huheader_src ) AND
           NOT line_exists( lt_huhdr_int[ huident = ls_huheader_src-huident ] ).

          LOOP AT SCREEN.
            IF screen-name CS 'DESTHU'.
              screen-input = 1.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.

          CLEAR zstr_consolidation_scr_0300-desthu.
          " Scanned HU is a shipping carton and is not part from the scanned pallet
          MESSAGE s019(zewm_cl_msg_con) DISPLAY LIKE wmegc_severity_err.
          RETURN.
        ENDIF.

        zstr_consolidation_scr_0300-hutype = ls_huheader-letyp.

        NEW /scwm/cl_ui_stock_fields( )->get_matkey_by_id(
          EXPORTING
            iv_matid = ls_huheader-pmat_guid                 " Material GUIDs with Conversion Exit
          IMPORTING
            ev_matnr = zstr_consolidation_scr_0300-newhu_pmat ).                " Material Number
***        zstr_consolidation_scr_0300-newhu_pmat = ls_huheader-pmat_guid.
      ENDIF.

      LEAVE TO SCREEN 0.
    WHEN 'CANC' OR 'CANCEL' OR 'LOGOUT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_370  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0370 INPUT.
  CASE sy-ucomm.
    WHEN 'CANC' OR 'CANCEL' OR 'LOGOUT' OR 'BACK'.
      LEAVE TO SCREEN 300.
    WHEN 'OK'.
      lo_controller = lcl_controller=>get_instance( ).
      lo_controller->handle_full_repack( iv_huident_src = zstr_consolidation_scr_0300-packhu
                                         iv_huident_new = zstr_consolidation_scr_0300-newhu
                                         iv_pmatnr      = zstr_consolidation_scr_0300-newhu_pmat ).
    WHEN 'REFRESH'.
      CALL SCREEN 0370.

    WHEN 'ENTER'.

      IF zstr_consolidation_scr_0300-newhu_pmat IS INITIAL.
        SET CURSOR FIELD gc_lbox_pmat.
      ELSEIF zstr_consolidation_scr_0300-newhu_pmat IS NOT INITIAL AND
            zstr_consolidation_scr_0300-newhu IS INITIAL.
        SET CURSOR FIELD gc_lbox_newhu.
      ENDIF.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module LISTBOX_0370 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE listbox_0370 OUTPUT.

  CLEAR lt_lbox_values.
  " Andriyan Yordanov - create list with posible packmat
  lo_controller = lcl_controller=>get_instance( ).

  lo_controller->handle_call_repack_scr(
    EXPORTING
      iv_packhu         = zstr_consolidation_scr_0300-packhu
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
***    ls_lbox_value-text = <ls_list_repackmatid>-txt.
    CLEAR lv_hutyp_txt.
    APPEND ls_lbox_value TO lt_lbox_values.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = gc_lbox_pmat
      values = lt_lbox_values.

  " End Logic Andriyan Yordanov

****  SELECT * FROM /scwm/thutypt INTO TABLE @DATA(lt_thtypt)
****    WHERE spras = @sy-langu.
****  IF sy-subrc <> 0.
****    RETURN.
****  ENDIF.
****
****  LOOP AT lt_thtypt INTO DATA(ls_thtypt).
****    CLEAR ls_lbox_value.
****    ls_lbox_value-key = |{ ls_thtypt-hutyp ALPHA = OUT }|.
****    ls_lbox_value-text = |{ ls_thtypt-hutyptext ALPHA = OUT }|.
****    APPEND ls_lbox_value TO lt_lbox_values.
****  ENDLOOP.
****
****  CALL FUNCTION 'VRM_SET_VALUES'
****    EXPORTING
****      id     = gc_lbox_hutyp
****      values = lt_lbox_values.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module PBO_0380 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_0380 OUTPUT.

  DATA(lo_cont_popup_pack_inst) = NEW cl_gui_custom_container( container_name = 'CTRL_PACKING_POPUP_INST').
  lo_view->show_pack_inst( iv_dlv_level     = abap_false
                           iv_product_level = abap_true
                           io_container     = lo_cont_popup_pack_inst ).

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module user_command_0380
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE user_command_0380.

  CASE sy-ucomm.
    WHEN 'OK' OR 'ENTER' OR 'CANC' OR 'CANCEL' OR 'LOGOUT'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module PBO_0410 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_0410 OUTPUT.

  IF zstr_consolidation_scr_0410-matnr_txt IS INITIAL.

    NEW /scwm/cl_ui_stock_fields( )->get_matkey_by_id(
      EXPORTING
        iv_matid = zstr_consolidation_scr_0410-matid         " Material GUIDs with Conversion Exit
      IMPORTING
        ev_matnr = zstr_consolidation_scr_0410-matnr         " Material Number
        ev_maktx = zstr_consolidation_scr_0410-matnr_txt ).  " Material Description

    lo_model = lcl_model=>get_instance( ).

    ASSIGN lo_model->mt_cons_hu_content[ matid = zstr_consolidation_scr_0410-matid ]-uom TO FIELD-SYMBOL(<lv_uom_matnr>).

    IF sy-subrc = 0.
      zstr_consolidation_scr_0410-meins = <lv_uom_matnr>.
    ENDIF.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0410  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0410 INPUT.
  lcl_controller=>get_instance( )->user_command_0410( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module PBO_0410 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_0420 OUTPUT.

  IF zstr_consolidation_scr_0400-serial_number IS NOT INITIAL.
    CLEAR zstr_consolidation_scr_0400-serial_number.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0410  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0420 INPUT.
  lcl_controller=>get_instance( )->user_command_0420( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module PBO_0610 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_0610 OUTPUT.

  IF zstr_consolidation_scr_0600-vendor_huident IS NOT INITIAL.
    CLEAR zstr_consolidation_scr_0600-vendor_huident.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0610  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0610 INPUT.
  lcl_controller=>get_instance( )->user_command_0610( ).
ENDMODULE.
