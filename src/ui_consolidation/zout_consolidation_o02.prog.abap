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
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'STANDARD'.
  SET TITLEBAR 'TITLE_0100'.
ENDMODULE.

MODULE pbo_0200 OUTPUT.

  lcl_controller=>get_instance( )->refresh_screen_0200( ).

  DATA(lo_container_hu) = NEW cl_gui_custom_container( container_name = 'CONT_HU_CONS' ).
  DATA(lo_view) = lcl_view=>get_instance( ).
  lo_view->show_alv_cons_hus( lo_container_hu ).

  DATA(lo_container_pack_inst) = NEW cl_gui_custom_container( container_name = 'CTRL_PACKING_INST').
  lo_view->show_pack_inst( iv_dlv_level = abap_true
                           io_container = lo_container_pack_inst ).

  DATA(lo_model) = lcl_model=>get_instance( ).
  lo_model->build_pick_table( iv_docid = zstr_consolidation_scr_0200-docid ).
  IF lo_model->mt_open_picking_hu IS INITIAL.
    LOOP AT SCREEN.
      IF screen-name CS 'PICKING_BTN'.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF lo_model->ms_cons_header-status_pick = TEXT-011.
    zstr_consolidation_scr_0200-picking_info = TEXT-014.
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off(
      EXPORTING
        iv_screlm_name = 'ZSTR_CONSOLIDATION_SCR_0200-PICKING_INFO' ).
    LOOP AT SCREEN.
      IF screen-name CS 'PICKING_BTN'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ELSE.
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
      EXPORTING
        iv_screlm_name = 'ZSTR_CONSOLIDATION_SCR_0200-PICKING_INFO' ).
    LOOP AT SCREEN.
      IF screen-name CS 'PICKING_BTN'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module LISTBOX_0220 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE listbox_0220 OUTPUT.

  DATA:
    lv_lbox_name   TYPE vrm_id,
    lv_partner     TYPE bu_partner,
    lv_descr       TYPE bu_descrip,
    lt_lbox_values TYPE vrm_values,
    ls_lbox_value  LIKE LINE OF lt_lbox_values,
    lt_carrier     TYPE STANDARD TABLE OF ztout_ship_calc.

  CLEAR lt_lbox_values.

  SELECT * FROM ztout_ship_calc INTO TABLE lt_carrier
    WHERE "lgnum = lcl_controller=>sv_lgnum AND
          docno = zstr_consolidation_scr_0200-docno.
  IF sy-subrc <> 0.
    "TODO: call interface. Add dummy data now
    APPEND VALUE #( "lgnum = lcl_controller=>sv_lgnum
                    docno = zstr_consolidation_scr_0200-docno
                    counter = 1
                    bupartner = '10080044' ) TO lt_carrier.
  ENDIF.

  LOOP AT lt_carrier INTO DATA(ls_carrier).
    CLEAR ls_lbox_value.
    ls_lbox_value-key = |{ ls_carrier-bupartner ALPHA = OUT }|.
    ls_lbox_value-key = |{ ls_lbox_value-key ALPHA = IN }|.

    CLEAR: lv_partner, lv_descr.
    lv_partner = |{ ls_carrier-bupartner ALPHA = IN }|.

    CALL FUNCTION 'BUPA_DESCRIPTION_READ'
      EXPORTING
        iv_partner     = lv_partner
      IMPORTING
        ev_description = lv_descr
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      CLEAR lv_descr.
    ENDIF.

    ls_lbox_value-text = lv_descr.
    APPEND ls_lbox_value TO lt_lbox_values.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = gc_lbox_carrier
      values = lt_lbox_values.

ENDMODULE.
