FUNCTION z_out_get_hu_from_quantity_ui .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_MATID) TYPE  /SCMB/MDL_MATID
*"     REFERENCE(IO_SP) TYPE REF TO  ZIF_OUTB_PACKING_WC_SP
*"  EXPORTING
*"     REFERENCE(EV_HUIDENT) TYPE  /SCWM/HUIDENT
*"     REFERENCE(EV_CANCELLED) TYPE  FLAG
*"----------------------------------------------------------------------
  CLEAR: ev_cancelled, ev_huident.
  CLEAR zstr_consolidation_scr_0410.

  go_enter_quan_popup = NEW #( ).

  go_enter_quan_popup->set_data( iv_matid = iv_matid io_sp = io_sp ).

  CALL SCREEN 0410 STARTING AT 10 10.
  IF go_enter_quan_popup->is_cancelled( ) EQ abap_true.
    ev_cancelled = abap_true.
    RETURN.
  ENDIF.
  ev_huident = go_enter_quan_popup->get_data( ).

ENDFUNCTION.
