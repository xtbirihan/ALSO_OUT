FUNCTION z_out_get_hu_ui .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(EV_HUIDENT) TYPE  /SCWM/HUIDENT
*"     REFERENCE(EV_CANCELLED) TYPE  FLAG
*"----------------------------------------------------------------------
  CLEAR: ev_cancelled, ev_huident.

  go_enter_hu_popup = NEW #( ).

  CALL SCREEN 2200 STARTING AT 10 10.
  IF go_enter_hu_popup->is_cancelled( ) EQ abap_true.
    ev_cancelled = abap_true.
    RETURN.
  ENDIF.
  ev_huident = go_enter_hu_popup->get_data( ).

ENDFUNCTION.
