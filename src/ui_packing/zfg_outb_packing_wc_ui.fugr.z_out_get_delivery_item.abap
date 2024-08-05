FUNCTION Z_OUT_GET_DELIVERY_ITEM.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(EV_CANCELLED) TYPE  FLAG
*"  CHANGING
*"     REFERENCE(CT_DELIVERY_ITEM) TYPE  ZTT_OUT_UI_DELIVERY
*"----------------------------------------------------------------------

  go_get_deliv_screen = NEW lcl_get_delivery_screen( ).

  go_get_deliv_screen->set_data( ct_delivery_item ).

  CALL SCREEN '2300' STARTING AT 10 10.

  ct_delivery_item = go_get_deliv_screen->get_selected_data( ).
  ev_cancelled = go_get_deliv_screen->is_cancelled( ).
ENDFUNCTION.
