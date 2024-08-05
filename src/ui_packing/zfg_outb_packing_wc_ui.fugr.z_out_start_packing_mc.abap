FUNCTION z_out_start_packing_mc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IO_SUB_CTRL) TYPE REF TO  ZIF_OUTB_WS_SUBSCR_UI
*"     REFERENCE(IO_MAIN_CTRL) TYPE REF TO
*"        ZIF_OUTB_PACKING_WORKCENTER_UI
*"  EXPORTING
*"     REFERENCE(EV_NEXT_PROD) TYPE  ZSTR_OUT_UI_COMMON-PRODUCT_EAN_MPN
*"  RAISING
*"      ZCX_WORKSTATION
*"----------------------------------------------------------------------
  CLEAR ev_next_prod.
  go_ws_main_ui = io_main_ctrl.
  go_ws_subscreen_ui = io_sub_ctrl.
  go_sc_ui ?= io_sub_ctrl.
  go_base_ui = go_sc_ui.

  go_sc_ui->init_data( ).
  gv_grp_verif = 'Destination Shipping Handling Unit'(des).
  go_ws_main_ui->init(
    EXPORTING
      io_subscreen_ui   = go_ws_subscreen_ui
  ).

  DATA(lv_main_screen) = go_ws_main_ui->get_main_screen_no( ).

  CALL SCREEN lv_main_screen.

  ev_next_prod = go_sc_ui->get_next_prod( ).

ENDFUNCTION.
