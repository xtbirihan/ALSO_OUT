FUNCTION Z_OUT_START_PACKING_SPLIT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IO_SUB_CTRL) TYPE REF TO  ZIF_OUTB_WS_SUBSCR_UI
*"     REFERENCE(IO_MAIN_CTRL) TYPE REF TO
*"        ZIF_OUTB_PACKING_WORKCENTER_UI
*"  EXPORTING
*"     REFERENCE(EV_NEXT_HU) TYPE  /SCWM/HUIDENT
*"  RAISING
*"      ZCX_WORKSTATION
*"----------------------------------------------------------------------

  go_ws_main_ui = io_main_ctrl.

  data(lo_sc_prev) = go_sc_ui.
  go_ws_subscreen_ui = io_sub_ctrl.
  go_sc_ui ?= io_sub_ctrl.
  go_base_ui = go_sc_ui.

  go_ws_main_ui->init(
    EXPORTING
      iv_keep_active    = abap_true
      io_subscreen_ui   = go_ws_subscreen_ui
  ).

  DATA(lv_main_screen) = go_ws_main_ui->get_main_screen_no( ).

  CALL SCREEN lv_main_screen.

  ev_next_hu = go_base_ui->get_next_hu( ).

  go_sc_ui = lo_sc_prev.
  go_ws_subscreen_ui = lo_sc_prev.
  go_sc_ui = lo_sc_prev.
  go_base_ui = lo_sc_prev.

ENDFUNCTION.
