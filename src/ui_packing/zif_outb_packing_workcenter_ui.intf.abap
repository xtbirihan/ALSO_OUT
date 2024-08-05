interface ZIF_OUTB_PACKING_WORKCENTER_UI
  public .


  methods GET_STATUS
    exporting
      !EV_STATUS type STRING
      !ET_EXCLUDES type STRING_TABLE .
  methods GET_TITLE
    exporting
      !EV_TITLE type SYTITLE
      !EV_PARAM type STRING .
  methods PROCESS_USER_COMMAND
    importing
      !IV_UCOMM type SYUCOMM
    exporting
      value(ES_BAPIRET) type BAPIRET2
      value(EV_LEAVE_SCREEN) type ABAP_BOOL
    raising
      ZCX_WORKSTATION .
  methods PAI
    raising
      ZCX_WORKSTATION .
  methods PBO
    exporting
      value(EV_TAB_SUBSCREEN) type SY-DYNNR
      value(EV_TAB_REPID) type SYREPID
      value(ES_SCREEN_DATA) type ZSTR_OUT_UI_COMMON .
  methods INIT
    importing
      !IO_SUBSCREEN_UI type ref to ZIF_OUTB_WS_SUBSCR_UI
      !IV_KEEP_ACTIVE type ABAP_BOOL optional
    exporting
      value(EV_DEFAULT_NEEDED) type ABAP_BOOL .
  methods GET_MAIN_SCREEN_NO
    returning
      value(RV_DYNNR) type SY-DYNNR .
  methods GET_SUBSCREEN_CTRL
    returning
      value(RO_SUBSCR) type ref to ZIF_OUTB_WS_SUBSCR_UI .
endinterface.
