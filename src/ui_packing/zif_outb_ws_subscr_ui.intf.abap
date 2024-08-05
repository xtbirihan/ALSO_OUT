interface ZIF_OUTB_WS_SUBSCR_UI
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
      !EV_PROCESSED type ABAP_BOOL
    raising
      ZCX_WORKSTATION .
  methods INIT
    importing
      !IV_WITH_SN_IDN_CHECK type ABAP_BOOL optional
    exporting
      value(EV_SUBSCREEN_NO) type SYDYNNR
      !EV_SUBSCREEN_PRG type SY-REPID
      !ES_COMMON_DATA type ZSTR_OUT_UI_COMMON
    raising
      ZCX_WORKSTATION .
  methods PBO_TAB
    exporting
      !ES_SCREEN_DATA type DATA .
  methods PAI_TAB default ignore
    importing
      !IS_SCREEN_DATA type DATA
    raising
      ZCX_WORKSTATION .
  methods LEAVE_SCREEN default ignore
    returning
      value(RV_LEAVE) type ABAP_BOOL .
endinterface.
