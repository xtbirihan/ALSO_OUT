interface ZIF_OUTB_PACKING_WC_BASE_UI
  public .


  interfaces ZIF_OUTB_WS_SUBSCR_UI .

  constants C_FUNC_CONFIRM type SYUCOMM value 'CONFIRM' ##NO_TEXT.
  constants C_FUNC_OK type SYUCOMM value 'OK' ##NO_TEXT.
  constants C_FUNC_CANCEL type SYUCOMM value 'CANCEL' ##NO_TEXT.

  methods PBO_REQ_SNS_IDN
    exporting
      !ES_SCREEN_DATA type ZSTR_OUT_REQUESTED_SNS_IDN .
  methods PAI_REQ_SNS_IDN
    importing
      !IS_SCREEN_DATA type ZSTR_OUT_REQUESTED_SNS_IDN
    raising
      ZCX_WORKSTATION .
  methods GET_NEXT_HU
    returning
      value(RV_NEXT_HU) type /SCWM/HUIDENT .
endinterface.
