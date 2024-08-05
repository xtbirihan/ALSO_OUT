interface ZIF_OUTB_PACKING_WC_SC_UI
  public .


  interfaces ZIF_OUTB_PACKING_WC_BASE_UI .
  interfaces ZIF_OUTB_WS_SUBSCR_UI .

  methods INIT_NO_W_DIFF
    importing
      !IO_HU_CONTENT_CC type ref to CL_GUI_CUSTOM_CONTAINER
      !IO_CAPTURED_SN_IDN_CC type ref to CL_GUI_CUSTOM_CONTAINER
    changing
      !CO_HU_CONTENT_TABLE type ref to CL_SALV_TABLE
      !CO_CAPTURED_SN_IDN_TABLE type ref to CL_SALV_TABLE
    raising
      ZCX_WORKSTATION .
  methods INIT_DATA
    raising
      ZCX_WORKSTATION .
  methods PBO_PACK
    importing
      !IO_CC_PACKING_INSTR type ref to CL_GUI_CUSTOM_CONTAINER
    exporting
      !ES_SCREEN_DATA type ZSTR_OUT_UI_PACK_DATA .
  methods PAI_PACK
    importing
      !IS_SCREEN_DATA type ZSTR_OUT_UI_PACK_DATA
    raising
      ZCX_WORKSTATION .
  methods INIT_W_DIFF
    importing
      !IO_HU_CONTENT_CC type ref to CL_GUI_CUSTOM_CONTAINER
      !IO_CAPTURED_SN_IDN_CC type ref to CL_GUI_CUSTOM_CONTAINER
      !IO_VERIFIED_ITEMS_CC type ref to CL_GUI_CUSTOM_CONTAINER
    changing
      !CO_HU_CONTENT_TABLE type ref to CL_SALV_TABLE
      !CO_CAPTURED_SN_IDN_TABLE type ref to CL_SALV_TABLE
      !CO_VERIFIED_ITEMS_TABLE type ref to CL_SALV_TABLE
    raising
      ZCX_WORKSTATION .
endinterface.
