interface ZIF_OUTB_PACKING_WC_SPO_UI
  public .


  interfaces ZIF_OUTB_PACKING_WC_BASE_UI .
  interfaces ZIF_OUTB_WS_SUBSCR_UI .

  methods INIT
    importing
      !IO_HU_CONTENT_CC type ref to CL_GUI_CUSTOM_CONTAINER
    changing
      !CO_HU_CONTENT_TABLE type ref to CL_SALV_TABLE
    raising
      ZCX_WORKSTATION .
  methods PBO_HU_ITEM
    exporting
      !ES_SCREEN_DATA type ZSTR_OUT_ITEM_TO_BE_PROC .
  methods PAI_HU_ITEM
    importing
      !IS_SCREEN_DATA type ZSTR_OUT_ITEM_TO_BE_PROC
    raising
      ZCX_WORKSTATION .
  methods INIT_DATA
    raising
      ZCX_WORKSTATION .
endinterface.
