interface ZIF_OUTB_PACKING_WC_SP
  public .


  types:
    tt_cap_nums TYPE STANDARD TABLE OF ztcross_cap_nums .
  types:
    BEGIN OF ty_quant_repack,
      stock_guid TYPE /lime/guid_stock,
      quant      TYPE /scwm/de_quantity,
      meins      TYPE meins,
    END OF ty_quant_repack .
  types:
    tt_quant_repack TYPE STANDARD TABLE OF ty_quant_repack WITH DEFAULT KEY .
  types:
    tt_term_def TYPE STANDARD TABLE OF zout_term_def WITH DEFAULT KEY .
  types:
    BEGIN OF ty_req_sns_idn_type,
      selnum      TYPE zde_selnum,
      description TYPE zde_number_description,
    END OF ty_req_sns_idn_type .
  types:
    tt_req_sns_idn_type TYPE STANDARD TABLE OF ty_req_sns_idn_type WITH DEFAULT KEY .
  types:
    tt_req_sns_idn TYPE STANDARD TABLE OF zstr_out_requested_sns_idn_t  WITH DEFAULT KEY
                   WITH NON-UNIQUE SORTED KEY docitm COMPONENTS docno itemno .
  types:
    tt_hu_content TYPE STANDARD TABLE OF zstr_out_ui_hu_cont WITH DEFAULT KEY .
  types:
    BEGIN OF ty_hu_with_quant,
      huident    TYPE /scwm/huident,
      quan       TYPE zde_qty_pc,
      meinh      TYPE meinh,
      quana      TYPE zde_qty_pc,
      altme      TYPE meins,
      dlv_cretst TYPE /scdl/dl_cretst,
    END OF ty_hu_with_quant .
  types:
    tt_hu_with_quant TYPE SORTED TABLE OF ty_hu_with_quant WITH NON-UNIQUE KEY huident meinh .

  constants C_SN_IDN_NOT_RELEVANT type CHAR01 value '-' ##NO_TEXT.

  methods ADJUST_QUANTITY
    importing
      !IV_DOCID type /SCDL/DL_DOCID
      !IV_ITMID type /SCDL/DL_ITEMID
      !IV_QTY type /SCWM/DE_QUANTITY
      !IV_MEINS type MEINS
    raising
      ZCX_WORKSTATION
      /SCWM/CX_SP .
  methods CANCEL_PICKING
    exporting
      !ET_BAPIRET type BAPIRET2_TAB
    raising
      ZCX_WORKSTATION .
  methods CLOSE_HU
    importing
      !IV_HU type /SCWM/HUIDENT
      !IV_HU_GUID type /SCWM/GUID_HU
    raising
      ZCX_WORKSTATION .
  methods CREATE_HU_IN_WC
    importing
      !IV_PMATID type /SCWM/DE_PMATID
      !IV_SOURCE_HU type /SCWM/HUIDENT
      !IV_DOCNO type /SCDL/DL_DOCNO optional
      !IT_STOCK_REPACK type TT_QUANT_REPACK
    exporting
      !EV_NEW_HU type /SCWM/HUIDENT
      !EV_NEW_HU_GUID type /SCWM/GUID_HU
      !ET_HUITM type /SCWM/TT_HUITM_INT
    raising
      ZCX_WORKSTATION .
  methods GET_HUS_FOR_PROD_BUFFER
    returning
      value(RT_HUS) type ZIF_OUTB_PACKING_WC_SP=>TT_HU_WITH_QUANT .
  methods DETERMINE_STAGE_AREA
    importing
      !IT_DOCID_ITEMID type /SCWM/DLV_DOCID_ITEM_TAB
    raising
      ZCX_WORKSTATION .
  methods IS_WORKCENTER_MASTERCARTON_REL
    returning
      value(RV_MC_REL) type ABAP_BOOL .
  methods GET_ENTITLED
    returning
      value(RV_ENTITLED) type /SCWM/DE_ENTITLED .
  methods GET_HU_CONTENT
    importing
      !IV_WITH_SN_IDN type ABAP_BOOL optional
    exporting
      !ET_DELIVERY_DATA type ZTT_OUT_UI_DELIVERY
      !ET_SN_IDN type TT_REQ_SNS_IDN
      !EV_PMATID type /SCWM/DE_MATID
    returning
      value(RT_HU_CONTENT) type TT_HU_CONTENT
    raising
      ZCX_WORKSTATION .
  methods GET_OPEN_WT_FOR_HU
    importing
      !IV_HUIDENT type /SCWM/HUIDENT
    returning
      value(RS_LTAP_MON) type /SCWM/S_TO_DET_MON .
  methods GET_PACKMAT
    importing
      !IT_MAT_QUANT type ZCL_CUBOID_ALGORITHM=>TT_MAT_CUBOID_INPUT
    exporting
      !EV_PMATID type /SCWM/DE_MATID
      !EV_PMAT type MATNR
      !ES_RESULT type ZCL_CUBOID_ALGORITHM=>TY_CUBOID_ALG_RESULT
    raising
      ZCX_WORKSTATION .
  methods GET_PACK_MAT_DATA
    importing
      !IV_PACK_MAT_ID type /SCMB/MDL_PMATID
    exporting
      !EV_PACK_MAT type /SCWM/DE_PMAT
      !EV_PACK_MAT_TEXT type MAKTX
      !EV_HUTYPE type /SCWM/DE_HUTYP
      !EV_HUTYPTEXT type /SCWM/DE_HUTYPT
    raising
      ZCX_WORKSTATION .
  methods GET_PROD_WEIGHT
    importing
      !IV_PRODID type /SCWM/DE_MATID
    returning
      value(RV_WEIGHT_IN_G) type /SCWM/DE_QUANTITY .
  methods GET_REQ_SNS_IDN_TYPE
    importing
      !IV_DOCID type /SCDL/DL_DOCID
      !IV_ITEMID type /SCDL/DL_ITEMID
    returning
      value(RT_REQ_SNS_IDN_TYPE) type TT_REQ_SNS_IDN_TYPE .
  methods GET_SOURCE_HU_GUID
    returning
      value(RV_GUID_HU) type /SCWM/GUID_HU .
  methods GET_TERMINAL_DEFAULTS
    returning
      value(RS_DEF) type ZOUT_TERM_DEF .
  methods GET_TERM_DEFAULTS_TABLE
    returning
      value(RT_TERM_DEF) type TT_TERM_DEF .
  methods GET_WEIGHTS
    exporting
      !EV_SCALE_HU_WEIGHT_IN_KG type ZDE_SCALE_HU_WEIGHT
      !EV_SYSTEM_HU_WEIGHT_IN_KG type ZDE_SCALE_HU_WEIGHT .
  methods GET_WEIGHT_DIFFERENCE
    importing
      !IV_OLD_PMATID type /SCWM/DE_PMATID
      !IV_NEW_PMATID type /SCWM/DE_PMATID
    returning
      value(RV_DIFF) type ZDE_SCALE_HU_WEIGHT
    raising
      ZCX_WORKSTATION .
  methods GET_WS_AND_HU
    exporting
      !EV_LGNUM type /SCWM/S_WRK_PACK-LGNUM
      !EV_WRKST type /SCWM/S_WRK_PACK-WORKSTATION
      !EV_LGPLA type /SCWM/LGPLA
      !EV_HUIDENT type /SCWM/DE_HUIDENT .
  methods INIT_WS
    importing
      !IV_LGNUM type /SCWM/S_WRK_PACK-LGNUM
      !IV_WRKST type /SCWM/S_WRK_PACK-WORKSTATION
      !IV_LGPLA type /SCWM/LGPLA
      !IV_HUIDENT type /SCWM/DE_HUIDENT
      !IV_MOVE_FROM_OTHER_DESK type ABAP_BOOL optional
      !IV_SCALE_WEIGHT_IN_KG type ZDE_SCALE_HU_WEIGHT
      !IV_PRODUCT_EAN_MPN type ZDE_INB_PRODUCT_EAN_MPN
    exporting
      !EV_PACKTYP type ZDE_PACKTYP
      !ET_BAPIRET type BAPIRET2_TAB
      !EV_PROCESSED_BY_OTHER_DESK type ABAP_BOOL
      !EV_WEIGHT_DIFF type ABAP_BOOL
      !EV_GET_HU_FOR_PROD type /SCMB/MDL_MATID
    raising
      ZCX_WORKSTATION .
  methods IS_WEIGHT_DIFFERENCE_EXCEEDED
    returning
      value(RV_EXCEEDED) type ABAP_BOOL .
  methods MOVE_HU
    importing
      !IV_HUIDENT type /SCWM/DE_HUIDENT
      !IV_LGPLA type /SCWM/LGPLA
    exporting
      !ET_BAPIRET type BAPIRET2_TAB
    raising
      ZCX_WORKSTATION .
  methods MOVE_HU_TO_LOST_AND_FOUND
    exporting
      !ET_BAPIRET type BAPIRET2_TAB
    raising
      ZCX_WORKSTATION .
  methods SAVE_REQ_SNS_IDN
    importing
      !IV_DOCNO type /SCDL/DL_DOCNO
      !IV_ITEMNO type /SCDL/DL_ITEMNO
      !IT_REQ_SNS_IDN_TYPE type TT_REQ_SNS_IDN
      !IV_NEW_GUID_HU type /SCWM/GUID_HU
      !IV_COMPLETE_OVERWRITE type ABAP_BOOL default ABAP_TRUE
      !IV_OLD_GUID_HU type /SCWM/GUID_HU optional .
  methods SET_HU_CONTENT_TEXT
    changing
      !CS_HU_CONT type ZSTR_OUT_UI_HU_CONT .
  methods SET_WORKSTATION
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_WRKST type /SCWM/DE_WORKSTATION
    raising
      ZCX_WORKSTATION .
  methods UNLOCK_HU
    raising
      ZCX_WORKSTATION .
  methods UPDATE_WEIGHT_IN_HU
    importing
      !IV_GUID_HU type /SCWM/GUID_HU
      !IV_SCALE_HU_WEIGHT type ZDE_SCALE_HU_WEIGHT
      !IV_SCALE_HU_MEINS type MEINS
      !IV_USE_STANDARD_WEIGHT type ABAP_BOOL optional
    raising
      ZCX_WORKSTATION .
  methods CHECK_SNS_IDN_IN_DB
    importing
      !IV_DOCNO type /SCDL/DL_DOCNO
      !IV_ITEMNO type /SCDL/DL_ITEMNO
      !IT_REQ_SNS_IDN_TYPE type TT_REQ_SNS_IDN
      !IV_OLD_GUID_HU type /SCWM/GUID_HU optional
    raising
      ZCX_WORKSTATION .
  methods IS_HU_KEP
    importing
      !IV_HUIDENT type /SCWM/HUIDENT
    returning
      value(RV_KEP) type ABAP_BOOL .
  methods GET_HUS_FOR_PROD
    importing
      !IV_PRODUCT_EAN_MPN type ZDE_INB_PRODUCT_EAN_MPN
    exporting
      !ET_HUS type TT_HU_WITH_QUANT
      !EV_MATNR type MATNR
      !EV_MATID type MARA-SCM_MATID_GUID16
    raising
      ZCX_WORKSTATION .
  methods GET_PICK_HU_PACKMAT
    exporting
      !EV_PMATID type /SCWM/DE_MATID
      !EV_PMAT type MATNR .
endinterface.
