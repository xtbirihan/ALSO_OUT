class ZCL_OUTB_PACKING_WC_SPO_UI definition
  public
  inheriting from ZCL_OUTB_PACKING_WC_BASE_UI
  final
  create private .

public section.

  interfaces ZIF_OUTB_PACKING_WC_SPO_UI .

  constants C_SUBSCREEN_REPID type SYREPID value 'SAPLZFG_OUTB_PACKING_WC_UI' ##NO_TEXT.
  constants C_SUBSREEN type SY-DYNNR value '3000' ##NO_TEXT.
  data MS_ITEM_TO_BE_PROC type ZSTR_OUT_ITEM_TO_BE_PROC .
  constants C_TITLE type STRING value 'SPO' ##NO_TEXT.

  class-methods CREATE_INSTANCE
    importing
      !IO_SP type ref to ZIF_OUTB_PACKING_WC_SP
    returning
      value(RO_INST) type ref to ZIF_OUTB_WS_SUBSCR_UI .
  methods CONSTRUCTOR
    importing
      !IV_LGNUM type /SCWM/S_WRK_PACK-LGNUM
      !IV_WRKST type /SCWM/S_WRK_PACK-WORKSTATION
      !IV_LGPLA type /SCWM/LGPLA
      !IV_HUIDENT type /SCWM/DE_HUIDENT .

  methods ZIF_OUTB_WS_SUBSCR_UI~GET_TITLE
    redefinition .
  methods ZIF_OUTB_WS_SUBSCR_UI~PROCESS_USER_COMMAND
    redefinition .
protected section.

  methods SET_HU_CONTENT_COLS
    redefinition .
  methods ADD_SERIDS
    redefinition .
private section.

  methods FINISH_PROCESS
    raising
      ZCX_WORKSTATION .
  methods PRODUCT_CHANGED
    importing
      !IV_PRODUCT_EAN_MPN type ZDE_INB_PRODUCT_EAN_MPN
    raising
      ZCX_WORKSTATION .
ENDCLASS.



CLASS ZCL_OUTB_PACKING_WC_SPO_UI IMPLEMENTATION.


  METHOD add_serids.
    TRY.
        super->add_serids(
          EXPORTING
            is_screen_data = is_screen_data
          RECEIVING
            rv_ok          = rv_ok ).
        IF rv_ok EQ abap_true.
          IF mt_req_sns_idn IS NOT INITIAL.
            finish_process( ).
          ENDIF.
        ENDIF.
      CATCH zcx_workstation INTO DATA(lx_wc).
        CLEAR: mt_req_sns_idn, ms_req_sns_idn, mt_req_sns_idn_type, mv_hu_content_current_idx, ms_hu_content_current.
        CLEAR ms_item_to_be_proc-product_ean_mpn.
        RAISE EXCEPTION lx_wc.
    ENDTRY.
  ENDMETHOD.


  METHOD constructor.
    super->constructor(
      EXPORTING
        iv_lgnum   = iv_lgnum                    " Warehouse Number/Warehouse Complex
        iv_wrkst   = iv_wrkst                   " Work Center
        iv_lgpla   = iv_lgpla                   " Storage Bin
        iv_huident = iv_huident                 " Handling Unit Identification
    ).
    mv_subscreen = c_subsreen.
    mv_subscreen_prg = c_subscreen_repid.
  ENDMETHOD.


  METHOD create_instance.
    io_sp->get_ws_and_hu(
      IMPORTING
        ev_lgnum   = DATA(lv_lgnum)
        ev_wrkst   = DATA(lv_wrkst)
        ev_lgpla   = DATA(lv_lgpla)
        ev_huident = DATA(lv_huident)
    ).
    DATA(lo_inst) = NEW zcl_outb_packing_wc_spo_ui(
                          iv_lgnum   = lv_lgnum
                          iv_wrkst   = lv_wrkst
                          iv_lgpla   = lv_lgpla
                          iv_huident = lv_huident
                        ).
    lo_inst->mo_sp = io_sp.
    ro_inst = lo_inst.
  ENDMETHOD.


  METHOD finish_process.
    CLEAR: mv_new_guid_hu, mv_new_hu.

    mo_sp->get_packmat(
      EXPORTING
        it_mat_quant = VALUE #( ( matid  = NEW /scwm/cl_ui_stock_fields( )->get_matid_by_no( iv_matnr =  ms_hu_content_current-matnr  )                " Product
                                  quan   = ms_hu_content_current-quan
                                  meins  = ms_hu_content_current-meins ) )
      IMPORTING
        ev_pmatid = DATA(lv_pmatid)                 " Product
    ).

    mo_sp->create_hu_in_wc(
      EXPORTING
        iv_pmatid = lv_pmatid
        iv_source_hu  = mv_huident                 " Handling Unit Identification
        iv_docno      = ms_hu_content_current-docno                 " Document Number
        it_stock_repack = VALUE #( ( stock_guid = ms_hu_content_current-stock_guid ) )                 " GUID Stock Item
      IMPORTING
        ev_new_hu     = mv_new_hu
        ev_new_hu_guid = mv_new_guid_hu
    ).

    DELETE mt_hu_content INDEX mv_hu_content_current_idx.

    IF mo_hu_content_table IS BOUND.
      mo_hu_content_table->refresh(
        EXPORTING
          s_stable     = VALUE #( row = abap_true col = abap_true )
      ).
    ENDIF.


    NEW /scwm/cl_dlv_management_prd( )->det_pickloc_stag_door(
      EXPORTING
        iv_whno                    = mv_lgnum                 " Warehouse Number/Warehouse Complex
        it_docid_itemid            = VALUE #( ( doccat = wmegc_doccat_pdo docid = ms_hu_content_current-docid itemid = ms_hu_content_current-itemid ) )                 " Document Identification with ID
      IMPORTING
        eo_message                 = DATA(lo_dlv_msg)                 " Messages for Delivery Processing with Number Support
    ).
    DATA(lt_dlv_err) = lo_dlv_msg->get_messages( iv_msgty = wmegc_severity_err ).
    IF lt_dlv_err IS NOT INITIAL.
      MESSAGE w006(zmc_out_ui_packing).
    ENDIF.

    save_sns_idn( ).

    TRY.
        mo_sp->close_hu(
          EXPORTING
            iv_hu      = mv_new_hu                 " Unique Internal Identification of a Handling Unit
            iv_hu_guid = mv_new_guid_hu                 " Unique Internal Identification of a Handling Unit
        ).
      CATCH zcx_workstation INTO data(lx_ws).
        RAISE EXCEPTION lx_ws.
    ENDTRY.

    determine_and_call_fin_steps( ).

    clear_data_when_prod_empty( ).
  ENDMETHOD.


  METHOD product_changed.
    DATA: lv_matnr TYPE matnr.

    clear_data_when_prod_empty( ).

    lv_matnr = get_prod_from_ean_mprn_matnr( iv_product_ean_mpn ).

    READ TABLE mt_hu_content INTO ms_hu_content_current
         WITH KEY matnr = lv_matnr.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e005(zmc_out_ui_packing) WITH iv_product_ean_mpn.
    ENDIF.
    mv_hu_content_current_idx = sy-tabix.

    CLEAR mt_req_sns_idn_type.
    mt_req_sns_idn_type = mo_sp->get_req_sns_idn_type(
                              iv_docid  = ms_hu_content_current-docid
                              iv_itemid = ms_hu_content_current-itemid
                          ).

    ms_item_to_be_proc-product_ean_mpn = iv_product_ean_mpn.

    IF mt_req_sns_idn_type IS NOT INITIAL.
      fill_req_sns_idn_type( ).
    ELSE.
      finish_process( ).
    ENDIF.

  ENDMETHOD.


  method SET_HU_CONTENT_COLS.
    CHECK mo_hu_content_table IS BOUND.

    DATA(lo_columns) = mo_hu_content_table->get_columns( ).
    TRY.
        lo_columns->get_column( columnname = c_field-sn_idn )->set_technical( ).
      CATCH cx_salv_not_found ##no_handler. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.
  endmethod.


  METHOD zif_outb_packing_wc_spo_ui~init.

    IF co_hu_content_table IS NOT BOUND.
      init_hu_content( io_hu_content_cc = io_hu_content_cc ).
      co_hu_content_table = mo_hu_content_table.
    ELSEIF mo_hu_content_table IS NOT BOUND.
      mo_hu_content_table = co_hu_content_table.
      TRY.
          mo_hu_content_table->set_data(
            CHANGING
              t_table = mt_hu_content
          ).
        CATCH cx_salv_no_new_data_allowed ##no_handler.

      ENDTRY.
      mo_hu_content_table->refresh( ).
    ENDIF.


  ENDMETHOD.


  method ZIF_OUTB_PACKING_WC_SPO_UI~INIT_DATA.
    mt_hu_content_original = mo_sp->get_hu_content( ).
    mt_hu_content = mt_hu_content_original.

    SORT mt_hu_content BY matnr docno.
  endmethod.


  METHOD zif_outb_packing_wc_spo_ui~pai_hu_item.
    IF is_screen_data-product_ean_mpn NE ms_item_to_be_proc-product_ean_mpn.
      product_changed( is_screen_data-product_ean_mpn ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_spo_ui~pbo_hu_item.
    es_screen_data = ms_item_to_be_proc.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'ITP' AND ms_item_to_be_proc-product_ean_mpn IS NOT INITIAL.
        screen-input = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_outb_ws_subscr_ui~get_title.
    ev_title = c_title.
  ENDMETHOD.


  METHOD zif_outb_ws_subscr_ui~process_user_command.
    super->zif_outb_ws_subscr_ui~process_user_command(
      EXPORTING
        iv_ucomm        = iv_ucomm
      IMPORTING
        es_bapiret      = es_bapiret
        ev_leave_screen = ev_leave_screen
        ev_processed    = ev_processed ).

    IF mt_hu_content IS INITIAL OR
       mv_next_hu IS NOT INITIAL AND mv_next_hu NE mv_huident.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
