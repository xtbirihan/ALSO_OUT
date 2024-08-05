class ZCL_OUTB_PACKING_WC_SLO_UI definition
  public
  inheriting from ZCL_OUTB_PACKING_WC_BASE_UI
  final
  create private .

public section.

  interfaces ZIF_OUTB_PACKING_WC_SLO_UI .

  constants C_SUBSCREEN_REPID type SYREPID value 'SAPLZFG_OUTB_PACKING_WC_UI' ##NO_TEXT.
  constants C_SUBSREEN type SY-DYNNR value '3100' ##NO_TEXT.
  constants C_TITLE type STRING value 'SLO' ##NO_TEXT.

  class-methods CREATE_INSTANCE
    importing
      !IO_SP type ref to ZIF_OUTB_PACKING_WC_SP
    returning
      value(RO_INST) type ref to ZIF_OUTB_WS_SUBSCR_UI .

  methods ZIF_OUTB_WS_SUBSCR_UI~GET_TITLE
    redefinition .
  methods ZIF_OUTB_WS_SUBSCR_UI~PBO_TAB
    redefinition .
  methods ZIF_OUTB_WS_SUBSCR_UI~PROCESS_USER_COMMAND
    redefinition .
protected section.

  methods ADD_SERIDS
    redefinition .
  methods FILL_EXCLUDED_FUNC
    redefinition .
  methods FUNC_DELETE_ALL
    redefinition .
  methods FUNC_DELETE_SELECTED_ITEM
    redefinition .
  methods GET_PACKING_INSTR
    redefinition .
  methods SET_HU_CONTENT_COLS
    redefinition .
  methods ADD_SERID
    redefinition .
private section.

  data MO_PACK_INSTR_TE type ref to CL_GUI_TEXTEDIT .
  data MS_ITEM_TO_BE_PROC type ZSTR_OUT_ITEM_TO_BE_PROC .

  methods AMOUNT_CHANGED
    raising
      ZCX_WORKSTATION .
  methods CHANGE_DELIVERY
    raising
      ZCX_WORKSTATION .
  methods CLEAR_WHEN_AMOUNT_EMPTY .
  methods FINISH_PROCESS
    raising
      ZCX_WORKSTATION .
  methods PACKING_MAT_CHANGED
    raising
      ZCX_WORKSTATION .
  methods READ_PRODUCT_DATA .
  methods CONSTRUCTOR
    importing
      !IV_LGNUM type /SCWM/S_WRK_PACK-LGNUM
      !IV_WRKST type /SCWM/S_WRK_PACK-WORKSTATION
      !IV_LGPLA type /SCWM/LGPLA
      !IV_HUIDENT type /SCWM/DE_HUIDENT .
ENDCLASS.



CLASS ZCL_OUTB_PACKING_WC_SLO_UI IMPLEMENTATION.


  METHOD add_serid.
    super->add_serid(
       iv_selnum   = iv_selnum
       iv_seldescr = iv_seldescr
       iv_serid    = iv_serid
       iv_docno  = ms_hu_content_current-docno
       iv_itemno = ms_hu_content_current-itemno
       ).
  ENDMETHOD.


  METHOD add_serids.
    CHECK super->add_serids(  is_screen_data = is_screen_data
                              iv_docno = ms_hu_content_current-docno
                              iv_itemno = ms_hu_content_current-itemno  ).

    ADD 1 TO ms_item_to_be_proc-amt_to_be_proc_pc.

    SUBTRACT 1 FROM mv_req_amount_in_pc.
    IF mv_req_amount_in_pc EQ 0 .
      TRY.
          mo_sp->check_sns_idn_in_db(
            EXPORTING
              iv_docno            = ms_hu_content_current-docno                 " Document Number
              iv_itemno           = ms_hu_content_current-itemno                 " Item Number
              it_req_sns_idn_type = CONV #( mt_req_sns_idn )
              iv_old_guid_hu      = mo_sp->get_source_hu_guid( )
          ).
        CATCH zcx_workstation INTO DATA(lo_cx).
          CLEAR: ms_req_sns_idn.
          RAISE EXCEPTION lo_cx.
      ENDTRY.
      APPEND LINES OF mt_req_sns_idn TO mt_sn_idn_overall_verified.
      finish_process( ).
    ENDIF.

  ENDMETHOD.


  METHOD amount_changed.
    CLEAR mt_req_sns_idn_type.

    determine_pack_data( ).

    IF mt_req_sns_idn_type IS INITIAL AND mv_current_ok_code IS INITIAL.
      finish_process( ).
    ENDIF.

  ENDMETHOD.


  METHOD change_delivery.
    DATA: lv_cancelled TYPE abap_bool.

    DATA(lt_del_itm) = mt_delivery.

    CALL FUNCTION 'Z_OUT_GET_DELIVERY_ITEM'
      IMPORTING
        ev_cancelled     = lv_cancelled                 " General Flag
      CHANGING
        ct_delivery_item = lt_del_itm.                 " Delivery Item (PRD) for Read Operations
    IF lv_cancelled EQ abap_true OR lt_del_itm IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE lt_del_itm INTO DATA(ls_del_itm) INDEX 1.
    READ TABLE mt_hu_content INTO ms_hu_content_current
         WITH KEY docid  = ls_del_itm-docid
                  itemid = ls_del_itm-itemid.

    mv_hu_content_current_idx = sy-tabix.

    amount_changed( ).

  ENDMETHOD.


  METHOD clear_when_amount_empty.
    CLEAR: mv_serid_itm_no,
           ms_item_to_be_proc-shu_hu_type,
           ms_item_to_be_proc-shu_hu_type_text,
           ms_item_to_be_proc-shu_pack_mat,
           ms_item_to_be_proc-shu_pack_matx,
           mt_req_sns_idn,
           mt_sn_idn_overall_verified,
           mv_new_hu,
           mt_new_huitm,
           mv_new_guid_hu.
    IF mo_req_sns_idn_table IS BOUND.
      mo_req_sns_idn_table->refresh( ).
    ENDIF.
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
    DATA(lo_inst) = NEW zcl_outb_packing_wc_slo_ui(
                          iv_lgnum   = lv_lgnum
                          iv_wrkst   = lv_wrkst
                          iv_lgpla   = lv_lgpla
                          iv_huident = lv_huident
                        ).
    lo_inst->mo_sp = io_sp.
    ro_inst = lo_inst.
  ENDMETHOD.


  METHOD determine_pack_data.
    TRY.
        mo_sp->get_packmat(
          EXPORTING
            it_mat_quant = VALUE #( ( matid = NEW /scwm/cl_ui_stock_fields( )->get_matid_by_no( iv_matnr =  ms_hu_content_current-matnr  )
                                      quan = COND #( WHEN ms_item_to_be_proc-amt_to_be_proc_pc IS NOT INITIAL
                                                     THEN ms_item_to_be_proc-amt_to_be_proc_pc
                                                     ELSE ms_item_to_be_proc-req_amt_pc )
                                      meins = ms_hu_content_current-meins ) )
          IMPORTING
            ev_pmatid = DATA(lv_pmatid)                 " Product
            ev_pmat   = DATA(lv_pmatnr)
        ).
      CATCH zcx_workstation INTO DATA(lx_ws).
        CLEAR ms_item_to_be_proc-amt_to_be_proc_pc.
        RAISE EXCEPTION lx_ws.
    ENDTRY.
    IF lv_pmatid IS INITIAL.
      CLEAR ms_item_to_be_proc-amt_to_be_proc_pc.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e011(zmc_out_ui_packing) WITH ms_hu_content_current-matnr.
    ENDIF.

    IF ms_item_to_be_proc-shu_pack_mat NE lv_pmatnr.
      ms_item_to_be_proc-shu_pack_mat = lv_pmatnr.
      ms_item_to_be_proc-pack_mat_id = lv_pmatid.
      packing_mat_changed( ).
    ENDIF.
  ENDMETHOD.


  METHOD fill_excluded_func.
    super->fill_excluded_func( ).
    IF ms_item_to_be_proc-amt_to_be_proc_pc IS INITIAL OR mt_req_sns_idn_type IS INITIAL.
      APPEND c_func_complete_ship_hu TO mt_excludes.
    ENDIF.
  ENDMETHOD.


  METHOD finish_process.
    DATA: lt_qty_repack        TYPE zif_outb_packing_wc_sp=>tt_quant_repack,
          lt_hu_content_repack LIKE mt_hu_content,
          lv_quant_remain      TYPE /scwm/de_quantity.

    CLEAR: mv_new_hu, mv_new_guid_hu.

    LOOP AT mt_hu_content_original INTO DATA(ls_ori)
         WHERE matid = ms_hu_content_current-matid
           AND docid = ms_hu_content_current-docid
           AND itemid = ms_hu_content_current-itemid.
      APPEND ls_ori TO lt_hu_content_repack.
      DELETE mt_hu_content_original.
    ENDLOOP.

    lv_quant_remain = ms_hu_content_current-quan.

    SORT lt_hu_content_repack BY quan.

    LOOP AT lt_hu_content_repack INTO ls_ori.
      APPEND VALUE zif_outb_packing_wc_sp=>ty_quant_repack(
                       stock_guid = ls_ori-stock_guid
                       quant = ls_ori-quan
                       meins = ls_ori-meins
                     ) TO lt_qty_repack REFERENCE INTO DATA(lr_quant).
      IF lr_quant->quant GE lv_quant_remain.
        lr_quant->quant = lv_quant_remain.
        EXIT.
      ELSE.
        SUBTRACT lr_quant->quant FROM lv_quant_remain.
      ENDIF.
    ENDLOOP.

    TRY.
        mo_sp->create_hu_in_wc(
          EXPORTING
            iv_pmatid     = ms_item_to_be_proc-pack_mat_id
            iv_source_hu  = mv_huident                 " Handling Unit Identification
            iv_docno      = ms_hu_content_current-docno                 " Document Number
            it_stock_repack = lt_qty_repack
          IMPORTING
            ev_new_hu     = mv_new_hu
            ev_new_hu_guid = mv_new_guid_hu
        ).
      CATCH zcx_workstation INTO DATA(lx_ws).
        APPEND LINES OF lt_hu_content_repack TO mt_hu_content_original.
        RAISE EXCEPTION lx_ws.
    ENDTRY.

    IF ms_item_to_be_proc-amt_to_be_proc_pc GE ms_hu_content_current-quan.
      DELETE mt_hu_content INDEX mv_hu_content_current_idx.
      CLEAR lt_hu_content_repack.

      IF mo_hu_content_table IS BOUND.
        mo_hu_content_table->refresh(
          EXPORTING
            s_stable     = VALUE #( row = abap_true col = abap_true )
        ).
      ENDIF.

    ELSE.

      SUBTRACT ms_item_to_be_proc-amt_to_be_proc_pc FROM ms_hu_content_current-quan.
      IF ms_hu_content_current-altme NE ms_hu_content_current-meins AND ms_hu_content_current-altme NE '*'.
        TRY.
            CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
              EXPORTING
                iv_matid     = ms_hu_content_current-matid
                iv_quan      = ms_hu_content_current-quan
                iv_unit_from = ms_hu_content_current-meins
                iv_unit_to   = ms_hu_content_current-altme
                iv_batchid   = VALUE /scwm/de_batchid( )
              IMPORTING
                ev_quan      = ms_hu_content_current-quan_mc.
          CATCH /scwm/cx_md.
            RAISE EXCEPTION TYPE zcx_workstation
                  MESSAGE e009(zmc_out_ui_packing) WITH ms_hu_content_current-matnr ms_hu_content_current-meins ms_hu_content_current-altme.
        ENDTRY.
      ENDIF.
      MODIFY mt_hu_content FROM ms_hu_content_current INDEX mv_hu_content_current_idx.

      LOOP AT lt_qty_repack INTO DATA(ls_qty_repack).
        READ TABLE lt_hu_content_repack REFERENCE INTO DATA(lr_ori)
             WITH KEY stock_guid = ls_qty_repack-stock_guid.

        IF sy-subrc EQ 0.
          DATA(lv_idx_hu_content) = sy-tabix.
          IF ls_qty_repack-quant EQ lr_ori->quan.
            DELETE lt_hu_content_repack INDEX lv_idx_hu_content.
          ELSE.
            SUBTRACT ls_qty_repack-quant FROM lr_ori->quan.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND LINES OF lt_hu_content_repack TO mt_hu_content_original.
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
      CATCH zcx_workstation INTO lx_ws.
        RAISE EXCEPTION lx_ws.
    ENDTRY.

    determine_and_call_fin_steps( iv_no_pack_info = abap_true ).

    CLEAR ms_item_to_be_proc-amt_to_be_proc_pc.
    clear_when_amount_empty( ).
  ENDMETHOD.


  METHOD function_complete_ship_hu.
    CHECK ms_item_to_be_proc-amt_to_be_proc_pc IS NOT INITIAL.

    APPEND LINES OF mt_req_sns_idn TO mt_sn_idn_overall_verified.

    finish_process( ).

  ENDMETHOD.


  METHOD func_delete_all.
    CALL METHOD super->func_delete_all( ).
    mv_req_amount_in_pc = ms_item_to_be_proc-req_amt_pc.
    CLEAR ms_item_to_be_proc-amt_to_be_proc_pc.
  ENDMETHOD.


  METHOD func_delete_selected_item.
    CHECK mt_req_sns_idn IS NOT INITIAL.

    IF super->func_delete_selected_item( ) IS NOT INITIAL.
      ADD 1 TO mv_req_amount_in_pc.
      SUBTRACT 1 FROM ms_item_to_be_proc-amt_to_be_proc_pc.
    ENDIF.

  ENDMETHOD.


  METHOD get_packing_instr.
    rv_pack_instr = super->get_packing_instr(
             it_matnr_entitled = VALUE #( ( matnr = ms_hu_content_current-matnr entitled = ms_hu_content_current-entitled ) )
             it_docid_itemid   = VALUE #( FOR item IN mt_current_dlv_items WHERE ( docid = ms_hu_content_current-docid
                                                                                   AND product-productno = ms_hu_content_current-matnr )
                                          ( docid = item-docid itemid = item-itemid ) ) ).
  ENDMETHOD.


  METHOD packing_mat_changed.
    mo_sp->get_pack_mat_data(
      EXPORTING
        iv_pack_mat_id   = ms_item_to_be_proc-pack_mat_id                  " Packaging Material
      IMPORTING
        ev_pack_mat      = ms_item_to_be_proc-shu_pack_mat                  " Packaging Material
        ev_pack_mat_text = ms_item_to_be_proc-shu_pack_matx                 " Material Description
        ev_hutype        = ms_item_to_be_proc-shu_hu_type                 " Handling Unit Type
        ev_hutyptext     = ms_item_to_be_proc-shu_hu_type_text                 " Description of Handling Unit Type
    ).

  ENDMETHOD.


  METHOD read_product_data.

    READ TABLE mt_hu_content INTO DATA(ls_hu_content) INDEX mv_hu_content_current_idx.

    ms_item_to_be_proc-mfrpn = ls_hu_content-mfrpn.
    ms_item_to_be_proc-ean11 = ls_hu_content-ean11 .
    ms_item_to_be_proc-matnr = ls_hu_content-matnr.
    ms_item_to_be_proc-maktx = ls_hu_content-maktx.
  ENDMETHOD.


  METHOD save_all_sn_idn.


    mo_sp->save_req_sns_idn(
      EXPORTING
        iv_docno            = ms_hu_content_current-docno                 " Document Number
        iv_itemno           = ms_hu_content_current-itemno                 " Item Number
        it_req_sns_idn_type = mt_sn_idn_overall_verified
        iv_new_guid_hu      = iv_guid_hu                 " Handling Unit Identification
        iv_complete_overwrite = abap_false
        iv_old_guid_hu      = mo_sp->get_source_hu_guid( )
    ).

  ENDMETHOD.


  METHOD set_delivery_data.
    ms_item_to_be_proc-out_del_item = condense( |{ condense( |{ ms_hu_content_current-docno ALPHA = OUT }| ) }/{ ms_hu_content_current-itemno ALPHA = OUT }| ).
    ms_item_to_be_proc-out_delivery = |{ ms_hu_content_current-docno ALPHA = OUT }|.
    ms_item_to_be_proc-req_amt_pc = ms_hu_content_current-quan.
    CLEAR ms_item_to_be_proc-amt_to_be_proc_pc.

    TRY.
        determine_pack_data( ).
      CATCH zcx_workstation ##no_handler.
    ENDTRY.

    mt_req_sns_idn_type = mo_sp->get_req_sns_idn_type(
                              iv_docid  = ms_hu_content_current-docid
                              iv_itemid = ms_hu_content_current-itemid
                          ).
    CLEAR ms_req_sns_idn.
    IF mt_req_sns_idn_type IS NOT INITIAL.
      fill_req_sns_idn_type( ).
      mv_req_amount_in_pc = ms_item_to_be_proc-req_amt_pc.
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


  METHOD zif_outb_packing_wc_base_ui~pai_req_sns_idn.
    DATA(lv_lines) = lines( mt_req_sns_idn ).
    CALL METHOD super->zif_outb_packing_wc_base_ui~pai_req_sns_idn
      EXPORTING
        is_screen_data = is_screen_data.

    IF lv_lines NE lines( mt_req_sns_idn ).
      SUBTRACT 1 FROM ms_item_to_be_proc-req_amt_pc.
      IF ms_item_to_be_proc-req_amt_pc EQ 0 .
        finish_process( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_outb_packing_wc_slo_ui~init.

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

    IF co_captured_sn_idn_table IS NOT BOUND.
      init_req_sns_idn(	io_captured_sn_idn_cc = io_captured_sn_idn_cc  ).
      co_captured_sn_idn_table = mo_req_sns_idn_table.
    ELSEIF mo_req_sns_idn_table IS NOT BOUND.
      mo_req_sns_idn_table = co_captured_sn_idn_table.
      TRY.
          mo_req_sns_idn_table->set_data(
            CHANGING
              t_table = mt_req_sns_idn
          ).
        CATCH cx_salv_no_new_data_allowed ##no_handler.
      ENDTRY.
      mo_req_sns_idn_table->refresh( ).
    ENDIF.

  ENDMETHOD.


  method ZIF_OUTB_PACKING_WC_SLO_UI~INIT_DATA.
    clear_when_amount_empty( ).

    mt_hu_content_original = mo_sp->get_hu_content( IMPORTING et_delivery_data = mt_delivery ).
    condense_hu_content( ).
    SORT mt_hu_content BY quan DESCENDING.
    read_product_data( ).

    READ TABLE mt_hu_content INTO ms_hu_content_current INDEX 1.
    mv_hu_content_current_idx = 1.
    ms_item_to_be_proc-out_del_item = condense( |{ condense( |{ ms_hu_content_current-docno ALPHA = OUT }| ) }/{ ms_hu_content_current-itemno ALPHA = OUT }| ).

  endmethod.


  METHOD zif_outb_packing_wc_slo_ui~pai_hu_item.
    IF is_screen_data-amt_to_be_proc_pc NE ms_item_to_be_proc-amt_to_be_proc_pc.
      IF is_screen_data-amt_to_be_proc_pc GT ms_hu_content_current-quan.
        RAISE EXCEPTION TYPE zcx_workstation
              MESSAGE e012(zmc_out_ui_packing).
      ENDIF.
      clear_when_amount_empty( ).
      ms_item_to_be_proc-amt_to_be_proc_pc = is_screen_data-amt_to_be_proc_pc.
      mv_req_amount_in_pc                  = is_screen_data-amt_to_be_proc_pc.

      amount_changed( ).
    ELSEIF is_screen_data-amt_to_be_proc_pc IS NOT INITIAL.
      IF is_screen_data-amt_to_be_proc_pc GT ms_hu_content_current-quan.
        RAISE EXCEPTION TYPE zcx_workstation
              MESSAGE e012(zmc_out_ui_packing).
      ENDIF.
      IF mt_req_sns_idn_type IS INITIAL AND mv_current_ok_code IS INITIAL.
        finish_process( ).
      ENDIF.
    ENDIF.
    IF is_screen_data-shu_pack_mat IS NOT INITIAL AND
       is_screen_data-shu_pack_mat NE ms_item_to_be_proc-shu_pack_mat.
      TRY.
          DATA(ls_prod) = /scmb/cl_md_access_mdl=>get_md_access( )->get_prod(
                                   iv_prodno    = is_screen_data-shu_pack_mat
                                 ).
        CATCH /scmb/cx_md_access. " Exception Class for Master Data Accesses
          RAISE EXCEPTION TYPE zcx_workstation MESSAGE e007(zmc_out_ui_packing) WITH is_screen_data-shu_pack_mat.
      ENDTRY.
      ms_item_to_be_proc-pack_mat_id = ls_prod-prodid.
      packing_mat_changed( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_slo_ui~pbo_hu_item.
    es_screen_data = ms_item_to_be_proc.
    IF co_packing_instr_te IS NOT BOUND.
      IF mo_pack_instr_te IS NOT BOUND.
        CREATE OBJECT mo_pack_instr_te
          EXPORTING
            parent                     = io_cc_packing_instr
            wordwrap_mode              = cl_gui_textedit=>wordwrap_at_windowborder
            wordwrap_to_linebreak_mode = cl_gui_textedit=>false
          EXCEPTIONS
            OTHERS                     = 1.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
        mo_pack_instr_te->set_readonly_mode(  ).
        mo_pack_instr_te->set_toolbar_mode( '0' ).
        co_packing_instr_te = mo_pack_instr_te.
      ENDIF.
    ELSE.
      mo_pack_instr_te = co_packing_instr_te.
    ENDIF.
    IF mo_pack_instr_te IS BOUND AND mv_recreate_pack_inst EQ abap_true.
      CLEAR mv_recreate_pack_inst.
      mo_pack_instr_te->set_textstream( get_packing_instr(   ) ).
    ENDIF.

    LOOP AT SCREEN.

      IF screen-name EQ 'ZSTR_OUT_ITEM_TO_BE_PROC-SHU_PACK_MAT'
         AND ms_item_to_be_proc-shu_pack_mat IS NOT INITIAL.
        screen-input = '0'.
      ENDIF.
      IF screen-name EQ 'ZSTR_OUT_ITEM_TO_BE_PROC-AMT_TO_BE_PROC_PC'
         AND mt_req_sns_idn_type IS NOT INITIAL.
        screen-input = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_outb_ws_subscr_ui~get_title.
    ev_title = c_title.
  ENDMETHOD.


  METHOD zif_outb_ws_subscr_ui~pbo_tab.
    super->zif_outb_ws_subscr_ui~pbo_tab( IMPORTING es_screen_data = es_screen_data ).
    LOOP AT screen.
      IF screen-name EQ 'BTN_COMPL_SHU' AND line_exists( mt_excludes[ table_line = c_func_complete_ship_hu ] ).
        screen-input = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_outb_ws_subscr_ui~process_user_command.
    CASE iv_ucomm.
      WHEN c_func_change_pack_mat.
        CLEAR: ms_item_to_be_proc-shu_pack_mat    ,
               ms_item_to_be_proc-shu_pack_matx   ,
               ms_item_to_be_proc-shu_hu_type     ,
               ms_item_to_be_proc-shu_hu_type_text.
        ev_processed = abap_true.
      WHEN c_func_change_delivery.
        change_delivery( ).
        ev_processed = abap_true.
      WHEN c_func_complete_ship_hu.
        function_complete_ship_hu( ).
      WHEN OTHERS.
        super->zif_outb_ws_subscr_ui~process_user_command(
          EXPORTING
            iv_ucomm        = iv_ucomm
          IMPORTING
            es_bapiret      = es_bapiret
            ev_leave_screen = ev_leave_screen
            ev_processed    = ev_processed ).
    ENDCASE.
    IF mt_hu_content IS INITIAL OR
       mv_next_hu IS NOT INITIAL AND mv_next_hu NE mv_huident.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
