class ZCL_OUTB_PACKING_WC_SC_UI definition
  public
  inheriting from ZCL_OUTB_PACKING_WC_BASE_UI
  final
  create public .

public section.

  interfaces ZIF_OUTB_PACKING_WC_SC_UI .

  constants C_SUBSCREEN_REPID type SYREPID value 'SAPLZFG_OUTB_PACKING_WC_UI' ##NO_TEXT.
  constants C_SUBSREEN_NO_WEIGHT_DIFF type SY-DYNNR value '3200' ##NO_TEXT.
  constants C_TITLE type STRING value 'SCO' ##NO_TEXT.
  constants C_SUBSREEN_WEIGHT_DIFF type SY-DYNNR value '3300' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !IV_LGNUM type /SCWM/S_WRK_PACK-LGNUM
      !IV_WRKST type /SCWM/S_WRK_PACK-WORKSTATION
      !IV_LGPLA type /SCWM/LGPLA
      !IV_HUIDENT type /SCWM/DE_HUIDENT
      !IO_SP type ref to ZIF_OUTB_PACKING_WC_SP .
  class-methods CREATE_INSTANCE
    importing
      !IO_SP type ref to ZIF_OUTB_PACKING_WC_SP
    returning
      value(RO_INST) type ref to ZIF_OUTB_WS_SUBSCR_UI .

  methods HANDLE_HU_CONTENT_SINGLE_CLICK
    redefinition .
  methods ZIF_OUTB_PACKING_WC_BASE_UI~PBO_REQ_SNS_IDN
    redefinition .
  methods ZIF_OUTB_WS_SUBSCR_UI~GET_TITLE
    redefinition .
  methods ZIF_OUTB_WS_SUBSCR_UI~INIT
    redefinition .
  methods ZIF_OUTB_WS_SUBSCR_UI~PBO_TAB
    redefinition .
  methods ZIF_OUTB_WS_SUBSCR_UI~PROCESS_USER_COMMAND
    redefinition .
  methods ZIF_OUTB_WS_SUBSCR_UI~PAI_TAB
    redefinition .
protected section.

  data MO_VERIFIED_ITEMS_CTRL type ref to CL_GUI_CUSTOM_CONTAINER .
  data MT_VERIFIED_ITEMS type ZIF_OUTB_PACKING_WC_SP=>TT_HU_CONTENT .

  methods ADD_SERID
    redefinition .
  methods ADD_SERIDS
    redefinition .
  methods FILL_EXCLUDED_FUNC
    redefinition .
  methods FUNC_DELETE_ALL
    redefinition .
  methods FUNC_DELETE_SELECTED_ITEM
    redefinition .
  methods SET_HU_CONTENT_COLS
    redefinition .
private section.

  types:
    BEGIN OF ty_item_to_be_proc,
           product_ean_mpn TYPE zde_inb_product_ean_mpn,
           repack_pc       TYPE zde_repack_pc,
         END OF ty_item_to_be_proc .

  data MV_HU_CONTENT_PRD_HOT type ABAP_BOOL .
  data MS_PACK type ZSTR_OUT_UI_PACK_DATA .
  data MO_PACK_INSTR_TE type ref to CL_GUI_TEXTEDIT .
  data MT_SN_IDN_OVERALL type ZIF_OUTB_PACKING_WC_SP=>TT_REQ_SNS_IDN .
  data MS_ITEM_TO_BE_PROC type TY_ITEM_TO_BE_PROC .

  methods FINISH_PROCESS .
  methods FUNCTION_REPACK_CHECK .
  methods REPACK_CURRENT_HU_CONTENT .
  methods PRODUCT_CHANGED
    importing
      !IV_PRODUCT_EAN_MPN type ZDE_INB_PRODUCT_EAN_MPN
    raising
      ZCX_WORKSTATION .
  methods INIT_VERIFIED_ITEMS
    importing
      !IO_VERIFIED_ITEMS_CC type ref to CL_GUI_CUSTOM_CONTAINER .
  methods PACKING_MAT_CHANGED
    raising
      ZCX_WORKSTATION .
  methods REFRESH_HU_W_SCANNED_SNS .
  methods HANDLE_VERIF_ITMS_SINGLE_CLICK
    for event LINK_CLICK of CL_SALV_EVENTS_TABLE
    importing
      !ROW .
  methods SET_VERIFIED_ITEMS_COLS .
ENDCLASS.



CLASS ZCL_OUTB_PACKING_WC_SC_UI IMPLEMENTATION.


  METHOD add_serid.
    IF mo_sp->is_weight_difference_exceeded( ) OR mv_split_mode EQ abap_true.
      super->add_serid(
          iv_selnum   = iv_selnum
          iv_seldescr = iv_seldescr
          iv_serid    = iv_serid
          iv_docno  = mr_hu_content_verified->docno
          iv_itemno = mr_hu_content_verified->itemno
          ).
    ELSE.
      super->add_serid(
         iv_selnum   = iv_selnum
         iv_seldescr = iv_seldescr
         iv_serid    = iv_serid
         iv_docno  = ms_hu_content_current-docno
         iv_itemno = ms_hu_content_current-itemno
         ).
    ENDIF.


  ENDMETHOD.


  METHOD add_serids.
    CHECK ( mv_current_ok_code IS INITIAL OR mv_current_ok_code EQ c_func_scan_2d_barcode )
          AND ms_req_sns_idn-serid_01_selnum IS NOT INITIAL.
    IF mo_sp->is_weight_difference_exceeded( ) OR mv_split_mode EQ abap_true.
      IF mr_hu_content_verified IS BOUND.
        IF super->add_serids( is_screen_data = is_screen_data
                              iv_docno       = mr_hu_content_verified->docno                 " Document Number
                              iv_itemno      = mr_hu_content_verified->itemno                 " Item Number
          ).
          refresh_repack_w_scanned_sns( ).
        ENDIF.
      ENDIF.
    ELSE.
      IF super->add_serids( is_screen_data = is_screen_data
                            iv_docno       = ms_hu_content_current-docno                 " Document Number
                            iv_itemno      = ms_hu_content_current-itemno                 " Item Number
        ).
        refresh_hu_w_scanned_sns( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD CHECK_ALL_SN_IDN.

    LOOP AT mt_sn_idn_overall_verified INTO DATA(ver) GROUP BY ( docno = ver-docno itemno = ver-itemno )
           INTO DATA(ls_grp_docitm).
      DATA(lt_req_sns) = VALUE zif_outb_packing_wc_sp=>tt_req_sns_idn( FOR req_sns IN GROUP ls_grp_docitm ( req_sns ) ).
      mo_sp->check_sns_idn_in_db(
        EXPORTING
          iv_docno            = ls_grp_docitm-docno                 " Document Number
          iv_itemno           = ls_grp_docitm-itemno                 " Item Number
          it_req_sns_idn_type = lt_req_sns
          iv_old_guid_hu      = iv_guid_hu                 " Handling Unit Identification
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD check_pick_hu_if_relevant.

    IF mv_split_mode EQ abap_false
       AND ms_hu_content_current-sn_idn_complete_ori EQ abap_true.
      READ TABLE mt_sn_idn_picking_hu TRANSPORTING NO FIELDS
           WITH KEY docitm COMPONENTS
                    docno  = mr_hu_content_verified->docno
                    itemno = mr_hu_content_verified->itemno
                    serial = mv_serial_number.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e026(zmc_out_ui_packing) WITH mv_serial_number.
      ENDIF.
    ENDIF.

    IF mv_split_mode EQ abap_true.
      IF mr_hu_content_verified->sn_idn_complete_ori EQ abap_true.
        READ TABLE mt_sn_idn_scanned_prev TRANSPORTING NO FIELDS
             WITH KEY docitm COMPONENTS
                      docno  = mr_hu_content_verified->docno
                      itemno = mr_hu_content_verified->itemno
                      serial = mv_serial_number.
        IF sy-subrc NE 0.
          READ TABLE mt_sn_idn_picking_hu TRANSPORTING NO FIELDS
               WITH KEY docitm COMPONENTS
                        docno  = mr_hu_content_verified->docno
                        itemno = mr_hu_content_verified->itemno
                        serial = mv_serial_number.
          IF sy-subrc NE 0.
            RAISE EXCEPTION TYPE zcx_workstation MESSAGE e026(zmc_out_ui_packing) WITH mv_serial_number.
          ENDIF.
        ENDIF.
      ENDIF.
      IF mr_hu_content_verified->sn_idn_complete_prev EQ abap_true.
        READ TABLE mt_sn_idn_scanned_prev TRANSPORTING NO FIELDS
             WITH KEY docitm COMPONENTS
                      docno  = mr_hu_content_verified->docno
                      itemno = mr_hu_content_verified->itemno
                      serial = mv_serial_number.
        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE zcx_workstation MESSAGE e027(zmc_out_ui_packing) WITH mv_serial_number.
        ENDIF.
      ENDIF.
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
    mo_sp = io_sp.
    IF mo_sp->is_workcenter_mastercarton_rel( ).
      mv_subscreen = c_subsreen_master_carton.
    ELSEIF mo_sp->is_weight_difference_exceeded( ).
      mv_subscreen = c_subsreen_weight_diff.
    ELSE.
      mv_subscreen = c_subsreen_no_weight_diff.
      mv_sns_enter_mode_active = abap_true.
    ENDIF.
    mv_subscreen_prg = c_subscreen_repid.
  ENDMETHOD.


  METHOD create_hu_repack_content.
    DATA: lt_qty_repack        TYPE zif_outb_packing_wc_sp=>tt_quant_repack,
          lt_hu_content_repack LIKE mt_hu_content,
          lv_quant_remain      TYPE /scwm/de_quantity.

    LOOP AT it_hu_content INTO DATA(ls_hu_content).
      CLEAR lt_hu_content_repack.

      LOOP AT mt_hu_content_original INTO DATA(ls_ori)
           WHERE matid = ls_hu_content-matid
             AND docid = ls_hu_content-docid
             AND itemid = ls_hu_content-itemid.
        APPEND ls_ori TO lt_hu_content_repack.
        DELETE mt_hu_content_original.
      ENDLOOP.
      lv_quant_remain = ls_hu_content-quan.

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
    ENDLOOP.


    TRY.
        mo_sp->create_hu_in_wc(
          EXPORTING
            iv_pmatid     = ms_pack-pack_mat_id
            iv_source_hu  = mv_huident                 " Handling Unit Identification
            iv_docno      = ms_hu_content_current-docno                 " Document Number
            it_stock_repack = lt_qty_repack
          IMPORTING
            ev_new_hu      = mv_new_hu
            ev_new_hu_guid = mv_new_guid_hu
        ).
      CATCH zcx_workstation INTO DATA(lx_ws).
        APPEND LINES OF lt_hu_content_repack TO mt_hu_content_original.
        RAISE EXCEPTION lx_ws.
    ENDTRY.

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

  ENDMETHOD.


  METHOD create_instance.
    io_sp->get_ws_and_hu(
      IMPORTING
        ev_lgnum   = DATA(lv_lgnum)
        ev_wrkst   = DATA(lv_wrkst)
        ev_lgpla   = DATA(lv_lgpla)
        ev_huident = DATA(lv_huident)
    ).
    DATA(lo_inst) = NEW zcl_outb_packing_wc_sc_ui(
                          iv_lgnum   = lv_lgnum
                          iv_wrkst   = lv_wrkst
                          iv_lgpla   = lv_lgpla
                          iv_huident = lv_huident
                          io_sp = io_sp
                        ).
    ro_inst = lo_inst.
  ENDMETHOD.


  METHOD display_sns_idns_for_current.
    IF ms_hu_content_current-sn_idn_complete EQ abap_false.
      mt_req_sns_idn_type = mo_sp->get_req_sns_idn_type(
                                iv_docid  = ms_hu_content_current-docid
                                iv_itemid = ms_hu_content_current-itemid
                            ).

      IF mt_req_sns_idn_type IS NOT INITIAL.
        fill_req_sns_idn_type( ).
      ENDIF.
    ELSE.
      LOOP AT mt_sn_idn_overall_verified INTO DATA(ls_snidn_verified)
           USING KEY docitm
           WHERE docno  = ms_hu_content_current-docno
             AND itemno = ms_hu_content_current-itemno.
        APPEND ls_snidn_verified TO mt_req_sns_idn.
      ENDLOOP.
      mv_serid_itm_no = 0.
      LOOP AT mt_req_sns_idn INTO DATA(req_sns) GROUP BY ( serial = req_sns-serial ) INTO DATA(ls_grp_serial_pos).
        ADD 1 TO mv_serid_itm_no.
        LOOP AT GROUP ls_grp_serial_pos REFERENCE INTO DATA(lr_serpos).
          lr_serpos->serial_pos = mv_serid_itm_no.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
    mo_req_sns_idn_table->refresh( ).
  ENDMETHOD.


  METHOD fill_excluded_func.
    super->fill_excluded_func( ).

    "Delete Missing Items
    IF mt_verified_items IS NOT INITIAL OR mv_split_mode EQ abap_true.
      APPEND c_func_delete_miss_itms TO mt_excludes.
    ENDIF.
    "Scan Barcode
    IF ms_hu_content_current-sn_idn_complete NE abap_false.
      APPEND c_func_scan_2d_barcode TO mt_excludes.
    ENDIF.
    "Complete Repack HU
    IF NOT mo_sp->is_weight_difference_exceeded( ) AND NOT mv_split_mode EQ abap_true.
      IF line_exists( mt_hu_content[ sn_idn_complete = abap_false ] ).
        APPEND c_func_complete_ship_hu TO mt_excludes.
      ENDIF.
    ELSEIF mv_split_mode EQ abap_true.
      IF ms_pack-shu_pack_mat IS INITIAL
         OR mt_verified_items IS INITIAL.
        APPEND c_func_complete_ship_hu TO mt_excludes.
      ENDIF.
    ELSE.
      IF line_exists( mt_verified_items[ sn_idn_complete = abap_false ] )  AND mv_sns_enter_mode_active EQ abap_true
         OR mt_verified_items IS INITIAL.
        APPEND c_func_complete_ship_hu TO mt_excludes.
      ENDIF.
      IF ms_pack-shu_pack_mat IS INITIAL
         AND mt_hu_content IS NOT INITIAL.
        APPEND c_func_complete_ship_hu TO mt_excludes.
      ENDIF.
      IF mv_user_is_supervisor EQ abap_false.
        IF mt_hu_content IS NOT INITIAL.
          APPEND c_func_complete_ship_hu TO mt_excludes.
        ENDIF.
      ENDIF.
    ENDIF.
    "Split, Repack all HU
    IF mv_split_mode EQ abap_true.
      APPEND c_func_split_hu TO mt_excludes.
    ELSEIF mo_sp->is_weight_difference_exceeded( ).
      APPEND c_func_repack_all TO mt_excludes.
      IF mt_verified_items IS INITIAL.
        APPEND c_func_split_hu TO mt_excludes.
      ELSEIF mt_hu_content IS NOT INITIAL AND mv_user_is_supervisor EQ abap_false.
        APPEND c_func_split_hu TO mt_excludes.
      ELSEIF mv_sns_enter_mode_active = abap_true.
        APPEND c_func_split_hu TO mt_excludes.
      ENDIF.
      IF mv_completion_started EQ abap_true.
        APPEND c_func_split_hu TO mt_excludes.
      ENDIF.

    ELSE.
      APPEND c_func_repack_all TO mt_excludes.
    ENDIF.
    "Change packing material
    IF mv_completion_started EQ abap_true.
      APPEND c_func_change_pack_mat TO mt_excludes.
    ENDIF.

    "Delte SN/IDN
    IF  mv_split_mode IS INITIAL AND NOT mo_sp->is_weight_difference_exceeded( )
        AND ms_hu_content_current-sn_idn_complete_ori EQ abap_true.
      APPEND c_func_delete_all TO mt_excludes.
      APPEND c_func_delete_selected_itm TO mt_excludes.
    ENDIF.
  ENDMETHOD.


  method FINISH_PROCESS.
  endmethod.


  METHOD free_handlers.
    super->free_handlers( ).
    IF mo_verified_items_table IS BOUND.
      DATA(lr_events) = CAST cl_salv_events_table(  mo_verified_items_table->get_event( ) ).
      SET HANDLER me->handle_verif_itms_single_click FOR lr_events ACTIVATION space.
    ENDIF.
  ENDMETHOD.


  METHOD function_repack_check.
    IF mt_req_sns_idn_type IS INITIAL OR
       ms_hu_content_current-sn_idn_complete EQ abap_true and ms_item_to_be_proc-repack_pc eq ms_hu_content_current-quan.
      repack_current_hu_content( ).
    ELSE.
      fill_req_sns_idn_type( ).
    ENDIF.
  ENDMETHOD.


  METHOD func_complete_ship_hu.
    DATA lv_answer TYPE c LENGTH 1.

    mv_completion_started = abap_true.
    IF mv_split_mode EQ abap_true OR  mo_sp->is_weight_difference_exceeded( ).
      READ TABLE mt_verified_items TRANSPORTING NO FIELDS
           WITH KEY sn_idn_complete = abap_false.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'Z_OUT_POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Missing SNs/IDNs'(snh)
            text_question         = 'SNs/IDNs are missing, please scan them'(snq)
            text_button_1         = 'OK'(ok_)         " Text on the first pushbutton
            text_button_2         = space
            display_cancel_button = abap_false              " Button for displaying cancel pushbutton
          IMPORTING
            answer                = lv_answer                " Return values: '1', '2', 'A'
          EXCEPTIONS
            OTHERS                = 0.
        mv_sns_enter_mode_active = abap_true.
        start_sns_scan_for_verified( ).
        RETURN.
      ELSE.
        mv_sns_enter_mode_active = abap_false.
      ENDIF.
    ENDIF.

    check_all_sn_idn( mo_sp->get_source_hu_guid( ) ).

    start_function_log( ).

    TRY.
        IF mv_split_mode EQ abap_true.
          func_complete_ship_hu_split(
           IMPORTING
              ev_leave_screen = ev_leave_screen ).
        ELSEIF mo_sp->is_weight_difference_exceeded( ) AND NOT mo_sp->is_workcenter_mastercarton_rel( ).
          func_complete_ship_hu_wdiff(
           IMPORTING
              ev_leave_screen = ev_leave_screen ).
        ELSE.
          func_complete_ship_hu_norm(
            IMPORTING
              ev_leave_screen = ev_leave_screen ).
        ENDIF.
        mv_completion_started = abap_false.

      CATCH zcx_workstation INTO DATA(lx_ws).
        add_exception( lx_ws ).
        mo_function_log->add_log( it_prot = lx_ws->messages ).
    ENDTRY.

    finish_function_log(
      EXPORTING
        iv_external_id        = 'COMPLETE_SHIP_HU'
        iv_save_only_if_error = abap_false
        iv_display            = abap_false
    ).

    IF lx_ws IS BOUND.
      RAISE EXCEPTION lx_ws.
    ENDIF.
  ENDMETHOD.


  METHOD func_complete_ship_hu_norm.
    CLEAR ev_leave_screen.

    IF line_exists( mt_verified_items[ sn_idn_complete = abap_false ] ).
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e010(zmc_out_ui_packing).
    ENDIF.

    mo_sp->get_weights(
      IMPORTING
        ev_scale_hu_weight_in_kg  = DATA(lv_scale_weight_kg)                 " Scale HU Weight (KG)
    ).

    IF ms_pack-pack_mat_id EQ mv_pmatid_source OR mo_sp->is_workcenter_mastercarton_rel( ).

      save_all_sn_idn( mo_sp->get_source_hu_guid( ) ).

      IF NOT mo_sp->is_workcenter_mastercarton_rel( ).
        mo_sp->update_weight_in_hu(
          EXPORTING
            iv_guid_hu         = mo_sp->get_source_hu_guid( )                 " Unique Internal Identification of a Handling Unit
            iv_scale_hu_weight = lv_scale_weight_kg                  " Scale HU Weight (KG)
            iv_scale_hu_meins  = zif_c_mdm_tool=>c_units-kg                 " Base Unit of Measure
        ).
      ENDIF.

      mv_new_hu = mv_huident.
      mv_new_guid_hu = mo_sp->get_source_hu_guid( ).

    ELSE.
      DATA(lv_weight_diff) = mo_sp->get_weight_difference( iv_old_pmatid = mv_pmatid_source iv_new_pmatid = ms_pack-pack_mat_id ).

      mo_sp->create_hu_in_wc(
        EXPORTING
          iv_pmatid       = ms_pack-pack_mat_id                 " Packaging Material
          iv_source_hu    = mv_huident                  " Handling Unit Identification
          it_stock_repack = VALUE #( FOR hu_cont IN mt_hu_content_original ( stock_guid = hu_cont-stock_guid
                                                                             quant = hu_cont-quan
                                                                             meins = hu_cont-meins  ) )
        IMPORTING
          ev_new_hu       = mv_new_hu                 " Handling Unit Identification
          ev_new_hu_guid  = mv_new_guid_hu                 " Unique Internal Identification of a Handling Unit
          et_huitm        = mt_new_huitm
      ).

      save_all_sn_idn( mv_new_guid_hu ).

      ADD lv_weight_diff TO lv_scale_weight_kg.
      mo_sp->update_weight_in_hu(
        EXPORTING
          iv_guid_hu         = mv_new_guid_hu                " Unique Internal Identification of a Handling Unit
          iv_scale_hu_weight = lv_scale_weight_kg                  " Scale HU Weight (KG)
          iv_scale_hu_meins  = zif_c_mdm_tool=>c_units-kg                 " Base Unit of Measure
      ).

    ENDIF.

    mo_sp->determine_stage_area(
       VALUE #( FOR GROUPS docitm OF hu_cont IN mt_hu_content
                  GROUP BY ( docid = hu_cont-docid itemid = hu_cont-itemid )
                   ( docid = docitm-docid itemid = docitm-itemid doccat = wmegc_doccat_pdo )  )
              ).

    mo_sp->close_hu(
      EXPORTING
        iv_hu      = mv_new_hu                 " Unique Internal Identification of a Handling Unit
        iv_hu_guid = mv_new_guid_hu                 " Unique Internal Identification of a Handling Unit
    ).

    CLEAR mt_hu_content.

    print_hu( mv_new_hu ).

    determine_and_call_fin_steps(
        iv_no_pack_info = abap_true
        iv_no_new_hu    = mo_sp->is_workcenter_mastercarton_rel( )
    ).

    ev_leave_screen = abap_true.
  ENDMETHOD.


  METHOD func_complete_ship_hu_split.
    CLEAR ev_leave_screen.

    IF line_exists( mt_verified_items[ sn_idn_complete = abap_false ] ).
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e010(zmc_out_ui_packing).
    ENDIF.

    IF ms_pack-pack_mat_id IS INITIAL.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e015(zmc_out_ui_packing).
    ENDIF.

    DATA(lt_stock_lines) = move_stock_lines_from_original( ).

    TRY.
        mo_sp->create_hu_in_wc(
          EXPORTING
            iv_pmatid       = ms_pack-pack_mat_id                 " Packaging Material
            iv_source_hu    = mv_huident                  " Handling Unit Identification
            it_stock_repack = lt_stock_lines
          IMPORTING
            ev_new_hu       = mv_new_hu                 " Handling Unit Identification
            ev_new_hu_guid  = mv_new_guid_hu                 " Unique Internal Identification of a Handling Unit
            et_huitm        = mt_new_huitm
        ).
        mt_hu_content_in_split = mt_hu_content.
      CATCH zcx_workstation INTO DATA(lx_wc).
        reset_stock_lines_in_original( ).
        RAISE EXCEPTION lx_wc.
    ENDTRY.

    DATA(lt_verified_items) = mt_verified_items.
    CLEAR mt_verified_items.
    mo_verified_items_table->refresh( ).

    save_all_sn_idn( mv_new_guid_hu ).

    TRY.
        mo_sp->update_weight_in_hu(
          EXPORTING
            iv_guid_hu         = mv_new_guid_hu                 " Unique Internal Identification of a Handling Unit
            iv_scale_hu_weight = 0                  " Scale HU Weight (KG)
            iv_scale_hu_meins  = zif_c_mdm_tool=>c_units-kg                 " Base Unit of Measure
            iv_use_standard_weight = abap_true
        ).
      CATCH zcx_workstation INTO DATA(lx_ws).
        cl_message_helper=>set_msg_vars_for_if_t100_msg( lx_ws ).
        IF mo_function_log IS BOUND.
          mo_function_log->add_message( ).
        ENDIF.
    ENDTRY.

    mo_sp->determine_stage_area(
       VALUE #( FOR GROUPS docitm OF hu_cont IN lt_verified_items
                  GROUP BY ( docid = hu_cont-docid itemid = hu_cont-itemid )
                   ( docid = docitm-docid itemid = docitm-itemid doccat = wmegc_doccat_pdo )  )
              ).

    mo_sp->close_hu(
      EXPORTING
        iv_hu      = mv_new_hu                 " Unique Internal Identification of a Handling Unit
        iv_hu_guid = mv_new_guid_hu                 " Unique Internal Identification of a Handling Unit
    ).


    print_hu( mv_new_hu ).

    determine_and_call_fin_steps(
        iv_no_pack_info = abap_true
        iv_no_new_hu    = xsdbool( mt_hu_content IS NOT INITIAL )
    ).

    IF mt_hu_content IS INITIAL.
      ev_leave_screen = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD func_complete_ship_hu_wdiff.
    CLEAR ev_leave_screen.

    IF line_exists( mt_verified_items[ sn_idn_complete = abap_false ] ).
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e010(zmc_out_ui_packing).
    ENDIF.

    mo_sp->get_weights(
      IMPORTING
        ev_scale_hu_weight_in_kg  = DATA(lv_scale_weight_kg)                 " Scale HU Weight (KG)
    ).

    IF ms_pack-pack_mat_id IS INITIAL.
      mo_sp->update_weight_in_hu(
        EXPORTING
          iv_guid_hu         = mo_sp->get_source_hu_guid( )                 " Unique Internal Identification of a Handling Unit
          iv_scale_hu_weight = lv_scale_weight_kg                  " Scale HU Weight (KG)
          iv_scale_hu_meins  = zif_c_mdm_tool=>c_units-kg                 " Base Unit of Measure
      ).
      mv_new_hu = mv_huident.
      mv_new_guid_hu = mo_sp->get_source_hu_guid( ).

      DATA(lt_verified_items) = mt_verified_items.
      CLEAR mt_verified_items.
      mo_verified_items_table->refresh( ).

      save_all_sn_idn( mv_new_guid_hu ).

    ELSE.
      DATA(lt_stock_lines) = move_stock_lines_from_original( ).

      TRY.
          mo_sp->create_hu_in_wc(
            EXPORTING
              iv_pmatid       = ms_pack-pack_mat_id                 " Packaging Material
              iv_source_hu    = mv_huident                  " Handling Unit Identification
              it_stock_repack = lt_stock_lines
            IMPORTING
              ev_new_hu       = mv_new_hu                 " Handling Unit Identification
              ev_new_hu_guid  = mv_new_guid_hu                 " Unique Internal Identification of a Handling Unit
          ).
        CATCH zcx_workstation INTO DATA(lx_wc).
          reset_stock_lines_in_original( ).
          RAISE EXCEPTION lx_wc.
      ENDTRY.
      lt_verified_items = mt_verified_items.

      save_all_sn_idn( mv_new_guid_hu ).

      CLEAR mt_verified_items.
      mo_verified_items_table->refresh( ).

      TRY.
          DATA(lv_weight_diff) = mo_sp->get_weight_difference( iv_old_pmatid = mv_pmatid_source iv_new_pmatid = ms_pack-pack_mat_id ).
        CATCH zcx_workstation	INTO DATA(lx_ws).
          cl_message_helper=>set_msg_vars_for_if_t100_msg( lx_ws ).
          IF mo_function_log IS BOUND.
            mo_function_log->add_message( ).
          ENDIF.
      ENDTRY.

      ADD lv_weight_diff TO lv_scale_weight_kg.
      mo_sp->update_weight_in_hu(
        EXPORTING
          iv_guid_hu         = mv_new_guid_hu                 " Unique Internal Identification of a Handling Unit
          iv_scale_hu_weight = lv_scale_weight_kg                  " Scale HU Weight (KG)
          iv_scale_hu_meins  = zif_c_mdm_tool=>c_units-kg                 " Base Unit of Measure
      ).

    ENDIF.


    mo_sp->determine_stage_area(
       VALUE #( FOR GROUPS docitm OF hu_cont IN lt_verified_items
                  GROUP BY ( docid = hu_cont-docid itemid = hu_cont-itemid )
                   ( docid = docitm-docid itemid = docitm-itemid doccat = wmegc_doccat_pdo )  )
              ).

    mo_sp->close_hu(
      EXPORTING
        iv_hu      = mv_new_hu                 " Unique Internal Identification of a Handling Unit
        iv_hu_guid = mv_new_guid_hu                 " Unique Internal Identification of a Handling Unit
    ).

    CLEAR mt_verified_items.

    print_hu( mv_new_hu ).


    determine_and_call_fin_steps(
        iv_no_pack_info = abap_true
        iv_no_new_hu    = xsdbool( mt_hu_content IS NOT INITIAL )
    ).

    IF mt_hu_content IS INITIAL.
      ev_leave_screen = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD func_delete_all.
    super->func_delete_all( ).
    TRY.
        refresh_hu_w_scanned_sns( ).
      CATCH zcx_workstation ##no_handler.
    ENDTRY.
  ENDMETHOD.


  METHOD func_delete_selected_item.
    DATA(lt_req_sns_idn_sel) = super->func_delete_selected_item( ).

    IF lt_req_sns_idn_sel IS INITIAL.
      RETURN.
    ENDIF.
    LOOP AT lt_req_sns_idn_sel INTO DATA(ls_req_sns_idn).
      DELETE mt_sn_idn_overall_verified
       WHERE docno  = ms_hu_content_current-docno
         AND itemno = ms_hu_content_current-itemno
         AND serial = ls_req_sns_idn-serial.

      APPEND ls_req_sns_idn TO rt_sel.

    ENDLOOP.
    TRY.
        refresh_hu_w_scanned_sns( ).
      CATCH zcx_workstation ##no_handler.
    ENDTRY.
  ENDMETHOD.


  METHOD func_repack_all.
    IF mt_verified_items IS INITIAL.
      DATA(lv_vitm_redisp) = abap_true.
    ENDIF.

    LOOP AT mt_hu_content INTO DATA(ls_hu_content).
      ms_hu_content_current = ls_hu_content.
      READ TABLE mt_verified_items REFERENCE INTO mr_hu_content_verified
           WITH KEY docid = ls_hu_content-docid
                    itemid = ls_hu_content-itemid.
      IF sy-subrc EQ 0.
        ADD ms_item_to_be_proc-repack_pc TO mr_hu_content_verified->quan.
      ELSE.
        APPEND ls_hu_content TO mt_verified_items REFERENCE INTO mr_hu_content_verified.
      ENDIF.
      IF ls_hu_content-sn_idn_complete EQ abap_true.
        mr_hu_content_verified->sn_idn_complete = ls_hu_content-sn_idn_complete.
        mr_hu_content_verified->sn_idn = ls_hu_content-sn_idn.
      ENDIF.
      DELETE mt_hu_content.
      IF ms_hu_content_current-sn_idn_complete EQ abap_true.
        move_current_sel_sns_idn( ).
      ENDIF.
    ENDLOOP.

    IF mo_hu_content_table IS BOUND.
      mo_hu_content_table->refresh( s_stable = VALUE #( row = abap_true ) ).
    ELSEIF mo_source_hu_content_table IS BOUND.
      mo_source_hu_content_table->refresh( s_stable = VALUE #( row = abap_true ) ).
    ENDIF.

    "Refresh UI tables
    IF mo_verified_items_table IS BOUND.
      IF lv_vitm_redisp EQ abap_true.
        TRY.
            mo_verified_items_table->set_data(
              CHANGING
                t_table = mt_verified_items
            ).
          CATCH cx_salv_no_new_data_allowed ##no_handler. " ALV: Setting New Data Not Allowed
        ENDTRY.
        reset_verifid_alv_table( mo_verified_items_table ).
      ELSE.
        mo_verified_items_table->refresh( s_stable = VALUE #( row = abap_true ) ).
      ENDIF.
    ELSEIF mo_dest_hu_content_table IS BOUND.
      IF lv_vitm_redisp EQ abap_true.
        TRY.
            mo_dest_hu_content_table->set_data(
              CHANGING
                t_table = mt_verified_items
            ).
          CATCH cx_salv_no_new_data_allowed ##no_handler. " ALV: Setting New Data Not Allowed
        ENDTRY.
        reset_verifid_alv_table( mo_dest_hu_content_table ).
      ELSE.
        mo_dest_hu_content_table->refresh( s_stable = VALUE #( row = abap_true ) ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD func_split_hu.
    CLEAR ev_leave_screen.

    IF mo_sp->is_weight_difference_exceeded( ).
      DATA(lt_hu_content) = mt_verified_items.
    ELSE.
      lt_hu_content = mt_hu_content.
    ENDIF.

    LOOP AT lt_hu_content REFERENCE INTO DATA(lr_hu_cont).
      CLEAR lr_hu_cont->t_color.
    ENDLOOP.

    DATA(lt_hu_content_changed) = zif_outb_packing_wc_sc_ui~create_split_ui( lt_hu_content ).

    IF mo_sp->is_weight_difference_exceeded( ).

      LOOP AT mt_verified_items REFERENCE INTO DATA(lr_verified).
        READ TABLE lt_hu_content_changed REFERENCE INTO DATA(lr_changed)
             WITH KEY docid = lr_verified->docid
                      itemid = lr_verified->itemid.
        IF sy-subrc NE 0.
          DELETE mt_sn_idn_overall_verified
           WHERE docno = lr_verified->docno
             AND itemno = lr_verified->itemno.
          DELETE mt_verified_items.
        ELSE.
          IF lr_verified->quan NE lr_changed->quan.
            DELETE mt_sn_idn_overall_verified
             WHERE docno = lr_verified->docno
               AND itemno = lr_verified->itemno.
            lr_verified->* = lr_changed->*.
            IF lr_verified->sn_idn_complete NE zif_outb_packing_wc_sp=>c_sn_idn_not_relevant.
              lr_verified->sn_idn_complete = abap_false.
              mo_sp->set_hu_content_text( CHANGING cs_hu_cont = lr_verified->* ).
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      mo_verified_items_table->refresh( VALUE #( row = abap_true ) ).

    ELSE.
      mt_hu_content = lt_hu_content_changed.
      mo_hu_content_table->refresh( VALUE #( row = abap_true ) ).
    ENDIF.
    IF mo_req_sns_idn_table IS BOUND.
      mo_req_sns_idn_table->refresh( VALUE #( row = abap_true ) ).
    ENDIF.

    IF mt_hu_content IS INITIAL.
      ev_leave_screen = abap_true.
    ENDIF.
    mv_recreate_pack_inst = abap_true.
  ENDMETHOD.


  METHOD get_expected_print_docus.
    super->get_expected_print_docus(
      EXPORTING
        iv_hu               = iv_hu
        iv_lgnum            = iv_lgnum
      IMPORTING
        ev_exp_docus        = ev_exp_docus
        ev_dest_stor_type_t = ev_dest_stor_type_t ).

    if mo_sp->is_workcenter_mastercarton_rel( ).
      CLEAR ev_dest_stor_type_t.
    endif.
  ENDMETHOD.


  METHOD get_packing_instr.
    DATA: lt_matnr_ent TYPE tt_matnr_entitled.
    DATA: lt_docid_itemid TYPE tt_docid_itemid.

    IF mo_sp->is_workcenter_mastercarton_rel( ).
      lt_matnr_ent = VALUE #( FOR hu_cont IN mt_hu_content ( matnr = hu_cont-matnr entitled = hu_cont-entitled ) ).
      lt_docid_itemid = VALUE #( FOR hu_cont IN mt_hu_content ( docid = hu_cont-docid itemid = hu_cont-itemid  ) ).
    ELSEIF mv_split_mode EQ abap_true.
      lt_matnr_ent = VALUE #( FOR hu_cont IN mt_verified_items ( matnr = hu_cont-matnr entitled = hu_cont-entitled  ) ).
      lt_docid_itemid = VALUE #( FOR hu_cont IN mt_verified_items ( docid = hu_cont-docid itemid = hu_cont-itemid  ) ).
    ELSEIF mo_sp->is_weight_difference_exceeded( ).
      lt_matnr_ent = VALUE #( FOR hu_cont IN mt_verified_items ( matnr = hu_cont-matnr entitled = hu_cont-entitled ) ).
      lt_docid_itemid = VALUE #( FOR hu_cont IN mt_verified_items ( docid = hu_cont-docid itemid = hu_cont-itemid  ) ).
    ELSE.
      lt_matnr_ent = VALUE #( FOR hu_cont IN mt_hu_content ( matnr = hu_cont-matnr entitled = hu_cont-entitled ) ).
      lt_docid_itemid = VALUE #( FOR hu_cont IN mt_hu_content ( docid = hu_cont-docid itemid = hu_cont-itemid  ) ).

    ENDIF.

    rv_pack_instr = super->get_packing_instr( it_matnr_entitled =  lt_matnr_ent
                                              it_docid_itemid   = lt_docid_itemid ).
  ENDMETHOD.


  METHOD handle_hu_content_single_click.
    DATA: lv_answer TYPE abap_bool.

    CHECK row NE mv_hu_content_clicked_idx AND row IS NOT INITIAL .
    READ TABLE mt_hu_content REFERENCE INTO DATA(lr_hu_content_current) INDEX mv_hu_content_current_idx.
    READ TABLE mt_hu_content REFERENCE INTO DATA(lr_hu_content_sel) INDEX row.
    IF lr_hu_content_sel IS INITIAL.
      RETURN.
    ENDIF.

    IF ms_hu_content_current-sn_idn_complete EQ abap_false AND mt_req_sns_idn IS NOT INITIAL.
      CALL FUNCTION 'Z_OUT_POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'SNs/IDNs not completed'(snc)
          text_question         = 'Not all of the SNs/IDNs are filled. Do you want to stop capturing?'(snf)
          text_button_1         = 'Stop capturing'(sca)         " Text on the first pushbutton
          text_button_2         = 'Continue scan'(csc)         " Text on the second pushbutton
          display_cancel_button = abap_false              " Button for displaying cancel pushbutton
        IMPORTING
          answer                = lv_answer                " Return values: '1', '2', 'A'
        EXCEPTIONS
          OTHERS                = 0.
      IF lv_answer EQ '2'.
        RETURN.
      ENDIF.
      CLEAR mt_req_sns_idn.
      CLEAR mt_req_sns_idn_type.
    ENDIF.

    lr_hu_content_sel->t_color = VALUE #( ( color = VALUE #( col = col_positive int = 0 inv = '0' ) ) ).
    IF lr_hu_content_current IS BOUND.
      CLEAR lr_hu_content_current->t_color.
    ENDIF.

    mv_hu_content_refresh = abap_true.
    CLEAR mt_req_sns_idn.
    ms_hu_content_current = lr_hu_content_sel->*.
    mv_hu_content_current_idx = row.
    mv_hu_content_clicked_idx = row.
    IF ms_hu_content_current-sn_idn_complete EQ abap_false
       OR mv_split_mode IS INITIAL AND NOT mo_sp->is_weight_difference_exceeded( ) AND ms_hu_content_current-sn_idn_complete EQ abap_true.
      CLEAR mv_serid_itm_no.
      CLEAR mt_req_sns_idn_type.
      display_sns_idns_for_current( ).

    ELSE.
      CLEAR mv_serid_itm_no.
      CLEAR: mt_req_sns_idn_type, mt_req_sns_idn_type.
    ENDIF.
    cl_gui_cfw=>set_new_ok_code( 'PAI' ).
    cl_gui_cfw=>flush( ).
  ENDMETHOD.


  METHOD handle_verif_itms_single_click.
  ENDMETHOD.


  METHOD init_verified_items.
    mo_verified_items_ctrl = io_verified_items_cc.
    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = mo_verified_items_ctrl                          " Abstract Container for GUI Controls
            container_name = mo_verified_items_ctrl->get_name( )
          IMPORTING
            r_salv_table   = mo_verified_items_table                         " Basis Class Simple ALV Tables
          CHANGING
            t_table        = mt_verified_items
        ).

        reset_verifid_alv_table( mo_verified_items_table ).


      CATCH cx_salv_msg ##no_handler. "May never happen, only if there is some basic GUI problem
    ENDTRY.
  ENDMETHOD.


  METHOD move_current_sel_sns_idn.
    LOOP AT mt_sn_idn_picking_hu INTO DATA(ls_sn_idn)
         USING KEY docitm
         WHERE docno EQ ms_hu_content_current-docno
           AND itemno EQ ms_hu_content_current-itemno.
      APPEND ls_sn_idn TO mt_sn_idn_overall_verified.
      DELETE mt_sn_idn_picking_hu USING KEY loop_key.
    ENDLOOP.
  ENDMETHOD.


  METHOD move_stock_lines_from_original.
    DATA: lt_qty_repack        TYPE zif_outb_packing_wc_sp=>tt_quant_repack,
          lt_hu_content_repack LIKE mt_hu_content,
          lv_quant_remain      TYPE /scwm/de_quantity.

    mt_hu_content_original_before = mt_hu_content_original.

    LOOP AT mt_verified_items INTO DATA(ls_verified).
      CLEAR: lt_hu_content_repack, lt_qty_repack.
      LOOP AT mt_hu_content_original INTO DATA(ls_ori)
           WHERE matid = ls_verified-matid
             AND docid = ls_verified-docid.

        APPEND ls_ori TO lt_hu_content_repack.
        DELETE mt_hu_content_original.
      ENDLOOP.

      lv_quant_remain = ls_verified-quan.

      SORT lt_hu_content_repack BY quan.

      LOOP AT lt_hu_content_repack INTO ls_ori.
        APPEND VALUE zif_outb_packing_wc_sp=>ty_quant_repack(
                         stock_guid = ls_ori-stock_guid
                         quant = ls_ori-quan
                         meins = ls_ori-meins
                       ) TO lt_qty_repack REFERENCE INTO DATA(lr_quant).
        IF lr_quant->quant GE lv_quant_remain. "Item used partially
          lr_quant->quant = lv_quant_remain.
          SUBTRACT lv_quant_remain FROM ls_ori-quan.
          APPEND ls_ori TO mt_hu_content_original.
          EXIT.
        ELSE. "Item ied completely
          SUBTRACT lr_quant->quant FROM lv_quant_remain.
        ENDIF.
      ENDLOOP.
      APPEND LINES OF lt_qty_repack TO rt_stock_lines.
    ENDLOOP.
  ENDMETHOD.


  METHOD packing_mat_changed.

    mo_sp->get_pack_mat_data(
      EXPORTING
        iv_pack_mat_id   = ms_pack-pack_mat_id                  " Packaging Material
      IMPORTING
        ev_pack_mat      = ms_pack-shu_pack_mat                  " Packaging Material
        ev_pack_mat_text = ms_pack-shu_pack_matx                 " Material Description
        ev_hutype        = ms_pack-shu_hu_type                 " Handling Unit Type
        ev_hutyptext     = ms_pack-shu_hu_type_text                 " Description of Handling Unit Type
    ).

    IF mo_pack_instr_te IS BOUND.
      mo_pack_instr_te->set_textstream( get_packing_instr(   ) ).
    ENDIF.
  ENDMETHOD.


  METHOD product_changed.
    clear_data_when_prod_empty( ).

    DATA(lv_matnr) = get_prod_from_ean_mprn_matnr( iv_product_ean_mpn ).

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


  ENDMETHOD.


  METHOD refresh_hu_w_scanned_sns.
    TYPES: tt_serial TYPE STANDARD TABLE OF gernr WITH EMPTY KEY.
    IF ms_hu_content_current-quan EQ lines( VALUE tt_serial( FOR GROUPS serial OF sn_idn IN mt_req_sns_idn
                                                        GROUP BY ( serial = sn_idn-serial ) WITHOUT MEMBERS
                                                                 ( serial-serial )
                                            )                ).

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

      ms_hu_content_current-sn_idn_complete = abap_true.
      READ TABLE mt_req_sns_idn INTO DATA(ls_req) INDEX 1.
      IF sy-subrc EQ 0.
        DELETE mt_sn_idn_overall_verified WHERE docno = ls_req-docno
                                            AND itemno = ls_req-itemno.
      ENDIF.
      APPEND LINES OF mt_req_sns_idn TO mt_sn_idn_overall_verified.
      IF mo_sp->is_weight_difference_exceeded( ) OR mv_split_mode EQ abap_true.
        CLEAR mt_req_sns_idn.
      ENDIF.
      CLEAR mt_req_sns_idn_type.
    ELSE.
      ms_hu_content_current-sn_idn_complete = abap_false.
    ENDIF.
    mo_sp->set_hu_content_text( CHANGING cs_hu_cont = ms_hu_content_current ).
    MODIFY mt_hu_content INDEX mv_hu_content_current_idx
      FROM ms_hu_content_current TRANSPORTING sn_idn sn_idn_complete.
    IF mo_hu_content_table IS BOUND.
      mo_hu_content_table->refresh( s_stable = VALUE #( row = abap_true ) ).
    ENDIF.
  ENDMETHOD.


  METHOD refresh_repack_w_scanned_sns.
    TYPES: tt_serial TYPE STANDARD TABLE OF gernr WITH EMPTY KEY.
    IF mr_hu_content_verified->quan EQ lines( VALUE tt_serial( FOR GROUPS serial OF sn_idn IN mt_req_sns_idn
                                                        GROUP BY ( serial = sn_idn-serial ) WITHOUT MEMBERS
                                                                 ( serial-serial )
                                            )                ).
      TRY.
          mo_sp->check_sns_idn_in_db(
            EXPORTING
              iv_docno            = mr_hu_content_verified->docno                 " Document Number
              iv_itemno           = mr_hu_content_verified->itemno                 " Item Number
              it_req_sns_idn_type = CONV #( mt_req_sns_idn )
              iv_old_guid_hu      = mo_sp->get_source_hu_guid( )
          ).
        CATCH zcx_workstation INTO DATA(lo_cx).
          CLEAR: ms_req_sns_idn.
          RAISE EXCEPTION lo_cx.
      ENDTRY.

      mr_hu_content_verified->sn_idn_complete = abap_true.
      mo_sp->set_hu_content_text( CHANGING cs_hu_cont = mr_hu_content_verified->* ).

      READ TABLE mt_req_sns_idn INTO DATA(ls_req) INDEX 1.
      IF sy-subrc EQ 0.
        DELETE mt_sn_idn_overall_verified WHERE docno = ls_req-docno
                                            AND itemno = ls_req-itemno.
      ENDIF.
      APPEND LINES OF mt_req_sns_idn TO mt_sn_idn_overall_verified.
      CLEAR: mt_req_sns_idn, mt_req_sns_idn_type.
      mo_req_sns_idn_table->refresh( ).
      CLEAR: mr_hu_content_verified->t_color.
      start_sns_scan_for_verified( ).
    ELSE.
      mr_hu_content_verified->sn_idn_complete = abap_false.
      mo_sp->set_hu_content_text( CHANGING cs_hu_cont = mr_hu_content_verified->* ).
    ENDIF.

    IF mo_verified_items_table IS BOUND.
      mo_verified_items_table->refresh( s_stable = VALUE #( row = abap_true ) ).
    ENDIF.
  ENDMETHOD.


  METHOD repack_current_hu_content.
    IF ms_item_to_be_proc-repack_pc EQ ms_hu_content_current-quan.
      APPEND ms_hu_content_current TO mt_verified_items.
      DELETE mt_hu_content INDEX mv_hu_content_current_idx.
    ENDIF.
  ENDMETHOD.


  METHOD reset_stock_lines_in_original.
    mt_hu_content_original = mt_hu_content_original_before.
  ENDMETHOD.


  METHOD reset_verifid_alv_table.
    io_hu_verified_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>none ).
    set_technical_hu_content_cols( io_hu_verified_table ).
    set_verified_items_cols( io_hu_verified_table ).

    DATA(lo_columns) = io_hu_verified_table->get_columns( ).
    lo_columns->set_optimize(  ).

    io_hu_verified_table->get_functions( )->set_all( abap_false ).
    io_hu_verified_table->display( ).
  ENDMETHOD.


  METHOD save_all_sn_idn.

    LOOP AT mt_sn_idn_overall_verified INTO DATA(ver) GROUP BY ( docno = ver-docno itemno = ver-itemno )
           INTO DATA(ls_grp_docitm).
      DATA(lt_req_sns) = VALUE zif_outb_packing_wc_sp=>tt_req_sns_idn( FOR req_sns IN GROUP ls_grp_docitm ( req_sns ) ).
      mo_sp->save_req_sns_idn(
        EXPORTING
          iv_docno            = ls_grp_docitm-docno                 " Document Number
          iv_itemno           = ls_grp_docitm-itemno                 " Item Number
          it_req_sns_idn_type = lt_req_sns
          iv_new_guid_hu      = iv_guid_hu                 " Handling Unit Identification
          iv_complete_overwrite = abap_false
          iv_old_guid_hu      = mo_sp->get_source_hu_guid( )
      ).
      "Set the return value of the split
      IF mt_sn_idn_scanned_prev IS NOT INITIAL.
        LOOP AT lt_req_sns INTO DATA(req_sns_g) GROUP BY ( serial = req_sns_g-serial ) INTO DATA(ls_grp_serial).
          DELETE mt_sn_idn_scanned_prev
                 WHERE docno  = ls_grp_docitm-docno                 " Document Number
                   AND itemno = ls_grp_docitm-itemno                 " Item Number
                   AND serial = ls_grp_serial-serial.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD scan_2d_barcode.
*    DEFINE add_serid.
*      IF cs_sn_idn-serid_&1_selnum IS NOT INITIAL.
*        cs_sn_idn-serid_&1 = |{ cs_sn_idn-serid_&1_selnum }-{ lv_ts }|.
*      ENDIF.
*    END-OF-DEFINITION.
*    DEFINE add_existing_serid.
*      cs_sn_idn-serid_&1 = ls_sn_idn-serid.
*    END-OF-DEFINITION.
*
*    IF mo_sp->is_weight_difference_exceeded( ) OR mv_split_mode EQ abap_true.
*      READ TABLE mt_sn_idn_picking_hu INTO DATA(ls_sn_idn)
*        WITH KEY docitm
*        COMPONENTS docno  = mr_hu_content_verified->docno
*                   itemno = mr_hu_content_verified->itemno.
*      IF sy-subrc EQ 0.
*        DATA(lv_idx) = CONV numc2( 0 ).
*        LOOP AT mt_sn_idn_picking_hu INTO ls_sn_idn
*             USING KEY docitm
*             WHERE docno  = mr_hu_content_verified->docno
*               AND itemno = mr_hu_content_verified->itemno
*               AND serial = ls_sn_idn-serial.
*          DELETE mt_sn_idn_picking_hu USING KEY loop_key.
*          READ TABLE mt_req_sns_idn_type TRANSPORTING NO FIELDS
*               WITH KEY selnum = ls_sn_idn-selnum.
*          IF sy-subrc EQ 0.
*            lv_idx = sy-tabix.
*            CASE lv_idx.
*              WHEN '01'.
*                add_existing_serid 01.
*              WHEN '02'.
*                add_existing_serid 02.
*              WHEN '03'.
*                add_existing_serid 03.
*              WHEN '04'.
*                add_existing_serid 04.
*              WHEN '05'.
*                add_existing_serid 05.
*              WHEN OTHERS.
*            ENDCASE.
*          ENDIF.
*        ENDLOOP.
*      ELSE.
*        GET TIME STAMP FIELD DATA(lv_ts).
*        add_serid: 01, 02, 03, 04, 05.
*      ENDIF.
*    ELSE.
*      GET TIME STAMP FIELD lv_ts.
*      add_serid: 01, 02, 03, 04, 05.
*    ENDIF.
  ENDMETHOD.


  METHOD set_hu_content_cols.
    CHECK mo_hu_content_table IS BOUND.

    DATA(lo_columns) = mo_hu_content_table->get_columns( ).
    TRY.
        lo_columns->get_column( columnname = c_field-altme )->set_technical( ).
      CATCH cx_salv_not_found ##no_handler. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.
    TRY.
        lo_columns->get_column( columnname = c_field-msehl )->set_technical( ).
      CATCH cx_salv_not_found ##no_handler. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.
    TRY.
        lo_columns->get_column( columnname = c_field-quan_mc )->set_technical( ).
      CATCH cx_salv_not_found ##no_handler. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.
    IF mv_hu_content_prd_hot EQ abap_true.
      TRY.
          CAST cl_salv_column_table( lo_columns->get_column( columnname = c_field-matnr ) )->set_cell_type( if_salv_c_cell_type=>hotspot ).
        CATCH cx_salv_not_found ##no_handler. " ALV: General Error Class (Checked in Syntax Check)
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  method SET_VERIFIED_ITEMS_COLS.
    CHECK mo_verified_items_table IS BOUND.

    DATA(lo_columns) = mo_verified_items_table->get_columns( ).
    TRY.
        lo_columns->get_column( columnname = c_field-altme )->set_technical( ).
      CATCH cx_salv_not_found ##no_handler. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.
    TRY.
        lo_columns->get_column( columnname = c_field-msehl )->set_technical( ).
      CATCH cx_salv_not_found ##no_handler. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.
    TRY.
        lo_columns->get_column( columnname = c_field-quan_mc )->set_technical( ).
      CATCH cx_salv_not_found ##no_handler. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.
    TRY.
        CAST cl_salv_column_table( lo_columns->get_column( columnname = c_field-matnr ) )->set_cell_type( if_salv_c_cell_type=>hotspot ).
      CATCH cx_salv_not_found ##no_handler. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.
  endmethod.


  METHOD start_sns_scan_for_verified.
    READ TABLE mt_verified_items REFERENCE INTO mr_hu_content_verified
         WITH KEY sn_idn_complete = abap_false.
    IF sy-subrc NE 0.
      mv_sns_enter_mode_active = abap_false.
    ELSE.
      IF mr_hu_content_verified->sn_idn_complete NE zif_outb_packing_wc_sp=>c_sn_idn_not_relevant.
        mr_hu_content_verified->sn_idn_complete = abap_false.
        mr_hu_content_verified->t_color = VALUE #( ( color = VALUE #( col = col_positive int = 0 inv = '0' ) ) ).
        mo_verified_items_table->refresh( VALUE #( row = abap_true ) ).
        CLEAR mv_serid_itm_no.

        CLEAR mt_req_sns_idn_type.
        mt_req_sns_idn_type = mo_sp->get_req_sns_idn_type(
                                  iv_docid  = mr_hu_content_verified->docid
                                  iv_itemid = mr_hu_content_verified->itemid
                              ).
        mv_sns_enter_mode_active = abap_true.
        fill_req_sns_idn_type( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_base_ui~pbo_req_sns_idn.
    CALL METHOD super->zif_outb_packing_wc_base_ui~pbo_req_sns_idn
      IMPORTING
        es_screen_data = es_screen_data.

    mv_sn_idn_input_active = abap_false.

    IF mv_sns_enter_mode_active EQ abap_false.
      mv_sn_idn_input_active = abap_false.
    ELSEIF mo_sp->is_weight_difference_exceeded( ).
      IF mr_hu_content_verified IS NOT BOUND OR mr_hu_content_verified->sn_idn_complete EQ abap_false.
        mv_sn_idn_input_active = abap_true.
      ENDIF.
    ELSEIF mv_split_mode EQ abap_true.
      IF mr_hu_content_verified IS BOUND AND mr_hu_content_verified->sn_idn_complete EQ abap_false.
        mv_sn_idn_input_active = abap_true.
      ENDIF.
    ELSE.
      IF ms_hu_content_current-sn_idn_complete EQ abap_false.
        mv_sn_idn_input_active = abap_true.
      ENDIF.
    ENDIF.

    LOOP AT SCREEN.
      IF mv_sn_idn_input_active NE abap_true.
        screen-active = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sc_ui~create_split_ui.
    DATA(lo_sc_split_ui) =  NEW zcl_outb_packing_wc_sc_ui(
      iv_lgnum   = mv_lgnum
      iv_wrkst   = mv_workstation
      iv_lgpla   = mv_lgpla
      iv_huident = mv_huident
      io_sp      = mo_sp
    ).

    lo_sc_split_ui->mv_subscreen = c_subsreen_split.

    lo_sc_split_ui->mt_hu_content_original = mt_hu_content_original.
    lo_sc_split_ui->mt_hu_content = it_hu_content.
    lo_sc_split_ui->mv_split_mode = abap_true.
    lo_sc_split_ui->mt_hu_content_in_split = it_hu_content.
    lo_sc_split_ui->ms_current_dlv_header = ms_current_dlv_header.
    lo_sc_split_ui->mt_current_dlv_items = mt_current_dlv_items.
    lo_sc_split_ui->mt_sn_idn_scanned_prev = mt_sn_idn_overall_verified.
    lo_sc_split_ui->mt_sn_idn_picking_hu = mt_sn_idn_picking_hu.
    lo_sc_split_ui->mv_recreate_pack_inst = abap_true.
    lo_sc_split_ui->mv_sns_enter_mode_active = abap_false.

    "SN IDN is completed on the screen, and not in PICKing HU
    LOOP AT lo_sc_split_ui->mt_hu_content REFERENCE INTO DATA(lr_hu_cont)
         WHERE sn_idn_complete_ori EQ abap_false
           AND sn_idn_complete EQ abap_true.
      lr_hu_cont->sn_idn_complete_prev = abap_true.
    ENDLOOP.

    CALL FUNCTION 'Z_OUT_START_PACKING_SPLIT'
      EXPORTING
        io_sub_ctrl  = lo_sc_split_ui
        io_main_ctrl = mo_main_ui
      IMPORTING
        ev_next_hu   = mv_next_hu.

    rt_hu_content_chng = lo_sc_split_ui->mt_hu_content_in_split.

    mt_hu_content_original = lo_sc_split_ui->mt_hu_content_original.
    mt_sn_idn_overall_verified = lo_sc_split_ui->mt_sn_idn_scanned_prev.

    "Adjust dsplayed SN/IDN
    LOOP AT mt_req_sns_idn INTO DATA(ls_sns_idn).
      READ TABLE mt_sn_idn_overall_verified TRANSPORTING NO FIELDS
           WITH KEY docitm
           COMPONENTS docno = ls_sns_idn-docno
                      itemno = ls_sns_idn-itemno
                      serial = ls_sns_idn-serial.
      IF sy-subrc NE 0.
        DELETE mt_req_sns_idn.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  method ZIF_OUTB_PACKING_WC_SC_UI~GET_NEXT_PROD.
    rv_next_prod = mv_next_prod.
  endmethod.


  METHOD zif_outb_packing_wc_sc_ui~init_data.

    mt_hu_content_original = mo_sp->get_hu_content(
      EXPORTING
        iv_with_sn_idn = abap_true
      IMPORTING
        et_delivery_data = mt_delivery
        et_sn_idn        = mt_sn_idn_overall ).
    condense_hu_content( ).

    mo_sp->get_packmat(
      EXPORTING
        it_mat_quant = VALUE #( FOR hu_content IN mt_hu_content
                                ( matid = hu_content-matid
                                  quan  = hu_content-quan
                                  meins = hu_content-meins ) )
      IMPORTING
        ev_pmatid = DATA(lv_pmatid)                 " Product
        ev_pmat   = DATA(lv_pmatnr)
    ).

    IF ms_pack-shu_pack_mat NE lv_pmatnr.
      ms_pack-shu_pack_mat = lv_pmatnr.
      ms_pack-pack_mat_id = lv_pmatid.
      packing_mat_changed( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sc_ui~init_no_w_diff.
    mv_hu_content_prd_hot  = abap_true. "HU content does not have hotspot

    IF co_hu_content_table IS NOT BOUND.
      init_hu_content( io_hu_content_cc = io_hu_content_cc ).
      co_hu_content_table = mo_hu_content_table.
    ELSEIF mo_hu_content_table IS NOT BOUND.
      mo_hu_content_table = co_hu_content_table.
*      mo_hu_content_table->set_data(
*        CHANGING
*          t_table = mt_hu_content
*      ).
      DATA(lr_events) = CAST cl_salv_events_table(  mo_hu_content_table->get_event( ) ).
      SET HANDLER me->handle_hu_content_single_click FOR lr_events.

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


  METHOD zif_outb_packing_wc_sc_ui~init_split.
    IF co_source_hu_content_table IS NOT BOUND.
      init_hu_content(
        EXPORTING
          io_hu_content_cc   =  io_source_hu_content_cc
        CHANGING
          co_hu_content_table = mo_source_hu_content_table
          co_hu_content_cc    = mo_source_hu_content_ctrl ).
      co_source_hu_content_table = mo_source_hu_content_table.
    ELSEIF mo_hu_content_table IS NOT BOUND.
      mo_source_hu_content_table = co_source_hu_content_table.
      TRY.
          mo_source_hu_content_table->set_data(
            CHANGING
              t_table = mt_hu_content
          ).
        CATCH cx_salv_no_new_data_allowed ##no_handler.
      ENDTRY.
      reset_hu_content_alv( mo_source_hu_content_table ).

      mo_source_hu_content_table->refresh( ).
    ENDIF.

    IF co_captured_sn_idn_table IS NOT BOUND.
      init_req_sns_idn(	io_captured_sn_idn_cc = io_captured_sn_idn_cc  ).
      co_captured_sn_idn_table = mo_req_sns_idn_table.
    ELSEIF mo_req_sns_idn_table IS NOT BOUND.
      mo_req_sns_idn_ctrl = io_captured_sn_idn_cc.
      mo_req_sns_idn_table = co_captured_sn_idn_table.
      TRY.
          mo_req_sns_idn_table->set_data(
            CHANGING
              t_table = mt_req_sns_idn
          ).
        CATCH cx_salv_no_new_data_allowed ##no_handler.
      ENDTRY.
      set_sn_idn_table_conf( ).
      mo_req_sns_idn_table->refresh( ).
    ENDIF.

    IF co_dest_hu_content_table IS NOT BOUND.
      init_verified_items( io_verified_items_cc = io_dest_hu_content_cc ).
      co_dest_hu_content_table = mo_verified_items_table.
    ELSEIF mo_verified_items_table IS NOT BOUND.
      mo_verified_items_table = co_dest_hu_content_table.
      TRY.
          mo_verified_items_table->set_data(
            CHANGING
              t_table = mt_verified_items
          ).
        CATCH cx_salv_no_new_data_allowed ##no_handler.
      ENDTRY.
      DATA(lr_events) = CAST cl_salv_events_table(  mo_verified_items_table->get_event( ) ).
      SET HANDLER me->handle_verif_itms_single_click FOR lr_events.

      mo_verified_items_table->refresh( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_outb_packing_wc_sc_ui~init_w_diff.
    mv_hu_content_prd_hot  = abap_false. "HU content does not have hotspot

    IF co_hu_content_table IS NOT BOUND.
      init_hu_content( io_hu_content_cc = io_hu_content_cc ).
      co_hu_content_table = mo_hu_content_table.
    ELSEIF mo_hu_content_table IS NOT BOUND.
      mo_hu_content_table = co_hu_content_table.
*      mo_hu_content_table->set_data(
*        CHANGING
*          t_table = mt_hu_content
*      ).
*      DATA(lr_events) = CAST cl_salv_events_table(  mo_hu_content_table->get_event( ) ).
*      SET HANDLER me->handle_hu_content_single_click FOR lr_events.

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

    IF co_verified_items_table IS NOT BOUND.
      init_verified_items( io_verified_items_cc = io_verified_items_cc ).
      co_verified_items_table = mo_verified_items_table.
    ELSEIF mo_verified_items_table IS NOT BOUND.
      mo_verified_items_table = co_verified_items_table.
*      mo_verified_items_table->set_data(
*        CHANGING
*          t_table = mt_verified_items
*      ).
      DATA(lr_events) = CAST cl_salv_events_table(  mo_verified_items_table->get_event( ) ).
      SET HANDLER me->handle_verif_itms_single_click FOR lr_events.

      mo_verified_items_table->refresh( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sc_ui~pai_pack.
    IF is_screen_data-shu_pack_mat IS NOT INITIAL AND
       is_screen_data-shu_pack_mat NE ms_pack-shu_pack_mat.

      TRY.
          DATA(ls_prod) = /scmb/cl_md_access_mdl=>get_md_access( )->get_prod(
                                   iv_prodno    = is_screen_data-shu_pack_mat
                                 ).
        CATCH /scmb/cx_md_access. " Exception Class for Master Data Accesses
          RAISE EXCEPTION TYPE zcx_workstation MESSAGE e007(ZMC_OUT_UI_PACKING) with is_screen_data-shu_pack_mat.
      ENDTRY.

      ms_pack-pack_mat_id = ls_prod-prodid.
      packing_mat_changed( ).

    ENDIF.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sc_ui~pbo_pack.
    es_screen_data = ms_pack.
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

      IF screen-name EQ 'ZSTR_OUT_UI_PACK_DATA-SHU_PACK_MAT' .
        IF mv_completion_started EQ abap_true.
          screen-input = '0'.
        ELSEIF ms_pack-shu_pack_mat IS INITIAL.
          screen-input = '1'.
        ENDIF.
      ENDIF.
      IF screen-name EQ 'BTN_CHGPMT'.
        IF mv_completion_started EQ abap_true.
          screen-input = '0'.
        ENDIF.
        IF mo_sp->is_workcenter_mastercarton_rel( ) .
          screen-active = '0'.
        ENDIF.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_outb_ws_subscr_ui~get_status.
    IF mo_sp->is_workcenter_mastercarton_rel( ).
      ev_status = c_main_status_mc.
    ELSE.
      ev_status = c_main_status_sc.
    ENDIF.
    et_excludes = mt_excludes.
  ENDMETHOD.


  METHOD zif_outb_ws_subscr_ui~get_title.
    CLEAR ev_param.
    IF mo_sp->is_workcenter_mastercarton_rel( ).
      ev_title = 'MC'.
    ELSEIF  mv_split_mode EQ abap_true.
      ev_title = 'SC_SPLIT'.
    ELSE.
      IF mo_sp->is_weight_difference_exceeded( ).
        ev_title = 'SC_VERIF'.
      ELSE.
        ev_title = 'SC'.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_outb_ws_subscr_ui~init.
    CALL METHOD super->zif_outb_ws_subscr_ui~init
      EXPORTING
        iv_with_sn_idn_check = abap_true
        io_main_ui           = io_main_ui
      IMPORTING
        ev_subscreen_no      = ev_subscreen_no
        ev_subscreen_prg     = ev_subscreen_prg
        es_common_data       = es_common_data.

    IF mt_delivery IS NOT INITIAL.
      DATA(ls_delivery) = mt_delivery[ 1 ].
      es_common_data-docno = ls_delivery-docno.
      get_delivery_data( iv_docid = ls_delivery-docid ).

    ENDIF.
    mo_sp->get_weights(
      IMPORTING
        ev_scale_hu_weight_in_kg  = es_common_data-scale_hu_weight                  " Scale HU Weight (KG)
        ev_system_hu_weight_in_kg = es_common_data-exp_hu_weight                 " Scale HU Weight (KG)
    ).
    es_common_data-meins_weight = zif_c_mdm_tool=>c_units-kg.
  ENDMETHOD.


  METHOD zif_outb_ws_subscr_ui~pai_tab.
    DATA ls_itm_to_be_proc TYPE ty_item_to_be_proc.
    MOVE-CORRESPONDING is_screen_data TO ls_itm_to_be_proc.

    IF ls_itm_to_be_proc-product_ean_mpn NE ms_item_to_be_proc-product_ean_mpn.
      product_changed( ls_itm_to_be_proc-product_ean_mpn ).
    ENDIF.

    IF ms_item_to_be_proc-product_ean_mpn IS NOT INITIAL AND ms_item_to_be_proc-repack_pc IS NOT INITIAL.
       function_repack_check( ).
    ENDIF.



  ENDMETHOD.


  METHOD zif_outb_ws_subscr_ui~pbo_tab.
    IF ms_item_to_be_proc-repack_pc EQ 0.
      ms_item_to_be_proc-repack_pc = 1.
    ENDIF.
    super->zif_outb_ws_subscr_ui~pbo_tab(
       IMPORTING
         es_screen_data = es_screen_data ).
    LOOP AT SCREEN.
      IF screen-name EQ 'BTN_REPACKHU' AND line_exists( mt_excludes[ table_line = c_func_complete_ship_hu ] ).
        screen-input = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
    MOVE-CORRESPONDING ms_item_to_be_proc to es_screen_data.
  ENDMETHOD.


  METHOD zif_outb_ws_subscr_ui~process_user_command.
    CHECK mv_error_in_pai EQ abap_false.
    CASE iv_ucomm.
      WHEN c_func_change_pack_mat.
        CLEAR: ms_pack-shu_pack_mat    ,
               ms_pack-shu_pack_matx   ,
               ms_pack-shu_hu_type     ,
               ms_pack-shu_hu_type_text.

      WHEN c_func_complete_ship_hu.
        func_complete_ship_hu(
          IMPORTING
            ev_leave_screen = ev_leave_screen ).

      WHEN c_func_split_hu.
        func_split_hu(
          IMPORTING
            ev_leave_screen = ev_leave_screen
        ).

      WHEN c_func_repack_all.
        func_repack_all( ).

      WHEN OTHERS.
        super->zif_outb_ws_subscr_ui~process_user_command(
          EXPORTING
            iv_ucomm        = iv_ucomm
          IMPORTING
            es_bapiret      = es_bapiret
            ev_leave_screen = ev_leave_screen
            ev_processed    = ev_processed ).
    ENDCASE.
    IF mv_next_hu IS NOT INITIAL AND mv_next_hu NE mv_huident.
      ev_leave_screen = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
