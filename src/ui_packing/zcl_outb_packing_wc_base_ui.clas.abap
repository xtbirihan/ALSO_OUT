class ZCL_OUTB_PACKING_WC_BASE_UI definition
  public
  abstract
  create protected .

public section.

  interfaces ZIF_OUTB_WS_SUBSCR_UI .
  interfaces ZIF_OUTB_PACKING_WC_BASE_UI .

  constants C_MAIN_STATUS type STRING value 'MAIN_SCREEN' ##NO_TEXT.
  constants C_MAIN_TITTLE type STRING value 'MAIN_TITLE' ##NO_TEXT.
  data MO_REQ_SNS_IDN_TABLE type ref to CL_SALV_TABLE .

  methods CONSTRUCTOR
    importing
      !IV_LGNUM type /SCWM/S_WRK_PACK-LGNUM
      !IV_WRKST type /SCWM/S_WRK_PACK-WORKSTATION
      !IV_LGPLA type /SCWM/LGPLA
      !IV_HUIDENT type /SCWM/DE_HUIDENT .
  methods HANDLE_HU_CONTENT_SINGLE_CLICK
    for event LINK_CLICK of CL_SALV_EVENTS_TABLE
    importing
      !ROW .
protected section.

  constants C_FUNC_CHANGE_DELIVERY type SYUCOMM value 'CHGDEL' ##NO_TEXT.   "Change Delivery
  constants C_FUNC_CHANGE_PACK_MAT type SYUCOMM value 'CHGPMT' ##NO_TEXT.   "Change Pack. Mat.
  constants C_FUNC_COMPLETE_SHIP_HU type SYUCOMM value 'COMSHU' ##NO_TEXT.   "Complete Ship HU
  constants C_FUNC_DELETE_ALL type SYUCOMM value 'DELALL' ##NO_TEXT.   "Delete All
  constants C_FUNC_DELETE_MISS_ITMS type SYUCOMM value 'DELMIS' ##NO_TEXT.
  constants C_FUNC_DELETE_SELECTED_ITM type SYUCOMM value 'DELSEL' ##NO_TEXT.   "Delete Selected Itm.
  constants C_FUNC_SCAN_2D_BARCODE type SYUCOMM value 'SCNBAR' ##NO_TEXT.
  constants C_SELDESCR type FIELDNAME value 'SELDESCR' ##NO_TEXT.
  constants C_SELNUM type FIELDNAME value 'SELNUM' ##NO_TEXT.
  constants C_SERID type FIELDNAME value 'SERID' ##NO_TEXT.
  data:
    BEGIN OF c_field,
      matnr           TYPE fieldname VALUE 'MATNR',
      maktx           TYPE fieldname VALUE 'MAKTX',
      mfrpn           TYPE fieldname VALUE 'MFRPN',
      ean11           TYPE fieldname VALUE 'EAN11',
      quan            TYPE fieldname VALUE 'QUAN',
      quan_mc         TYPE fieldname VALUE 'QUAN_MC',
      altme           TYPE fieldname VALUE 'ALTME',
      msehl           TYPE fieldname VALUE 'MSEHL',
      docid           TYPE fieldname VALUE 'DOCID',
      itemid          TYPE fieldname VALUE 'ITEMID',
      docno           TYPE fieldname VALUE 'DOCNO',
      meins           TYPE fieldname VALUE 'MEINS',
      stock_guid      TYPE fieldname VALUE 'STOCK_GUID',
      itemno          TYPE fieldname VALUE 'ITEMNO',
      matid           TYPE fieldname VALUE 'MATID',
      sn_idn          TYPE fieldname VALUE 'SN_IDN',
      sn_idn_complete TYPE fieldname VALUE 'SN_IDN_COMPLETE',
    END OF c_field .
  data MO_HU_CONTENT_CTRL type ref to CL_GUI_CUSTOM_CONTAINER .
  data MO_HU_CONTENT_TABLE type ref to CL_SALV_TABLE .
  data MO_REQ_SNS_IDN_CTRL type ref to CL_GUI_CUSTOM_CONTAINER .
  data MO_SP type ref to ZIF_OUTB_PACKING_WC_SP .
  data MS_HU_CONTENT_CURRENT type ZSTR_OUT_UI_HU_CONT .
  data MS_REQ_SNS_IDN type ZSTR_OUT_REQUESTED_SNS_IDN .
  data MT_DELIVERY type ZTT_OUT_UI_DELIVERY .
  data MT_EXCLUDES type STRING_TABLE .
  data MT_HU_CONTENT type ZIF_OUTB_PACKING_WC_SP=>TT_HU_CONTENT .
  data MT_HU_CONTENT_ORIGINAL type ZIF_OUTB_PACKING_WC_SP=>TT_HU_CONTENT .
  data:
    mt_req_sns_idn TYPE STANDARD TABLE OF zstr_out_requested_sns_idn_t .
  data MT_REQ_SNS_IDN_TYPE type ZIF_OUTB_PACKING_WC_SP=>TT_REQ_SNS_IDN_TYPE .
  data MV_HUIDENT type /SCWM/DE_HUIDENT .
  data MV_HU_CONTENT_CURRENT_IDX type I .
  data MV_LGNUM type /SCWM/S_WRK_PACK-LGNUM .
  data MV_LGPLA type /SCWM/LGPLA .
  data MV_NEW_GUID_HU type /SCWM/GUID_HU .
  data MV_NEW_HU type /SCWM/HUIDENT .
  data MV_NEXT_HU type /SCWM/HUIDENT .
  data MV_SCALE_WEIGHT_IN_KG type ZDE_SCALE_HU_WEIGHT .
  data MV_SERID_ITM_NO type /SCWM/DE_ITEMNO_R .
  data MV_SUBSCREEN type SYDYNNR .
  data MV_SUBSCREEN_PRG type SYREPID .
  data MV_USER_IS_SUPERVISOR type ABAP_BOOL .
  data MV_WORKSTATION type /SCWM/S_WRK_PACK-WORKSTATION .
  data MO_VERIFIED_ITEMS_TABLE type ref to CL_SALV_TABLE .

  methods FILL_EXCLUDED_FUNC .
  methods ADD_SERID
    importing
      !IV_SELNUM type ZDE_SELNUM
      !IV_SELDESCR type ZDE_NUMBER_DESCRIPTION
      !IV_SERID type /SCWM/DE_SERID .
  methods CLEAR_DATA_WHEN_PROD_EMPTY .
  methods CONDENSE_HU_CONTENT .
  methods DETERMINE_AND_CALL_FIN_STEPS
    importing
      !IV_NO_PACK_INFO type ABAP_BOOL optional .
  methods DISPLAY_LOG .
  methods FILL_REQ_SNS_IDN_TYPE .
  methods FINISH_FUNCTION_LOG
    importing
      !IV_EXTERNAL_ID type STRING
      !IV_SAVE_ONLY_IF_ERROR type ABAP_BOOL
      !IV_DISPLAY type ABAP_BOOL
    exporting
      value(EV_DISPLAYED) type ABAP_BOOL .
  methods FUNC_DELETE_ALL .
  methods FUNC_DELETE_SELECTED_ITEM
    returning
      value(RT_SEL) type ZIF_OUTB_PACKING_WC_SP=>TT_REQ_SNS_IDN .
  methods GET_EXPECTED_PRINT_DOCUS
    importing
      !IV_HU type /SCWM/HUIDENT
      !IV_LGNUM type /SCWM/LGNUM
    exporting
      !EV_EXP_DOCUS type STRING
      !EV_DEST_STOR_TYPE_T type STRING .
  methods GET_PACKING_INSTR
    returning
      value(RV_PACK_INSTR) type STRING .
  methods INIT_HU_CONTENT
    importing
      !IO_HU_CONTENT_CC type ref to CL_GUI_CUSTOM_CONTAINER .
  methods INIT_REQ_SNS_IDN
    importing
      !IO_CAPTURED_SN_IDN_CC type ref to CL_GUI_CUSTOM_CONTAINER .
  methods SAVE_SNS_IDN .
  methods SCAN_2D_BARCODE
    changing
      !CS_SN_IDN type ZSTR_OUT_REQUESTED_SNS_IDN .
  methods SET_HU_CONTENT_COLS
  abstract .
  methods START_FUNCTION_LOG .
  methods ADD_SERIDS
    importing
      !IS_SCREEN_DATA type ZSTR_OUT_REQUESTED_SNS_IDN
    returning
      value(RV_OK) type ABAP_BOOL .
  methods SET_TECHNICAL_HU_CONTENT_COLS
    importing
      !IO_HU_CONTENT_TABLE type ref to CL_SALV_TABLE .
  methods GET_PROD_FROM_EAN_MPRN_MATNR
    importing
      !IV_PRODUCT_EAN_MPN type ZDE_INB_PRODUCT_EAN_MPN
    returning
      value(RV_MATNR) type MATNR
    raising
      ZCX_WORKSTATION .
private section.

  constants C_FUNC_LEAVE type SYUCOMM value 'LEAVE' ##NO_TEXT.
  data MV_BALLOGHNDL type BALLOGHNDL .
  constants C_FUNC_CHANGE_CARRIER type SYUCOMM value 'CHANGE_CAR' ##NO_TEXT.

  methods FUNC_CHANGE_CARRIER .
  methods FUNC_DELETE_MISS_ITMS
    exporting
      !EV_LEAVE_SCREEN type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_OUTB_PACKING_WC_BASE_UI IMPLEMENTATION.


  METHOD add_exception.
    TRY.
        CALL METHOD cl_message_helper=>set_msg_vars_for_if_t100_msg
          EXPORTING
            text = io_t100.
      CATCH cx_sy_message_illegal_text.    "
    ENDTRY.

    sy-msgty = COND #( WHEN iv_msgty IS NOT INITIAL THEN iv_msgty ELSE 'E' ).

    mo_function_log->add_message( ).
  ENDMETHOD.


  METHOD add_serid.
    APPEND INITIAL LINE TO mt_req_sns_idn REFERENCE INTO DATA(lr_sns_idn).
    lr_sns_idn->serial_pos = mv_serid_itm_no.
    lr_sns_idn->selnum   = iv_selnum.
    lr_sns_idn->seldescr = iv_seldescr.
    lr_sns_idn->serid    = iv_serid.
    lr_sns_idn->docno    = iv_docno.
    lr_sns_idn->itemno   = iv_itemno.
    lr_sns_idn->serial   = mv_serial_number.

  ENDMETHOD.


  METHOD add_serids.
    DATA lv_count_serid TYPE i.
    DEFINE check_serid.
      IF is_screen_data-serid_&1 IS NOT INITIAL.
        ADD 1 TO lv_count_serid.
        ms_req_sns_idn-serid_&1 = is_screen_data-serid_&1.
        IF is_screen_data-serid_&1_selnum EQ zcl_outb_packing_wc_sp=>c_serial_num_id_type.
          IF strlen( ms_req_sns_idn-serid_&1 ) LE 18.
            mv_serial_number = ms_req_sns_idn-serid_&1.
          ELSE.
            mv_serial_number = substring( val = ms_req_sns_idn-serid_&1 off = strlen( ms_req_sns_idn-serid_&1 ) - 18 ).
          ENDIF.
          READ TABLE mt_req_sns_idn TRANSPORTING NO FIELDS
               WITH KEY serial = mv_serial_number.
          IF sy-subrc EQ 0.
            CLEAR: ms_req_sns_idn-serid_01, ms_req_sns_idn-serid_02, ms_req_sns_idn-serid_03, ms_req_sns_idn-serid_04,
                   ms_req_sns_idn-serid_05.
            RAISE EXCEPTION TYPE zcx_workstation MESSAGE e020(zmc_out_ui_packing) WITH mv_serial_number.
          ENDIF.
        ENDIF.
      ENDIF.
    END-OF-DEFINITION.

    CLEAR mv_serial_number.
    check_serid: 01, 02, 03, 04, 05.

    IF lv_count_serid EQ 0.
      RETURN.
    ELSEIF  mv_serial_number IS INITIAL.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e031(zmc_out_ui_packing).
    ENDIF.

    TRY.
        check_pick_hu_if_relevant( ).

        IF iv_docno IS NOT INITIAL.
          mo_sp->check_sns_idn_in_db(
            iv_docno            = iv_docno
            iv_itemno           = iv_itemno
            it_req_sns_idn_type = VALUE #( ( serial = mv_serial_number docno = iv_docno itemno = iv_itemno ) )
            iv_old_guid_hu      = mo_sp->get_source_hu_guid( )
          ).
        ENDIF.

      CATCH zcx_workstation INTO DATA(lx_ws).
        CLEAR: ms_req_sns_idn-serid_01, ms_req_sns_idn-serid_02, ms_req_sns_idn-serid_03, ms_req_sns_idn-serid_04,
               ms_req_sns_idn-serid_05.
        RAISE EXCEPTION lx_ws.
    ENDTRY.

    IF lv_count_serid NE lines( mt_req_sns_idn_type ).
      RETURN.
    ENDIF.

    READ TABLE mt_req_sns_idn TRANSPORTING NO FIELDS
         WITH KEY serial = mv_serial_number.
    IF sy-subrc EQ 0.
      CLEAR: ms_req_sns_idn-serid_01, ms_req_sns_idn-serid_02, ms_req_sns_idn-serid_03, ms_req_sns_idn-serid_04,
             ms_req_sns_idn-serid_05.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e020(zmc_out_ui_packing) WITH mv_serial_number.
    ENDIF.

*    CATCH zcx_workstation. " Workstation errors
*    READ TABLE mt_sn_idn_overall_verified TRANSPORTING NO FIELDS
*         WITH KEY serial = mv_serial_number.
*    IF sy-subrc EQ 0.
*      CLEAR: ms_req_sns_idn-serid_01, ms_req_sns_idn-serid_02, ms_req_sns_idn-serid_03, ms_req_sns_idn-serid_04,
*             ms_req_sns_idn-serid_05.
*      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e020(zmc_out_ui_packing) WITH mv_serial_number.
*    ENDIF.

    ADD 1 TO mv_serid_itm_no.

    IF is_screen_data-serid_01_selnum IS NOT INITIAL.
      add_serid(
        EXPORTING
          iv_selnum   = is_screen_data-serid_01_selnum
          iv_seldescr = is_screen_data-serid_01_seldescr
          iv_serid    = is_screen_data-serid_01
      ).
    ENDIF.
    IF is_screen_data-serid_02_selnum IS NOT INITIAL.
      add_serid(
        EXPORTING
          iv_selnum   = is_screen_data-serid_02_selnum
          iv_seldescr = is_screen_data-serid_02_seldescr
          iv_serid    = is_screen_data-serid_02
      ).
    ENDIF.
    IF is_screen_data-serid_03_selnum IS NOT INITIAL.
      add_serid(
        EXPORTING
          iv_selnum   = is_screen_data-serid_03_selnum
          iv_seldescr = is_screen_data-serid_03_seldescr
          iv_serid    = is_screen_data-serid_03
      ).
    ENDIF.
    IF is_screen_data-serid_04_selnum IS NOT INITIAL.
      add_serid(
        EXPORTING
          iv_selnum   = is_screen_data-serid_04_selnum
          iv_seldescr = is_screen_data-serid_04_seldescr
          iv_serid    = is_screen_data-serid_04
      ).
    ENDIF.
    IF is_screen_data-serid_05_selnum IS NOT INITIAL.
      add_serid(
        EXPORTING
          iv_selnum   = is_screen_data-serid_05_selnum
          iv_seldescr = is_screen_data-serid_05_seldescr
          iv_serid    = is_screen_data-serid_05
      ).
    ENDIF.
    rv_ok = abap_true.

    CLEAR: ms_req_sns_idn-serid_01, ms_req_sns_idn-serid_02, ms_req_sns_idn-serid_03, ms_req_sns_idn-serid_04,
           ms_req_sns_idn-serid_05.

    "Renumber item ID
    renumber_sns_idns_itemid( ).
    IF mo_req_sns_idn_table IS BOUND.
      mo_req_sns_idn_table->refresh( ).
    ENDIF.

  ENDMETHOD.


  method CHECK_PICK_HU_IF_RELEVANT.
  endmethod.


  METHOD check_sn_idn.
    mo_sp->check_sns_idn_in_db(
        iv_docno            = ms_hu_content_current-docno
        iv_itemno           = ms_hu_content_current-itemno
        it_req_sns_idn_type = CONV #( mt_req_sns_idn )
        iv_old_guid_hu      = iv_old_guid_hu
    ).
  ENDMETHOD.


  METHOD clear_data_when_prod_empty.
    CLEAR: mv_serid_itm_no, mt_req_sns_idn, mv_new_hu, mv_new_guid_hu, mt_new_huitm, mt_req_sns_idn_type, ms_req_sns_idn.
  ENDMETHOD.


  METHOD condense_hu_content.
    DATA lt_hu_content_grp LIKE mt_hu_content.

    LOOP AT mt_hu_content_original INTO DATA(content) GROUP BY ( matid = content-matid
                                                                 docid =  content-docid
                                                                 itemid = COND /scdl/dl_itemid( WHEN iv_only_doc_level EQ abap_false
                                                                                                THEN content-itemid
                                                                                                ELSE 0 ) )
           REFERENCE INTO DATA(lr_grp).

      CLEAR lt_hu_content_grp.

      LOOP AT GROUP lr_grp INTO DATA(ls_content).
        APPEND ls_content TO lt_hu_content_grp.
      ENDLOOP.

      IF lines( lt_hu_content_grp ) EQ 1.
        APPEND LINES OF lt_hu_content_grp TO mt_hu_content.
      ELSE.
        READ TABLE lt_hu_content_grp INTO DATA(ls_hu_contens_sum) INDEX 1.
        CLEAR ls_hu_contens_sum-stock_guid.
        LOOP AT lt_hu_content_grp FROM 2 INTO ls_content.
          ADD ls_content-quan TO ls_hu_contens_sum-quan.
          IF ls_hu_contens_sum-altme EQ ls_content-altme.
            ADD ls_content-quan_mc TO ls_hu_contens_sum-quan_mc.
          ELSE.
            ls_hu_contens_sum-altme = '*'.
            ls_hu_contens_sum-msehl = '*'.
            ls_hu_contens_sum-quan_mc = 0.
          ENDIF.
        ENDLOOP.
        ls_hu_contens_sum-quan_mc_displ = ls_hu_contens_sum-quan_mc.
        APPEND ls_hu_contens_sum TO mt_hu_content.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
    mv_lgnum   = iv_lgnum  .
    mv_workstation   = iv_wrkst  .
    mv_lgpla   = iv_lgpla  .
    mv_huident = iv_huident.
    SELECT COUNT( * ) FROM zout_wh_superv
           WHERE uname EQ @sy-uname
             AND lgnum EQ @iv_lgnum
           INTO @DATA(lv_cnt).
    IF lv_cnt NE 0.
      mv_user_is_supervisor = abap_true.
    ENDIF.
    IF sy-tcode EQ 'ZPACKING_WORKCENTERN'.
      mv_user_is_supervisor = abap_false.
    ENDIF.

    mo_text_handling = zcl_text_handling=>get_instance_for_warehouse( iv_lgnum ).
  ENDMETHOD.


  METHOD determine_and_call_fin_steps.
    DATA lv_huident TYPE /scwm/huident.
    DATA lv_prod TYPE  zstr_out_ui_common-product_ean_mpn.

    IF mt_hu_content IS INITIAL AND iv_no_new_hu EQ abap_false.
      DATA(lv_with_hu) = abap_true.
    ENDIF.

    get_expected_print_docus(
        EXPORTING
          iv_hu         = mv_new_hu                 " Handling Unit Identification
          iv_lgnum      = mv_lgnum
        IMPORTING
          ev_exp_docus        = DATA(lv_exp_docus)
          ev_dest_stor_type_t = DATA(lv_dest_stor_type_t)
    ).

    IF iv_no_pack_info EQ abap_false.
      DATA(lv_pack_instr) = get_packing_instr(
                  it_matnr_entitled =  VALUE #( ( matnr = ms_hu_content_current-matnr entitled = ms_hu_content_current-entitled  ) )
                  it_docid_itemid   = VALUE #( FOR item IN mt_current_dlv_items WHERE ( docid = ms_hu_content_current-docid
                                                                                    AND itemid = ms_hu_content_current-itemid )
                             ( docid = item-docid itemid = item-itemid ) ) ).

      CALL FUNCTION 'Z_OUT_DISPLAY_PACK_INST'
        EXPORTING
          iv_pack_inst = lv_pack_instr
          iv_with_hu   = COND flag( WHEN lv_exp_docus IS NOT INITIAL THEN abap_false ELSE lv_with_hu )
        IMPORTING
          ev_huident   = lv_huident.                  " Handling Unit Identification
      IF lv_huident IS NOT INITIAL.
        mv_next_hu = lv_huident.
      ENDIF.
    ENDIF.

    IF mo_sp->is_workcenter_mastercarton_rel( ) EQ abap_false.
      CALL FUNCTION 'Z_OUT_DISPLAY_EXP_DOC'
        EXPORTING
          iv_exp_doc          = lv_exp_docus
          iv_dest_stor_type_t = lv_dest_stor_type_t
          iv_with_hu          = lv_with_hu
        IMPORTING
          ev_huident          = lv_huident.                 " Handling Unit Identification

      IF lv_huident IS NOT INITIAL.
        mv_next_hu = lv_huident.
      ENDIF.
    ELSE.
      CLEAR mv_next_prod.
      CALL FUNCTION 'Z_OUT_DISPLAY_EXP_DOC_W_PROD'
        EXPORTING
          iv_exp_doc = lv_exp_docus
        IMPORTING
          ev_prod    = lv_prod.                 " Handling Unit Identification

      IF lv_prod IS NOT INITIAL.
        mv_next_prod = lv_prod.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD display_log.
    DATA: ls_display_profile TYPE  bal_s_prof.

    CHECK mo_function_log IS BOUND.

    TRY.
        CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
          IMPORTING
            e_s_display_profile = ls_display_profile.                  " Display Profile
        mo_function_log->display_log( iv_loghandle = mv_balloghndl is_display_profile = ls_display_profile ).
      CATCH /scwm/cx_basics ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  method FILL_EXCLUDED_FUNC ##needed.
  endmethod.


  METHOD fill_req_sns_idn_type.
    DATA: lv_num       TYPE n LENGTH 2,
          lv_fieldname TYPE fieldname.
    CLEAR ms_req_sns_idn.
    LOOP AT mt_req_sns_idn_type INTO DATA(ls_type).
      lv_num = sy-tabix.
      lv_fieldname = |{ c_serid }_{ lv_num }_{ c_selnum }|.
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE ms_req_sns_idn TO FIELD-SYMBOL(<lv_selnum>).
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      <lv_selnum> = ls_type-selnum.

      lv_fieldname = |{ c_serid }_{ lv_num }_{ c_seldescr }|.
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE ms_req_sns_idn TO FIELD-SYMBOL(<lv_description>).
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      <lv_description> = ls_type-description.
    ENDLOOP.
  ENDMETHOD.


  METHOD finish_function_log.
    DATA: ls_display_profile TYPE  bal_s_prof.
    IF iv_save_only_if_error EQ abap_true AND mo_function_log->get_severity( ) NA 'AEX'.
      RETURN.
    ENDIF.
    mo_function_log->save_applog(
      EXPORTING
        is_log       = VALUE #( extnumber = |Warehouse { mv_lgnum }, Workst. { mv_workstation }, Function { iv_external_id } |
                                object = zif_wme_c=>gs_msgobj-zewm
                                subobject = zif_wme_c=>gs_msgsubobj-zout_pack_ui )
      IMPORTING
        ev_loghandle = mv_balloghndl ) ##NO_TEXT.                 " Log Handle

    TRY.
        mo_function_log->save_applog2db( iv_loghandle = mv_balloghndl ).
      CATCH /scwm/cx_basics ##NO_HANDLER.
    ENDTRY.

    IF iv_display EQ abap_true AND mo_function_log->get_severity( ) CA 'AEX'.
      TRY.
          CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
            IMPORTING
              e_s_display_profile = ls_display_profile.                  " Display Profile
          mo_function_log->display_log( iv_loghandle = mv_balloghndl is_display_profile = ls_display_profile ).
          ev_displayed = abap_true.
        CATCH /scwm/cx_basics ##NO_HANDLER.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD free_handlers.
    IF mo_hu_content_table IS BOUND.
      DATA(lr_events) = CAST cl_salv_events_table(  mo_hu_content_table->get_event( ) ).
      SET HANDLER me->handle_hu_content_single_click FOR lr_events ACTIVATION abap_false.
    ENDIF.
  ENDMETHOD.


  method FUNC_CHANGE_CARRIER.
  endmethod.


  METHOD func_delete_all.
    CHECK mo_req_sns_idn_table IS BOUND.
    CLEAR mt_req_sns_idn.
    mo_req_sns_idn_table->refresh(
      EXPORTING
        s_stable     = VALUE #( row = abap_true col = abap_true ) ).
    DELETE mt_sn_idn_overall_verified
      WHERE docno  = ms_hu_content_current-docno
        AND itemno = ms_hu_content_current-itemno.

    IF mt_req_sns_idn_type IS INITIAL OR ms_req_sns_idn IS INITIAL.
      mt_req_sns_idn_type = mo_sp->get_req_sns_idn_type(
                                iv_docid  = ms_hu_content_current-docid
                                iv_itemid = ms_hu_content_current-itemid
                            ).

      IF mt_req_sns_idn_type IS NOT INITIAL.
        fill_req_sns_idn_type( ).
      ENDIF.
    ENDIF.
    CLEAR mv_serid_itm_no.
  ENDMETHOD.


  METHOD func_delete_miss_itms.
    data lt_bapiret TYPE bapiret2_tab.

    start_function_log( ).
    TRY.
        mo_sp->cancel_picking(
          IMPORTING
            et_bapiret = lt_bapiret                 " Error Messages
        ).
        mo_function_log->add_log( it_prot = lt_bapiret ).
        CLEAR lt_bapiret.
        mo_sp->move_hu_to_lost_and_found(
          IMPORTING
            et_bapiret = lt_bapiret                 " Error Messages
        ).
        mo_function_log->add_log( it_prot = lt_bapiret ).
        CLEAR lt_bapiret.
      CATCH zcx_workstation INTO DATA(lx_ws).
        mo_function_log->add_log( it_prot = lx_ws->messages ).
    ENDTRY.

    finish_function_log(
      EXPORTING
        iv_external_id        = 'Delete Missing Items'
        iv_save_only_if_error = abap_false
        iv_display            = abap_true
    ) ##no_text.
  ENDMETHOD.


  METHOD func_delete_selected_item.
    DATA lt_req_sns_idn_sel LIKE mt_req_sns_idn.
    CHECK mo_req_sns_idn_table IS BOUND.

    mo_req_sns_idn_table->get_metadata( ).
    DATA(lt_sel) = mo_req_sns_idn_table->get_selections( )->get_selected_rows( ).
    IF lt_sel IS INITIAL.
      RETURN.
    ENDIF.
*    cl_gui_cfw=>flush( ).
*    mo_req_sns_idn_table->check_changed_data( ).
*    mo_req_sns_idn_table->get_selected_rows(
*      IMPORTING
*        et_index_rows = DATA(lt_sel)                 " Indexes of Selected Rows
*    ).
    LOOP AT lt_sel INTO DATA(lv_sel).
      READ TABLE mt_req_sns_idn INDEX lv_sel
           INTO DATA(ls_req_sns_idn).

      APPEND ls_req_sns_idn TO lt_req_sns_idn_sel.

    ENDLOOP.
    LOOP AT lt_req_sns_idn_sel INTO ls_req_sns_idn.
      DELETE mt_req_sns_idn WHERE serial = ls_req_sns_idn-serial.
      APPEND ls_req_sns_idn TO rt_sel.

    ENDLOOP.

    renumber_sns_idns_itemid( ).

    "if only one item is deleted, than it is not varified anymore

    DELETE mt_sn_idn_overall_verified
      WHERE docno  = ms_hu_content_current-docno
        AND itemno = ms_hu_content_current-itemno.

    "Refresh display
    mo_req_sns_idn_table->refresh(
      EXPORTING
        s_stable     = VALUE #( row = abap_true col = abap_true )
    ).

    "Input must be displayed
    IF mt_req_sns_idn_type IS INITIAL OR ms_req_sns_idn IS INITIAL.
      mt_req_sns_idn_type = mo_sp->get_req_sns_idn_type(
                                iv_docid  = ms_hu_content_current-docid
                                iv_itemid = ms_hu_content_current-itemid
                            ).

      IF mt_req_sns_idn_type IS NOT INITIAL.
        fill_req_sns_idn_type( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_delivery_data.
    DATA(lo_dlv) = /scwm/cl_dlv_management_prd=>get_instance( ).
    TRY.
        lo_dlv->query(
          EXPORTING
            it_docid        = VALUE #(  ( doccat = wmegc_doccat_pdo  docid = iv_docid ) )
            is_read_options = VALUE #(  )
            is_exclude_data = VALUE #(  )
            is_include_data = VALUE #( head_partyloc = abap_true head_text = abap_true head_textline = abap_true
                                       item_partyloc = abap_true  )
          IMPORTING
            et_headers        = DATA(lt_dlv_header)
            et_items          = data(lt_dlv_item)
        ).
        READ TABLE lt_dlv_header INTO ms_current_dlv_header INDEX 1.
        mt_current_dlv_items = VALUE #( for dlv_item in lt_dlv_item WHERE ( docid = ms_current_dlv_header-docid ) ( dlv_item ) ).
      CATCH /scdl/cx_delivery INTO DATA(lo_cx).
    ENDTRY.
  ENDMETHOD.


  METHOD get_expected_print_docus.
    ev_exp_docus        = '1. BOM' ##no_text.
    ev_dest_stor_type_t = 'Handling Unit Destination: Storage Type of the POSC Warehouse Task' ##no_text.
  ENDMETHOD.


  method GET_PACKING_INSTR.
    rv_pack_instr = 'All in one'(aio).
  endmethod.


  method GET_PROD_FROM_EAN_MPRN_MATNR.
    DATA: lv_matnr TYPE matnr.

    "Determine possible material numbers
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = iv_product_ean_mpn
      IMPORTING
        output = rv_matnr
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      lv_matnr = iv_product_ean_mpn.
    ENDIF.

    SELECT FROM mara
           FIELDS matnr
           WHERE matnr EQ @lv_matnr
              OR ean11 EQ @iv_product_ean_mpn
              OR mfrpn EQ @iv_product_ean_mpn
           INTO TABLE @DATA(lt_matnr_det).
    IF lt_matnr_det IS INITIAL.
      SELECT FROM mean
             FIELDS matnr
             WHERE ean11 EQ @iv_product_ean_mpn
             INTO TABLE @lt_matnr_det.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e010(zmc_workstation) WITH iv_product_ean_mpn.
      ENDIF.
      rv_matnr = lt_matnr_det[ 1 ]-matnr.
    ELSE.
      rv_matnr = lt_matnr_det[ 1 ]-matnr.
    ENDIF.

  endmethod.


  method HANDLE_HU_CONTENT_SINGLE_CLICK ##needed.

  endmethod.


  METHOD init_hu_content.
    co_hu_content_cc = io_hu_content_cc.
    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = co_hu_content_cc                          " Abstract Container for GUI Controls
            container_name = co_hu_content_cc->get_name( )
          IMPORTING
            r_salv_table   = co_hu_content_table                          " Basis Class Simple ALV Tables
          CHANGING
            t_table        = mt_hu_content
        ).

        reset_hu_content_alv( co_hu_content_table ).
        .

      CATCH cx_salv_msg ##no_handler. "May never happen, only if there is some basic GUI problem
    ENDTRY.
  ENDMETHOD.


  METHOD init_packing_mon.
********************************************************************
*& Key          : <AAHMEDOV>-Nov 28, 2023
*& Request No.  : "GAP-012 - Outbound_VCE_carrier_software_integration"
********************************************************************
*& Description  : Navigate to ZPACK monitor with pre-selected delivery
********************************************************************
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_DOCNO) TYPE  /SCWM/TT_DOCNO
*"----------------------------------------------------------------------
    DATA: lv_lgnum       TYPE /scwm/lgnum,
          lv_monitor     TYPE /scwm/de_monitor,
          ls_opt         TYPE ctu_params,
          ls_bdcdata_fld TYPE bdcdata,
          ls_bdcdata_scr TYPE bdcdata,
          lt_bdcdata     TYPE TABLE OF bdcdata.

    IF sy-repid = zif_wme_c=>gs_tcodes-zout_cons.
      TRY.

          NEW /scwm/cl_dlv_management_prd( )->query(
            EXPORTING
              it_docno      = VALUE /scwm/dlv_docno_itemno_tab( FOR <ls_docno> IN it_docno
                                                                ( docno = |{ <ls_docno>-docno ALPHA = IN }| ) )
            IMPORTING
              et_hu_headers = DATA(lt_hu_hdr)
          ).

          LOOP AT lt_hu_hdr ASSIGNING FIELD-SYMBOL(<ls_hu_hdr>).
            CHECK <ls_hu_hdr>-zz_packtyp EQ zif_wme_c=>gs_hupacktyp-slo
             OR <ls_hu_hdr>-zz_packtyp EQ zif_wme_c=>gs_hupacktyp-spo.

            MESSAGE s023(zmc_out_ui_packing) DISPLAY LIKE wmegc_severity_err.
            RETURN.

          ENDLOOP.

        CATCH /scdl/cx_delivery. " For New Exceptions Use /SCDL/CX_DELIVERY_T100query
      ENDTRY.
    ENDIF.

    GET PARAMETER ID '/SCWM/LGN' FIELD lv_lgnum.

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = lv_lgnum
        iv_process   = zif_param_const=>c_zmon_0001
        iv_parameter = zif_param_const=>c_mon_zpack
      IMPORTING
        ev_constant  = DATA(lv_mon_name) ).

    SET PARAMETER ID zif_param_const=>c_monitor FIELD lv_mon_name.

    ls_bdcdata_scr-program  = '/SCWM/R_WME_MONITOR'.
    ls_bdcdata_scr-dynpro   = '0001'.
    ls_bdcdata_scr-dynbegin = abap_true.
    APPEND ls_bdcdata_scr TO lt_bdcdata.

    ls_bdcdata_fld-fnam = 'BDC_OKCODE'.
    ls_bdcdata_fld-fval = |=%_GC 305 25|.
    APPEND ls_bdcdata_fld TO lt_bdcdata.

    ls_bdcdata_scr-program  = '/SCWM/SAPLWIP_DELIVERY_OUT'.
    ls_bdcdata_scr-dynpro   = '0100'.
    ls_bdcdata_scr-dynbegin = abap_true.
    APPEND ls_bdcdata_scr TO lt_bdcdata.

    ls_bdcdata_fld-fnam = 'BDC_CURSOR'.
    ls_bdcdata_fld-fval = 'S_DOCNO-LOW'.
    APPEND ls_bdcdata_fld TO lt_bdcdata.

    ls_bdcdata_fld-fnam = 'BDC_OKCODE'.
    ls_bdcdata_fld-fval = |=%006|.
    APPEND ls_bdcdata_fld TO lt_bdcdata.

    ls_bdcdata_fld-fnam = 'P_DB'.
    ls_bdcdata_fld-fval = abap_true.
    APPEND ls_bdcdata_fld TO lt_bdcdata.



    "**********************************************************************
    "open screen for entering multiple values:
    ls_bdcdata_scr-program  = 'SAPLALDB'.
    ls_bdcdata_scr-dynpro   = '3000'.
    ls_bdcdata_scr-dynbegin = abap_true.
    APPEND ls_bdcdata_scr TO lt_bdcdata.

    ls_bdcdata_fld-fnam = 'BDC_OKCODE'.
    ls_bdcdata_fld-fval = '=ACPT'.
    APPEND ls_bdcdata_fld TO lt_bdcdata.

    ls_bdcdata_fld-fnam = 'BDC_CURSOR'.
    ls_bdcdata_fld-fval = |RSCSEL_255-SLOW_I(01)|.
    APPEND ls_bdcdata_fld TO lt_bdcdata.

    LOOP AT it_docno ASSIGNING FIELD-SYMBOL(<ls_data>).

      ls_bdcdata_fld-fnam = |RSCSEL_255-SLOW_I(0{ sy-tabix })|.
      ls_bdcdata_fld-fval = <ls_data>-docno.
      APPEND ls_bdcdata_fld TO lt_bdcdata.

    ENDLOOP.
    "**********************************************************************

    ls_bdcdata_scr-program  = '/SCWM/SAPLWIP_DELIVERY_OUT'.
    ls_bdcdata_scr-dynpro   = '0100'.
    ls_bdcdata_scr-dynbegin = abap_true.
    APPEND ls_bdcdata_scr TO lt_bdcdata.

    ls_bdcdata_fld-fnam = 'BDC_CURSOR'.
    ls_bdcdata_fld-fval = 'S_DOCNO-LOW'.
    APPEND ls_bdcdata_fld TO lt_bdcdata.

    ls_bdcdata_fld-fnam = 'BDC_OKCODE'.
    ls_bdcdata_fld-fval = '=CRET'.
    APPEND ls_bdcdata_fld TO lt_bdcdata.

    ls_bdcdata_fld-fnam = 'P_DB'.
    ls_bdcdata_fld-fval = abap_true.
    APPEND ls_bdcdata_fld TO lt_bdcdata.

    ls_opt-dismode = 'E'.  ""-> use P if you don't want to display report screens
    ls_opt-updmode = 'L'.
    ls_opt-defsize = 'X'.
    ls_opt-nobinpt = 'X'.

    CALL TRANSACTION zif_param_const=>c_monitor USING lt_bdcdata OPTIONS FROM ls_opt.

    CLEAR: lv_monitor.

    SET PARAMETER ID '/SCWM/MON' FIELD lv_monitor.

  ENDMETHOD.


  METHOD init_req_sns_idn.


    mo_req_sns_idn_ctrl = io_captured_sn_idn_cc.
    TRY.

        cl_salv_table=>factory(
          EXPORTING
            r_container    = mo_req_sns_idn_ctrl                          " Abstract Container for GUI Controls
          IMPORTING
            r_salv_table   = mo_req_sns_idn_table                          " Basis Class Simple ALV Tables
          CHANGING
            t_table        = mt_req_sns_idn
        ).


        DATA(lo_columns) = mo_req_sns_idn_table->get_columns( ).
        TRY.
            lo_columns->get_column( c_serid )->set_output_length( 50 ).
          CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
        ENDTRY.

        TRY.
            lo_columns->get_column( c_seldescr )->set_output_length( 35 ).
          CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
        ENDTRY.
        TRY.
            lo_columns->get_column( c_selnum )->set_output_length( 8 ).
          CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
        ENDTRY.

        TRY.
            lo_columns->get_column( columnname = c_field-docno )->set_technical( ).
          CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
        ENDTRY.
        TRY.
            lo_columns->get_column( columnname = c_field-itemno )->set_technical( ).
          CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
        ENDTRY.

        mo_req_sns_idn_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>multiple ).
        mo_req_sns_idn_table->display( ).


      CATCH cx_salv_msg ##no_handler. "May never happen, only if there is some basic GUI problem
    ENDTRY.
  ENDMETHOD.


  METHOD print_hu.
    DATA lv_qname    TYPE trfcqnam.

    CHECK mo_sp->is_hu_kep( iv_huident ).

    lv_qname = |{ zif_wme_c=>gs_vce_queue-vce }{ zif_wme_c=>gs_vce_queue-ship }{ sy-datum }{ iv_huident ALPHA = OUT }|.

    CALL FUNCTION 'TRFC_SET_QIN_PROPERTIES'
      EXPORTING
        qin_name   = lv_qname
        no_execute = abap_true.

    CALL FUNCTION 'Z_VCE_SHIP_REQUEST' IN BACKGROUND TASK
      EXPORTING
        iv_lgnum   = mv_lgnum
        iv_huident = iv_huident.

    COMMIT WORK.
  ENDMETHOD.


  METHOD renumber_sns_idns_itemid.
    mv_serid_itm_no = 0.
    LOOP AT mt_req_sns_idn INTO DATA(req_sns) GROUP BY ( serial_pos = req_sns-serial_pos ) INTO DATA(ls_grp_serial_pos).
      ADD 1 TO mv_serid_itm_no.
      LOOP AT GROUP ls_grp_serial_pos REFERENCE INTO DATA(lr_serpos).
        lr_serpos->serial_pos = mv_serid_itm_no.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD reset_hu_content_alv.
    io_hu_content_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>none ).
    set_technical_hu_content_cols( io_hu_content_table ).

    set_hu_content_cols( io_hu_content_table ).

    DATA(lo_columns) = io_hu_content_table->get_columns( ).
    lo_columns->set_optimize(  ).

    DATA(lr_events) = CAST cl_salv_events_table(  io_hu_content_table->get_event( ) ).
    SET HANDLER me->handle_hu_content_single_click FOR lr_events.

    io_hu_content_table->get_functions( )->set_all( abap_false ).
    io_hu_content_table->display( ).
  ENDMETHOD.


  METHOD save_sns_idn.

    mo_sp->save_req_sns_idn(
      EXPORTING
        iv_docno            = ms_hu_content_current-docno
        iv_itemno           = ms_hu_content_current-itemno
        it_req_sns_idn_type = CONV #( mt_req_sns_idn )
        iv_guid_hu          = mv_new_guid_hu
    ).

  ENDMETHOD.


  METHOD scan_2d_barcode.
*    DEFINE add_serid.
*      IF cs_sn_idn-serid_&1_selnum IS NOT INITIAL.
*        cs_sn_idn-serid_&1 = |{ cs_sn_idn-serid_&1_selnum }-{ lv_ts }|.
*      ENDIF.
*    END-OF-DEFINITION.
*
*    GET TIME STAMP FIELD DATA(lv_ts).
*    add_serid: 01, 02, 03, 04, 05.
  ENDMETHOD.


  METHOD set_sn_idn_table_conf.
    DATA(lo_columns) = mo_req_sns_idn_table->get_columns( ).
    TRY.
        lo_columns->get_column( c_serid )->set_output_length( 50 ).
      CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.

    TRY.
        lo_columns->get_column( c_seldescr )->set_output_length( 35 ).
      CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.
    TRY.
        lo_columns->get_column( c_selnum )->set_output_length( 8 ).
      CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.

    TRY.
        lo_columns->get_column( columnname = c_field-docno )->set_technical( ).
      CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.
    TRY.
        lo_columns->get_column( columnname = c_field-itemno )->set_technical( ).
      CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.

    TRY.
        lo_columns->get_column( columnname = c_field-itemno )->set_technical( ).
      CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.

    TRY.
        lo_columns->get_column( columnname = c_field-guid_hu )->set_technical( ).
      CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.

    TRY.
        lo_columns->get_column( columnname = c_field-guid_hu )->set_technical( ).
      CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.

    TRY.
        lo_columns->get_column( columnname = c_field-serial )->set_technical( ).
      CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.
    mo_req_sns_idn_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>multiple ).
  ENDMETHOD.


  METHOD set_technical_hu_content_cols.
    CHECK io_hu_content_table IS BOUND.

    DATA(lo_columns) = io_hu_content_table->get_columns( ).
    TRY.
        lo_columns->get_column( columnname = c_field-docid )->set_technical( ).
      CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.
    TRY.
        lo_columns->get_column( columnname = c_field-itemid )->set_technical( ).
      CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.
    TRY.
        lo_columns->get_column( columnname = c_field-docno )->set_technical( ).
      CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.
    TRY.
        lo_columns->get_column( columnname = c_field-itemno )->set_technical( ).
      CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.
    TRY.
        lo_columns->get_column( columnname = c_field-meins )->set_technical( ).
      CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.
    TRY.
        lo_columns->get_column( columnname = c_field-stock_guid )->set_technical( ).
      CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.
    TRY.
        lo_columns->get_column( columnname = c_field-matid )->set_technical( ).
      CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.
    TRY.
        lo_columns->get_column( columnname = c_field-sn_idn_complete )->set_technical( ).
      CATCH cx_salv_not_found ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
    ENDTRY.
  ENDMETHOD.


  METHOD start_function_log.
    mo_function_log = NEW /scwm/cl_log( iv_lgnum     = mv_lgnum
                                        iv_balobj    = zif_wme_c=>gs_msgobj-zewm
                                        iv_balsubobj = zif_wme_c=>gs_msgsubobj-zout_pack_ui ).
  ENDMETHOD.


  METHOD zif_outb_packing_wc_base_ui~get_next_hu.
    rv_next_hu = mv_next_hu.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_base_ui~pai_req_sns_idn.
    TRY.
        add_serids( is_screen_data ).
      CATCH zcx_workstation INTO DATA(lx_ws).
        mv_error_in_pai = abap_true.
        RAISE EXCEPTION lx_ws.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_outb_packing_wc_base_ui~pbo_req_sns_idn.
    DATA: lv_num       TYPE n LENGTH 2,
          lv_fieldname TYPE fieldname.
    LOOP AT SCREEN.
      IF screen-group1+0(1) = 'R'.
        lv_num = screen-group1+1(2).
        lv_fieldname = |{ c_serid }_{ lv_num }_{ c_selnum }|.
        ASSIGN COMPONENT lv_fieldname OF STRUCTURE ms_req_sns_idn TO FIELD-SYMBOL(<lv_selnum>).
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.
        IF <lv_selnum> IS INITIAL.
          screen-active = '0'.
        ENDIF.
      ENDIF.
      IF ms_req_sns_idn IS INITIAL AND screen-name EQ 'BTN_SCNBAR'.
        screen-active = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
    es_screen_data = ms_req_sns_idn.
    IF ms_req_sns_idn-serid_01_seldescr IS NOT INITIAL AND ms_req_sns_idn-serid_01 IS INITIAL.
      SET CURSOR FIELD 'ZSTR_OUT_REQUESTED_SNS_IDN-SERID_01'.
    ELSEIF ms_req_sns_idn-serid_02_seldescr IS NOT INITIAL AND ms_req_sns_idn-serid_02 IS INITIAL.
      SET CURSOR FIELD 'ZSTR_OUT_REQUESTED_SNS_IDN-SERID_02'.
    ELSEIF ms_req_sns_idn-serid_03_seldescr IS NOT INITIAL AND ms_req_sns_idn-serid_03 IS INITIAL.
      SET CURSOR FIELD 'ZSTR_OUT_REQUESTED_SNS_IDN-SERID_03'.
    ELSEIF ms_req_sns_idn-serid_04_seldescr IS NOT INITIAL AND ms_req_sns_idn-serid_04 IS INITIAL.
      SET CURSOR FIELD 'ZSTR_OUT_REQUESTED_SNS_IDN-SERID_04'.
    ELSEIF ms_req_sns_idn-serid_05_seldescr IS NOT INITIAL AND ms_req_sns_idn-serid_05 IS INITIAL..
      SET CURSOR FIELD 'ZSTR_OUT_REQUESTED_SNS_IDN-SERID_05'.
    ENDIF.
  ENDMETHOD.


  METHOD zif_outb_ws_subscr_ui~get_status.
    ev_status = c_main_status.
    et_excludes = mt_excludes.
  ENDMETHOD.


  METHOD zif_outb_ws_subscr_ui~get_subscreen.
    ev_repid = mv_subscreen_prg.
    ev_screen = mv_subscreen.
  ENDMETHOD.


  METHOD zif_outb_ws_subscr_ui~get_title.
    ev_title = c_main_tittle.
    MESSAGE i000(zmc_out_ui_packing) WITH space INTO ev_param.
  ENDMETHOD.


  METHOD zif_outb_ws_subscr_ui~init.

    ev_subscreen_no = mv_subscreen.
    ev_subscreen_prg = mv_subscreen_prg.

    IF mo_hu_content_table IS BOUND.
      mt_hu_content_original = mo_sp->get_hu_content(
               EXPORTING iv_with_sn_idn = iv_with_sn_idn_check
               IMPORTING et_delivery_data = mt_delivery ).
    ENDIF.

    mo_sp->get_ws_and_hu(
      IMPORTING
        ev_lgpla   = es_common_data-stbin
        ev_huident = es_common_data-huident
    ).

  ENDMETHOD.


  METHOD zif_outb_ws_subscr_ui~pai_tab.
    mv_current_ok_code = iv_ucomm.
    mv_error_in_pai = abap_false.
  ENDMETHOD.


  METHOD zif_outb_ws_subscr_ui~pbo_tab.
    CLEAR mt_excludes.

    IF mt_req_sns_idn IS INITIAL.
      APPEND c_func_delete_all TO mt_excludes.
      APPEND c_func_delete_selected_itm TO mt_excludes.
    ENDIF.
    fill_excluded_func( ).
    LOOP AT SCREEN.
      IF screen-name EQ 'BTN_DELMIS' AND mv_user_is_supervisor EQ abap_false.
        screen-active = 0.
      ENDIF.
      IF screen-name EQ 'BTN_DELSEL' AND line_exists( mt_excludes[ table_line = c_func_delete_selected_itm ] ).
        screen-active = 0.
      ENDIF.
      IF screen-name EQ 'BTN_DELALL' AND line_exists( mt_excludes[ table_line = c_func_delete_all ] ).
        screen-active = 0.
      ENDIF.
      IF screen-group1 EQ 'SNS' AND  mt_req_sns_idn_type IS INITIAL.
        screen-active = 0.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.

    IF mo_req_sns_idn_ctrl IS BOUND.
      mo_req_sns_idn_ctrl->set_visible( COND #( WHEN mt_req_sns_idn_type IS INITIAL THEN abap_false ELSE abap_true  ) ).
    ENDIF.


  ENDMETHOD.


  METHOD zif_outb_ws_subscr_ui~process_user_command.
    CLEAR: es_bapiret, ev_leave_screen, ev_processed.

    CASE iv_ucomm.
      WHEN c_func_change_carrier.
        func_change_carrier( ).
      WHEN c_func_leave.
        ev_leave_screen = abap_true.
        ev_processed = abap_true.
        free_handlers( ).
      WHEN c_func_delete_all.
        func_delete_all( ).
        ev_processed = abap_true.
      WHEN c_func_delete_selected_itm.
        func_delete_selected_item( ).
        ev_processed = abap_true.
      WHEN c_func_delete_miss_itms.
        func_delete_miss_itms(
          IMPORTING
            ev_leave_screen = ev_leave_screen
        ).
        ev_processed = abap_true.
      WHEN c_func_scan_2d_barcode.
        DATA(ls_req_sns_idn) = ms_req_sns_idn.
        scan_2d_barcode( CHANGING cs_sn_idn = ls_req_sns_idn ).
        add_serids( ls_req_sns_idn ).
      WHEN c_func_disp_log.
        display_log( ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
