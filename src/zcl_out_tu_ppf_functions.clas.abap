class ZCL_OUT_TU_PPF_FUNCTIONS definition
  public
  final
  create public .

public section.

  interfaces /SCDL/IF_SP1_MESSAGE_HANDLER .

  class-methods ADD_MESSAGE
    importing
      !IV_PROBLEMCLASS type BAL_S_MSG-PROBCLASS default '4'
    returning
      value(RV_LOG_IS_FULL) type SAP_BOOL .
  class-methods LOAD_ENDED
    importing
      !IV_TU_NUM type /SCWM/DE_TU_NUM
      !IV_TU_SR_ACT_NUM type /SCWM/DE_SR_ACT_NUM
      !IV_APPLICATION_LOG type BALLOGHNDL
      !IV_LGNUM type /SCWM/LGNUM
    returning
      value(RV_OK) type ABAP_BOOL .
  class-methods SEND_MANIFEST
    importing
      !IV_TU_NUM type /SCWM/DE_TU_NUM
      !IV_TU_SR_ACT_NUM type /SCWM/DE_SR_ACT_NUM
      !IV_APPLICATION_LOG type BALLOGHNDL
      !IV_LGNUM type /SCWM/LGNUM
    returning
      value(RV_OK) type ABAP_BOOL .
  methods CONSTRUCTOR
    importing
      !IV_TU_NUM type /SCWM/DE_TU_NUM
      !IV_TU_SR_ACT_NUM type /SCWM/DE_SR_ACT_NUM
      !IV_LGNUM type /SCWM/LGNUM .
  methods POST_GI_FOR_HUS_WITH_SP
    importing
      !IT_GM_HU type /SCWM/T_GM_HU
      !IV_DOCID type /SCDL/DL_DOCID
    raising
      ZCX_WORKSTATION .
  PROTECTED SECTION.
private section.

  class-data SV_APPL_LOG_HANDLE type BALLOGHNDL .
  data MV_TU_NUM type /SCWM/DE_TU_NUM .
  data MV_TU_SR_ACT_NUM type /SCWM/DE_SR_ACT_NUM .
  data MV_LGNUM type /SCWM/LGNUM .

  methods POST_GI_FOR_HUS
    importing
      !IT_GM_HU type /SCWM/T_GM_HU
    raising
      ZCX_WORKSTATION .
  methods POST_GI_WITH_CLASS
    importing
      !IT_GM_HU type /SCWM/T_GM_HU
      !IT_DLV type /SCWM/DLV_DOCID_ITEM_TAB .
  methods POST_GI_WITH_FM
    importing
      !IT_GM_ITM type /SCWM/TT_GMITEM .
  methods PROCESS_TU_ON_LOAD_END
    returning
      value(RV_OK) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_OUT_TU_PPF_FUNCTIONS IMPLEMENTATION.


  METHOD /scdl/if_sp1_message_handler~add_message ##needed.
**********************************************************************
*& Key           : LH-101223
*& Request No.   : GAP-091 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Collecting messages from the SP class
**********************************************************************
  ENDMETHOD.


  METHOD /scdl/if_sp1_message_handler~add_message2 ##needed.
**********************************************************************
*& Key           : LH-101223
*& Request No.   : GAP-091 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Collecting messages from the SP class
**********************************************************************
  ENDMETHOD.


  METHOD /scdl/if_sp1_message_handler~add_system_message ##needed.
**********************************************************************
*& Key           : LH-101223
*& Request No.   : GAP-091 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Collecting messages from the SP class
**********************************************************************
  ENDMETHOD.


  METHOD /scdl/if_sp1_message_handler~add_system_message2 ##needed.
**********************************************************************
*& Key           : LH-101223
*& Request No.   : GAP-091 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Collecting messages from the SP class
**********************************************************************
  ENDMETHOD.


  METHOD add_message.
**********************************************************************
*& Key           : LH-111123
*& Request No.   : GAP-12 – "VCE Carrier integration”
**********************************************************************
*& Description (short)
*& Add message to the log
**********************************************************************
    IF sv_appl_log_handle IS INITIAL.
      cl_log_ppf=>add_message(
        EXPORTING
          ip_problemclass = iv_problemclass
          ip_handle       = sv_appl_log_handle ).
    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
**********************************************************************
*& Key           : LH-111123
*& Request No.   : GAP-12 – "VCE Carrier integration”
**********************************************************************
*& Description (short)
*& Initialize member attributes
**********************************************************************
    mv_tu_num        = iv_tu_num.
    mv_tu_sr_act_num = iv_tu_sr_act_num.
    mv_lgnum         = iv_lgnum.
  ENDMETHOD.


  METHOD load_ended.
**********************************************************************
*& Key           : LH-111123
*& Request No.   : GAP-91 – "Load RF”
**********************************************************************
*& Description (short)
*& Process TU if load ended
**********************************************************************
    sv_appl_log_handle = iv_application_log.
    /scwm/cl_tm=>set_lgnum( iv_lgnum ).

    rv_ok = NEW zcl_out_tu_ppf_functions(
      iv_tu_num        = iv_tu_num
      iv_tu_sr_act_num = iv_tu_sr_act_num
      iv_lgnum         = iv_lgnum
    )->process_tu_on_load_end( ).

  ENDMETHOD.


  METHOD post_gi_for_hus.
**********************************************************************
*& Key           : LH-101223
*& Request No.   : GAP-091 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Post GI for HU with class /scwm/cl_goods_movement. The GI is final
*& and does not trigger DLV GI
**********************************************************************

    /scwm/cl_tm=>set_lgnum( mv_lgnum ).

    CHECK it_gm_hu IS NOT INITIAL.

    DATA(lv_gm_error) = abap_false.
    DATA(lv_save_error) = abap_false.
    /scwm/cl_goods_movement=>post_hu(
      EXPORTING it_hu      = it_gm_hu
                iv_gmcat   = /scwm/if_docflow_c=>sc_gi
      IMPORTING eo_message = DATA(lo_msg) ).
    " Message handling
    IF lo_msg IS BOUND.
      DATA(lt_message) = lo_msg->get_messages( ).
      LOOP AT lt_message INTO DATA(ls_message).
        IF ls_message-symsg-msgid CA wmegc_severity_eax.
          lv_gm_error = abap_true.
        ENDIF.
        MOVE-CORRESPONDING ls_message-symsg TO sy.
        add_message( ).
      ENDLOOP.
    ENDIF.
    IF lv_gm_error EQ abap_false.
      /scwm/cl_goods_movement=>save_gm(
        IMPORTING
          eo_message =  lo_msg                " Delivery Messages
      ).
      IF lo_msg IS BOUND.
        lt_message = lo_msg->get_messages( ).
        LOOP AT lt_message INTO ls_message.
          IF ls_message-symsg-msgid CA wmegc_severity_eax.
            lv_save_error = abap_true.
          ENDIF.
          MOVE-CORRESPONDING ls_message-symsg TO sy.
          add_message( ).
        ENDLOOP.
      ENDIF.
      IF lv_save_error EQ abap_true.
        ROLLBACK WORK.
      ELSE.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.

    /scwm/cl_tm=>cleanup( ).
    /scwm/cl_tm=>set_lgnum( mv_lgnum ).


  ENDMETHOD.


  METHOD post_gi_for_hus_with_sp.
**********************************************************************
*& Key           : LH-101223
*& Request No.   : GAP-091 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Post GI for HU
**********************************************************************

    CONSTANTS: lc_post_gi_hu  TYPE string VALUE '/SCWM/ACT_HU_POST_GM'.

    /scwm/cl_tm=>cleanup( ).
    /scwm/cl_tm=>set_lgnum( mv_lgnum ).

    DATA: lt_a_head  TYPE /scdl/t_sp_a_head ##needed,
          lt_sp_a_hu TYPE /scwm/t_sp_a_hu ##needed.

    DATA(lo_outbound_dlv_handler) = NEW /scwm/cl_sp_prd_out( io_message_handler = me
                                                            iv_mode = /scdl/cl_sp=>sc_mode_classic
                                                            ).


    lo_outbound_dlv_handler->lock(
      EXPORTING
        inkeys   = VALUE /scdl/t_sp_k_head( ( docid = iv_docid ) )
        aspect   = /scwm/if_sp_c=>sc_asp_head
        lockmode = /scdl/cl_sp_prd_inb=>/scdl/if_sp1_locking~sc_shared_lock
      IMPORTING
        rejected = DATA(lv_rejected) ).

    IF lv_rejected = abap_false.

      lo_outbound_dlv_handler->select_by_relation(
        EXPORTING
          relation     = /scwm/if_sp_c=>sc_rel_head_to_hu                 " Relation Name
          inrecords    = VALUE /scdl/t_sp_k_head( ( docid = iv_docid ) )                " Source Aspects
          aspect       = /scwm/if_sp_c=>sc_asp_head                 " Source Aspect for Relation
        IMPORTING
          rejected     = lv_rejected
          outrecords   = lt_sp_a_hu
      ).
      lo_outbound_dlv_handler->execute(
        EXPORTING
          aspect       = /scwm/if_sp_c=>sc_asp_hu
          inkeys       = VALUE /scdl/t_sp_k_hu( FOR guid_hu IN it_gm_hu ( docid = iv_docid huid = guid_hu-guid_hu ) )
          action       = lc_post_gi_hu
          relation_inkey = VALUE /scdl/s_sp_k_head( docid = iv_docid )
          relation     = /scwm/if_sp_c=>sc_rel_head_to_hu
        IMPORTING
          outrecords   = lt_a_head
          rejected     = lv_rejected ).


      IF lv_rejected = abap_false.
        lo_outbound_dlv_handler->save( IMPORTING rejected = lv_rejected ).
        COMMIT WORK AND WAIT.

      ELSE.
        /scwm/cl_tm=>cleanup( ).
        /scwm/cl_tm=>set_lgnum( mv_lgnum ).

        ROLLBACK WORK.
        RAISE EXCEPTION TYPE zcx_workstation
          MESSAGE w049(zmc_workstation).
      ENDIF.
    ENDIF.
    lo_outbound_dlv_handler->unlock(
      EXPORTING
        inkeys = VALUE /scdl/t_sp_k_head( ( docid = iv_docid ) )
        aspect = /scwm/if_sp_c=>sc_asp_head ).

    /scwm/cl_tm=>cleanup( ).
    /scwm/cl_tm=>set_lgnum( mv_lgnum ).
  ENDMETHOD.


  METHOD post_gi_with_class.
**********************************************************************
*& Key           : LH-101223
*& Request No.   : GAP-091 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Post GI for HU with class /scwm/cl_goods_movement. The GI is final
*& and does not trigger DLV GI
**********************************************************************
    CHECK it_gm_hu IS NOT INITIAL.

    DATA(lv_gm_error) = abap_false.
    DATA(lv_save_error) = abap_false.
    /scwm/cl_goods_movement=>post_dlv_hu(
      EXPORTING it_hu      = it_gm_hu
                it_dlv     = it_dlv
                iv_gmcat   = /scwm/if_docflow_c=>sc_gi
      IMPORTING eo_message = DATA(lo_msg) ).
    " Message handling
    IF lo_msg IS BOUND.
      DATA(lt_message) = lo_msg->get_messages( ).
      LOOP AT lt_message INTO DATA(ls_message).
        IF ls_message-symsg-msgid CA wmegc_severity_eax.
          lv_gm_error = abap_true.
        ENDIF.
        MOVE-CORRESPONDING ls_message-symsg TO sy.
        add_message( ).
      ENDLOOP.
    ENDIF.
    IF lv_gm_error EQ abap_false.
      /scwm/cl_goods_movement=>save_gm(
        IMPORTING
          eo_message =  lo_msg                " Delivery Messages
      ).
      IF lo_msg IS BOUND.
        lt_message = lo_msg->get_messages( ).
        LOOP AT lt_message INTO ls_message.
          IF ls_message-symsg-msgid CA wmegc_severity_eax.
            lv_save_error = abap_true.
          ENDIF.
          MOVE-CORRESPONDING ls_message-symsg TO sy.
          add_message( ).
        ENDLOOP.
      ENDIF.
      IF lv_save_error EQ abap_true.
        ROLLBACK WORK.
      ELSE.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD post_gi_with_fm.
**********************************************************************
*& Key           : LH-101223
*& Request No.   : GAP-091 – "Loading via RF”
**********************************************************************
*& Description (short)
*& Post GI for HU with FM '/SCWM/GM_CREATE'. The GI is final
*& and does not trigger DLV GI
**********************************************************************
    DATA:
      lt_bapiret TYPE  bapirettab,
      lv_msg     TYPE string ##needed.

    DATA(ls_gm_header) = VALUE /scwm/s_gmheader(
                                  lgnum = mv_lgnum
                                  created_by = sy-uname
                                  code = 'DLV_GM'
                                  new_number = abap_true
                                  compl = abap_true
                               ).
    GET TIME STAMP FIELD ls_gm_header-created_at.

    TRY.
        CALL FUNCTION '/SCWM/GM_CREATE'
          EXPORTING
            is_header  = ls_gm_header
            it_item    = it_gm_itm
          IMPORTING
            et_bapiret = lt_bapiret.
      CATCH /scwm/cx_core.
        DATA(lv_error) = abap_true.
    ENDTRY.
    LOOP AT lt_bapiret INTO DATA(ls_bapiret).
      MESSAGE ID ls_bapiret-id TYPE ls_bapiret-type NUMBER ls_bapiret-number
                 WITH ls_bapiret-message_v1 ls_bapiret-message_v2 ls_bapiret-message_v3 ls_bapiret-message_v4
                 INTO lv_msg.
      add_message( ).
      IF ls_bapiret-type CA wmegc_severity_eax.
        lv_error = abap_true.
      ENDIF.
    ENDLOOP.
    CALL FUNCTION '/SCWM/GM_CLEANUP'
      EXPORTING
        iv_save = abap_true.
    CLEAR lt_bapiret.
    IF lv_error EQ abap_false.
      TRY.
          CALL FUNCTION '/SCWM/GM_POST'
            EXPORTING
              iv_dlv     = abap_true            " Called out of Save of Delivery/Warehouse Request
            IMPORTING
              et_bapiret = lt_bapiret.                  " Highest message severity, which happened during goods moveme
        CATCH /scwm/cx_core. " WME Core allgemeine Fehlerklasse
          lv_error = abap_true.
      ENDTRY.
    ENDIF.
    LOOP AT lt_bapiret INTO ls_bapiret.
      MESSAGE ID ls_bapiret-id TYPE ls_bapiret-type NUMBER ls_bapiret-number
                 WITH ls_bapiret-message_v1 ls_bapiret-message_v2 ls_bapiret-message_v3 ls_bapiret-message_v4
                 INTO lv_msg.
      add_message( ).
      IF ls_bapiret-type CA wmegc_severity_eax.
        lv_error = abap_true.
      ENDIF.
    ENDLOOP.
    IF lv_error EQ abap_false.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ENDMETHOD.


  METHOD process_tu_on_load_end.
**********************************************************************
*& Key           : LH-111123
*& Request No.   : GAP-91 – "Load RF”
**********************************************************************
*& Description (short)
*& Process TU...
**********************************************************************
    DATA:
      lv_msg      TYPE string ##needed,
      lt_huref_tu TYPE  /scwm/tt_huref_int,
      lt_gm_hu    TYPE /scwm/t_gm_hu.

    rv_ok = abap_true.

    TRY.
        DATA(lo_tu) = /scwm/cl_sr_bom=>get_instance( )->get_bo_tu_by_key( VALUE /scwm/s_tu_sr_act_num( tu_num = mv_tu_num tu_sr_act_num = mv_tu_sr_act_num ) ).
        IF lo_tu IS NOT BOUND.
          RETURN.
        ENDIF.

        "Get HUs and deliveries for the TU
        lo_tu->get_data(
          EXPORTING
            iv_add_info = abap_true
          IMPORTING
            es_tuhdr    = DATA(ls_tu_data) ##needed                 " Transporteinheit
            et_tu_dlv   = DATA(lt_tudlv_hu)                 " Lieferungen und HU´s der Transporteinheiten
        ).

        "Select HUs
        CALL FUNCTION '/SCWM/HU_SELECT_GEN'
          EXPORTING
            iv_lgnum   = mv_lgnum                 " Lagernummer/Lagerkomplex
            it_guid_hu = VALUE /scwm/tt_guid_hu( FOR tudlv IN lt_tudlv_hu WHERE ( top_hu IS NOT INITIAL )
                                      ( guid_hu = tudlv-top_hu ) )                " Tabelle mit HU-Guids
          IMPORTING
            et_huref   = lt_huref_tu
          EXCEPTIONS
            OTHERS     = 0.

        DATA(lo_dlv) = /scwm/cl_dlv_management_prd=>get_instance( ).
        DATA(lo_tudlv_manager) = /scwm/cl_sr_tudlv=>get_instance( ).

        "Read delivery data to get the status load_completed
        TRY.
            lo_dlv->query(
              EXPORTING
                it_docid        = VALUE #( FOR GROUPS doc OF ref IN lt_huref_tu WHERE ( doccat = wmegc_doccat_pdo )
                                           GROUP BY ( docid = ref-docid )
                                          ( doccat = wmegc_doccat_pdo  docid = doc-docid ) )
                is_read_options = VALUE #(  )
                is_exclude_data = VALUE #(  )
              IMPORTING
                et_headers        = DATA(lt_dlv_header)
                et_hu_top         = DATA(lt_hu_top)
                et_hu_ref         = DATA(lt_hu_ref)
            ).
          CATCH /scdl/cx_delivery INTO DATA(lo_cx).
            IF lo_cx IS INSTANCE OF if_t100_message.
              DATA(lo_t100) = CAST if_t100_message( lo_cx ).
              MESSAGE lo_t100 TYPE 'E' DISPLAY LIKE 'E'.
            ELSE.
              RETURN.
            ENDIF.
        ENDTRY.

        lo_tudlv_manager->setup(
                            EXPORTING
                              it_docid = VALUE #( FOR dlv IN lt_dlv_header ( docid = dlv-docid ) ) ).

        DATA(lo_bom) = /scwm/cl_sr_bom=>get_instance( ).

        "Check delivery: check all the TUs assigned to this delivery, whether load ended, if yes, then do
        " GI for the delivery HUs
        LOOP AT lt_dlv_header REFERENCE INTO DATA(lr_dlv_hdr).
          DATA(lt_tu_act_key) = VALUE /scwm/tt_tu_sr_act_num( ).

          DATA(lv_dlv_stat_loaded) = VALUE #( lr_dlv_hdr->status[ status_type = /scdl/if_dl_status_c=>sc_t_loading ]-status_value OPTIONAL ).
          IF lv_dlv_stat_loaded EQ /scdl/if_dl_status_c=>sc_v_finished. "loading complet -> GI
            "Get the TUs
            lo_tudlv_manager->get_tu_by_dlvh(
                                EXPORTING
                                  iv_docid    = lr_dlv_hdr->docid
                                  iv_direct     = abap_false
                                IMPORTING
                                  et_tu_act_key = lt_tu_act_key ).
            "and check the status loand ended
            DATA(lv_load_ended) = abap_true.
            LOOP AT lt_tu_act_key INTO DATA(ls_tu_sr_act_num).
              DATA(lo_bo_tu) = lo_bom->get_bo_tu_by_key( is_tu_sr_act_num = ls_tu_sr_act_num ).
              DATA(lv_tu_end_load) = abap_true.

              TRY.
                  IF lo_bo_tu IS BOUND.
                    CALL METHOD lo_bo_tu->get_status_by_id
                      EXPORTING
                        iv_status_type  = wmesr_status_load_end
                      RECEIVING
                        ev_status_value = lv_tu_end_load.
                    IF lv_tu_end_load EQ abap_false.
                      lv_load_ended = abap_false.
                    ENDIF.
                  ENDIF.
                CATCH /scwm/cx_sr_error ##no_handler.
                  lv_load_ended = abap_false.
              ENDTRY.
            ENDLOOP.

            IF lv_load_ended EQ abap_true.

              CLEAR: lt_gm_hu.
              LOOP AT lt_hu_ref REFERENCE INTO DATA(lr_hu_ref)
                   WHERE  docid = lr_dlv_hdr->docid.
                READ TABLE lt_hu_top REFERENCE INTO DATA(lr_hu_top)
                     WITH KEY guid_hu = lr_hu_ref->guid_hu.
                IF sy-subrc EQ 0.
                  APPEND VALUE /scwm/s_gm_hu( lgnum = mv_lgnum huident = lr_hu_top->huident guid_hu = lr_hu_top->guid_hu ) TO lt_gm_hu.
                ENDIF.
              ENDLOOP.
              IF lines( lt_tu_act_key ) EQ 1.
                TRY.
                    post_gi_for_hus_with_sp(
                      it_gm_hu = lt_gm_hu                " Handling Unit Data for Goods Movements
                      iv_docid = lr_dlv_hdr->docid                  " Document ID
                    ).
                  CATCH zcx_workstation. " Workstation errors.
                    MESSAGE e050(zmc_out) WITH lr_dlv_hdr->docno INTO lv_msg.
                    add_message(  ).
                ENDTRY.

              ENDIF.

            ENDIF.
          ENDIF.
        ENDLOOP.
      CATCH /scwm/cx_sr_error. " Class for Exceptions in Shipping/Receiving.
        MESSAGE e043(zmc_out) WITH mv_tu_num INTO lv_msg.
        add_message(  ).
        RETURN.
    ENDTRY.

    "check if status is already set
    TRY.
        lo_tu->/scwm/if_sr_bo~check_in_out(
            iv_act   = wmesr_act_check_out ).

        lo_bom->save( ).
        COMMIT WORK.
      CATCH /scwm/cx_sr_error .
        ROLLBACK WORK.
        CALL METHOD /scwm/cl_tm=>cleanup( ).
        MESSAGE e114(zmc_rfui) WITH mv_tu_num INTO lv_msg.
        add_message( ).
    ENDTRY.

  ENDMETHOD.


  METHOD send_manifest.
**********************************************************************
*& Key           : LH-111123
*& Request No.   : GAP-12 – "VCE Carrier integration”
**********************************************************************
*& Description (short)
*& Send manifest request and process the response
**********************************************************************
    DATA ls_request TYPE zif_vce_manifest_model=>ty_request.
    sv_appl_log_handle = iv_application_log.
    TRY.
        DATA(lo_request) = NEW zcl_vce_manifest_request( iv_lgnum = iv_lgnum iv_application_log = iv_application_log ).

      CATCH zcx_core_exception.
        MESSAGE e040(zmc_out) INTO DATA(lv_msg) ##needed.
        add_message( iv_problemclass = sppf_pclass_1 ).
        RETURN.
    ENDTRY.
    lo_request->build_request_data(
      EXPORTING
        iv_input =  VALUE zcl_vce_manifest_request=>ty_build_parameters( tu_num = iv_tu_num tu_sr_act_num = iv_tu_sr_act_num )
      IMPORTING
        es_data  = ls_request
    ).


    lo_request->send_post_request( is_data = ls_request ).

    lo_request->process_response( ).

    rv_ok = abap_true.
  ENDMETHOD.
ENDCLASS.
