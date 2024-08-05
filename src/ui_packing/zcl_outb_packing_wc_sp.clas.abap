CLASS zcl_outb_packing_wc_sp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_outb_packing_wc_sp .

    CONSTANTS c_serial_num_id_type TYPE zde_id_type VALUE '001' ##NO_TEXT.

    CLASS-METHODS class_constructor .
protected section.

  data MO_FUNCTION_LOG type ref to /SCWM/CL_LOG .

  methods ADD_EXCEPTION
    importing
      !IO_T100 type ref to IF_T100_MESSAGE
      !IV_MSGTY type SYMSGTY default 'E' .
private section.

  types:
    BEGIN OF ty_hu_doc_ref_qty,
        docid   TYPE /scdl/dl_docid,
***        itemid  TYPE /scdl/dl_itemid,
        huident TYPE /scwm/de_huident,
        qty     TYPE /scwm/de_quantity,
***        uom     TYPE /scwm/de_base_uom,
      END OF ty_hu_doc_ref_qty .
  types:
    tt_hu_doc_ref_qty  TYPE STANDARD TABLE OF ty_hu_doc_ref_qty .

  class-data ST_SNS_IDN_TYPE type ZIF_OUTB_PACKING_WC_SP=>TT_REQ_SNS_IDN_TYPE .
  class-data:
    st_term_def TYPE STANDARD TABLE OF zout_term_def .
  class-data SV_TEST_MODE type ABAP_BOOL .
  data MO_PACK_WM type ref to /SCWM/CL_WM_PACKING .
  data MS_HUHDR type /SCWM/S_HUHDR_INT .
  data MS_TERMNAL_DEFAULTS type ZOUT_TERM_DEF .
  data MS_WEIGHT_DEVIATIONS type ZOUT_WEIGHT_DEV .
  data MS_WORKSTATION type /SCWM/TWORKST .
  data MS_WORKSTTYP type /SCWM/TWRKTYP .
  data MT_DLV_ITEM type /SCWM/DLV_ITEM_OUT_PRD_TAB .
  data MT_HUHDR type /SCWM/TT_HUHDR_INT .
  data MT_HUITM type /SCWM/TT_HUITM_INT .
  data MV_SCALE_WEIGHT_IN_KG type ZDE_SCALE_HU_WEIGHT .
  data MV_WEIGHT_DIFFERENCE_EXCEEDED type ABAP_BOOL .
  data MV_MASTERCARTON_REL type ABAP_BOOL .
  data MT_HUS_FRO_PROD_BUFFER type ZIF_OUTB_PACKING_WC_SP=>TT_HU_WITH_QUANT .

  methods CHECK_WEIGHT_DIFFERENCE
    raising
      ZCX_WORKSTATION .
  methods LOCK_HU
    raising
      ZCX_WORKSTATION .
  methods MOVE_AND_UNPACK_HU
    importing
      !IV_HUIDENT type /SCWM/DE_HUIDENT
      !IV_LGPLA type /SCWM/LGPLA
    exporting
      !ET_BAPIRET type BAPIRET2_TAB
    raising
      ZCX_WORKSTATION .
ENDCLASS.



CLASS ZCL_OUTB_PACKING_WC_SP IMPLEMENTATION.


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


  METHOD check_weight_difference.
    DATA: lv_system_weight_g      TYPE /scwm/de_quantity,
          lv_weight_diff_in_gr    TYPE /scwm/de_quantity,
          lv_one_peace_weight_kg  TYPE /scwm/de_quantity,
          lv_one_peace_weight_g   TYPE /scwm/de_quantity,
          lv_min_one_peace_weight TYPE /scwm/de_quantity,
          lv_perc_dev             TYPE /scwm/de_quantity.


    mv_weight_difference_exceeded = abap_false.
    IF ms_huhdr-unit_gw EQ zif_c_mdm_tool=>c_units-gramm.
      lv_system_weight_g = ms_huhdr-g_weight.
    ELSE.
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
        EXPORTING
          input    = ms_huhdr-g_weight                " Input Value
          unit_in  = ms_huhdr-unit_gw            " Unit of input value
          unit_out = zif_c_mdm_tool=>c_units-gramm          " Unit of output value
        IMPORTING
          output   = lv_system_weight_g                 " Output value
        EXCEPTIONS
          OTHERS   = 0.
      IF sy-subrc <> 0.

      ENDIF.
    ENDIF.

    "Check the absulute weight difference
    lv_weight_diff_in_gr = abs( mv_scale_weight_in_kg * 1000 - lv_system_weight_g ).
    IF lv_weight_diff_in_gr LE ms_weight_deviations-max_abs_weight_devi .
      mv_weight_difference_exceeded = abap_false.
      RETURN.
    ENDIF.

    "Find the leightest product
    LOOP AT mt_huitm INTO DATA(itm) GROUP BY ( matid = itm-matid ) INTO DATA(ls_prod_grp).
      lv_one_peace_weight_g = zif_outb_packing_wc_sp~get_prod_weight( ls_prod_grp-matid ).
      IF lv_one_peace_weight_g IS INITIAL.
        TRY.
            DATA(lv_matnr) = /scmb/cl_md_access_mdl=>get_md_access( )->get_prod( iv_prodid = ls_prod_grp-matid )-prodno.
          CATCH /scmb/cx_md_access ##no_handler.
            "may not occure
        ENDTRY.
        RAISE EXCEPTION TYPE zcx_workstation
          MESSAGE e009(zmc_out_ui_packing) WITH lv_matnr zif_c_mdm_tool=>c_units-piece zif_c_mdm_tool=>c_units-gramm.
      ELSE.
        IF lv_min_one_peace_weight IS INITIAL OR lv_one_peace_weight_g LT lv_min_one_peace_weight.
          lv_min_one_peace_weight = lv_one_peace_weight_g.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF lv_min_one_peace_weight IS NOT INITIAL.
      TRY.
          lv_perc_dev = ( lv_weight_diff_in_gr / lv_min_one_peace_weight ) * 100.
        CATCH cx_sy_arithmetic_overflow.
          lv_perc_dev = 999999.
      ENDTRY.
      IF lv_perc_dev GT ms_weight_deviations-max_weight_devi_lp.
        mv_weight_difference_exceeded = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD class_constructor.
**********************************************************************
*& Key           : LH-090123
*& Request No.   : GAP-022 – “KEP Workcenter”
**********************************************************************
*& Description (short)
*& Read data for buffering
**********************************************************************
    IF sy-tcode = 'ZPACKING_WC_TEST'.
      sv_test_mode = abap_true.
    ENDIF.
    SELECT * FROM zout_term_def INTO TABLE st_term_def.

    SELECT selnum number_description AS description
      FROM ztcross_numbers INTO CORRESPONDING FIELDS OF TABLE st_sns_idn_type
      WHERE spras EQ sy-langu.

  ENDMETHOD.


  METHOD lock_hu.
    CALL FUNCTION 'ENQUEUE_/SCWM/EHU'
      EXPORTING
        mode_/scwm/s_huhdr_int = 'E'
        huident                = ms_huhdr-huident
        lgnum                  = ms_huhdr-lgnum
      EXCEPTIONS
        foreign_lock           = 1
        system_failure         = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
      MESSAGE ID sy-msgid TYPE   wmegc_severity_suc   NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD move_and_unpack_hu.
**********************************************************************
*& Key           : LH-090123
*& Request No.   : GAP-022 – “KEP Workcenter”
**********************************************************************
*& Description (short)
*& Move HU, and if it is not the top HU then remove it from the TOP HU
**********************************************************************
    DATA: lt_bapiret  TYPE bapirettab,
          lv_severity TYPE bapi_mtype,
          lv_tanum    TYPE /scwm/tanum,
          lt_ltap_vb  TYPE /scwm/tt_ltap_vb ##needed.

    ASSIGN mt_huhdr[ top = abap_true ] TO FIELD-SYMBOL(<ls_top_hu>).
    IF sy-subrc NE 0. "there is no top HU - nothing to move
      RETURN.
    ENDIF.

    ASSIGN mt_huhdr[ huident = iv_huident ] TO FIELD-SYMBOL(<ls_act_hu>).
    IF sy-subrc NE 0. "there is no HU - nothing to move
      RETURN.
    ENDIF.

    "move top HU to storage bin
    IF <ls_top_hu>-lgpla NE iv_lgpla.
      CALL FUNCTION '/SCWM/TO_CREATE_MOVE_HU'                    "#EC ENHOK
        EXPORTING
          iv_lgnum       = ms_termnal_defaults-lgnum
          it_create_hu   = VALUE /scwm/tt_to_crea_hu( (
                                              huident = <ls_top_hu>-huident
                                              squit   = abap_true
                                              nlpla   = iv_lgpla
                                              procty  = zif_wme_c=>gs_procty-immediate
                                              norou   = wmegc_norou_yes
                                           ) )
          iv_wtcode      = wmegc_wtcode_adhoc_hu
          iv_commit_work = abap_true
        IMPORTING
          et_ltap_vb     = lt_ltap_vb
          et_bapiret     = lt_bapiret
          ev_severity    = lv_severity.
      IF lv_severity CA wmegc_severity_eax.
        ROLLBACK WORK.
        DATA(ls_error) = VALUE bapiret2( lt_bapiret[ type = lv_severity ] OPTIONAL ).
        RAISE EXCEPTION TYPE zcx_workstation
              MESSAGE ID ls_error-id TYPE lv_severity NUMBER ls_error-number
              WITH ls_error-message_v1 ls_error-message_v2
                   ls_error-message_v3 ls_error-message_v4
              EXPORTING
                messages = lt_bapiret
             .
      ENDIF.
    ENDIF.

    mo_pack_wm->move_hu(
      EXPORTING
        iv_hu  = <ls_act_hu>-guid_hu                 " Unique Internal Identification of a Handling Unit
        iv_bin = iv_lgpla                 " Storage Bin for Stock Transfer
      EXCEPTIONS
        error  = 1                " Error, see log
        OTHERS = 2
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
         MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    mo_pack_wm->save(
      EXPORTING
        iv_commit = 'X'
        iv_wait   = 'X'
      EXCEPTIONS
        error     = 1                " See Log
        OTHERS    = 2
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
         MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~adjust_quantity.
**********************************************************************
*& Key           : LH-090123
*& Request No.   : GAP-022 – “KEP Workcenter”
**********************************************************************
*& Description (short)
*& Set additional quantity in delivery item
**********************************************************************
    DATA:
      ls_prcode TYPE /scwm/s_act_prcodes,
      lt_itm    TYPE /scdl/t_sp_a_item,
      ls_action TYPE /scdl/s_sp_act_action.

    DO 3 TIMES. "There has been some performance issue, so we need to retry in special case
      /scwm/cl_tm=>cleanup( ).
      /scwm/cl_tm=>set_lgnum( iv_lgnum = ms_termnal_defaults-lgnum ).
      lock_hu( ).

      DATA(lo_msg_box) = NEW /scdl/cl_sp_message_box( ).
      DATA(lo_sp) = NEW /scdl/cl_sp_prd_out(
          io_message_box = lo_msg_box
          iv_doccat      = /scdl/if_dl_doc_c=>sc_doccat_out_prd
          iv_mode        = /scdl/cl_sp=>sc_mode_classic ).


      ls_prcode = VALUE #(  doccat       =  /scdl/if_dl_doc_c=>sc_doccat_out_prd
                            itemtype     =  /scdl/if_dl_doc_c=>sc_itemtype_odlv
                            prcode       =  'O001'
                            qty	         =  iv_qty
                            uom	         =  iv_meins
                            "prcode_adj_qty  = 6
                        ).
      lo_sp->lock(
        EXPORTING
          inkeys   = VALUE /scdl/t_sp_k_item(  ( docid = iv_docid itemid = iv_itmid ) )
          aspect   = /scdl/if_sp_c=>sc_asp_item
          lockmode = /scdl/cl_sp_prd_inb=>/scdl/if_sp1_locking~sc_exclusive_lock
        IMPORTING
          rejected     = DATA(lv_rejected)
          return_codes = DATA(lt_ret_codes) )              .

      IF lv_rejected EQ abap_false.
        lo_sp->select(
          EXPORTING
            inkeys       = VALUE /scdl/t_sp_k_item( ( docid = iv_docid itemid = iv_itmid  ) )
            aspect       = /scdl/if_sp_c=>sc_asp_item
          IMPORTING
            outrecords   = lt_itm
            rejected     = lv_rejected
            return_codes = lt_ret_codes
        ).
      ENDIF.

      IF lv_rejected EQ abap_false.

        CLEAR ls_action.
        ls_action-action_code = /scwm/if_dl_c=>sc_ac_prcode_add.
        CREATE DATA ls_action-action_control TYPE /scwm/dlv_prcode_add_str.
        ASSIGN ls_action-action_control->* TO FIELD-SYMBOL(<ls_parameter>).
        MOVE-CORRESPONDING ls_prcode TO <ls_parameter>.

        lo_sp->execute(
          EXPORTING
            aspect       = /scdl/if_sp_c=>sc_asp_item                " Aspect Name
            inkeys       = VALUE /scdl/t_sp_k_item( ( docid = iv_docid itemid = iv_itmid ) )                " Objects to Act On
            inparam      = ls_action                " Parameters
            action       = /scdl/if_sp_c=>sc_act_execute_action                " Name of Action
          IMPORTING
            outrecords   = lt_itm                " Changed Aspect Objects
            rejected     = lv_rejected
            return_codes = lt_ret_codes
*             relation_outrecord =                  " Changed Target Aspect Line
        ).
      ENDIF.

      IF line_exists( lt_ret_codes[ failed = abap_true ] ).
        ROLLBACK WORK.
        DATA(lv_success) = abap_false.
        DATA(lt_messages) = lo_msg_box->get_messages( ).

        READ TABLE lt_messages TRANSPORTING NO FIELDS
             WITH KEY msgid =  '/SCWM/DELIVERY'
                      msgno = '240'.
        IF sy-subrc EQ 0.
          WAIT UP TO 1 SECONDS. "Performance issues -> wait
          CONTINUE.
        ELSE.
          RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE e016(zmc_workstation)
            EXPORTING messages = CORRESPONDING #( lo_msg_box->get_messages( ) MAPPING id         = msgid
                                                                                     number     = msgno
                                                                                     type       = msgty
                                                                                     message_v1 = msgv1
                                                                                     message_v2 = msgv2
                                                                                     message_v3 = msgv3
                                                                                     message_v4 = msgv4 ).
        ENDIF.
      ENDIF.

      IF lv_rejected EQ abap_false.
        lo_sp->save(
          EXPORTING
            synchronously = abap_true
          IMPORTING
            rejected      =  lv_rejected
        ).
      ENDIF.

      IF lv_rejected = abap_false.
        lv_success = abap_true.
        COMMIT WORK AND WAIT.
        EXIT.
      ELSE.
        lv_success = abap_false.

        ROLLBACK WORK.
        lt_messages = lo_msg_box->get_messages( ).

        RAISE EXCEPTION TYPE zcx_workstation
          MESSAGE e016(zmc_workstation)
          EXPORTING messages = CORRESPONDING #( lt_messages MAPPING id         = msgid
                                                                    number     = msgno
                                                                    type       = msgty
                                                                    message_v1 = msgv1
                                                                    message_v2 = msgv2
                                                                    message_v3 = msgv3
                                                                    message_v4 = msgv4 ).
      ENDIF.
    ENDDO.
    IF lv_success EQ abap_false.
      RAISE EXCEPTION TYPE zcx_workstation
        MESSAGE e016(zmc_workstation)
        EXPORTING messages = CORRESPONDING #( lt_messages MAPPING id         = msgid
                                                                  number     = msgno
                                                                  type       = msgty
                                                                  message_v1 = msgv1
                                                                  message_v2 = msgv2
                                                                  message_v3 = msgv3
                                                                  message_v4 = msgv4 ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~cancel_picking.
**********************************************************************
*& Key           : LH-090123
*& Request No.   : GAP-022 – “KEP Workcenter”
**********************************************************************
*& Description (short)
*& Cancel picking for all the HU Items
**********************************************************************
    DATA: lo_api_cancpick   TYPE REF TO /scwm/if_api_cancel_picking.
    DATA: ls_bapiret    TYPE bapiret2,
          lt_bapiret    TYPE bapiret2_tab,
          lt_pdo_itms   TYPE /scwm/if_api_mfg_stage=>yt_single_order,
          lt_gmitem_all TYPE /scwm/if_api_cancel_picking=>yt_gm_release,
          lt_rel_stock  TYPE /scwm/if_api_cancel_picking=>yt_release.

    IF /scwm/cl_tm=>sv_lgnum IS INITIAL.
      CALL METHOD /scwm/cl_tm=>set_lgnum
        EXPORTING
          iv_lgnum = ms_termnal_defaults-lgnum.
    ENDIF.

*   Get API Service
    /scwm/cl_api_factory=>get_service(
         IMPORTING
        eo_api = lo_api_cancpick
    ).

    LOOP AT mt_huitm INTO DATA(ls_huitm)
         WHERE doccat EQ wmegc_doccat_pdo..
      CLEAR: lt_pdo_itms,  lt_bapiret, ls_bapiret.

      lt_rel_stock = VALUE #( ( guid_parent = ls_huitm-guid_parent
                                guid_stock  = ls_huitm-guid_stock ) ).
      TRY.
*         trigger cancel picking
          lo_api_cancpick->release_stock(
            EXPORTING
              iv_whno            = ms_termnal_defaults-lgnum
              iv_only_release    = 'X'
              it_stock           = lt_rel_stock
            IMPORTING
              et_gmitem          = DATA(lt_gmitem)
              ev_save_required   = DATA(lv_save_required)
              eo_message         = DATA(lo_message) ).

          IF lo_message IS BOUND.
*           Collect all messages
            lo_message->get_messages(
                           IMPORTING
                             et_bapiret = lt_bapiret
                                      ).
            READ TABLE lt_bapiret TRANSPORTING NO FIELDS WITH KEY type = 'E'.
            IF sy-subrc NE 0.
              APPEND LINES OF lt_gmitem TO lt_gmitem_all.
            ENDIF.
            APPEND LINES OF lt_bapiret TO et_bapiret.
          ENDIF.
        CATCH /scwm/cx_api_cancel_picking INTO DATA(lx_api).
          IF lo_message IS BOUND.
*           Collect all messages
            lo_message->get_messages(
                           IMPORTING
                             et_bapiret = lt_bapiret
                                      ).
            et_bapiret = lt_bapiret.
          ELSE.
            ls_bapiret-id         = lx_api->if_t100_message~t100key-msgid.
            ls_bapiret-number     = lx_api->if_t100_message~t100key-msgno.
            ls_bapiret-type       = 'E'.
            ls_bapiret-message_v1 = lx_api->mv_msgv1.
            ls_bapiret-message_v2 = lx_api->mv_msgv2.
            ls_bapiret-message_v3 = lx_api->mv_msgv3.
            ls_bapiret-message_v4 = lx_api->mv_msgv4.
            APPEND ls_bapiret TO et_bapiret.
            RAISE EXCEPTION TYPE zcx_workstation
              EXPORTING
                messages = et_bapiret.
          ENDIF.
      ENDTRY.
    ENDLOOP.
    IF lines( lt_gmitem_all ) > 0.  "some stock items are released
      TRY.
          lo_api_cancpick->save( IMPORTING eo_message = lo_message ).
        CATCH /scwm/cx_api_cancel_picking INTO DATA(lx_cancpick) ##NO_HANDLER.
      ENDTRY.
      IF ( lo_message IS BOUND AND lo_message->check( EXPORTING iv_msgty = wmegc_severity_err ) = abap_true )
           OR ( lo_message IS BOUND AND lx_cancpick IS BOUND ).
        lo_message->get_messages(
                       IMPORTING
                         et_bapiret = et_bapiret
                                  ).
        RAISE EXCEPTION TYPE zcx_workstation
          EXPORTING
            messages = et_bapiret.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~check_sns_idn_in_db.

    DATA(lt_data_in_db) = zcl_crud_ztcross_cap_nums=>select_multi_by_docno_itemno(
        iv_lgnum    = ms_workstation-lgnum
        iv_docno    = iv_docno
        iv_itemno   = iv_itemno
      ).

    DELETE lt_data_in_db WHERE id_type NE c_serial_num_id_type.
    LOOP AT it_req_sns_idn_type INTO DATA(sns_idn) GROUP BY ( serial = sns_idn-serial docno = sns_idn-docno itemno = sns_idn-itemno )
         REFERENCE INTO  DATA(lr_grp).

      LOOP AT lt_data_in_db REFERENCE INTO DATA(lr_data_in_db)
           WHERE serial  EQ lr_grp->serial.
        IF iv_old_guid_hu IS INITIAL OR lr_data_in_db->guid_hu NE iv_old_guid_hu.
          RAISE EXCEPTION  TYPE zcx_workstation
                MESSAGE e022(zmc_out_ui_packing) WITH lr_data_in_db->serial lr_data_in_db->docno lr_data_in_db->itemno.
        ENDIF.

      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~close_hu.
**********************************************************************
*& Key           : LH-090123
*& Request No.   : GAP-022 – “KEP Workcenter”
**********************************************************************
*& Description (short)
*& Close HU
**********************************************************************
    /scwm/cl_tm=>cleanup( ).
    /scwm/cl_tm=>set_lgnum( ms_workstation-lgnum ).
    lock_hu( ).

    TRY.
        mo_pack_wm->init_by_workstation(
          EXPORTING
            is_workstation   = ms_workstation                  " Work Station Profile
            ir_huident       = VALUE #( ( sign = 'I' option = 'EQ' low = iv_hu ) )                 " Ranges Table Type for Field Name HUIDENT
            iv_doccat        = wmegc_doccat_pdo                 " Doc. Category for Doc. Reference and Doc.-Related Stocks
          EXCEPTIONS
            error            = 1                " Error, see log
            OTHERS           = 2
        ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_workstation
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.


        CALL METHOD mo_pack_wm->hu_process_completed
          EXPORTING
            iv_hu  = iv_hu_guid
          EXCEPTIONS
            error  = 1
            OTHERS = 2.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_workstation
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
        mo_pack_wm->save(
          EXCEPTIONS
            error     = 1                " See Log
            OTHERS    = 2
        ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_workstation
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        /scwm/cl_tm=>cleanup( ).
        lock_hu( ).

      CATCH zcx_workstation INTO DATA(lx_ws).
        /scwm/cl_tm=>cleanup( ).
        lock_hu( ).
        RAISE EXCEPTION lx_ws.

    ENDTRY.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~create_hu_in_wc.
**********************************************************************
*& Key           : LH-090123
*& Request No.   : GAP-022 – “KEP Workcenter”
**********************************************************************
*& Description (short)
*& Create the HU in the working center
**********************************************************************
    CLEAR: ev_new_hu, ev_new_hu_guid, et_huitm.


    /scwm/cl_tm=>cleanup( ).
    /scwm/cl_tm=>set_lgnum( ms_workstation-lgnum ).

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = ms_workstation-lgnum                  " Warehouse Number/Warehouse Complex
        iv_process   = zif_param_const=>c_zout_0001                 " Process ID (Specification, Program, BAdI etc.)
        iv_parameter = zif_param_const=>c_procty_kep                " Parameter ID for process
      IMPORTING
        et_range  = DATA(lt_rng_procty_kep)                 " Parameter-Framework Low
    ).

    TRY.
        mo_pack_wm->init_by_workstation(
          EXPORTING
            is_workstation   = ms_workstation                  " Work Station Profile
            ir_huident       = VALUE #( ( sign = 'I' option = 'EQ' low = iv_source_hu ) )                 " Ranges Table Type for Field Name HUIDENT
            ir_docno         = COND #( WHEN iv_docno IS NOT INITIAL THEN VALUE #( ( sign = 'I' option = 'EQ' low = iv_docno ) ) )                " SELECT-OPTIONS Table
            iv_doccat        = wmegc_doccat_pdo                 " Doc. Category for Doc. Reference and Doc.-Related Stocks
          EXCEPTIONS
            error            = 1                " Error, see log
            OTHERS           = 2
        ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_workstation
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        mo_pack_wm->create_hu(
          EXPORTING
            iv_pmat      = iv_pmatid                " Material GUID16 with Conversion Exit
            i_location   = ms_termnal_defaults-work_desk_stbin
          RECEIVING
            es_huhdr     = DATA(ls_huhdr_new)
          EXCEPTIONS
            error        = 1
            OTHERS       = 2
        ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_workstation
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        mo_pack_wm->get_hu(
          EXPORTING
            iv_huident = iv_source_hu                 " Handling Unit Identification
            iv_lock    = 'E'
          IMPORTING
            es_huhdr   = DATA(ls_huhdr_source)                 " Internal Structure for Processing the HU Header
          EXCEPTIONS
            not_found  = 1
            OTHERS     = 2
        ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_workstation
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.



        LOOP AT it_stock_repack INTO DATA(ls_stock_repack).
          READ TABLE mt_huitm INTO DATA(ls_huitm)
               WITH KEY guid_stock = ls_stock_repack-stock_guid.
          ASSERT sy-subrc EQ 0.
          READ TABLE mt_dlv_item REFERENCE INTO DATA(lr_dlv_item)
               WITH KEY docid = ls_huitm-qdocid
                        itemid = ls_huitm-qitmid.
          IF sy-subrc EQ 0.
            IF lr_dlv_item->sapext-/scwm/procty IN lt_rng_procty_kep.
              DATA(lv_kep_process) = abap_true.
            ENDIF.
          ENDIF.

          mo_pack_wm->repack_stock(
            EXPORTING
              iv_dest_hu    = ls_huhdr_new-guid_hu                 " Unique Internal Identification of a Handling Unit
              iv_source_hu  = ls_huhdr_source-guid_hu                 " Unique Internal Identification of a Handling Unit
              iv_stock_guid = ls_stock_repack-stock_guid                 " GUID Stock Item
              is_quantity   = COND #( WHEN ls_stock_repack-quant IS INITIAL
                                      THEN VALUE #( quan = ls_huitm-quan unit = ls_huitm-meins )                 " Quantity Structure
                                      ELSE VALUE #( quan = ls_stock_repack-quant unit = ls_stock_repack-meins ) )
             EXCEPTIONS
               error         = 1                " See Log
               OTHERS        = 2
          ).
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDLOOP.

        IF lv_kep_process EQ abap_true.
          mo_pack_wm->get_hu(
            EXPORTING
              iv_guid_hu = ls_huhdr_new-guid_hu                 " Handling Unit Identification
            IMPORTING
              es_huhdr   = DATA(ls_huhdr_for_change)                 " Internal Structure for Processing the HU Header
            EXCEPTIONS
              OTHERS     = 0
          ).
          ls_huhdr_for_change-prces = zif_wme_c=>gs_prces-kcar.

          mo_pack_wm->change_huhdr(
            EXPORTING
              is_huhdr   = ls_huhdr_for_change                  " Internal Structure for Processing the HU Header
            EXCEPTIONS
              error      = 1
              not_locked = 2
              OTHERS     = 3
          ).
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zcx_workstation
               MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.

        mo_pack_wm->save(
          EXCEPTIONS
            error     = 1                " See Log
            OTHERS    = 2
        ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_workstation
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
        mo_pack_wm->get_hu(
          EXPORTING
            iv_guid_hu = ls_huhdr_new-guid_hu                 " Unique Internal Identification of a Handling Unit
          IMPORTING
            et_huitm   = et_huitm                 " Material Items in the HU
           EXCEPTIONS
             OTHERS     = 0
        ).
        /scwm/cl_tm=>cleanup( ).
        lock_hu( ).
      CATCH zcx_workstation INTO DATA(lx_ws).
        APPEND LINES OF mo_pack_wm->/scwm/if_pack_bas~go_log->get_prot( ) TO lx_ws->messages.
        /scwm/cl_tm=>cleanup( ).
        lock_hu( ).
        RAISE EXCEPTION lx_ws.
    ENDTRY.
    ev_new_hu = ls_huhdr_new-huident.
    ev_new_hu_guid = ls_huhdr_new-guid_hu.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~determine_stage_area.
    NEW /scwm/cl_dlv_management_prd( )->det_pickloc_stag_door(
      EXPORTING
        iv_whno                    = ms_workstation-lgnum                 " Warehouse Number/Warehouse Complex
        it_docid_itemid            = it_docid_itemid
      IMPORTING
        eo_message                 = DATA(lo_dlv_msg)                 " Messages for Delivery Processing with Number Support
    ).
    DATA(lt_dlv_err) = lo_dlv_msg->get_messages( iv_msgty = wmegc_severity_err ).
    IF lt_dlv_err IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE w006(zmc_out_ui_packing).
    ENDIF.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~get_entitled.
    rv_entitled = ms_huhdr-entitled.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~get_hus_for_prod.
    DATA:
      lt_huhdr TYPE  /scwm/tt_huhdr_int,
      lt_huitm TYPE  /scwm/tt_huitm_int,
      lv_matnr TYPE matnr.

    CLEAR: ev_matnr, et_hus.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = iv_product_ean_mpn
      IMPORTING
        output = lv_matnr
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      lv_matnr = iv_product_ean_mpn.
    ENDIF.

    SELECT FROM mara
           FIELDS matnr, scm_matid_guid16
           WHERE matnr EQ @lv_matnr
              OR ean11 EQ @iv_product_ean_mpn
              OR mfrpn EQ @iv_product_ean_mpn
           INTO TABLE @DATA(lt_matnr_det).
    IF lt_matnr_det IS INITIAL.
      SELECT FROM mean INNER JOIN mara
             ON mean~matnr EQ mara~matnr
             FIELDS mara~matnr, mara~scm_matid_guid16
             WHERE mean~ean11 EQ @iv_product_ean_mpn
             INTO TABLE @lt_matnr_det.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e010(zmc_workstation) WITH iv_product_ean_mpn.
      ENDIF.
    ENDIF.

    "Get Delivery Items
    ev_matnr = lv_matnr = lt_matnr_det[ 1 ]-matnr.
    ev_matid = lt_matnr_det[ 1 ]-scm_matid_guid16.

    CALL FUNCTION '/SCWM/HU_SELECT_GEN'
      EXPORTING
        iv_lgnum = ms_workstation-lgnum
        ir_lgpla = VALUE  rseloption( ( sign = 'I'
                                        option = 'EQ'
                                        low = ms_workstation-lgpla  ) )
        ir_matnr = VALUE  rseloption( ( sign = 'I'
                                        option = 'EQ'
                                        low = lv_matnr  ) )
        ir_copst = VALUE  rseloption( ( sign = 'I'
                                        option = 'EQ'
                                        low = space  ) )
      IMPORTING
        et_huhdr = lt_huhdr
        et_huitm = lt_huitm
      EXCEPTIONS
        OTHERS   = 0.

    IF lt_huitm IS NOT INITIAL.
      DATA(lo_dlv) = /scwm/cl_dlv_management_prd=>get_instance( ).
      TRY.
          lo_dlv->query(
            EXPORTING
              it_docid        = VALUE #(  FOR cont IN lt_huitm ( doccat = wmegc_doccat_pdo docid = cont-qdocid ) )
              iv_whno         = ms_huhdr-lgnum
              is_read_options = VALUE #(  )
              is_include_data = VALUE #(  )
            IMPORTING
              et_headers        = DATA(lt_dlv_header)
              et_items          = DATA(lt_dlv_item)
          ).
        CATCH /scdl/cx_delivery INTO DATA(lo_cx).

      ENDTRY.

      LOOP AT lt_huhdr INTO DATA(ls_huhdr).
        LOOP AT lt_huitm INTO DATA(ls_huitm)
             WHERE guid_parent EQ ls_huhdr-guid_hu.
          READ TABLE lt_dlv_item REFERENCE INTO DATA(lr_dlv_item)
               WITH KEY docid = ls_huitm-qdocid
                        itemid = ls_huitm-qitmid.
          INSERT VALUE zif_outb_packing_wc_sp~ty_hu_with_quant(
                           huident = ls_huhdr-huident
                           meinh   = ls_huitm-meins
                           quan    = ls_huitm-quan
                           altme   = ls_huitm-altme
                           quana    = ls_huitm-quana
                           dlv_cretst   = lr_dlv_item->admin-cretst
                           ) INTO TABLE et_hus.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~get_hus_for_prod_buffer.
    rt_hus = mt_hus_fro_prod_buffer.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~get_hu_content.
**********************************************************************
*& Key           : LH-090123
*& Request No.   : GAP-022 – “KEP Workcenter”
**********************************************************************
*& Description (short)
*& Get the HU content, and read the delivery data and product data for the items
**********************************************************************
    TYPES: tt_serial TYPE STANDARD TABLE OF ztcross_cap_nums-serial WITH DEFAULT KEY.
    DATA: ls_mat_pack   TYPE  /scwm/s_material_pack,
          ls_mat_global TYPE  /scwm/s_material_global,
          ls_mara       TYPE mara.

    DATA: lt_selection TYPE /scwm/dlv_selection_tab,
          ls_selection LIKE LINE OF lt_selection,
          ls_t300      TYPE /scwm/s_t300_md.

    DATA: lv_vsola     TYPE /scwm/ltap-vsola.

    CLEAR et_delivery_data.
    "Get Delivery numbers
    DATA(lo_dlv) = /scwm/cl_dlv_management_prd=>get_instance( ).
    TRY.
        lo_dlv->query(
          EXPORTING
            it_docid        = VALUE #(  FOR cont IN mt_huitm ( doccat = wmegc_doccat_pdo docid = cont-qdocid ) )
            iv_whno         = ms_huhdr-lgnum
            is_read_options = VALUE #(  )
            is_include_data = VALUE #(  )
          IMPORTING
            et_headers        = DATA(lt_dlv_header)
            et_items          = DATA(lt_dlv_item)
        ).
      CATCH /scdl/cx_delivery INTO DATA(lo_cx).

    ENDTRY.

    IF mt_huitm IS NOT INITIAL.
      SELECT * FROM t006a INTO TABLE @DATA(lt_unit_text)
               FOR ALL ENTRIES IN @mt_huitm
               WHERE spras EQ @sy-langu
                 AND msehi EQ @mt_huitm-altme.
    ENDIF.

    "Fill content data from the HU Item
    LOOP AT mt_huitm INTO DATA(ls_huitm).
      APPEND INITIAL LINE TO rt_hu_content REFERENCE INTO DATA(lr_cont).


      TRY.
          CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
            EXPORTING
              iv_matid      = ls_huitm-matid
              iv_lgnum      = ms_huhdr-lgnum
              iv_lgtyp      = ms_huhdr-lgtyp
              iv_entitled   = ms_huhdr-entitled
            IMPORTING
              es_mat_global = ls_mat_global
              es_mat_pack   = ls_mat_pack.
        CATCH /scwm/cx_md.
          RAISE EXCEPTION TYPE zcx_workstation.
      ENDTRY.

      IF ls_mara-matnr NE ls_mat_global-matnr.
        SELECT SINGLE * FROM mara INTO @ls_mara
               WHERE matnr EQ @ls_mat_global-matnr.
      ENDIF.

      READ TABLE lt_unit_text INTO DATA(ls_unit_text)
           WITH KEY msehi = ls_huitm-altme.
      IF sy-subrc NE 0.
        CLEAR ls_unit_text.
      ENDIF.
      lr_cont->matnr   = ls_mat_global-matnr.
      lr_cont->maktx   = ls_mat_global-maktx.
      lr_cont->quan    = ls_huitm-quan.
      lr_cont->quan_mc = COND #( WHEN ls_huitm-altme NE ls_huitm-meins THEN ls_huitm-quana ).
      lr_cont->altme   = COND #( WHEN lr_cont->quan_mc IS NOT INITIAL THEN ls_huitm-altme ).
      lr_cont->msehl   = COND #( WHEN lr_cont->quan_mc IS NOT INITIAL THEN ls_unit_text-msehl ).
      lr_cont->mfrpn   = ls_mara-mfrpn.
      lr_cont->ean11   = ls_mara-ean11.
      lr_cont->docid   = ls_huitm-qdocid.
      lr_cont->itemid  = ls_huitm-qitmid.
      lr_cont->meins   = ls_huitm-meins.
      lr_cont->matid   = ls_huitm-matid.
      lr_cont->stock_guid = ls_huitm-guid_stock.
      READ TABLE lt_dlv_header REFERENCE INTO DATA(lr_hdr)
           WITH KEY docid = ls_huitm-qdocid.
      IF sy-subrc EQ 0.
        lr_cont->docno = lr_hdr->docno.
      ENDIF.
      READ TABLE lt_dlv_item REFERENCE INTO DATA(lr_dlv_item)
           WITH KEY docid = ls_huitm-qdocid
                    itemid = ls_huitm-qitmid.
      IF sy-subrc EQ 0.
        APPEND lr_dlv_item->* TO mt_dlv_item.
        lr_cont->itemno = lr_dlv_item->itemno.
      ELSE.
        CONTINUE.
      ENDIF.
      IF ls_huitm-altme NE lr_dlv_item->qty-uom.
        TRY.
            CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
              EXPORTING
                iv_matid     = lr_dlv_item->product-productid
                iv_quan      = lr_dlv_item->qty-qty
                iv_unit_from = lr_dlv_item->qty-uom
                iv_unit_to   = ls_huitm-altme
                iv_batchid   = VALUE /scwm/de_batchid( )
              IMPORTING
                ev_quan      = lv_vsola.
          CATCH /scwm/cx_md.
            RAISE EXCEPTION TYPE zcx_workstation
                  MESSAGE e009(ZMC_OUT_UI_PACKING) WITH lr_dlv_item->product-productno lr_dlv_item->qty-uom ls_huitm-altme.
        ENDTRY.
      ELSE.
        CLEAR lv_vsola.
      ENDIF.
      APPEND VALUE zstr_out_ui_delivery(
                    docno = lr_hdr->docno
                    matnr = lr_dlv_item->product-productno
                    maktx = lr_dlv_item->product-product_text
                    quan  = lr_dlv_item->qty-qty
                    quan_mc = lv_vsola
                    altme   = lr_cont->altme
                    msehl   = lr_cont->msehl
                    docid   = ls_huitm-qdocid
                    itemid  = ls_huitm-qitmid
                    meins = lr_dlv_item->qty-uom
                    itemno = lr_dlv_item->itemno
                    sn_idn_complete = COND #( WHEN lr_dlv_item->eew-zzindenttab01 IS INITIAL THEN zif_outb_packing_wc_sp~c_sn_idn_not_relevant )
                  ) TO et_delivery_data.
    ENDLOOP.
    IF iv_with_sn_idn EQ abap_true AND et_delivery_data IS NOT INITIAL.
      SELECT FROM ztcross_cap_nums AS data
             LEFT OUTER JOIN ztcross_numbers AS seltxt
               ON data~lgnum   EQ seltxt~lgnum
              AND data~id_type EQ seltxt~selnum
              AND seltxt~spras EQ @sy-langu
        FIELDS serial, id_type AS selnum, full_serial_number AS serid, seltxt~number_description AS seldescr
        FOR ALL ENTRIES IN @et_delivery_data
        WHERE data~lgnum  EQ @ms_huhdr-lgnum
          AND data~docno  EQ @et_delivery_data-docno
          AND data~itemno EQ @et_delivery_data-itemno
        INTO CORRESPONDING FIELDS OF TABLE @et_sn_idn.

      LOOP AT et_delivery_data REFERENCE INTO DATA(lr_delivery)
          WHERE sn_idn_complete EQ space.
        IF lr_delivery->quan EQ lines( VALUE tt_serial( FOR GROUPS serial OF sn_idn IN et_sn_idn USING KEY docitm
                                                        WHERE ( docno = lr_delivery->docno AND itemno EQ lr_delivery->itemno )
                                                        GROUP BY ( serial = sn_idn-serial )
                                                                 ( serial-serial )
                                     )                ).
          lr_delivery->sn_idn_complete = abap_true.
        ELSE.
          lr_delivery->sn_idn_complete = abap_false.
        ENDIF.

      ENDLOOP.
    ENDIF.

    SORT et_delivery_data BY docno itemno.
    DELETE ADJACENT DUPLICATES FROM et_delivery_data COMPARING docno itemno.
    LOOP AT rt_hu_content REFERENCE INTO DATA(lr_hu_cont).
      READ TABLE et_delivery_data REFERENCE INTO DATA(lr_dlv_data)
           WITH KEY docid = lr_hu_cont->docid
                    itemid = lr_hu_cont->itemid.
      IF sy-subrc EQ 0.
        lr_hu_cont->sn_idn_complete = lr_dlv_data->sn_idn_complete.
        zif_outb_packing_wc_sp~set_hu_content_text( CHANGING cs_hu_cont = lr_hu_cont->* ).

      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~get_open_wt_for_hu.
    DATA: ls_selcrit TYPE /scwm/s_to_selcrit_mon,
          lt_vlenr_r TYPE rseloption,
          lt_to      TYPE /scwm/tt_to_det_mon.

    "Add open WT selection criteria
    ls_selcrit-r_tostat = VALUE #( ( sign = wmegc_sign_inclusive
                                     option = wmegc_option_eq
                                     low = wmegc_to_open )
                                     ( sign = wmegc_sign_inclusive
                                     option = wmegc_option_eq
                                     low = wmegc_to_inactiv )
                                     ).

    "Add HU Number selection criteria
    ls_selcrit-r_lenr = VALUE #( (   sign = wmegc_sign_inclusive
                                     option = wmegc_option_eq
                                     low = iv_huident ) ).

    DO 3 TIMES.
      "Select Open HU WTs
      CALL FUNCTION '/SCWM/TO_GET_WIP'
        EXPORTING
          iv_lgnum   = ms_workstation-lgnum
          iv_open    = abap_true
          iv_srcdata = abap_true
          iv_dstdata = abap_true
          is_selcrit = ls_selcrit
        IMPORTING
          et_to      = lt_to.
      IF lt_to IS INITIAL.
        WAIT UP TO 1 SECONDS.
        CONTINUE.
      ENDIF.
      READ TABLE lt_to INTO rs_ltap_mon
           WITH KEY tostat = wmegc_to_inactiv.
      IF sy-subrc NE 0.
        READ TABLE lt_to INTO rs_ltap_mon INDEX 1.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~get_packmat.
**********************************************************************
*& Key           : LH-090123
*& Request No.   : GAP-022 – “KEP Workcenter”
**********************************************************************
*& Description (short)
*& Determine packagin material from the cuboid algorithm
**********************************************************************
    DATA(lo_pmat) = NEW zcl_ship_pmat_real_algorithm( ).
    DATA(lt_pmat_ids) = NEW zcl_packmmat_algo( ms_termnal_defaults-lgnum )->get_pmat_planned_shipping( ).

    TRY.
        NEW zcl_cuboid_algorithm( ms_termnal_defaults-lgnum )->pack_by_best_pmat(
              EXPORTING
                 it_materials = it_mat_quant
                 it_packmat   = VALUE #( FOR <ls_pmat_id> IN lt_pmat_ids ( pmat_guid = <ls_pmat_id>-matid  ) )
               RECEIVING
                 rt_pack_result = DATA(lt_pack_result) ).
      CATCH zcx_core_exception INTO DATA(lx_core).
        cl_message_helper=>set_msg_vars_for_if_t100_msg( lx_core ).
        RAISE EXCEPTION TYPE zcx_workstation
          MESSAGE ID sy-msgid TYPE wmegc_severity_err NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDTRY.
    IF lt_pack_result IS NOT INITIAL.
      es_result = lt_pack_result[ 1 ].
      ev_pmatid = es_result-pmat_guid.
      READ TABLE lt_pmat_ids INTO DATA(ls_pmat_ids)
           WITH KEY matid = ev_pmatid.
      ev_pmat = ls_pmat_ids-matnr.
    ENDIF.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~get_pack_mat_data.
**********************************************************************
*& Key           : LH-090123
*& Request No.   : GAP-022 – “KEP Workcenter”
**********************************************************************
*& Description (short)
*& Get the packagin material data
**********************************************************************
    DATA: ls_mat_pack   TYPE  /scwm/s_material_pack,
          ls_mat_global TYPE  /scwm/s_material_global.

    CLEAR: ev_pack_mat, ev_pack_mat_text, ev_hutype, ev_hutyptext.

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid      = iv_pack_mat_id
            iv_lgnum      = ms_workstation-lgnum
            iv_lgtyp      = ms_huhdr-lgtyp
            iv_entitled   = ms_huhdr-entitled
          IMPORTING
            es_mat_global = ls_mat_global
            es_mat_pack   = ls_mat_pack.
      CATCH /scwm/cx_md.
        RAISE EXCEPTION TYPE zcx_workstation.
    ENDTRY.

    IF ls_mat_pack-pmtyp IS INITIAL.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e022(zmc_workstation) WITH ls_mat_global-matnr.
    ENDIF.

    ev_hutype = ls_mat_pack-hutyp.
    SELECT SINGLE FROM /scwm/thutypt
           FIELDS hutyptext
           WHERE spras EQ @sy-langu
             AND hutyp EQ @ev_hutype
           INTO @ev_hutyptext .

    ev_pack_mat = ls_mat_global-matnr.
    ev_pack_mat_text = ls_mat_global-maktx.

  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~get_pick_hu_packmat.
    CLEAR: ev_pmat, ev_pmatid.
    ev_pmat = ms_huhdr-pmat.
    ev_pmatid = ms_huhdr-pmat_guid.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~get_prod_weight.
    DATA: lt_uom   TYPE /scwm/tt_material_uom.
    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid    = iv_prodid
            iv_entitled = ms_huhdr-entitled
          IMPORTING
            et_mat_uom  = lt_uom.
      CATCH /scwm/cx_md.
        RETURN.
    ENDTRY.
    READ TABLE lt_uom INTO DATA(ls_uom)
         WITH KEY meinh = zif_wme_c=>gs_uom-pc.
    IF sy-subrc NE 0 OR ls_uom-brgew IS INITIAL.
      RETURN.
    ENDIF.
    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        input    = ls_uom-brgew                 " Input Value
        unit_in  = ls_uom-gewei            " Unit of input value
        unit_out = zif_wme_c=>gs_uom-g            " Unit of output value
      IMPORTING
        output   = rv_weight_in_g                 " Output value
      EXCEPTIONS
        OTHERS   = 0.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~get_req_sns_idn_type.
**********************************************************************
*& Key           : LH-090123
*& Request No.   : GAP-022 – “KEP Workcenter”
**********************************************************************
*& Description (short)
*& Read the zzindenttab* fields from the delivery item, and fill requested
*& SN-IDN types table
**********************************************************************
    DATA ls_type LIKE LINE OF st_sns_idn_type.
    DEFINE add_sns_idn_type.
      IF lr_dlv_itm->eew-zzindenttab&1 IS NOT INITIAL.
        READ TABLE st_sns_idn_type INTO ls_type
             WITH KEY selnum = lr_dlv_itm->eew-zzindenttab&1.
        IF sy-subrc EQ 0.
          APPEND ls_type TO rt_req_sns_idn_type.
        ENDIF.
      ENDIF.
    END-OF-DEFINITION.
    READ TABLE mt_dlv_item REFERENCE INTO DATA(lr_dlv_itm)
         WITH KEY docid = iv_docid
                  itemid = iv_itemid.
    IF sv_test_mode EQ abap_true AND lr_dlv_itm->eew-zzindenttab01 IS INITIAL.
      lr_dlv_itm->eew-zzindenttab01 = '001'.
      lr_dlv_itm->eew-zzindenttab02 = '002'.
      lr_dlv_itm->eew-zzindenttab03 = '003'.
    ENDIF.
    IF lr_dlv_itm IS BOUND.
      add_sns_idn_type: 01, 02, 03, 04, 05.
    ENDIF.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~get_source_hu_guid.
    rv_guid_hu = ms_huhdr-guid_hu.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~get_terminal_defaults.
**********************************************************************
*& Key           : LH-090123
*& Request No.   : GAP-022 – “KEP Workcenter”
**********************************************************************
*& Description (short)
*& Get the current terminal and  read the terminal defaults
**********************************************************************
    DATA: lv_terminal TYPE /scwm/de_wc_terminal.
    CALL FUNCTION 'TH_USER_INFO'
      EXPORTING
        client   = sy-mandt
        user     = sy-uname
      IMPORTING
        terminal = lv_terminal.

    TRY.
        rs_def = st_term_def[ terminal = lv_terminal ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~get_term_defaults_table.
**********************************************************************
*& Key           : LH-090123
*& Request No.   : GAP-022 – “KEP Workcenter”
**********************************************************************
*& Description (short)
*& Fill the Terminal table
**********************************************************************

    SELECT * FROM zout_term_def INTO TABLE st_term_def.
    rt_term_def = st_term_def.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~get_weights.
    CLEAR: ev_scale_hu_weight_in_kg, ev_system_hu_weight_in_kg.

    ev_scale_hu_weight_in_kg = mv_scale_weight_in_kg.
    IF ms_huhdr-unit_gw EQ zif_c_mdm_tool=>c_units-kg.
      ev_system_hu_weight_in_kg = ms_huhdr-g_weight.
    ELSE.
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
        EXPORTING
          input    = ms_huhdr-g_weight                " Input Value
          unit_in  = ms_huhdr-unit_gw            " Unit of input value
          unit_out = zif_c_mdm_tool=>c_units-kg          " Unit of output value
        IMPORTING
          output   = ev_system_hu_weight_in_kg                 " Output value
        EXCEPTIONS
          OTHERS   = 0.
      IF sy-subrc <> 0.

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~get_weight_difference.
    DATA lv_old_pmat_weigth TYPE /scwm/de_quantity.
    DATA lv_new_pmat_weigth TYPE /scwm/de_quantity.

    lv_old_pmat_weigth = zif_outb_packing_wc_sp~get_prod_weight( iv_prodid = iv_old_pmatid ) / 1000.
    IF lv_old_pmat_weigth IS INITIAL.
      TRY.
          DATA(lv_prod) = /scmb/cl_md_access_mdl=>get_md_access(    )->get_prod( iv_prodid = iv_old_pmatid )-prodno.
        CATCH /scmb/cx_md_access.
      ENDTRY.
      RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE e025(zmc_out_ui_packing) WITH lv_prod.
    ENDIF.

    lv_new_pmat_weigth = zif_outb_packing_wc_sp~get_prod_weight( iv_prodid = iv_new_pmatid ) / 1000.
    IF lv_new_pmat_weigth IS INITIAL.
      TRY.
          lv_prod = /scmb/cl_md_access_mdl=>get_md_access(    )->get_prod( iv_prodid = iv_new_pmatid )-prodno.
        CATCH /scmb/cx_md_access.
      ENDTRY.
      RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE e025(zmc_out_ui_packing) WITH lv_prod.
    ENDIF.
    rv_diff = lv_new_pmat_weigth - lv_old_pmat_weigth.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~get_ws_and_hu.
**********************************************************************
*& Key           : LH-090123
*& Request No.   : GAP-022 – “KEP Workcenter”
**********************************************************************
*& Description (short)
*& returns basic data
**********************************************************************
    ev_lgnum   = ms_workstation-lgnum.
    ev_wrkst   = ms_workstation-workstation.
    ev_lgpla   = ms_huhdr-lgpla.
    ev_huident = ms_huhdr-huident.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~init_ws.
**********************************************************************
*& Key           : LH-090123
*& Request No.   : GAP-022 – “KEP Workcenter”
**********************************************************************
*& Description (short)
*& Initialize workcenter, read the HU, move it to the Working Place and
*& perform additional checks
**********************************************************************
    DATA: lv_top_hu               TYPE /scwm/de_huident,
          ls_hu_doc_ref_qty       TYPE ty_hu_doc_ref_qty,
          lt_hu_doc_ref_qty       TYPE tt_hu_doc_ref_qty,
          lv_weight_diff_in_gr    TYPE /scwm/de_quantity,
          lv_system_weight_g      TYPE /scwm/de_quantity,
          lv_one_peace_weight     TYPE /scwm/de_quantity,
          lv_min_one_peace_weight TYPE /scwm/de_quantity,
          lv_perc_dev             TYPE /scwm/de_quantity.

    CLEAR: ev_packtyp, et_bapiret, ev_processed_by_other_desk, ev_weight_diff.
    CLEAR: mt_dlv_item, mt_huhdr, mt_huitm, ms_huhdr, mv_weight_difference_exceeded.

    zif_outb_packing_wc_sp~set_workstation(
      iv_lgnum = iv_lgnum
      iv_wrkst = iv_wrkst ).

    CLEAR: mt_dlv_item.

    IF ms_workstation IS INITIAL.
      RETURN.
    ENDIF.

    /scwm/cl_tm=>cleanup( ).
    /scwm/cl_tm=>set_lgnum( iv_lgnum ).
    /scwm/cl_wm_packing=>get_instance(
        IMPORTING eo_instance = mo_pack_wm ).

    ms_workstation-lgpla = iv_lgpla.
    mo_pack_wm->init_by_workstation(
      EXPORTING
        is_workstation   = ms_workstation                 " Work Station Profile
      EXCEPTIONS
        error            = 1                " Error, see log
        OTHERS           = 2 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*    /scwm/cl_wm_packing=>get_workcenter_bins(
*      EXPORTING
*        is_workstation = ms_workstation
*      IMPORTING
*        et_lagp        = DATA(lt_lagp)
*      EXCEPTIONS
*        OTHERS         = 99 ).
*
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE zcx_workstation
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
    SELECT lgnum, lgpla
      FROM /scwm/lagp INTO TABLE @DATA(lt_lagp)
      WHERE lgnum EQ @ms_workstation-lgnum
        AND lgtyp EQ @ms_workstation-lgtyp.

    IF NOT line_exists( lt_lagp[ lgnum = iv_lgnum
                                 lgpla = iv_lgpla ] ).
      "Storage bin &1 is not part of work station &2
      IF sv_test_mode EQ abap_false.
        RAISE EXCEPTION TYPE zcx_workstation
          MESSAGE s014(zmc_out) WITH iv_lgpla
                                     iv_wrkst.
      ENDIF.
    ENDIF.

    " Get all data for the relevant HU """""" I don't know what will need
    CLEAR: mt_huhdr, mt_huhdr, mt_huhdr.
    mo_pack_wm->get_hu(
      EXPORTING
        iv_huident = iv_huident                 " Handling Unit Identification
*        iv_lock    =    ???????              " Single-Character Indicator
      IMPORTING
**        et_huident = DATA(lt_huidetn)
        et_huitm   = DATA(lt_huitm)                " Material Items in the HU
        es_huhdr   = ms_huhdr                 " Internal Structure for Processing the HU Header
        et_huhdr   = mt_huhdr                 " Table Type for HU Headers in the Internal Structure
        et_hutree  = DATA(lt_hutree)                 " Table with HU Hierarchy Entries
**        et_huref   = DATA(lt_huref)                 " Table with HU References
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2 ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
      MESSAGE ID sy-msgid TYPE   wmegc_severity_suc   NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    READ TABLE mt_huhdr INTO DATA(ls_act_hu)
         WITH KEY huident = iv_huident.

    READ TABLE lt_hutree INTO DATA(ls_hutree)
         WITH KEY guid_parent = ls_act_hu-guid_hu.
    IF sy-subrc EQ 0.
      "HU contains other HUs. Select an HU with products only
      RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE s018(zmc_out).
    ENDIF.


    DATA(lt_term_def) = zif_outb_packing_wc_sp~get_term_defaults_table( ).
    ms_termnal_defaults = zif_outb_packing_wc_sp~get_terminal_defaults( ).
    DELETE lt_term_def WHERE terminal EQ ms_termnal_defaults-terminal.

    ASSIGN mt_huhdr[ top = abap_true ] TO FIELD-SYMBOL(<ls_top_hu>).

    IF <ls_top_hu>-lgpla NE iv_lgpla.
      "Relevant HU is not located in selected bin
      RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE s015(zmc_out).
    ENDIF.

    IF line_exists( lt_term_def[ work_desk_stbin = <ls_top_hu>-lgpla  ] ) AND iv_move_from_other_desk EQ abap_false.
      ev_processed_by_other_desk = abap_true.
*      "HU &1 is beeing proceed by another desk
*      RAISE EXCEPTION TYPE zcx_workstation
*            MESSAGE s001(zmc_out_ui_packing) WITH iv_huident .
    ENDIF.
    IF <ls_top_hu>-lgpla EQ ms_termnal_defaults-lostnfound_stbin.
      "HU &1 in the Lost and Found StBin. Scan another HU
      RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE s002(zmc_out_ui_packing) WITH iv_huident .
    ENDIF.
    IF <ls_top_hu>-lgtyp EQ ms_termnal_defaults-vas_lev3_stype.
      "Scanned HU is relevant for VAS Processing. Move it to the VAS WC
      RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE s003(zmc_out_ui_packing) WITH iv_huident .
    ENDIF.

    IF <ls_top_hu>-lgpla EQ ms_termnal_defaults-buffer_stbin OR iv_move_from_other_desk EQ abap_true.
      move_and_unpack_hu(
        EXPORTING
          iv_huident = iv_huident
          iv_lgpla   = ms_termnal_defaults-work_desk_stbin
        IMPORTING
          et_bapiret = et_bapiret ).
    ELSEIF <ls_top_hu>-lgpla EQ ms_termnal_defaults-work_desk_stbin.
      IF ls_act_hu-top NE abap_true.
        move_and_unpack_hu(
          EXPORTING
            iv_huident = iv_huident
            iv_lgpla   = ms_termnal_defaults-work_desk_stbin
          IMPORTING
            et_bapiret = et_bapiret ).
      ENDIF.
    ELSE.
      "Scanned HU is not in a relevant Storage Bin
      RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE s004(zmc_out_ui_packing) WITH iv_huident .
    ENDIF.

    "Reread the HU if it was removed from the Top HU
    IF ls_act_hu-top EQ abap_false.
      mo_pack_wm->get_hu(
        EXPORTING
          iv_huident = iv_huident                 " Handling Unit Identification
        IMPORTING
          et_huitm   = lt_huitm                " Material Items in the HU
          es_huhdr   = ms_huhdr                 " Internal Structure for Processing the HU Header
          et_huhdr   = mt_huhdr                 " Table Type for HU Headers in the Internal Structure
        EXCEPTIONS
          OTHERS     = 0 ). " handlet before
    ENDIF.

    LOOP AT lt_huitm ASSIGNING FIELD-SYMBOL(<ls_huitem>)
         WHERE guid_parent EQ ls_act_hu-guid_hu.
      APPEND <ls_huitem> TO mt_huitm.
    ENDLOOP.


    /scwm/cl_tm=>cleanup( ).

    ev_packtyp = ls_act_hu-zz_packtyp.
    IF sv_test_mode EQ abap_true.
      ev_packtyp = 'SC'.
    ENDIF.

    mv_scale_weight_in_kg = iv_scale_weight_in_kg.
    "calculate deviations

    IF ms_huhdr-unit_gw EQ 'G'.
      lv_system_weight_g = ms_huhdr-g_weight.
    ELSE.
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
        EXPORTING
          input    = ms_huhdr-g_weight                " Input Value
          unit_in  = ms_huhdr-unit_gw            " Unit of input value
          unit_out = zif_c_mdm_tool=>c_units-gramm          " Unit of output value
        IMPORTING
          output   = lv_system_weight_g                 " Output value
        EXCEPTIONS
          OTHERS   = 0.
      IF sy-subrc <> 0.

      ENDIF.
    ENDIF.
    mv_scale_weight_in_kg = iv_scale_weight_in_kg.
    lv_weight_diff_in_gr = abs( mv_scale_weight_in_kg * 1000 - lv_system_weight_g ).
    IF lv_weight_diff_in_gr GT ms_weight_deviations-max_abs_weight_devi .
      ev_weight_diff = abap_true.
    ELSE.
      LOOP AT mt_huitm INTO DATA(itm) GROUP BY ( matid = itm-matid ) INTO DATA(ls_prod_grp).
        TRY.
            CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
              EXPORTING
                iv_matid     = ls_prod_grp-matid
                iv_quan      = CONV /scwm/de_quantity( 1 )
                iv_unit_from = zif_c_mdm_tool=>c_units-piece
                iv_unit_to   = zif_c_mdm_tool=>c_units-gramm
                iv_batchid   = VALUE /scwm/de_batchid( )
              IMPORTING
                ev_quan      = lv_one_peace_weight.
            IF lv_min_one_peace_weight IS INITIAL OR lv_one_peace_weight LT lv_min_one_peace_weight.
              lv_min_one_peace_weight = lv_one_peace_weight.
            ENDIF.
          CATCH /scwm/cx_md.
            RAISE EXCEPTION TYPE zcx_workstation.
        ENDTRY.
      ENDLOOP.
      IF lv_one_peace_weight IS NOT INITIAL.
        TRY.
            lv_perc_dev = ( lv_weight_diff_in_gr / lv_min_one_peace_weight ) * 100.
          CATCH cx_sy_arithmetic_overflow.
            lv_perc_dev = 999999.
        ENDTRY.
        IF lv_perc_dev GT ms_weight_deviations-max_weight_devi_lp.
          ev_weight_diff = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
    mv_weight_difference_exceeded = ev_weight_diff.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~is_hu_kep.
    DATA:
      lt_huhdr TYPE  /scwm/tt_huhdr_int,
      lt_huitm TYPE  /scwm/tt_huitm_int.

    CALL FUNCTION '/SCWM/HU_SELECT_GEN'
      EXPORTING
        iv_lgnum   = ms_workstation-lgnum
        ir_huident = VALUE  rseloption( ( sign = 'I'
                                          option = 'EQ'
                                          low = iv_huident  ) )
      IMPORTING
        et_huhdr   = lt_huhdr
        et_huitm   = lt_huitm
      EXCEPTIONS
        OTHERS     = 0.

    READ TABLE lt_huitm INTO DATA(ls_itm)
         WITH KEY qdoccat = wmegc_doccat_pdo.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
    "Get Deliveri
    DATA(lo_dlv) = /scwm/cl_dlv_management_prd=>get_instance( ).
    TRY.
        lo_dlv->query(
          EXPORTING
            it_docid        = VALUE #(  ( doccat = wmegc_doccat_pdo docid = ls_itm-qdocid itemid = ls_itm-qitmid ) )
            iv_whno         = ms_workstation-lgnum
            is_read_options = VALUE #(  )
            is_include_data = VALUE #(  )
          IMPORTING
            et_headers        = DATA(lt_dlv_header)
            et_items          = DATA(lt_dlv_item)
        ).
      CATCH /scdl/cx_delivery INTO DATA(lo_cx).

    ENDTRY.
    IF lt_dlv_item IS NOT INITIAL.

      zcl_param=>get_parameter(
        EXPORTING
          iv_lgnum     = ms_workstation-lgnum                  " Warehouse Number/Warehouse Complex
          iv_process   = zif_param_const=>c_zout_0001                 " Process ID (Specification, Program, BAdI etc.)
          iv_parameter = zif_param_const=>c_procty_kep                " Parameter ID for process
        IMPORTING
          et_range  = DATA(lt_rng_procty_kep)                 " Parameter-Framework Low
      ).
      rv_kep = xsdbool( lt_dlv_item[ 1 ]-sapext-/scwm/procty IN lt_rng_procty_kep ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~is_weight_difference_exceeded.
    rv_exceeded = mv_weight_difference_exceeded.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~is_workcenter_mastercarton_rel.
    rv_mc_rel = mv_mastercarton_rel.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~move_hu.
**********************************************************************
*& Key           : LH-090123
*& Request No.   : GAP-022 – “KEP Workcenter”
**********************************************************************
*& Description (short)
*& Move HU.
**********************************************************************
    DATA: lt_bapiret  TYPE bapirettab,
          lv_severity TYPE bapi_mtype,
          lv_tanum    TYPE /scwm/tanum,
          lt_ltap_vb  TYPE /scwm/tt_ltap_vb ##needed.


    CALL FUNCTION '/SCWM/TO_CREATE_MOVE_HU'                    "#EC ENHOK
      EXPORTING
        iv_lgnum       = ms_termnal_defaults-lgnum
        it_create_hu   = VALUE /scwm/tt_to_crea_hu( (
                                            huident = iv_huident
                                            squit   = abap_true
                                            nlpla   = iv_lgpla
                                            procty  = zif_wme_c=>gs_procty-immediate
                                            norou   = wmegc_norou_yes
                                         ) )
        iv_wtcode      = wmegc_wtcode_adhoc_hu
        iv_commit_work = abap_true
      IMPORTING
        et_ltap_vb     = lt_ltap_vb
        et_bapiret     = lt_bapiret
        ev_severity    = lv_severity.
    IF lv_severity CA wmegc_severity_eax.
      ROLLBACK WORK.
      DATA(ls_error) = VALUE bapiret2( lt_bapiret[ type = lv_severity ] OPTIONAL ).
      RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE ID ls_error-id TYPE lv_severity NUMBER ls_error-number
            WITH ls_error-message_v1 ls_error-message_v2
                 ls_error-message_v3 ls_error-message_v4
            EXPORTING
              messages = lt_bapiret
           .
    ENDIF.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~move_hu_to_lost_and_found.
**********************************************************************
*& Key           : LH-090123
*& Request No.   : GAP-022 – “KEP Workcenter”
**********************************************************************
*& Description (short)
*& Move HU to lost and found storage bin
**********************************************************************
    zif_outb_packing_wc_sp~move_hu(
      EXPORTING
        iv_huident = ms_huhdr-huident                 " Handling Unit Identification
        iv_lgpla   = ms_termnal_defaults-lostnfound_stbin                 " Storage Bin
      IMPORTING
        et_bapiret = et_bapiret                 " Error Messages
    ).

  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~save_req_sns_idn.
**********************************************************************
*& Key           : LH-090123
*& Request No.   : GAP-022 – “KEP Workcenter”
**********************************************************************
*& Description (short)
*& Add the SN-IDN numbers to the ztcross_cap_nums table for the delivery
*& item
**********************************************************************
    DATA: lt_db_tab_ori TYPE SORTED TABLE OF ztcross_cap_nums WITH NON-UNIQUE KEY id_type full_serial_number,
          lt_db_tab_mod TYPE STANDARD TABLE OF ztcross_cap_nums WITH EMPTY KEY.

    LOOP AT it_req_sns_idn_type INTO DATA(sns_idn) GROUP BY ( serial = sns_idn-serial ) ASCENDING REFERENCE INTO  DATA(lr_grp).
      DATA(lv_sn_serial) = CONV ztcross_cap_nums-serial( space ).

      LOOP AT GROUP lr_grp INTO DATA(ls_search).
        IF ls_search-selnum EQ c_serial_num_id_type .
          IF ls_search-serial IS INITIAL.
            lv_sn_serial = ls_search-serid.
          ELSE.
            lv_sn_serial = ls_search-serial.
          ENDIF.
          EXIT.
        ENDIF.
      ENDLOOP.


      LOOP AT GROUP lr_grp INTO DATA(ls_req_idn_type).
        APPEND VALUE ztcross_cap_nums(
                        lgnum              = ms_workstation-lgnum
                        docno              = iv_docno
                        itemno             = iv_itemno
                        serial             = lv_sn_serial
                        id_type            = ls_req_idn_type-selnum
                        full_serial_number = ls_req_idn_type-serid
                        guid_hu            = iv_new_guid_hu
                      ) TO lt_db_tab_mod.
      ENDLOOP.
    ENDLOOP.

    IF iv_complete_overwrite EQ abap_true.
      IF iv_old_guid_hu IS INITIAL.
        zcl_crud_ztcross_cap_nums=>delete_multi_by_docno_itemno(
          EXPORTING
            iv_lgnum  = ms_workstation-lgnum
            iv_docno  = iv_docno
            iv_itemno = iv_itemno                 " Item Number
            iv_no_commit = abap_true
        ).
      ELSE.
        zcl_crud_ztcross_cap_nums=>delete_multi_by_doc_item_huid(
          EXPORTING
            iv_lgnum  = ms_workstation-lgnum
            iv_docno  = iv_docno
            iv_itemno = iv_itemno                 " Item Number
            iv_guid_hu = iv_old_guid_hu                 " Item Number
            iv_no_commit = abap_true
        ).
      ENDIF.
    ENDIF.

    zcl_crud_ztcross_cap_nums=>modify_multi_entries(
      it_cap_nums = lt_db_tab_mod ).

    COMMIT WORK.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~set_hu_content_text.
    IF cs_hu_cont-sn_idn_complete =  zif_outb_packing_wc_sp~c_sn_idn_not_relevant.
      cs_hu_cont-sn_idn = space.
    ELSEIF cs_hu_cont-sn_idn_complete EQ abap_false.
      cs_hu_cont-sn_idn = icon_red_light. "'Not OK'(nok).
    ELSEIF cs_hu_cont-sn_idn_complete EQ abap_true.
      cs_hu_cont-sn_idn = icon_green_light. "'OK'(ok_).
    ENDIF.
  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~set_workstation.
**********************************************************************
*& Key           : LH-090123
*& Request No.   : GAP-022 – “KEP Workcenter”
**********************************************************************
*& Description (short)
*& Fill the workstation memeber variables
**********************************************************************
    IF ms_workstation-workstation <> iv_wrkst
       OR ms_workstation-lgnum <> iv_lgnum.
      CALL FUNCTION '/SCWM/TWORKST_READ_SINGLE'
        EXPORTING
          iv_lgnum       = iv_lgnum
          iv_workstation = iv_wrkst
        IMPORTING
          es_workst      = ms_workstation
          es_wrktyp      = ms_worksttyp
        EXCEPTIONS
          OTHERS         = 3.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_workstation
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      IF ms_worksttyp-trtyp <> '1'.
        RAISE EXCEPTION TYPE zcx_workstation
        MESSAGE e114(/scwm/ui_packing).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~unlock_hu.
    CALL FUNCTION 'DEQUEUE_/SCWM/EHU'
      EXPORTING
        huident = ms_huhdr-huident
        lgnum   = ms_huhdr-lgnum
      EXCEPTIONS
        OTHERS  = 0.

  ENDMETHOD.


  METHOD zif_outb_packing_wc_sp~update_weight_in_hu.
    /scwm/cl_tm=>cleanup( ).
    /scwm/cl_tm=>set_lgnum( ms_workstation-lgnum ).

    TRY.
        mo_pack_wm->init_by_workstation(
          EXPORTING
            is_workstation   = ms_workstation                  " Work Station Profile
            iv_doccat        = wmegc_doccat_pdo                 " Doc. Category for Doc. Reference and Doc.-Related Stocks
          EXCEPTIONS
            error            = 1                " Error, see log
            OTHERS           = 2
        ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_workstation
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        mo_pack_wm->get_hu(
          EXPORTING
            iv_guid_hu = iv_guid_hu                " Handling Unit Identification
          IMPORTING
            es_huhdr   = DATA(ls_huhdr_source)                 " Internal Structure for Processing the HU Header
          EXCEPTIONS
            not_found  = 1
            OTHERS     = 2
        ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_workstation
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        IF iv_use_standard_weight EQ abap_true.
          ls_huhdr_source-zz_scale_hu_weight = ls_huhdr_source-g_weight.
          ls_huhdr_source-zz_scale_hu_meins = ls_huhdr_source-unit_gw.
        ELSE.
          ls_huhdr_source-zz_scale_hu_weight = iv_scale_hu_weight.
          ls_huhdr_source-zz_scale_hu_meins = iv_scale_hu_meins.
        ENDIF.
        CLEAR ls_huhdr_source-zz_packtyp.

        mo_pack_wm->/scwm/if_pack_bas~change_huhdr(
          EXPORTING
            is_huhdr   =  ls_huhdr_source                " Internal Structure for Processing the HU Header
          EXCEPTIONS
            error      = 1
            not_locked = 2
            OTHERS     = 3
        ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_workstation
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        mo_pack_wm->save(
          EXCEPTIONS
            error     = 1                " See Log
            OTHERS    = 2
        ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_workstation
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        /scwm/cl_tm=>cleanup( ).
        lock_hu( ).

      CATCH zcx_workstation INTO DATA(lx_ws).
        /scwm/cl_tm=>cleanup( ).
        lock_hu( ).

        RAISE EXCEPTION lx_ws.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
