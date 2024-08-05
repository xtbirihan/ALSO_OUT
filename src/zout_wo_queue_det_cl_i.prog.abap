*&---------------------------------------------------------------------*
*& Include          ZOUT_WO_QUEUE_DET_CL_I
*&---------------------------------------------------------------------*
CLASS lcl_wo_queue_det IMPLEMENTATION.

  METHOD constructor.
********************************************************************
*& Key          : <aahmedov>-130623
*& Request No.   : GAPs 17 - Picking WO Bundling
********************************************************************
*& Description  :
********************************************************************
    mo_log = NEW /scwm/cl_log( ).

    mv_lgnum = iv_lgnum.

    mt_queue_selopt = VALUE #( FOR <q> IN so_que
        ( sign = <q>-sign option = <q>-option low = <q>-low high = <q>-high  ) ).

    IF mv_lgnum IS INITIAL.
      mo_log->save_applog(
        EXPORTING
          is_log = VALUE #( extnumber = |{ TEXT-001 } |
                            object = zif_whse_order=>log-object
                            subobject = zif_whse_order=>log-subobject ) ).
      " Log Handle

      RAISE EXCEPTION NEW zcx_core_exception( ).
    ENDIF.

    mv_lgnum = validate_lgnum( iv_lgnum = iv_lgnum ).
    mo_packmmat_algo = NEW zcl_packmmat_algo( iv_lgnum = mv_lgnum ).

    zcl_crud_ztout_queue_det=>select_multi_by_queue(
      EXPORTING
        iv_lgnum     = mv_lgnum
        it_queue_r   = mt_queue_selopt
      IMPORTING
        et_queue_det = mt_queue_det ).

    IF mt_queue_det IS INITIAL.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD sel_wo.
********************************************************************
*& Key          : <aahmedov>-130623
*& Request No.  : GAPs 17 - Picking WO Bundling
********************************************************************
*& Description  :
********************************************************************
    DATA: lt_range TYPE rsds_frange_t.

    CLEAR: et_who,
           et_whohu,
           et_ordim_o.

    APPEND INITIAL LINE TO lt_range ASSIGNING FIELD-SYMBOL(<range>).
    "add filter by STATUS INITIAL
    <range> = VALUE #( fieldname = zif_whse_order=>wo_mapping_fieldname-status
                       selopt_t = VALUE #( ( sign = wmegc_sign_inclusive
                                             option = wmegc_option_eq
                                             low = wmegc_wo_open ) ) ).

    "add filter by QUEUE
    APPEND INITIAL LINE TO lt_range ASSIGNING <range>.
    <range> = VALUE #( fieldname = zif_whse_order=>wo_mapping_fieldname-queue
                       selopt_t = VALUE #( FOR GROUPS OF <queue_det> IN mt_queue_det
                                            GROUP BY <queue_det>-inqueue
                                            ( sign = wmegc_sign_inclusive
                                              option = wmegc_option_eq
                                              low = <queue_det>-inqueue  ) ) ).

    TRY.
        CALL FUNCTION '/SCWM/WHO_RANGE_GET'
          EXPORTING
            iv_lgnum = mv_lgnum
            it_range = lt_range
          IMPORTING
            et_who   = et_who
            et_whohu = et_whohu.
      CATCH /scwm/cx_core.
        mo_log->add_message( ).
        RETURN.
    ENDTRY.

    IF lines( et_who ) = 0.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCWM/TO_READ_WHO_MULT'
      EXPORTING
        iv_lgnum    = mv_lgnum
        it_whoid    = VALUE /scwm/tt_whoid( FOR GROUPS OF <who> IN et_who
                                  GROUP BY <who>-who
                                  ( who = <who>-who ) )
      IMPORTING
        et_ordim_o  = et_ordim_o
      EXCEPTIONS
        wrong_input = 1
        not_found   = 2
        error       = 3
        OTHERS      = 4.
    IF sy-subrc <> 0.
      mo_log->add_message( ).
    ENDIF.

    DATA(lt_queue_r) = VALUE rseloption( FOR GROUPS OF <who_queue> IN et_who
                                        GROUP BY <who_queue>-queue
                                        ( sign = wmegc_sign_inclusive
                                              option = wmegc_option_eq
                                              low = <who_queue>-queue  ) ).

    DELETE mt_queue_det WHERE inqueue NOT IN lt_queue_r.

  ENDMETHOD.

  METHOD update_wo.
********************************************************************
*& Key          : <aahmedov>-130623
*& Request No.  : GAPs 17 - Picking WO Bundling
********************************************************************
*& Description  :
********************************************************************
    DATA: lt_who         TYPE /scwm/tt_who_int,
          ls_ordim_o     TYPE /scwm/ordim_o,
          lt_ordim_o     TYPE /scwm/tt_ordim_o,
          ls_ordim_o_int TYPE /scwm/s_ordim_o_int,
          lt_ordim_o_int TYPE /scwm/tt_ordim_o_int,
          lt_wo_rsrc_ty  TYPE /scwm/tt_wo_rsrc_ty,
          ls_wo_rsrc_ty  TYPE /scwm/wo_rsrc_ty,
          ls_attributes  TYPE /scwm/s_who_att,
          ls_t346        TYPE /scwm/t346,
          lv_content     TYPE /scwm/de_content,
          et_wo_rsrc_ty  TYPE /scwm/tt_wo_rsrc_ty,
          lv_subrc       TYPE sy-subrc.

    FIELD-SYMBOLS: <who> TYPE /scwm/s_who_int.

    lt_who = it_who.

    LOOP AT lt_who ASSIGNING <who>.
      ls_wo_rsrc_ty-who = <who>-who.
      APPEND ls_wo_rsrc_ty TO lt_wo_rsrc_ty.
    ENDLOOP.
*   Get index records for all WOs
    CALL FUNCTION '/SCWM/RSRC_WHO_RSTYP_GET'
      EXPORTING
        iv_lgnum      = mv_lgnum
        iv_rfind      = abap_true
      CHANGING
        ct_wo_rsrc_ty = lt_wo_rsrc_ty.

    LOOP AT lt_who ASSIGNING <who>.
*   Don't create index records for Super-WOs of category Load/Unload
      IF ( <who>-type = wmegc_wcr_lu AND <who>-flgwho = abap_true ).
        CONTINUE.
      ENDIF.

      CALL FUNCTION '/SCWM/T346_READ_SINGLE'
        EXPORTING
          iv_lgnum  = mv_lgnum
          iv_queue  = <who>-queue
        IMPORTING
          es_t346   = ls_t346
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
             INTO mv_message.
        mo_log->add_message( ).
        RETURN.
      ENDIF.

      CLEAR: ls_ordim_o_int,
             lt_ordim_o_int,
             lt_ordim_o,
             et_wo_rsrc_ty.

*     Read WO to get open WTs
      lt_ordim_o = VALUE #( FOR <ordim_o> IN it_ordim_o
                                WHERE ( who = <who>-who )
                                ( CORRESPONDING #( <ordim_o> ) ) ).

*   For the queue change of Load/unload subWO the topWO's WT must be collected
      IF <who>-type = wmegc_wcr_lu AND <who>-topwhoid IS NOT INITIAL.
        TRY.
            CALL FUNCTION '/SCWM/WHO_SELECT'
              EXPORTING
                iv_to      = abap_true
                iv_lgnum   = mv_lgnum
                iv_who     = <who>-topwhoid
              IMPORTING
                et_ordim_o = lt_ordim_o.
          CATCH /scwm/cx_core.
            mo_log->add_message( ).
            CLEAR lt_ordim_o.
        ENDTRY.
      ENDIF.

      READ TABLE lt_wo_rsrc_ty INTO ls_wo_rsrc_ty
        WITH KEY who = <who>-who.

      IF ( ( sy-subrc NE 0 ) AND
           ( ls_t346-rfrsrc = wmegc_rfrsrc_rs OR
               ls_t346-rfrsrc = wmegc_rfrsrc_rf OR
               ls_t346-rfrsrc = wmegc_rfrsrc_mfs_with_rsrc ) ).
        CLEAR lt_ordim_o_int.
        LOOP AT lt_ordim_o INTO ls_ordim_o ##INTO_OK.
          MOVE-CORRESPONDING ls_ordim_o TO ls_ordim_o_int ##ENH_OK.
          APPEND ls_ordim_o_int TO lt_ordim_o_int.
        ENDLOOP.
        IF <who>-flginv   = abap_true AND
           lt_ordim_o_int IS INITIAL.
*         get all activity types for inventory
          SELECT a~act_type INTO ls_ordim_o_int-act_type
                 UP TO 1 ROWS
                 FROM /scwm/tactty AS a
                 INNER JOIN /scwm/taareas AS b
                 ON  a~lgnum    = b~lgnum
                 AND a~act_type = b~act_type
                 WHERE a~lgnum   = mv_lgnum
                 AND   a~trart   = wmegc_trart_inv
                 AND   b~area    = <who>-areawho.      "#EC CI_BUFFJOIN
          ENDSELECT.

          ls_ordim_o_int-trart      = wmegc_trart_inv.
          ls_ordim_o_int-queue      = VALUE #( it_who[ who = <who>-who ]-queue OPTIONAL ).
          ls_ordim_o_int-lgnum      = <who>-lgnum.
          ls_ordim_o_int-aarea      = <who>-areawho.
          ls_ordim_o_int-areawho    = <who>-areawho.
          APPEND ls_ordim_o_int TO lt_ordim_o_int.
        ENDIF.
*       Determine the RF content
        CALL FUNCTION '/SCWM/WHO_DET_RF_RSRC_CONTENT'
          EXPORTING
            iv_lgnum    = mv_lgnum
            is_who      = <who>
            it_ordim_o  = lt_ordim_o_int
          IMPORTING
            ev_content  = lv_content
          EXCEPTIONS
            wrong_trart = 1
            wrong_iproc = 2
            tproc_error = 3
            OTHERS      = 4.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO mv_message.
          mo_log->add_message( ).
          RETURN.
        ENDIF.
*       Set queue temporary otherwise next fm works on the "old" data
        <who>-queue = VALUE #( it_who[ who = <who>-who ]-queue OPTIONAL ).

*       Create the index entries
        CALL FUNCTION '/SCWM/RSRC_WHO_CREATE'
          EXPORTING
            iv_lgnum        = mv_lgnum
            iv_content      = lv_content
            is_who          = <who>
          IMPORTING
            ev_lowest_lsd   = <who>-lsd
            et_wo_rsrc_ty   = et_wo_rsrc_ty
          CHANGING
            ct_ordim_o      = lt_ordim_o_int
          EXCEPTIONS
            no_qualif_rstyp = 1
            OTHERS          = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
             INTO mv_message.
          mo_log->add_message( ).
          RETURN.
        ENDIF.
        IF et_wo_rsrc_ty[] IS INITIAL.
          MESSAGE s039(/scwm/rsrc) WITH <who>-who INTO mv_message.
          mo_log->add_message( ).
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.

*creating whoid table from who table
    LOOP AT lt_who ASSIGNING <who>.
*     Clearing for security, than setting the queue
      CLEAR: ls_attributes,
         lv_subrc.
      MOVE-CORRESPONDING <who> TO ls_attributes ##ENH_OK.
      ls_attributes-queue  = VALUE #( it_who[ who = <who>-who ]-queue OPTIONAL ).
      ls_attributes-lsd    = <who>-lsd.

      CALL FUNCTION '/SCWM/WHO_UPDATE'
        EXPORTING
          iv_lgnum      = mv_lgnum
          iv_db_update  = abap_true
          iv_who        = <who>-who
          iv_queue      = abap_true
          is_attributes = ls_attributes
        EXCEPTIONS
          read_error    = 1
          attributes    = 2
          OTHERS        = 3.
      IF sy-subrc <> 0.
        lv_subrc = sy-subrc.
        mo_log->add_message( ).
        EXIT.
      ELSE.
        DATA(lv_queue_old) = VALUE /scwm/de_queue( lt_wo_rsrc_ty[ who = <who>-who ]-queue OPTIONAL ).
        MESSAGE s014(zmc_whse_order)
         WITH <who>-who
              lv_queue_old
              ls_attributes-queue
              INTO mv_message.
        mo_log->add_message( ).
      ENDIF.
    ENDLOOP.

*   If error occurs - rollback
    IF lv_subrc <> 0.
      CALL FUNCTION 'DB_ROLLBACK'.
      MESSAGE s002(/scwm/l3) INTO mv_message.
      mo_log->add_message( ).
      RETURN.
    ELSE.
*   commit work and wait to display new data after auto-refresh
      COMMIT WORK AND WAIT.
    ENDIF.

    CALL FUNCTION 'DEQUEUE_ALL'.

  ENDMETHOD.

  METHOD validate_lgnum.
********************************************************************
*& Key          : <aahmedov>-130623
*& Request No.  : GAPs 17 - Picking WO Bundling
********************************************************************
*& Description  :
********************************************************************
    IF iv_lgnum IS INITIAL.
      MESSAGE e008(/scmb/prr) INTO mv_message.
      mo_log->add_message( ).
    ENDIF.

    CALL FUNCTION '/SCWM/T340D_READ_SINGLE'
      EXPORTING
        iv_lgnum  = iv_lgnum
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE e008(/scmb/prr) INTO mv_message.
      mo_log->add_message( ).
    ENDIF.

    rv_lgnum = iv_lgnum.

  ENDMETHOD.

  METHOD main.
********************************************************************
*& Key          : <aahmedov>-130623
*& Request No.  : GAPs 17 - Picking WO Bundling
********************************************************************
*& Description  :
********************************************************************
    DATA: lt_aarea_r            TYPE /scwm/tt_aarea_r,
          lt_nltyp_r            TYPE /scwm/tt_lgtyp_r,
          lt_ppmat_r            TYPE /scwm/tt_pmat_r,
          lt_nestlvl_r          TYPE RANGE OF zde_nestlvl,
          lt_prule_pmat_mapping TYPE lt_prule_pmat_mapping,
          lt_who_changed        TYPE /scwm/tt_who_int,
          lt_who_changed_r      TYPE /scwm/tt_who_r,
          lv_nltyp              TYPE /scwm/lgtyp,
          lv_nltyp_check_ok     TYPE xfeld VALUE abap_true,
          lv_aarea_twomh_ok     TYPE xfeld VALUE abap_false,
          ls_mat_lgnum          TYPE /scwm/s_material_lgnum,
          lv_is_noncnv_ok       TYPE xfeld VALUE abap_true,
          lt_lagps              TYPE /scwm/tt_lagps.

    sel_wo(
      IMPORTING
        et_who     = DATA(lt_who)
        et_whohu   = DATA(lt_whohu)
        et_ordim_o = DATA(lt_ordim_o) ).

    IF lines( lt_who ) = 0.
      MESSAGE e015(zmc_whse_order) INTO mv_message.
      mo_log->add_message( ).

      MESSAGE i013(zmc_whse_order) INTO mv_message.
      mo_log->add_message( ).

      save_log( ).

      RETURN.
    ENDIF.

    DATA(lt_pmatid_mast) = mo_packmmat_algo->get_pmat_carton( ).

    IF lt_pmatid_mast IS NOT INITIAL.
      lt_prule_pmat_mapping = VALUE #( FOR <mc> IN lt_pmatid_mast
          ( ppmat = zif_wme_c=>gs_ppmat-mast
            pmatid = <mc>-matid ) ).
    ENDIF.

    "read activity areas
    CALL FUNCTION '/SCWM/LAGPS_READ_MULTI'
      EXPORTING
        iv_lgnum       = mv_lgnum
        ir_storage_bin = VALUE rseloption( FOR GROUPS OF <vlpla> IN lt_ordim_o
                                              GROUP BY <vlpla>-vlpla
                                               ( sign = wmegc_sign_inclusive
                                                 option = wmegc_option_eq
                                                 low = <vlpla>-vlpla )  )
      IMPORTING
        et_lagps       = lt_lagps
      EXCEPTIONS
        wrong_input    = 1
        not_found      = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      mo_log->add_message( ).
      RETURN.
    ENDIF.

    LOOP AT lt_who ASSIGNING FIELD-SYMBOL(<ls_who>).

      DATA(ls_ordim_o) = VALUE /scwm/ordim_o( lt_ordim_o[ who = <ls_who>-who
                                                          trart = wmegc_trart_pick ] OPTIONAL ).

      DATA(lv_aarea) = VALUE /scwm/de_aarea( lt_lagps[ act_type = CONV #( /scmb/cl_sc_dlvoconst=>_c_atype_pick )
                                                       lgpla = ls_ordim_o-vlpla ]-aarea OPTIONAL ).

      DATA(lv_nestlvl) = COND zde_nestlvl( WHEN ls_ordim_o-shiphuid IS INITIAL
                                            AND ls_ordim_o-huid IS INITIAL
                                                THEN space
                                           WHEN ls_ordim_o-shiphuid IS NOT INITIAL
                                            AND ls_ordim_o-huid = ls_ordim_o-shiphuid
                                                THEN 1
                                           WHEN ls_ordim_o-shiphuid IS NOT INITIAL
                                            AND ls_ordim_o-huid <> ls_ordim_o-shiphuid
                                                THEN 2
                                           WHEN ls_ordim_o-shiphuid IS INITIAL
                                                THEN 1 ).

      lv_nltyp = ls_ordim_o-nltyp.

      DATA(lv_pmat_guid) = VALUE /scwm/de_pmatid( lt_whohu[ who = <ls_who>-who ]-pmat_guid OPTIONAL ).

      LOOP AT mt_queue_det ASSIGNING FIELD-SYMBOL(<ls_queue_det>)
         WHERE inqueue = <ls_who>-queue.

        IF <ls_queue_det>-aarea IS NOT INITIAL.

          APPEND VALUE #( sign = wmegc_sign_inclusive
                          option = wmegc_option_eq
                          low = <ls_queue_det>-aarea ) TO lt_aarea_r.

        ENDIF.

        IF <ls_queue_det>-twomh IS NOT INITIAL.
* if at least one WT (of the current WO) has the "real-AA" --> this check is ok

          DATA(lt_aarea_who_r) = VALUE /scwm/tt_aarea( FOR <ls_ordim_o> IN lt_ordim_o
                                          WHERE ( who = <ls_who>-who )
                                          ( aarea = VALUE #( lt_lagps[ act_type = if_flog_process_receipts=>c_acttype_pick
                                                                       lgpla = <ls_ordim_o>-vlpla ]-aarea OPTIONAL ) ) ).

          zcl_param=>get_parameter(
            EXPORTING
              iv_lgnum     = mv_lgnum                 " Warehouse Number/Warehouse Complex
              iv_process   = zif_param_const=>c_zout_0007                 " Process ID (Specification, Program, BAdI etc.)
              iv_parameter = zif_param_const=>c_aa_twomh                 " Parameter ID for process
            IMPORTING
              et_range     = DATA(lt_twomh_aarea_r)                 " SELECT-OPTIONS Table
          ).

          LOOP AT lt_aarea_who_r ASSIGNING FIELD-SYMBOL(<aarea>).
            CHECK <aarea> IN lt_twomh_aarea_r.
            lv_aarea_twomh_ok = abap_true.
            EXIT.
          ENDLOOP.

        ELSE.

          lv_aarea_twomh_ok = abap_true.

        ENDIF.

        IF <ls_queue_det>-nonconveyable IS NOT INITIAL.

          TRY.
              CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
                EXPORTING
                  iv_lgnum     = mv_lgnum
                  iv_matid     = ls_ordim_o-matid "UM 16.11.2023 - changed from the pack.mat. to the WT-mat.
                  iv_entitled  = ls_ordim_o-entitled
                  iv_lgtyp     = ls_ordim_o-vltyp
                IMPORTING
                  es_mat_lgnum = ls_mat_lgnum.

            CATCH /scwm/cx_md.                          "#EC NO_HANDLER
          ENDTRY.

          IF ls_mat_lgnum IS NOT INITIAL.
            lv_is_noncnv_ok = xsdbool( <ls_queue_det>-nonconveyable EQ ls_mat_lgnum-zz1_nonconveyable_whd ).
          ENDIF.

        ENDIF.

        IF <ls_queue_det>-putwall IS NOT INITIAL.

          lv_nltyp_check_ok = ls_ordim_o-zzputwall.

        ELSEIF <ls_queue_det>-nltyp IS NOT INITIAL.

          APPEND INITIAL LINE TO lt_nltyp_r ASSIGNING FIELD-SYMBOL(<ls_nltyp_r>).

          <ls_nltyp_r> = VALUE #( sign = wmegc_sign_inclusive
                                  option = wmegc_option_eq
                                  low = <ls_queue_det>-nltyp ).

          lv_nltyp_check_ok = xsdbool( lv_nltyp IN lt_nltyp_r ).

        ENDIF.

        IF <ls_queue_det>-ppmat IS NOT INITIAL.

          lt_ppmat_r = VALUE #( FOR <prulep_pmat> IN lt_prule_pmat_mapping
                                WHERE ( ppmat EQ <ls_queue_det>-ppmat  ) "append only master carton entries
                                ( sign = wmegc_sign_inclusive
                                  option = wmegc_option_eq
                                  low = <prulep_pmat>-pmatid ) ).
        ENDIF.

        IF <ls_queue_det>-nestlvl IS NOT INITIAL.

          APPEND INITIAL LINE TO lt_nestlvl_r ASSIGNING FIELD-SYMBOL(<ls_nestlvl_r>).

          <ls_nestlvl_r> = VALUE #( sign = wmegc_sign_inclusive
                                  option = wmegc_option_eq
                                  low = <ls_queue_det>-nestlvl ).

        ENDIF.

        DATA(lv_outqueue) = COND #( WHEN lv_aarea IN lt_aarea_r AND
                                         lv_aarea_twomh_ok EQ abap_true AND
                                         lv_nltyp_check_ok EQ abap_true AND
                                         lv_pmat_guid IN lt_ppmat_r AND
                                         lv_nestlvl IN lt_nestlvl_r AND
                                         lv_is_noncnv_ok EQ abap_true
                                    THEN <ls_queue_det>-outqueue
                                    ELSE <ls_who>-queue ).

        "reset to the default values of the range tables and the flags
        CLEAR: lt_aarea_r,
               lt_nltyp_r,
               lt_ppmat_r,
               lt_nestlvl_r,
               lv_aarea_twomh_ok.

        lv_is_noncnv_ok = abap_true.
        lv_nltyp_check_ok = abap_true.

        " if outqueue for the respecting input queue is found,
        " then exit the loop for the determination conditions
        CHECK lv_outqueue <> <ls_who>-queue.

        <ls_who>-queue = lv_outqueue.
        APPEND <ls_who> TO lt_who_changed.
        APPEND VALUE #( sign = wmegc_sign_inclusive
                        option = wmegc_option_eq
                        low = <ls_who>-who ) TO lt_who_changed_r.

        EXIT.

      ENDLOOP.

    ENDLOOP.

    IF lt_who_changed IS NOT INITIAL.
      update_wo( it_who     = lt_who_changed
                 it_ordim_o = VALUE #( FOR <ordim_o> IN lt_ordim_o
                                        WHERE ( who IN lt_who_changed_r )
                                        ( CORRESPONDING #( <ordim_o> ) ) ) ).
    ELSE.
      MESSAGE i013(zmc_whse_order) INTO mv_message.
      mo_log->add_message( ).
    ENDIF.

    save_log( ).

  ENDMETHOD.

  METHOD save_log.
********************************************************************
*& Key          : <BSUGAREV>-Nov 30, 2023
*& Request No.  : GAP-017 FS Picking WO Bundling
********************************************************************
*& Description  :
********************************************************************
    DATA: ls_display_profile TYPE bal_s_prof.

    DATA(lt_bapiret) = mo_log->get_prot( ).
    IF lines( lt_bapiret ) = 0.
      RETURN.
    ENDIF.

    mo_log->create_log(
      EXPORTING
        is_log = VALUE #( extnumber = |{ TEXT-001 } { mv_lgnum }|
                          object    = zif_whse_order=>log-object
                          subobject = zif_whse_order=>log-subobject )
    ).

    mo_log->convert_bapiret2applog( ).

    " if the report is being executed in background, don't display the log
    IF sy-batch <> abap_true.

      CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
        IMPORTING
          e_s_display_profile = ls_display_profile.

      ls_display_profile-use_grid = abap_true.

      TRY.
          mo_log->display_log( is_display_profile = ls_display_profile ).

          mo_log->save_applog2db( ).

          COMMIT WORK.
        CATCH /scwm/cx_basics ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
