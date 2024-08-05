*&---------------------------------------------------------------------*
*& Include          ZOUT_ROUTING_CREATE
*& Original Object, Subroutine: ROUTING_CREATE (/SCWM/LL03AF1L)
*&---------------------------------------------------------------------*

FORM routing_create_cust USING    is_ltap     TYPE /scwm/ltap
                         CHANGING et_ltap_vb TYPE /scwm/tt_ltap_vb
                    RAISING /scwm/cx_core.
********************************************************************
*& Key          : AYORDANOV-Jan 9, 2024
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
********************************************************************
*& Description  :
********************************************************************
  DATA: lv_activate    TYPE xfeld.

  DATA: ls_ltap        TYPE /scwm/ltap.
  DATA: ls_rl03a       TYPE /scwm/rl03a.
  DATA: ls_huhdr_int   TYPE /scwm/s_huhdr_int.
  DATA: ls_t331        TYPE /scwm/t331.
  DATA: ls_vltyp       TYPE /scwm/t331.
  DATA: ls_t333        TYPE /scwm/t333.
  DATA: ls_ordim_o     TYPE /scwm/s_ordim_o_int.
  DATA: ls_ordim_o2    TYPE /scwm/ordim_o.

  DATA: lt_huhdr_int TYPE /scwm/tt_huhdr_int,
        lt_ordim_o   TYPE /scwm/tt_ordim_o.

  FIELD-SYMBOLS: <ls_huhdr>   TYPE /scwm/s_huhdr_int,
                 <ls_tap>     TYPE /scwm/ltap,
                 <ls_ordim_o> TYPE /scwm/ordim_o.

  CHECK is_ltap-iprocty IS NOT INITIAL OR
        is_ltap-iltyp   IS NOT INITIAL OR
        is_ltap-ilpla   IS NOT INITIAL.

  CLEAR et_ltap_vb.

  MESSAGE i130(/scwm/rem_bin_det) WITH is_ltap-vlenr INTO gv_msgtext.
  CALL METHOD go_prot->add_message( ip_row = gv_prot_row ).

  CALL FUNCTION '/SCWM/HUHEADER_READ'
    EXPORTING
      iv_huident   = is_ltap-vlenr
      iv_db_select = 'A'
      iv_nobuff    = ' '
      iv_lock      = ' '
    IMPORTING
      es_huheader  = ls_huhdr_int
      et_huheader  = lt_huhdr_int
    EXCEPTIONS
      not_found    = 1
      input        = 2
      error        = 3
      OTHERS       = 4.

  CASE sy-subrc.
    WHEN 0.

      " HU exists
      ls_ltap-vlenr    = ls_huhdr_int-huident.
      ls_ltap-sguid_hu = ls_huhdr_int-guid_hu.

      "     check if HU-WT/WHO has to be updated with new PICK_COMP_DT/LSD
      IF ls_huhdr_int-flgmove IS NOT INITIAL. " there is a HU-WT

        CLEAR lt_ordim_o.
        CALL FUNCTION '/SCWM/TO_READ_SRC'
          EXPORTING
            iv_lgnum   = ls_huhdr_int-lgnum
            iv_huident = ls_huhdr_int-huident
          IMPORTING
            et_ordim_o = lt_ordim_o
          EXCEPTIONS
            OTHERS     = 0.

        IF ( is_ltap-pick_comp_dt IS NOT INITIAL OR " there is a time
             is_ltap-gi_tst IS NOT INITIAL )   AND
           is_ltap-vlpla        IS NOT INITIAL AND " bin as source
           is_ltap-routeto      IS     INITIAL AND " no route-WT
           is_ltap-flghuto      IS     INITIAL.    " new-WT is Prod-WT

          "  Check if WT to Pick Point exists and update HU WT
          LOOP AT lt_ordim_o ASSIGNING <ls_ordim_o>
                             WHERE flghuto = 'X'
                               AND ( routeto = wmegc_routeto_layout OR
                                     routeto = wmegc_routeto_pp ).

            IF is_ltap-pick_comp_dt IS NOT INITIAL AND
               ( <ls_ordim_o>-pick_comp_dt IS INITIAL OR
                 <ls_ordim_o>-pick_comp_dt > is_ltap-pick_comp_dt ).
              <ls_ordim_o>-pick_comp_dt = is_ltap-pick_comp_dt.

              READ TABLE gt_ordim_upd INTO ls_ordim_o
                WITH KEY tanum = <ls_ordim_o>-tanum.
              IF sy-subrc = 0.
                ls_ordim_o-pick_comp_dt = <ls_ordim_o>-pick_comp_dt.
                ls_ordim_o-who_lsd_adj  = 'X'.
                MODIFY gt_ordim_upd FROM ls_ordim_o INDEX sy-tabix.
              ELSE.
                MOVE-CORRESPONDING <ls_ordim_o> TO ls_ordim_o.
                ls_ordim_o-who_lsd_adj  = 'X'.
                APPEND ls_ordim_o TO gt_ordim_upd.
              ENDIF.
            ENDIF.

            IF is_ltap-gi_tst IS NOT INITIAL AND
               ( <ls_ordim_o>-pick_comp_dt IS INITIAL OR
                 <ls_ordim_o>-pick_comp_dt > is_ltap-gi_tst ).
              "              Take over GI Timestamp if lower than Pick Completion
              <ls_ordim_o>-pick_comp_dt = is_ltap-gi_tst.
              READ TABLE gt_ordim_upd INTO ls_ordim_o
                WITH KEY tanum = <ls_ordim_o>-tanum.
              IF sy-subrc = 0.
                ls_ordim_o-pick_comp_dt = <ls_ordim_o>-pick_comp_dt.
                ls_ordim_o-who_lsd_adj  = 'X'.
                MODIFY gt_ordim_upd FROM ls_ordim_o INDEX sy-tabix.
              ELSE.
                MOVE-CORRESPONDING <ls_ordim_o> TO ls_ordim_o.
                ls_ordim_o-who_lsd_adj  = 'X'.
                APPEND ls_ordim_o TO gt_ordim_upd.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
        "       Check if the existing HU WT is an active HU WT - if not,
        "       we still need to create an active WT
        "       (e.g. in TCD scenario there could be one inactive TCD Pick WT)
        READ TABLE lt_ordim_o INTO ls_ordim_o2
          WITH KEY vlenr = ls_huhdr_int-huident
                   flghuto = 'X'
                   tostat  = wmegc_to_open.
        IF sy-subrc = 0.
          " for distribution equipment delivery it could be that the WT should have been added to an existing WO w/o LOSC
          " if CRETO is set accordingly, remove the flags for the inactive WT
          PERFORM ddul_losc_adapt_inactive_wt USING is_ltap.

          READ TABLE tap TRANSPORTING NO FIELDS
            WITH KEY tanum = ls_ordim_o2-tanum
                     tostat = wmegc_to_confirmed.
          IF sy-subrc <> 0.
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.

      IF ls_huhdr_int-no_move IS NOT INITIAL.
        "       HU is locked
        MESSAGE e828 WITH is_ltap-vlenr INTO gv_msgtext.
        CALL METHOD go_prot->add_message( ip_field = 'IV_HUIDENT' ).
        RAISE EXCEPTION TYPE /scwm/cx_core.
      ENDIF.
    WHEN 1.
      "    NO HU
      MESSAGE e222 WITH is_ltap-vlenr  INTO gv_msgtext.
      CALL METHOD go_prot->add_message( ip_field = 'IV_HUIDENT' ).
      RAISE EXCEPTION TYPE /scwm/cx_core.
    WHEN OTHERS.
      "     Fatal error
      MESSAGE e830 WITH is_ltap-vlenr  INTO gv_msgtext.
      CALL METHOD go_prot->add_message( ip_field = 'IV_HUIDENT' ).
      RAISE EXCEPTION TYPE /scwm/cx_core.
  ENDCASE.

  " save inactive WT in global table
  CALL FUNCTION '/SCWM/ROUTING_L_TO_MAIN'
    EXPORTING
      iv_action      = wmegc_insert
      iv_guid_hu     = ls_ltap-sguid_hu
      iv_guid_sub_hu = is_ltap-sguid_hu
      iv_guid_to     = is_ltap-guid_to
      iv_routeto     = is_ltap-routeto
      io_prot        = go_prot
      iv_row         = is_ltap-seqno.

  ls_ltap-lgnum      = is_ltap-lgnum.
  IF is_ltap-iprocty IS NOT INITIAL.
    ls_ltap-procty   = is_ltap-iprocty.
  ELSE.
    ls_ltap-procty   = is_ltap-procty.
  ENDIF.
  ls_ltap-created_by = is_ltap-created_by.
  ls_ltap-flghuto    = 'X'.

  " check if intermediate storage type is pick point
  CALL FUNCTION '/SCWM/T331_READ_SINGLE'
    EXPORTING
      iv_lgnum = is_ltap-lgnum
      iv_lgtyp = is_ltap-iltyp
    IMPORTING
      es_t331  = ls_t331
    EXCEPTIONS
      OTHERS   = 0.

  IF is_ltap-routeto IS INITIAL                  AND
     ( ls_t331-st_role = wmegc_strole_ppoint  OR
       ls_t331-st_role = wmegc_strole_ippoint OR
       is_ltap-ippoint IS NOT INITIAL           ).
    ls_ltap-routeto    = wmegc_routeto_pp.
  ELSE.
    ls_ltap-routeto    = wmegc_routeto_layout.
  ENDIF.

  " check if there is already a route WT
  READ TABLE tap ASSIGNING <ls_tap>
       WITH KEY vlenr   = ls_ltap-vlenr
                routeto = ls_ltap-routeto.
  IF sy-subrc = 0.
    IF <ls_tap>-pick_comp_dt > is_ltap-pick_comp_dt AND
       is_ltap-pick_comp_dt IS NOT INITIAL.
      <ls_tap>-pick_comp_dt = is_ltap-pick_comp_dt.
    ENDIF.
    IF <ls_tap>-gi_tst > is_ltap-gi_tst AND
       is_ltap-gi_tst IS NOT INITIAL.
      <ls_tap>-gi_tst = is_ltap-gi_tst.
    ENDIF.

    IF <ls_tap>-tostat = wmegc_to_confirmed OR
       <ls_tap>-pquit  = 'X'.
      " existing LOSC WT is confirmed immediately => activate current WT
      lv_activate = 'X'.
    ELSE.
      "for distribution equipment delivery it could be that the WT should have been added to an existing WO w/o LOSC
      "if CRETO is set accordingly, remove the flags for the inactive WT
      PERFORM ddul_losc_adapt_inactive_wt USING is_ltap.
      RETURN.
    ENDIF.
  ENDIF.

  TRY.
      IF lv_activate IS INITIAL.
        " check if the inactive WT is a follow up WT from a Resource
        IF is_ltap-wtcode = wmegc_wtcode_rsrc.
          READ TABLE tap ASSIGNING <ls_tap>
               WITH KEY guid_to = is_ltap-guid_to.
          IF sy-subrc = 0.
            " clear given WHO and WTCODE
            ls_ltap-who = <ls_tap>-who.
            ls_ltap-wtcode = <ls_tap>-wtcode.
            CLEAR <ls_tap>-who.
            CLEAR <ls_tap>-wtcode.
          ENDIF.
        ELSEIF is_ltap-wtcode = wmegc_wtcode_coll_hu.
          ls_ltap-wtcode = is_ltap-wtcode.
        ENDIF.

        IF is_ltap-imfs_wtcode IS NOT INITIAL.
          " MFS
          ls_ltap-wtcode     = is_ltap-imfs_wtcode.
          ls_ltap-mfs_cs     = is_ltap-imfs_cs.
          ls_ltap-mfs_nocapa = is_ltap-imfs_nocapa.
        ENDIF.

        IF is_ltap-log_guid_hu IS NOT INITIAL. "Collective/empty HU scenario
          ls_ltap-log_guid_hu = is_ltap-log_guid_hu.
        ENDIF.

        ls_ltap-vanzl     = 1.

        ls_ltap-sloc_type = is_ltap-sloc_type.
        ls_ltap-srsrc     = is_ltap-srsrc.
        ls_ltap-stu_num   = is_ltap-stu_num.
        ls_ltap-vltyp     = is_ltap-vltyp.
        ls_ltap-vlpla     = is_ltap-vlpla.

        ls_ltap-nlenr     = ls_ltap-vlenr.
        ls_ltap-dloc_type = wmegc_bin.
        ls_ltap-nltyp     = is_ltap-iltyp.
        ls_ltap-nlber     = is_ltap-ilber.
        ls_ltap-nlpla     = is_ltap-ilpla.
        ls_ltap-dguid_hu  = ls_ltap-sguid_hu.
        ls_ltap-seqno     = is_ltap-seqno.

        ls_ltap-pick_comp_dt  = is_ltap-pick_comp_dt.
        ls_ltap-gi_tst        = is_ltap-gi_tst.
        ls_ltap-jit_relevance = is_ltap-jit_relevance.
        ls_ltap-mes_ind       = is_ltap-mes_ind.

        IF is_ltap-kanban IS NOT INITIAL AND is_ltap-rdocid IS INITIAL.
          " Kanban with Warehouse Tasks - take over Kanban ID
          ls_ltap-kanban = is_ltap-kanban.
        ENDIF.

        " Correction if consolidation group is set in huhdr copy to ltap.
        IF ls_ltap-dstgrp IS INITIAL.
          ls_ltap-dstgrp = ls_huhdr_int-dstgrp.
        ENDIF.

        ls_ltap-gr_open   = is_ltap-gr_open.

        CALL FUNCTION '/SCWM/T333_READ_SINGLE'
          EXPORTING
            iv_lgnum  = ls_ltap-lgnum
            iv_procty = ls_ltap-procty
          IMPORTING
            es_t333   = ls_t333
          EXCEPTIONS
            not_found = 1.

        IF sy-subrc <> 0.
          MESSAGE e001(/scwm/l3) WITH ls_ltap-procty INTO gv_msgtext.
          CALL METHOD go_prot->add_message( ip_field = 'PROCTY' ).
          RAISE EXCEPTION TYPE /scwm/cx_core.
        ENDIF.
        IF ls_t333-vquit IS NOT INITIAL.
          ls_rl03a-squit = 'X'.
        ENDIF.
        " WT need PRCES, PROCS because it is for this step
        " For layout storage control do not take over step,
        " there might be a different one assigned to the activity
        IF is_ltap-prces IS NOT INITIAL.
          ls_ltap-prces     = is_ltap-prces.
          ls_ltap-procs     = is_ltap-procs.

          "for distribution equipment delivery it could be that the WT has to be added to an existing WO even for LOSC
          "if CRETO is set accordingly, take over the WO (and CRETO if it is the first WT)
          IF is_ltap-creto  = wmegc_creto_dd_load AND
             is_ltap-tostat = wmegc_to_inactiv    AND
             ls_rl03a-squit IS INITIAL.
            ls_ltap-creto = is_ltap-creto.
            ls_ltap-who   = is_ltap-who.
            IF ls_ltap-whoseq IS INITIAL AND ls_ltap-who IS NOT INITIAL.
              ls_ltap-whoseq = /scwm/cl_dd_deliver=>get_instance( )->get_whoseq(
                                                                    iv_procty  = ls_ltap-procty
                                                                    iv_who     = ls_ltap-who
                                                                    iv_nobuf   = abap_true
                                                                    is_hu_dest = VALUE #( guid_hu  = ls_ltap-sguid_hu
                                                                                          tt_nlpla = VALUE #( ( ls_ltap-vlpla ) ) ) ).
            ENDIF.
            "remove WO assignment for inactive WT
            PERFORM ddul_losc_adapt_inactive_wt USING is_ltap.
          ENDIF.
        ENDIF.
        ls_ltap-wcr      = ls_t333-wcr.
        ls_ltap-priority = ls_t333-priority.
        IF ls_t333-trart = wmegc_trart_tr.
          ls_ltap-trart = wmegc_trart_int.
        ELSE.
          ls_ltap-trart = ls_t333-trart.
        ENDIF.
        ls_ltap-act_type = ls_t333-act_type.

        CALL FUNCTION '/SCWM/L_TO_PREPARE_ITEM_INT'
          EXPORTING
            is_t333  = ls_t333
            is_t340d = gs_t340d
            iv_row   = gv_prot_row
          CHANGING
            cs_ltap  = ls_ltap
            cs_rl03a = ls_rl03a.

        CALL FUNCTION '/SCWM/L_TO_ADD_ITEM_INT'
          EXPORTING
            is_rl03a   = ls_rl03a
          IMPORTING
            et_ltap_vb = et_ltap_vb
          CHANGING
            cs_ltap    = ls_ltap.

        CALL FUNCTION '/SCWM/ROUTING_L_TO_MAIN'
          EXPORTING
            iv_action      = wmegc_insert
            iv_guid_hu     = ls_ltap-sguid_hu
            iv_guid_sub_hu = ls_ltap-sguid_hu
            iv_guid_to     = ls_ltap-guid_to
            iv_routeto     = ls_ltap-routeto
            io_prot        = go_prot
            iv_row         = ls_ltap-seqno.
      ENDIF.
      IF ls_ltap-pquit IS NOT INITIAL OR
         lv_activate IS NOT INITIAL.
        IF lv_activate IS NOT INITIAL.
          ls_ltap = <ls_tap>.
        ENDIF.
        " Route TO is confirmed during creation
        " So activate Product TO.
        READ TABLE tap ASSIGNING <ls_tap>
                   WITH KEY guid_to = is_ltap-guid_to.
        <ls_tap>-tostat = wmegc_to_open.

        IF is_ltap-creto = wmegc_creto_dd_load.
          " was cleared before for inactive task, but for immediate confirmation it will activate, so it must be reset to 7
          " so that same unload WHO can be found via processor
          <ls_tap>-creto = is_ltap-creto.
        ENDIF.

        PERFORM routing_chg_src       USING    ls_ltap 0.

        IF <ls_tap>-vltyp IS NOT INITIAL.
          CALL FUNCTION '/SCWM/T331_READ_SINGLE'
            EXPORTING
              iv_lgnum = <ls_tap>-lgnum
              iv_lgtyp = <ls_tap>-vltyp
            IMPORTING
              es_t331  = ls_vltyp
            EXCEPTIONS
              OTHERS   = 2.
          IF sy-subrc <> 0.
            CALL METHOD go_prot->add_message( ip_field = 'VLTYP' ).
            RAISE EXCEPTION TYPE /scwm/cx_core.
          ENDIF.
        ENDIF.

        CALL FUNCTION '/SCWM/TO_ROUTING_ACTIVATE_INT2'
          EXPORTING
            io_prot     = go_prot
            iv_prot_row = <ls_tap>-seqno
            iv_queue    = 'X'
            is_vltyp    = ls_vltyp
          CHANGING
            cs_ordim_o  = <ls_tap>.                                "#EC ENHOK

      ELSEIF is_ltap-creto = wmegc_creto_dd_load.
        " remove WO assignment for inactive WT
        PERFORM ddul_losc_adapt_inactive_wt USING is_ltap.
      ENDIF.

    CATCH /scwm/cx_core.
      " Take back Changes in HU Memory
      CALL FUNCTION '/SCWM/HUMAIN_REFRESH'
        EXPORTING
          iv_restore = 'X'.
      " Take back Changes in storage bin Memory
      CALL FUNCTION '/SCWM/LAGP_ROLLBACK_SINGLE'
        EXPORTING
          iv_mode = wmegc_lagp_rollb.

      " undo first TO
      RAISE EXCEPTION TYPE /scwm/cx_core.

  ENDTRY.

ENDFORM.
