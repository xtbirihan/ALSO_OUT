**********************************************************************
*& Key           : AMOGYORODI-230814
*& Request No.   :
**********************************************************************
*& Description (short)
*&
*&
**********************************************************************
*----------------------------------------------------------------------*
***INCLUDE ZOUT_CONSOLIDATION_CI2.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_report_model
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_model IMPLEMENTATION.
  METHOD get_instance.
    IF so_instance IS INITIAL.
      CREATE OBJECT so_instance.
    ENDIF.
    ro_instance = so_instance.
  ENDMETHOD.

  METHOD set_lgtype.

    mv_lgtyp = iv_lgtyp.

  ENDMETHOD.

***  METHOD set_aarea.
***
***    mv_aarea = iv_aarea.
***
***  ENDMETHOD.

  METHOD set_procs.

    mv_procs = iv_procs.

  ENDMETHOD.

  METHOD get_open_deliveries.

    DATA:
      ls_t300_md TYPE /scwm/s_t300_md.

    CALL FUNCTION '/SCWM/T300_MD_READ_SINGLE'
      EXPORTING
        iv_lgnum   = lcl_controller=>sv_lgnum
      IMPORTING
        es_t300_md = ls_t300_md
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lt_selection) = VALUE /scwm/dlv_selection_tab(
      ( fieldname = /scdl/if_dl_logfname_c=>sc_locationid_wh_h
        sign   = /scmb/cl_search=>sc_sign_i
        option = /scmb/cl_search=>sc_eq
        low    = ls_t300_md-scuguid ) ).

    IF it_docid IS INITIAL.
      APPEND VALUE #( fieldname = /scdl/if_dl_logfname_c=>sc_status_value_dpi_i
        sign      = /scmb/cl_search=>sc_sign_e
        option    = /scmb/cl_search=>sc_eq
        low       = /scdl/if_dl_status_c=>sc_v_finished ) TO lt_selection.
      APPEND VALUE #( fieldname = /scdl/if_dl_logfname_c=>sc_status_value_dpi_i
        sign      = /scmb/cl_search=>sc_sign_e
        option    = /scmb/cl_search=>sc_eq
        low       = /scdl/if_dl_status_c=>sc_v_not_relevant ) TO lt_selection.
      APPEND VALUE #( fieldname = /scdl/if_dl_logfname_c=>sc_status_value_dpi_i
        sign      = /scmb/cl_search=>sc_sign_e
        option    = /scmb/cl_search=>sc_eq
        low       = /scdl/if_dl_status_c=>sc_v_not_started ) TO lt_selection.
    ELSE.
      LOOP AT it_docid INTO DATA(ls_docid).
        APPEND VALUE #( fieldname = /scdl/if_dl_logfname_c=>sc_docid_h
          sign      = /scmb/cl_search=>sc_sign_i
          option    = /scmb/cl_search=>sc_eq
          low       = ls_docid-docid ) TO lt_selection.
      ENDLOOP.
    ENDIF.

    DATA(ls_read_options) = VALUE /scwm/dlv_query_contr_str(
      data_retrival_only = abap_true
      mix_in_object_instances = abap_true
      docflow_succ_1level_only = abap_true
      item_part_select = abap_true ).

    DATA(ls_include_data) = VALUE /scwm/dlv_query_incl_str_prd( head_status     = abap_true
                                                                head_status_dyn = abap_true
                                                                head_partyloc   = abap_true
                                                                head_text       = abap_true
                                                                head_textline   = abap_true
                                                                item_status     = abap_true
                                                                item_text       = abap_true
                                                                item_textline   = abap_true ).
    IF it_docid IS NOT INITIAL.
      ls_include_data-head_status_dyn = abap_true.
    ENDIF.

    " we should do clear in order to refresh the BO Andriyan Yordanov
    CALL FUNCTION '/SCWM/HUMAIN_REFRESH'.
    /scwm/cl_tm=>cleanup( ).
    " end Andriyan Yordanov

    TRY.
        /scwm/cl_dlv_management_prd=>get_instance( )->query(
          EXPORTING
            it_selection    = lt_selection
            is_read_options = ls_read_options
            is_include_data = ls_include_data
          IMPORTING
             et_headers	    = DATA(lt_dlv_headers) "mt_dlv_headers
             et_items       = DATA(lt_dlv_items) ). "mt_dlv_items ).

        zcl_param=>get_parameter(
          EXPORTING
            iv_lgnum     = lcl_controller=>sv_lgnum
            iv_process   = zif_param_const=>c_zcons_0001
            iv_parameter = zif_param_const=>c_cons_procty
          IMPORTING
            et_range     = DATA(lt_procty_r) ).

        LOOP AT lt_dlv_headers INTO DATA(ls_dlv_header).
          LOOP AT lt_dlv_items TRANSPORTING NO FIELDS
            WHERE docid = ls_dlv_header-docid AND
                  sapext-/scwm/procty IN lt_procty_r.
            EXIT.
          ENDLOOP.
          IF sy-subrc = 0.
            APPEND ls_dlv_header TO mt_dlv_headers.

            LOOP AT lt_dlv_items INTO DATA(ls_dlv_item)
              WHERE docid = ls_dlv_header-docid.
              APPEND ls_dlv_item TO mt_dlv_items.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
      CATCH /scdl/cx_delivery ##NO_HANDLER.
    ENDTRY.

    build_cons_dlv_table( ).

  ENDMETHOD.

  METHOD build_cons_dlv_table.

    DATA:
      lv_customer      TYPE bu_partner,
      lv_customer_desc TYPE bu_descrip,
      lv_carrier_desc  TYPE bu_descrip,
      lv_num_hus       TYPE int2,
      lv_hus_ready     TYPE int2,
      lv_num_pallet    TYPE int2,
      lv_num_parcel    TYPE int2,
      lt_lgpla         TYPE /scwm/tt_lgpla,
      lt_huhdr         TYPE /scwm/tt_huhdr_int,
      lt_wavehdr       TYPE /scwm/tt_wavehdr_int,
      lt_waveitm       TYPE /scwm/tt_waveitm_int,
      lr_guid_hu       TYPE RANGE OF /scwm/guid_hu.

    CLEAR mt_cons_headers.

    IF mt_dlv_headers IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCWM/WAVE_CLEANUP'
      EXPORTING
        iv_lgnum   = lcl_controller=>sv_lgnum
        iv_rdoccat = wmegc_doccat_pdo.

    DATA(lt_docid) = VALUE rseloption( FOR header IN mt_dlv_headers ( sign = /scmb/cl_search=>sc_sign_i option = /scmb/cl_search=>sc_eq low = header-docid ) ).

    CALL FUNCTION '/SCWM/WAVE_SELECT'
      EXPORTING
        ir_rdocid  = lt_docid
      IMPORTING
        et_wavehdr = lt_wavehdr
        et_waveitm = lt_waveitm.

    LOOP AT mt_dlv_headers INTO DATA(ls_header).
      CLEAR lt_lgpla.
      get_party_role(
        EXPORTING
          is_dlv_header = ls_header
          iv_party_role = /scdl/if_dl_partyloc_c=>sc_party_role_stprt
        IMPORTING
          ev_party_no   = lv_customer
          ev_party_text = lv_customer_desc ).

      get_party_role(
        EXPORTING
          is_dlv_header = ls_header
          iv_party_role = /scdl/if_dl_partyloc_c=>sc_party_role_carr
        IMPORTING
          ev_party_text = lv_carrier_desc ).

      CLEAR: lv_num_hus, lv_hus_ready, lv_num_pallet, lv_num_parcel.

      DATA(lo_hu) = NEW /scwm/cl_dlv_prd2hum( ).
      CLEAR lt_huhdr.
      TRY.
          lo_hu->get_hu_for_prd_fd(
            EXPORTING iv_docid  = ls_header-docid
                      iv_doccat = wmegc_doccat_pdo
            IMPORTING et_huhdr  = lt_huhdr ).
        CATCH /scdl/cx_delivery.
          CONTINUE.
      ENDTRY.

      LOOP AT lt_huhdr INTO DATA(ls_huhdr) WHERE lgtyp = mv_lgtyp.

        READ TABLE lt_huhdr TRANSPORTING NO FIELDS
          WITH KEY higher_guid = ls_huhdr-guid_hu.
        IF sy-subrc <> 0.
          "Don't show empty HUs
          READ TABLE mt_cons_huitm TRANSPORTING NO FIELDS
            WITH KEY guid_parent = ls_huhdr-guid_hu.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
        ENDIF.

        lv_num_hus = lv_num_hus + 1.

        READ TABLE lt_lgpla TRANSPORTING NO FIELDS
          WITH KEY table_line = ls_huhdr-lgpla.
        IF sy-subrc <> 0.
          APPEND ls_huhdr-lgpla TO lt_lgpla.
        ENDIF.

        IF is_hu_ready( iv_huident = ls_huhdr-huident ) = abap_true.
          lv_hus_ready = lv_hus_ready + 1.
        ENDIF.

        IF is_pallet( is_huhdr = ls_huhdr ).
          lv_num_pallet = lv_num_pallet + 1.
        ELSE.
          lv_num_parcel = lv_num_parcel + 1.
        ENDIF.
      ENDLOOP.

      READ TABLE lt_waveitm INTO DATA(ls_waveitm) WITH KEY rdocid = ls_header-docid.
      IF sy-subrc <> 0.
        CLEAR ls_waveitm.
      ENDIF.

      IF ls_waveitm IS NOT INITIAL.
        READ TABLE lt_wavehdr INTO DATA(ls_wavehdr) WITH KEY wave = ls_waveitm-wave.
        IF sy-subrc <> 0.
          CLEAR ls_wavehdr.
        ENDIF.
      ENDIF.

      DATA(lv_cutoff) = CONV string( ls_wavehdr-cutoff_dt ).

      READ TABLE mt_dlv_items INTO DATA(ls_item)
        WITH KEY docid = ls_header-docid.
      IF sy-subrc <> 0.
        CLEAR ls_item.
      ENDIF.

      APPEND VALUE #( docid = ls_header-docid
                      docno = |{ ls_header-docno ALPHA = OUT }|
                      partyno = lv_customer
                      party_descr = lv_customer_desc
                      carrier_descr = lv_carrier_desc
                      cutoff_time = COND #( WHEN strlen( lv_cutoff ) >= 8 THEN lv_cutoff+8 ELSE space )
                      priority = ls_item-delterm-priority "ls_header-sapext-/scwm/priority
                      num_pallets = lv_num_pallet
                      num_parcels = lv_num_parcel
                      status_pick = get_status( is_dlv_header = ls_header iv_status_type = /scdl/if_dl_status_c=>sc_t_picking )
                      status_cons = COND #( WHEN lv_num_hus   = 0 THEN TEXT-008
                                            WHEN lv_hus_ready = 0 THEN TEXT-009
                                            WHEN lv_num_hus > lv_hus_ready THEN TEXT-010
                                            WHEN lv_num_hus > lv_hus_ready THEN TEXT-011
                                            ELSE TEXT-008 )
                      num_cons_lanes = lines( lt_lgpla )
                      cons_lane = COND #( WHEN lines( lt_lgpla ) = 0 THEN space
                                          WHEN lines( lt_lgpla ) = 1 THEN lt_lgpla[ 1 ]
                                          ELSE '*' )
                       ) TO mt_cons_headers.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_party_role.

    CLEAR: ev_party_no,
           ev_party_text.

    IF is_dlv_header IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        ev_party_no = is_dlv_header-partyloc[ party_role = iv_party_role ]-partyno.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    CALL FUNCTION 'BUPA_DESCRIPTION_READ'
      EXPORTING
        iv_partner     = ev_party_no
      IMPORTING
        ev_description = ev_party_text
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      CLEAR ev_party_text.
    ENDIF.

  ENDMETHOD.

  METHOD get_status.
    TRY.
        DATA(lv_status_value) = is_dlv_header-status[ status_type = iv_status_type ]-status_value.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    CASE lv_status_value.
      WHEN 0.
        rv_status = TEXT-008.
      WHEN 1.
        rv_status = TEXT-009.
      WHEN 2.
        rv_status = TEXT-010.
      WHEN 9.
        rv_status = TEXT-011.
    ENDCASE.
  ENDMETHOD.

  METHOD get_hus_on_cons_bin.

    DATA:
      lt_lgtyp   TYPE /scwm/tt_lgtyp_r,
      lt_lgpla   TYPE /scwm/tt_lgpla_r,
      lt_huident TYPE /scwm/tt_huident.

    CLEAR: mt_cons_huhdr, mt_cons_huitm.

    IF iv_lgpla IS INITIAL AND mv_lgtyp IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_lgpla IS NOT INITIAL.
      lt_lgpla = VALUE #( ( sign   = /scmb/cl_search=>sc_sign_i
                            option = /scmb/cl_search=>sc_eq
                            low    = iv_lgpla ) ).
    ENDIF.

    IF mv_lgtyp IS NOT INITIAL.
      lt_lgtyp = VALUE #( ( sign   = /scmb/cl_search=>sc_sign_i
                            option = /scmb/cl_search=>sc_eq
                            low    = mv_lgtyp ) ).
    ENDIF.

    NEW /scwm/cl_mon_stock( iv_lgnum = lcl_controller=>sv_lgnum )->get_hu(
      EXPORTING
        iv_skip_bin      = abap_false
        iv_skip_resource = abap_true
        iv_skip_tu       = abap_true
        it_lgpla_r       = lt_lgpla
        it_lgtyp_r       = lt_lgtyp
      IMPORTING
        et_hu_mon        = DATA(lt_hus_on_bin)
        ev_error         = DATA(lv_error) ).
    IF lv_error = abap_true.
      RETURN.
    ENDIF.

    LOOP AT lt_hus_on_bin INTO DATA(ls_hu_on_bin).
      APPEND VALUE #( lgnum   = ls_hu_on_bin-lgnum
                      huident = ls_hu_on_bin-huident
                      vhi     = ls_hu_on_bin-vhi ) TO lt_huident.
    ENDLOOP.

    CALL FUNCTION '/SCWM/HUMAIN_REFRESH'.

    CALL FUNCTION '/SCWM/HU_READ_MULT'
      EXPORTING
        it_huident = lt_huident
        iv_lgnum   = lcl_controller=>sv_lgnum
        iv_top     = abap_true
      IMPORTING
        et_huhdr   = mt_cons_huhdr
        et_huitm   = mt_cons_huitm
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT mt_cons_huitm INTO DATA(ls_huitem)
      WHERE qdocid IS NOT INITIAL.
      APPEND VALUE #( docid = ls_huitem-qdocid ) TO et_docid.
    ENDLOOP.

  ENDMETHOD.

  METHOD build_cons_hu_table.

    DATA:
      lv_stat_slt_stock      TYPE zde_serial_status,
      lv_stat_slt_stock_curr TYPE zde_serial_status,
      lv_counter_mc          TYPE /scwm/dl_counter,
      lv_hutype              TYPE string,
      lv_quana_mc_pall       TYPE /scwm/de_quantity,
      lv_quana_mc_pall_empt  TYPE /scwm/de_quantity,
      ls_mc_cons_hu          TYPE zstr_cons_hu_header,
      lt_cons_hus            TYPE ztt_cons_hu_header.

    FIELD-SYMBOLS:
      <ls_cons_hu> TYPE zstr_cons_hu_header.

    CLEAR mt_cons_hus.

    IF iv_docid IS INITIAL.
      RETURN.
    ENDIF.

    IF ms_cons_header IS INITIAL OR
      ms_cons_header-docid <> iv_docid.
      READ TABLE mt_cons_headers INTO ms_cons_header
        WITH KEY docid = iv_docid.
    ENDIF.

    CALL FUNCTION '/SCWM/HUMAIN_REFRESH'.
    /scwm/cl_tm=>cleanup( ).

    DATA(lo_hu) = NEW /scwm/cl_dlv_prd2hum( ).

    TRY.
        lo_hu->get_hu_for_prd_fd(
          EXPORTING iv_docid  = iv_docid
                    iv_doccat = wmegc_doccat_pdo
          IMPORTING et_huhdr  = DATA(lt_huhdr)
                    et_huitm  = DATA(lt_huitm)
                    et_huref  = DATA(lt_huref)
                    et_high   = DATA(lt_high) ).
      CATCH /scdl/cx_delivery.
        RETURN.
    ENDTRY.

    get_serials( iv_docid ).

    " Andriyan Yordanov
    " refresh the stock for the relevant DLV
    LOOP AT lt_huhdr ASSIGNING FIELD-SYMBOL(<ls_cons_huitm_upd>).
      DELETE mt_cons_huhdr WHERE guid_hu = <ls_cons_huitm_upd>-guid_hu.
      DELETE mt_cons_huitm WHERE guid_parent = <ls_cons_huitm_upd>-guid_hu.
    ENDLOOP.

    APPEND LINES OF lt_huhdr TO mt_cons_huhdr.
    APPEND LINES OF lt_huitm TO mt_cons_huitm.
    " end Andriyan Yordanov

    zcl_param=>get_parameter(
     EXPORTING
       iv_lgnum     = lcl_controller=>sv_lgnum
       iv_process   = zif_param_const=>c_zrfui_0006
       iv_parameter = zif_param_const=>c_party_role_cust_print
     IMPORTING
       et_list      = DATA(lt_cust_value_partyrole) ).

    LOOP AT lt_huhdr INTO DATA(ls_huhdr) WHERE lgtyp = mv_lgtyp.

      IF <ls_cons_hu> IS ASSIGNED.
        UNASSIGN <ls_cons_hu>.
      ENDIF.

      IF is_hu_ready( iv_huident = ls_huhdr-huident ) = abap_true AND lcl_controller=>sv_show_compl = abap_false.
        CONTINUE.
      ENDIF.

      IF ls_huhdr-bottom = abap_true AND ls_huhdr-top = abap_false.
        "Don't show empty HUs
        READ TABLE mt_cons_huitm TRANSPORTING NO FIELDS
          WITH KEY guid_parent = ls_huhdr-guid_hu.

        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        READ TABLE lt_huhdr INTO DATA(ls_huhdr_top) WITH KEY guid_hu = ls_huhdr-higher_guid.
        IF sy-subrc = 0.
          IF is_pallet( is_huhdr = ls_huhdr_top ).
            CLEAR lv_hutype.
            lv_hutype = get_hutype( is_huhdr = ls_huhdr ).

            READ TABLE lt_cons_hus ASSIGNING <ls_cons_hu>
              WITH KEY tophu  = ls_huhdr_top-huident
                       hutype = lv_hutype.
            IF sy-subrc <> 0.
              APPEND INITIAL LINE TO lt_cons_hus ASSIGNING <ls_cons_hu>.

              IF lv_hutype = TEXT-017.

                " Аndriyan Yordanov Amazon role
                ASSIGN mt_dlv_headers[ docid = iv_docid ] TO FIELD-SYMBOL(<ls_dlv_header>).

                IF sy-subrc = 0.
                  get_party_role(
                    EXPORTING
                      is_dlv_header = <ls_dlv_header>
                      iv_party_role = c_cust_partno_spr
                    IMPORTING
                      ev_party_no   = DATA(lv_partyno_spr) ).

                  IF NOT line_exists( lt_cust_value_partyrole[ table_line = lv_partyno_spr ] ).
                    CLEAR lv_partyno_spr.
                  ENDIF.
                ENDIF.

                <ls_cons_hu>-huident =  COND #( WHEN lv_partyno_spr IS NOT INITIAL
                                                   THEN   ls_huhdr-huident
                                                   ELSE '-' ).
                " end Andriyan Yordanov
              ELSE.
                <ls_cons_hu>-huident = ls_huhdr-huident.
              ENDIF.
              <ls_cons_hu>-num_shmc = <ls_cons_hu>-num_shmc + 1.
            ENDIF.
            <ls_cons_hu>-tophu = ls_huhdr_top-huident.
          ENDIF.
        ENDIF.
      ENDIF.

      IF <ls_cons_hu> IS NOT ASSIGNED.
        APPEND INITIAL LINE TO lt_cons_hus ASSIGNING <ls_cons_hu>.

        CLEAR lv_hutype.
        lv_hutype = get_hutype( is_huhdr = ls_huhdr ).
        IF lv_hutype = TEXT-017.
          " Аndriyan Yordanov Amazon role
          ASSIGN mt_dlv_headers[ docid = iv_docid ] TO <ls_dlv_header>.

          IF sy-subrc = 0.
            get_party_role(
              EXPORTING
                is_dlv_header = <ls_dlv_header>
                iv_party_role = c_cust_partno_spr
              IMPORTING
                ev_party_no   = lv_partyno_spr ).

            IF NOT line_exists( lt_cust_value_partyrole[ table_line = lv_partyno_spr ] ).
              CLEAR lv_partyno_spr.
            ENDIF.

          ENDIF.

          <ls_cons_hu>-huident =  COND #( WHEN lv_partyno_spr IS NOT INITIAL
                                             THEN   ls_huhdr-huident
                                             ELSE '-' ).

***          <ls_cons_hu>-huident = '-'.
          " end Andriyan Yordanov
        ELSE.
          <ls_cons_hu>-huident = ls_huhdr-huident.
        ENDIF.

        CLEAR <ls_cons_hu>-num_shmc.
      ENDIF.

      <ls_cons_hu>-hutype = ls_huhdr-letyp.
      <ls_cons_hu>-lgpla = ls_huhdr-lgpla.
      <ls_cons_hu>-serial_status = get_serial_status( iv_huident = ls_huhdr-huident ).

      <ls_cons_hu>-hutype_desc = get_hutype( ls_huhdr ).

      IF is_hu_ready( iv_huident = ls_huhdr-huident ) = abap_true OR is_hu_ready( iv_huident = ls_huhdr_top-huident ) = abap_true.
        <ls_cons_hu>-cons_compl = TEXT-018.
      ELSE.
        <ls_cons_hu>-cons_compl = TEXT-019.
      ENDIF.
    ENDLOOP.

***    DATA(lt_cons_hu_tmp) = lt_cons_hus.

    LOOP AT lt_cons_hus ASSIGNING  <ls_cons_hu> WHERE hutype_desc = TEXT-017
                                            GROUP BY ( tophu =  <ls_cons_hu>-tophu )
                                          ASSIGNING FIELD-SYMBOL(<ls_grp_mc_dlv>).
      CLEAR ls_mc_cons_hu.

      LOOP AT GROUP <ls_grp_mc_dlv> ASSIGNING FIELD-SYMBOL(<ls_grp_mc_view>).

        IF <ls_grp_mc_view>-huident = '-'.

          ls_mc_cons_hu = <ls_grp_mc_view>.
          ls_mc_cons_hu-num_shmc += 1.

          ls_mc_cons_hu-serial_status = COND #( WHEN ls_mc_cons_hu-serial_status = space AND
                                                     <ls_grp_mc_view>-serial_status <> space
                                                  THEN <ls_grp_mc_view>-serial_status
                                                WHEN ls_mc_cons_hu-serial_status <> space AND
                                                     <ls_grp_mc_view>-serial_status = space
                                                  THEN ls_mc_cons_hu-serial_status
                                                WHEN  ls_mc_cons_hu-serial_status = icon_led_red AND
                                                   <ls_grp_mc_view>-serial_status <> icon_led_red
                                                   THEN icon_led_yellow
                                                WHEN ls_mc_cons_hu-serial_status = icon_led_green AND
                                                   <ls_grp_mc_view>-serial_status <> icon_led_green
                                                   THEN icon_led_yellow
                                                 ELSE ls_mc_cons_hu-serial_status ).

        ELSE.
          " this is amazon
          APPEND <ls_grp_mc_view> TO mt_cons_hus.
        ENDIF.

      ENDLOOP.

      CHECK ls_mc_cons_hu IS NOT INITIAL.

      CLEAR lv_quana_mc_pall.
      lv_quana_mc_pall =  REDUCE /scwm/de_quantity( INIT lv_quana = lv_quana_mc_pall FOR <ls_stock_pall> IN lt_huitm
                                                                             WHERE ( guid_parent = lt_huhdr[ huident = <ls_grp_mc_dlv>-tophu ]-guid_hu )
                                                                                     NEXT lv_quana += <ls_stock_pall>-quana ).

      IF lv_quana_mc_pall > 0.

        DATA(lo_model) = lcl_model=>get_instance( ).
        ls_mc_cons_hu-num_shmc = ls_mc_cons_hu-num_shmc + lv_quana_mc_pall.

        CLEAR: lv_counter_mc,
               lv_stat_slt_stock,
               lv_stat_slt_stock_curr.

        DO lv_quana_mc_pall TIMES.
          lv_counter_mc += 1.
          lv_stat_slt_stock = lo_model->get_serial_status( iv_huident        = <ls_grp_mc_dlv>-tophu
                                                           iv_pall_group_mc  = lv_counter_mc
                                                           iv_sn_screen_sub  = abap_true ).

          IF lv_stat_slt_stock_curr IS INITIAL.
            lv_stat_slt_stock_curr = lv_stat_slt_stock.
          ELSEIF lv_stat_slt_stock_curr = icon_led_red AND
                lv_stat_slt_stock <> icon_led_red.
            lv_stat_slt_stock_curr = icon_led_yellow.
          ELSEIF lv_stat_slt_stock_curr = icon_led_green AND
                 lv_stat_slt_stock <> icon_led_green.
            lv_stat_slt_stock_curr = icon_led_yellow.
          ENDIF.
        ENDDO.

        " calc. status on the whole record of the pall. stock which is on pall + stock which is in the MC
        IF ls_mc_cons_hu-serial_status IS INITIAL AND
           lv_stat_slt_stock_curr IS NOT INITIAL.
          ls_mc_cons_hu-serial_status = lv_stat_slt_stock_curr.
        ELSEIF ls_mc_cons_hu-serial_status = icon_led_red AND
               lv_stat_slt_stock_curr <> space AND
               lv_stat_slt_stock_curr <> icon_led_red.
          ls_mc_cons_hu-serial_status =  icon_led_yellow.
        ELSEIF ls_mc_cons_hu-serial_status = icon_led_green AND
               lv_stat_slt_stock_curr <> space AND
               lv_stat_slt_stock_curr <> icon_led_green.
          ls_mc_cons_hu-serial_status =  icon_led_yellow.
        ENDIF.
      ENDIF.

      APPEND ls_mc_cons_hu TO mt_cons_hus.

    ENDLOOP.

    LOOP AT lt_cons_hus ASSIGNING FIELD-SYMBOL(<ls_cons_ship_cart_hu>) WHERE hutype_desc = TEXT-016.

      CLEAR <ls_cons_ship_cart_hu>-num_shmc.
      APPEND <ls_cons_ship_cart_hu> TO mt_cons_hus.
    ENDLOOP.

    LOOP AT lt_cons_hus ASSIGNING FIELD-SYMBOL(<ls_cons_pall_hu>) WHERE hutype_desc = TEXT-015.

      <ls_cons_pall_hu>-num_shmc = REDUCE #( INIT lv_mc_num = 0 FOR <ls_mc_per_pall> IN mt_cons_hus
                                                                     WHERE ( hutype_desc = TEXT-017 AND
                                                                             tophu       = <ls_cons_pall_hu>-huident )
                                                                             NEXT lv_mc_num += <ls_mc_per_pall>-num_shmc ).

      IF line_exists( mt_cons_hus[ tophu = <ls_cons_pall_hu>-tophu ]  ).
        " add Shiping HU here
        <ls_cons_pall_hu>-num_shmc =  <ls_cons_pall_hu>-num_shmc  +  REDUCE #( INIT lv_mc_num = 0 FOR <ls_mc_per_pall> IN mt_cons_hus
                                                               WHERE ( hutype_desc = TEXT-016 AND
                                                                       tophu       = <ls_cons_pall_hu>-huident )
                                                                       NEXT lv_mc_num += 1 ).
      ENDIF.

      IF <ls_cons_pall_hu>-num_shmc = 0.
        CLEAR lv_quana_mc_pall_empt.
        " we have empty pall up to now .
        <ls_cons_pall_hu>-num_shmc = <ls_cons_pall_hu>-num_shmc + REDUCE /scwm/de_quantity( INIT lv_quana = lv_quana_mc_pall_empt FOR <ls_stock_pall> IN lt_huitm
                                                                             WHERE ( guid_parent = lt_huhdr[ huident = <ls_cons_pall_hu>-huident ]-guid_hu )
                                                                                     NEXT lv_quana += <ls_stock_pall>-quana ).
      ENDIF.

      CHECK <ls_cons_pall_hu>-num_shmc > 0.
      APPEND <ls_cons_pall_hu> TO mt_cons_hus.

    ENDLOOP.

    DATA(lt_sort_cons_hu) = mt_cons_hus.
    CLEAR  mt_cons_hus.

    LOOP AT lt_sort_cons_hu ASSIGNING FIELD-SYMBOL(<ls_sort_hus>) WHERE tophu IS INITIAL.

      APPEND <ls_sort_hus> TO mt_cons_hus.

      LOOP AT lt_sort_cons_hu ASSIGNING FIELD-SYMBOL(<ls_subhu>) WHERE tophu = <ls_sort_hus>-huident .
        APPEND <ls_subhu> TO mt_cons_hus.
        DELETE lt_sort_cons_hu.
      ENDLOOP.

    ENDLOOP.

***
***
***    LOOP AT  lt_cons_hus ASSIGNING <ls_cons_hu>
***      WHERE tophu IS NOT INITIAL.
***
***      READ TABLE mt_cons_hus TRANSPORTING NO FIELDS
***        WITH KEY huident = <ls_cons_hu>-tophu.
***      IF sy-subrc <> 0.
***        "Add entry for topHU
***        READ TABLE lt_cons_hus INTO DATA(ls_cons_hu_top)
***          WITH KEY huident = <ls_cons_hu>-tophu.
***        IF sy-subrc = 0.
***          APPEND ls_cons_hu_top TO mt_cons_hus.
***          DELETE lt_cons_hu_tmp WHERE huident =  <ls_cons_hu>-tophu.
***        ENDIF.
***      ENDIF.
***
***      IF <ls_cons_hu>-hutype_desc = TEXT-017.
***
***        READ TABLE mt_cons_hus ASSIGNING FIELD-SYMBOL(<ls_cons_hu_mc>)
***          WITH KEY tophu = <ls_cons_hu>-tophu.
***
***        IF sy-subrc = 0 AND
***           <ls_cons_hu_mc>-huident = '-'.
***
***          <ls_cons_hu_mc>-num_shmc = <ls_cons_hu_mc>-num_shmc + 1.
***          CONTINUE.
***
***        ELSE.
***          APPEND <ls_cons_hu> TO mt_cons_hus.
***        ENDIF.
***      ENDIF.
***
***      DELETE lt_cons_hu_tmp WHERE huident = <ls_cons_hu>-huident AND tophu = <ls_cons_hu>-tophu.
***    ENDLOOP.
***
***    lt_cons_hus = lt_cons_hu_tmp.
***
***    LOOP AT lt_cons_hus ASSIGNING <ls_cons_hu>
***      WHERE tophu IS INITIAL.
***
***      IF <ls_cons_hu>-hutype_desc = TEXT-017.
***        LOOP AT mt_cons_hus ASSIGNING <ls_cons_hu_mc>
***          WHERE huident = '-' AND tophu IS INITIAL.
***          EXIT.
***        ENDLOOP.
***        IF sy-subrc = 0.
***          <ls_cons_hu_mc>-num_shmc = <ls_cons_hu_mc>-num_shmc + 1.
***          CONTINUE.
***        ENDIF.
***      ENDIF.
***
***      <ls_cons_hu>-num_shmc = 1.
***      APPEND <ls_cons_hu> TO mt_cons_hus.
***    ENDLOOP.

  ENDMETHOD.

  METHOD get_serials.

    IF ms_cons_header-docid <> iv_docid.
      READ TABLE mt_cons_headers INTO ms_cons_header
        WITH KEY docid = iv_docid.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

    CLEAR mt_serials.
    " Changed by Andriyan Yordanov 07.02.2024 - use of general crud object and alpha convert
    mt_serials = zcl_crud_ztcross_cap_nums=>select_multi_by_docno(
                         iv_lgnum = lcl_controller=>sv_lgnum
                         iv_docno = |{ ms_cons_header-docno ALPHA = IN }| ).
    " end of chnage

  ENDMETHOD.

  METHOD is_pallet.

    DATA:
       lv_hutypgrp TYPE /scwm/de_hutypgrp.

    IF is_huhdr-hutypgrp IS NOT INITIAL.
      lv_hutypgrp = is_huhdr-hutypgrp.
    ELSE.
      lv_hutypgrp = iv_hutypgrp.
    ENDIF.

    " Andriyan Yordanov
    IF zcl_crud_ztcross_cart_type=>select_by_hutype_group(
       EXPORTING
         iv_lgnum            = lcl_controller=>sv_lgnum                 " Warehouse Number/Warehouse Complex
         iv_hutypgrp         = lv_hutypgrp ) = zcl_crud_ztcross_cart_type=>c_pall_type.   " Handling Unit Type Group
      rv_pallet = abap_true.
    ELSE.
      rv_pallet = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD is_master_carton.

    DATA:
       lv_hutypgrp TYPE /scwm/de_hutypgrp.

    IF is_huhdr-hutypgrp IS NOT INITIAL.
      lv_hutypgrp = is_huhdr-hutypgrp.
    ELSE.
      lv_hutypgrp = iv_hutypgrp.
    ENDIF.

    IF zcl_crud_ztcross_cart_type=>select_by_hutype_group(
       EXPORTING
         iv_lgnum            = lcl_controller=>sv_lgnum                 " Warehouse Number/Warehouse Complex
         iv_hutypgrp         = lv_hutypgrp ) = zcl_crud_ztcross_cart_type=>c_master_carton_type.   " Handling Unit Type Group
      rv_mc = abap_true.
    ELSE.
      rv_mc = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD is_shipping_carton.

    DATA:
       lv_hutypgrp TYPE /scwm/de_hutypgrp.

    IF is_huhdr-hutypgrp IS NOT INITIAL.
      lv_hutypgrp = is_huhdr-hutypgrp.
    ELSE.
      lv_hutypgrp = iv_hutypgrp.
    ENDIF.

    IF zcl_crud_ztcross_cart_type=>select_by_hutype_group(
      EXPORTING
        iv_lgnum            = lcl_controller=>sv_lgnum                 " Warehouse Number/Warehouse Complex
        iv_hutypgrp         = lv_hutypgrp ) = zcl_crud_ztcross_cart_type=>c_shipping_carton_type.   " Handling Unit Type Group
      rv_sc = abap_true.
    ELSE.
      rv_sc = abap_false.
    ENDIF.
    " End Andriyan Yordanov
  ENDMETHOD.

  METHOD get_hutype_for_pmat.

    DATA:
      ls_mat_pack TYPE /scwm/s_material_pack,
      ls_t307     TYPE /scwm/t307,
      ls_huhdr    TYPE /scwm/s_huhdr_int.

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid    = iv_pmatid
*           IV_ENTITLED =
            iv_lgnum    = lcl_controller=>sv_lgnum
          IMPORTING
            es_mat_pack = ls_mat_pack.
        IF ls_mat_pack IS INITIAL.
          RETURN.
        ENDIF.

        ev_hutype = ls_mat_pack-hutyp.

        CALL FUNCTION '/SCWM/T307_READ_SINGLE'
          EXPORTING
            iv_lgnum    = lcl_controller=>sv_lgnum
            iv_letyp    = ls_mat_pack-hutyp
          IMPORTING
            es_t307     = ls_t307
          EXCEPTIONS
            not_found   = 1
            wrong_input = 2
            OTHERS      = 3.
        IF sy-subrc <> 0.
          CLEAR ev_descr.
        ELSE.
          ls_huhdr-hutypgrp = ls_t307-hutypgrp.
          IF is_pallet( is_huhdr = ls_huhdr ).
            ev_descr = TEXT-015.
          ELSEIF is_shipping_carton( is_huhdr = ls_huhdr ).
            ev_descr = TEXT-016.
          ELSEIF is_master_carton( is_huhdr = ls_huhdr ).
            ev_descr = TEXT-017.
          ELSE.
            CLEAR ev_descr.
          ENDIF.
        ENDIF.
      CATCH /scwm/cx_md.
        CLEAR: ev_hutype, ev_descr.
    ENDTRY.
  ENDMETHOD.

  METHOD get_hutype.
    IF is_pallet( is_huhdr = is_huhdr ).
      rv_hutype = TEXT-015.
    ELSEIF is_shipping_carton( is_huhdr = is_huhdr ).
      rv_hutype = TEXT-016.
    ELSEIF is_master_carton( is_huhdr = is_huhdr ).
      rv_hutype = TEXT-017.
    ELSE.
      CLEAR rv_hutype.
    ENDIF.
  ENDMETHOD.

  METHOD init_diffhu_tables.

    mt_cons_huitm_exc = mt_cons_huitm.
    build_cons_huitem_table( EXPORTING iv_huident = iv_huident
                                       it_huitem = mt_cons_huitm
                             CHANGING  ct_hu_content = mt_cons_hu_content ).

    mt_diff_hu_src = mt_cons_hu_content.
    CLEAR: mt_scan_huitm, mt_diff_hu_dest.

  ENDMETHOD.

  METHOD get_text_dlv.

    DATA: lt_text      TYPE zif_text_handling=>tt_text.

    ASSIGN mt_dlv_headers[ docid = ms_cons_header-docid ] TO FIELD-SYMBOL(<ls_dlv_header>).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lo_text_handling) = zcl_text_handling=>get_instance_for_warehouse( lcl_controller=>sv_lgnum  ).

    " get DLV _header first
    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = lcl_controller=>sv_lgnum                 " Warehouse Number/Warehouse Complex
        iv_process   = zif_param_const=>c_zout_0009             " Process ID (Specification, Program, BAdI etc.)
        iv_parameter = zif_param_const=>c_text_type_pack_inst   " Parameter ID for process
      IMPORTING
        et_range  = DATA(lt_textid_param_r) ).                 " Parameter-Framework Low

    IF lt_textid_param_r IS NOT INITIAL.
      lt_text = VALUE #( FOR <ls_hdr_text> IN <ls_dlv_header>-text
                                WHERE ( text_type IN lt_textid_param_r )
                                  FOR <ls_hdr_text_line> IN <ls_hdr_text>-text_lines
                                    ( text = <ls_hdr_text_line>-tdline ) ).
    ENDIF.

    ASSIGN <ls_dlv_header>-partyloc[ party_role = c_cust_partno_spr ] TO FIELD-SYMBOL(<ls_party_cust_prof>).

    IF sy-subrc = 0.
      " add Customer Profile
      DATA(lt_section_txt) = lo_text_handling->get_texts_from_bp(
                     iv_business_partner = <ls_party_cust_prof>-partyno
                     it_text_type        = VALUE #( ( zif_wme_c=>gc_text_types-cust_packing ) )
                     iv_no_del_note      = abap_true ).

      IF lines( lt_section_txt ) > 0.
        APPEND VALUE zif_text_handling=>ty_text( text = |{  TEXT-045 }: { <ls_party_cust_prof>-partyno ALPHA = OUT }| ) TO lt_text.
        APPEND LINES OF lt_section_txt  TO lt_text.
      ENDIF.
    ENDIF.

    " add Customer on itel level. We will have just one cust. for all items
    CLEAR lt_section_txt.
    LOOP AT mt_dlv_items ASSIGNING FIELD-SYMBOL(<ls_dlv_item>)
                                         WHERE docid = ms_cons_header-docid.

      ASSIGN <ls_dlv_item>-partyloc[ partyno = /scdl/if_dl_partyloc_c=>sc_party_role_sotprt ] TO FIELD-SYMBOL(<ls_customer>).
      CHECK sy-subrc = 0.

      lt_section_txt = lo_text_handling->get_texts_from_bp(
             iv_business_partner = <ls_customer>-partyno
             it_text_type = VALUE #( ( zif_wme_c=>gc_text_types-cust_packing ) )
             iv_no_del_note = abap_true ).
      EXIT.
    ENDLOOP.

    IF lines( lt_section_txt ) > 0.
      APPEND VALUE zif_text_handling=>ty_text( text = |{ TEXT-046 }: { <ls_customer>-partyno ALPHA = OUT }| ) TO lt_text.
      APPEND LINES OF lt_section_txt  TO lt_text.
    ENDIF.

    IF lines( lt_text ) = 0.
      APPEND VALUE zif_text_handling=>ty_text( text = |{ TEXT-048 }| ) TO lt_text.
    ENDIF.

    rv_text_dlv = lo_text_handling->create_pack_instr_string( lt_text ).

  ENDMETHOD.

  METHOD get_text_hu.

    DATA: lv_huident TYPE /scwm/de_huident,
          lt_text    TYPE zif_text_handling=>tt_text,
          lt_huitm   TYPE /scwm/tt_huitm_int.

    DATA(lo_text_handling) = zcl_text_handling=>get_instance_for_warehouse( lcl_controller=>sv_lgnum  ).

    IF ms_cons_hu-huident IS INITIAL.
      RETURN.
    ELSE.
      lv_huident = |{ ms_cons_hu-huident ALPHA = IN }|.
    ENDIF.

    CALL FUNCTION '/SCWM/HUMAIN_REFRESH'.

    CALL FUNCTION '/SCWM/HU_READ'
      EXPORTING
        iv_huident = lv_huident
        iv_appl    = wmegc_huappl_wme
      IMPORTING
        et_huitm   = lt_huitm
      EXCEPTIONS
        deleted    = 1
        not_found  = 2
        error      = 3
        OTHERS     = 4.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_huitm ASSIGNING FIELD-SYMBOL(<ls_huitem_prd>)
                                       GROUP BY ( entitled = <ls_huitem_prd>-entitled
                                                  matid    = <ls_huitem_prd>-matid ).

      NEW /scwm/cl_ui_stock_fields( )->get_matkey_by_id(
      EXPORTING
        iv_matid = <ls_huitem_prd>-matid
      IMPORTING
        ev_matnr =  DATA(lv_matnr)
        ev_maktx =  DATA(lv_maktx) ).

      DATA(lt_section_txt) = lo_text_handling->get_texts_from_prod( iv_product  = lv_matnr
                                                                    iv_entitled = <ls_huitem_prd>-entitled
                                                                    iv_lgnum    = lcl_controller=>sv_lgnum
                                                                    it_text_type = VALUE #( ( zif_wme_c=>gc_text_types-material_packing ) ) ).

      CHECK lt_section_txt IS NOT INITIAL.
      APPEND VALUE zif_text_handling=>ty_text( text = |{  TEXT-047 }: { lv_matnr ALPHA = OUT } | ) TO lt_text.
      APPEND LINES OF lt_section_txt  TO lt_text.
      CLEAR lv_matnr.
    ENDLOOP.

    IF lines( lt_text ) = 0 .
      APPEND VALUE zif_text_handling=>ty_text( text = |{ TEXT-048 }| ) TO lt_text.
    ENDIF.

    rv_text_dlv = lo_text_handling->create_pack_instr_string( lt_text ).

  ENDMETHOD.

  METHOD get_text_dlv_hu.

    DATA(lv_text_dlv) = get_text_dlv( ).
    DATA(lv_text_hu)  = get_text_hu( ).

    IF lv_text_dlv = TEXT-048 AND
       lv_text_hu  <> TEXT-048.
      rv_text_dlv_hu = lv_text_hu.
    ELSEIF lv_text_dlv <> TEXT-048 AND
           lv_text_hu  = TEXT-048.
      rv_text_dlv_hu = lv_text_dlv.
    ELSEIF lv_text_dlv = TEXT-048 AND
           lv_text_hu  = TEXT-048.
      rv_text_dlv_hu = lv_text_dlv.
    ELSE.
      rv_text_dlv_hu = |{ lv_text_dlv  }{ cl_abap_char_utilities=>cr_lf }{ lv_text_hu }|.
    ENDIF.

  ENDMETHOD.

  METHOD check_sn_track.

    DATA: lt_bupa   TYPE bup_t_cent_id,
          lt_but000 TYPE bup_but000_t.

    ASSIGN mt_dlv_headers[ docid = ms_cons_header-docid ] TO FIELD-SYMBOL(<ls_dlv_header>).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN <ls_dlv_header>-partyloc[ party_role = c_cust_partno_spr ]-partyno TO FIELD-SYMBOL(<lv_party_cust_prof>).

    IF sy-subrc = 0.
      APPEND VALUE #( partner = <lv_party_cust_prof> ) TO lt_bupa.
    ENDIF.

    ASSIGN <ls_dlv_header>-partyloc[ party_role = /scdl/if_dl_partyloc_c=>sc_party_role_stprt ]-partyno TO FIELD-SYMBOL(<lv_party_stprt>).

    IF sy-subrc = 0.
      APPEND VALUE #( partner = <lv_party_stprt> ) TO lt_bupa.
    ENDIF.

    LOOP AT mt_dlv_items ASSIGNING FIELD-SYMBOL(<ls_dlv_item>)
                                     WHERE docid = ms_cons_header-docid.

      ASSIGN <ls_dlv_item>-partyloc[ partyno = /scdl/if_dl_partyloc_c=>sc_party_role_sotprt ]-partyno TO FIELD-SYMBOL(<lv_party_sotprt>).
      CHECK sy-subrc = 0.
      APPEND VALUE #( partner = <lv_party_sotprt> ) TO lt_bupa.

      EXIT.
    ENDLOOP.

    CALL FUNCTION 'BUP_BUT000_MSELECT_WITH_PARTNR'
      EXPORTING
        it_partner      = lt_bupa
      IMPORTING
        et_but000       = lt_but000
      EXCEPTIONS
        blocked_partner = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF line_exists( lt_but000[ zz_save_sn_per_hu = abap_true ] ).
      rv_sn_scan = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD get_serial_status.

    DATA:
      lv_count_blank  TYPE int2,
      lv_count_red    TYPE int2,
      lv_count_green  TYPE int2,
      lv_count_yellow TYPE int2,
      lv_count_subhu  TYPE int2,

      lv_quant        TYPE /scwm/de_quantity,
      lv_quana        TYPE /scwm/de_quantity,
      lt_serials_main TYPE STANDARD TABLE OF ztcross_cap_nums.

    READ TABLE mt_cons_huhdr INTO DATA(ls_huhdr)
      WITH KEY huident = iv_huident.

    IF sy-subrc <> 0.
      rv_status = icon_led_red.
      RETURN.
    ENDIF.

    IF is_pallet( is_huhdr = ls_huhdr ) AND
       iv_calc_stock_onhu = abap_false. " stock which is directly on the pallet

      IF iv_pall_group_mc = 0. " we requested just the SN which is directly stored on the Pallet
        LOOP AT mt_cons_huhdr INTO DATA(ls_subhu)
          WHERE higher_guid = ls_huhdr-guid_hu .

          lv_count_subhu = lv_count_subhu + 1.
          CASE get_serial_status( iv_huident = ls_subhu-huident ).
            WHEN icon_led_green.
              lv_count_green = lv_count_green + 1.
            WHEN icon_led_yellow.
              lv_count_yellow = lv_count_yellow + 1.
            WHEN icon_led_red.
              lv_count_red = lv_count_red + 1.
            WHEN icon_led_red.
              lv_count_blank = lv_count_blank + 1.
          ENDCASE.
        ENDLOOP.
      ELSE.
        lv_count_subhu = 1.
      ENDIF.

      " Get stock (MC) which is directly stored on the Pallet
      CASE get_serial_status( iv_huident         = iv_huident
                              iv_pall_group_mc   = iv_pall_group_mc
                              iv_calc_stock_onhu = abap_true
                              iv_sn_screen_sub   = iv_sn_screen_sub ).
        WHEN icon_led_green.
          lv_count_subhu += 1.
          lv_count_green = lv_count_green + 1.
        WHEN icon_led_yellow.
          lv_count_subhu += 1.
          lv_count_yellow = lv_count_yellow + 1.
        WHEN icon_led_red.
          lv_count_subhu += 1.
          lv_count_red = lv_count_red + 1.
        WHEN icon_led_red.
          lv_count_subhu += 1.
          lv_count_blank = lv_count_blank + 1.
      ENDCASE.

      IF lv_count_blank = lv_count_subhu.
        rv_status = space.
        RETURN.
      ENDIF.

      IF lv_count_green = 0 AND
         lv_count_yellow = 0 AND
         lv_count_red > 0.

        rv_status = icon_led_red.
        RETURN.

      ENDIF.

      IF lv_count_green > 0 AND
         lv_count_yellow = 0 AND
         lv_count_red = 0.

        rv_status = icon_led_green.
        RETURN.

      ENDIF.

      IF lv_count_yellow > 0.
        rv_status = icon_led_yellow.
        RETURN.
      ENDIF.

      IF lv_count_green > 0 AND
         lv_count_red > 0.
        rv_status = icon_led_yellow.
        RETURN.
      ENDIF.

    ENDIF.

    CLEAR mt_serials.
    mt_serials = zcl_crud_ztcross_cap_nums=>select_multi_by_huid(
      iv_lgnum = lcl_controller=>sv_lgnum
      iv_huid  = ls_huhdr-guid_hu ).

    IF iv_pall_group_mc IS NOT INITIAL.
      DELETE mt_serials WHERE group_mc <> iv_pall_group_mc.
    ENDIF.

    " Andriyan Yordanov --- new calc logic
    LOOP AT mt_cons_huitm  ASSIGNING FIELD-SYMBOL(<ls_huitem>)
                                          WHERE guid_parent = ls_huhdr-guid_hu
                               GROUP BY ( qdocid  = <ls_huitem>-qdocid
                                          qitmid = <ls_huitem>-qitmid )
                            ASSIGNING FIELD-SYMBOL(<ls_group_huitm>).

      ASSIGN mt_dlv_items[ docid  = <ls_group_huitm>-qdocid
                           itemid = <ls_group_huitm>-qitmid ] TO FIELD-SYMBOL(<ls_dlv_item>).

      CHECK sy-subrc = 0.

      CHECK <ls_dlv_item>-eew-zzindenttab01 IS NOT INITIAL.

      DATA(lv_id_type) = <ls_dlv_item>-eew-zzindenttab01.

      LOOP AT GROUP <ls_group_huitm> ASSIGNING FIELD-SYMBOL(<ls_huitem_dlv_qty>).
        lv_quant = lv_quant + <ls_huitem_dlv_qty>-quan.
        CHECK iv_calc_stock_onhu = abap_true.
        CHECK <ls_huitem_dlv_qty>-altme <> <ls_huitem_dlv_qty>-meins.
        lv_quana += <ls_huitem_dlv_qty>-quana.
      ENDLOOP.

      CHECK iv_sn_screen_sub = abap_true.
      TRY.
          lv_quant = lv_quant DIV lv_quana.
        CATCH cx_sy_zerodivide.
      ENDTRY.

    ENDLOOP.

    IF lv_id_type IS INITIAL.
      CLEAR rv_status.
      RETURN.
    ENDIF.

    lt_serials_main  = VALUE #( FOR <ls_sernr_main> IN mt_serials
                                            WHERE ( id_type = lv_id_type )
                                            ( <ls_sernr_main> ) ).

    DATA(lv_scanned_sn) = lines( lt_serials_main ).

    IF lv_scanned_sn = lv_quant.
      rv_status = icon_led_green.
    ELSEIF lv_scanned_sn > 0.
      rv_status = icon_led_yellow.
    ELSE.
      rv_status = icon_led_red.
    ENDIF.
    "End Andriyan Yordanov new calc loggic

****
****    CLEAR mt_serials.
****    mt_serials = zcl_crud_ztcross_cap_nums=>select_multi_by_huid(
****      iv_lgnum = lcl_controller=>sv_lgnum
****      iv_huid  = ls_huhdr-guid_hu ).
****
****    LOOP AT mt_cons_huitm INTO DATA(ls_huitem) WHERE guid_parent = ls_huhdr-guid_hu.
****      lv_count_it = lv_count_it + 1.
****
****      READ TABLE mt_dlv_items INTO DATA(ls_dlv_item)
****        WITH KEY docid = ls_huitem-qdocid
****                 itemid = ls_huitem-qitmid.
****      IF sy-subrc <> 0.
****        rv_status = icon_led_yellow.
****        RETURN.
****      ENDIF.
****
****      IF ls_dlv_item-eew-zzindenttab01 IS INITIAL.
****        lv_count_ok = lv_count_ok + 1.
****      ENDIF.
****
****      LOOP AT mt_serials INTO DATA(ls_serial)
****        WHERE guid_hu = ls_huhdr-guid_hu.
****
****        IF ls_dlv_item-eew-zzindenttab01 IS NOT INITIAL AND ls_serial-id_type = ls_dlv_item-eew-zzindenttab01.
****          lv_count_1 = lv_count_1 + 1.
****        ENDIF.
****
****        IF ls_dlv_item-eew-zzindenttab02 IS NOT INITIAL AND ls_serial-id_type = ls_dlv_item-eew-zzindenttab02.
****          lv_count_2 = lv_count_2 + 1.
****        ENDIF.
****
****        IF ls_dlv_item-eew-zzindenttab03 IS NOT INITIAL AND ls_serial-id_type = ls_dlv_item-eew-zzindenttab03.
****          lv_count_3 = lv_count_3 + 1.
****        ENDIF.
****
****        IF ls_dlv_item-eew-zzindenttab04 IS NOT INITIAL AND ls_serial-id_type = ls_dlv_item-eew-zzindenttab04.
****          lv_count_4 = lv_count_4 + 1.
****        ENDIF.
****
****        IF ls_dlv_item-eew-zzindenttab05 IS NOT INITIAL AND ls_serial-id_type = ls_dlv_item-eew-zzindenttab05.
****          lv_count_5 = lv_count_5 + 1.
****        ENDIF.
****      ENDLOOP.
****      IF sy-subrc = 0.
****        lv_count_st = lv_count_st + 1.
****      ELSE.
****        CONTINUE.
****      ENDIF.
****
****      IF ls_dlv_item-eew-zzindenttab01 IS NOT INITIAL AND lv_count_1 <> ls_huitem-quan.
****        CONTINUE.
****      ENDIF.
****
****      IF ls_dlv_item-eew-zzindenttab02 IS NOT INITIAL AND lv_count_2 <> ls_huitem-quan.
****        CONTINUE.
****      ENDIF.
****
****      IF ls_dlv_item-eew-zzindenttab03 IS NOT INITIAL AND lv_count_3 <> ls_huitem-quan.
****        CONTINUE.
****      ENDIF.
****
****      IF ls_dlv_item-eew-zzindenttab04 IS NOT INITIAL AND lv_count_3 <> ls_huitem-quan.
****        CONTINUE.
****      ENDIF.
****
****      IF ls_dlv_item-eew-zzindenttab05 IS NOT INITIAL AND lv_count_3 <> ls_huitem-quan.
****        CONTINUE.
****      ENDIF.
****
****      lv_count_ok = lv_count_ok + 1.
****    ENDLOOP.
****
****    IF lv_count_ok = lv_count_it.
****      IF ls_dlv_item-eew-zzindenttab01 IS INITIAL.
****        CLEAR rv_status.
****      ELSE.
****        rv_status = icon_led_green.
****      ENDIF.
****    ELSEIF lv_count_st > 0.
****      rv_status = icon_led_yellow.
****    ELSE.
****      rv_status = icon_led_red.
****    ENDIF.

  ENDMETHOD.

  METHOD build_pick_table.

    DATA:
      lv_charg           TYPE /scwm/de_charg,
      lv_cnt             TYPE int4,
      lv_hutype          TYPE string,
      ls_mat_global      TYPE /scwm/s_material_global,
      ls_huhdr_top       TYPE /scwm/s_huhdr_int,
      lt_ordim_o         TYPE /scwm/tt_ordim_o,
      lt_ordim_o_all     TYPE /scwm/tt_ordim_o,
      lt_ordim_c         TYPE /scwm/tt_ordim_c,
      lt_ordim_c_all     TYPE /scwm/tt_ordim_c,
      lt_huident         TYPE /scwm/tt_huident,
      lt_huhdr           TYPE /scwm/tt_huhdr_int,
      lt_who_r           TYPE rsds_selopt_t,
      lt_who             TYPE /scwm/tt_who_int,
      lt_open_picking_hu TYPE ztt_consolidation_open_pick_hu.

    CLEAR: mt_open_picking, mt_open_picking_hu.

    LOOP AT mt_dlv_items INTO DATA(ls_item)
      WHERE docid = iv_docid.

      READ TABLE ls_item-status INTO DATA(ls_status)
        WITH KEY status_type = /scdl/if_dl_status_c=>sc_t_picking.
      IF ls_status-status_value < 1 OR ls_status-status_value = 9.
        CONTINUE.
      ENDIF.

      TRY.
          CLEAR ls_mat_global.
          CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
            EXPORTING
              iv_matid      = ls_item-product-productid
              iv_lgnum      = lcl_controller=>sv_lgnum
            IMPORTING
              es_mat_global = ls_mat_global.
        CATCH /scwm/cx_md.
          CLEAR ls_mat_global.
      ENDTRY.

      IF ls_item-batchid IS NOT INITIAL.
        CLEAR lv_charg.

        TRY.
            /scwm/cl_batch_appl=>get_batchno_by_id(
              EXPORTING
                iv_batchid = ls_item-batchid
                iv_lgnum   = lcl_controller=>sv_lgnum
              IMPORTING
                ev_charg   = lv_charg ).
          CATCH /scwm/cx_batch_precheck.
            CLEAR lv_charg.
        ENDTRY.
      ENDIF.

      APPEND VALUE #(
        docno  = |{ ls_item-docno ALPHA = OUT }|
        itemno = ls_item-itemno
        matnr  = ls_item-product-productno
        maktx  = ls_mat_global-maktx
        charg  = lv_charg
        quan   = ls_item-qty-qty
        unit   = ls_item-qty-uom ) TO mt_open_picking.

      CLEAR: lt_ordim_c, lt_ordim_o.

      CALL FUNCTION '/SCWM/TO_READ_WHR'
        EXPORTING
          iv_lgnum   = lcl_controller=>sv_lgnum
          iv_rdocid  = ls_item-docid
          iv_ritmid  = ls_item-itemid
          iv_flglock = abap_false
        IMPORTING
          et_ordim_o = lt_ordim_o
          et_ordim_c = lt_ordim_c
        EXCEPTIONS
          OTHERS     = 1.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      APPEND LINES OF lt_ordim_o TO lt_ordim_o_all.
      APPEND LINES OF lt_ordim_c TO lt_ordim_c_all.
    ENDLOOP.

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = lcl_controller=>sv_lgnum
        iv_process   = zif_param_const=>c_zcons_0001
        iv_parameter = zif_param_const=>c_cons_procty
      IMPORTING
        et_range     = DATA(lt_procty_r) ).

    LOOP AT lt_ordim_o_all INTO DATA(ls_ordim_o).
      IF ls_ordim_o-vlenr IS NOT INITIAL.
        APPEND VALUE #( lgnum = lcl_controller=>sv_lgnum
                        huident = ls_ordim_o-vlenr ) TO lt_huident.
      ENDIF.
      IF ls_ordim_o-nlenr IS NOT INITIAL.
        APPEND VALUE #( lgnum = lcl_controller=>sv_lgnum
                        huident = ls_ordim_o-nlenr ) TO lt_huident.
      ENDIF.

      IF ls_ordim_o-procty IN lt_procty_r.
        APPEND VALUE #( sign = /scmb/cl_search=>sc_sign_i option = /scmb/cl_search=>sc_eq low = ls_ordim_o-who ) TO lt_who_r.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_ordim_c_all INTO DATA(ls_ordim_c).
      IF ls_ordim_c-vlenr IS NOT INITIAL.
        APPEND VALUE #( lgnum = lcl_controller=>sv_lgnum
                        huident = ls_ordim_c-vlenr ) TO lt_huident.
      ENDIF.
      IF ls_ordim_c-nlenr IS NOT INITIAL.
        APPEND VALUE #( lgnum = lcl_controller=>sv_lgnum
                        huident = ls_ordim_c-nlenr ) TO lt_huident.
      ENDIF.

      IF ls_ordim_c-procty IN lt_procty_r.
        APPEND VALUE #( sign = /scmb/cl_search=>sc_sign_i option = /scmb/cl_search=>sc_eq low = ls_ordim_c-who ) TO lt_who_r.
      ENDIF.
    ENDLOOP.

    IF lt_huident IS NOT INITIAL.

      CALL FUNCTION '/SCWM/HUMAIN_REFRESH'.

      CALL FUNCTION '/SCWM/HU_READ_MULT'
        EXPORTING
          it_huident = lt_huident
          iv_lgnum   = lcl_controller=>sv_lgnum
        IMPORTING
          et_huhdr   = lt_huhdr
        EXCEPTIONS
          OTHERS     = 1.
      IF sy-subrc <> 0.
        CLEAR lt_huhdr.
      ENDIF.
    ENDIF.

    IF lt_who_r IS NOT INITIAL.
      TRY.
          CALL FUNCTION '/SCWM/WHO_GET_MULTI'
            EXPORTING
              iv_lgnum       = lcl_controller=>sv_lgnum
              it_whoid_range = lt_who_r
            IMPORTING
              et_who         = lt_who.
          DELETE lt_who WHERE status <> wmegc_wo_open AND status <> wmegc_wo_in_process AND status <> wmegc_wo_on_hold.
        CATCH /scwm/cx_core.
          CLEAR lt_who.
      ENDTRY.
    ENDIF.

    IF lt_who IS NOT INITIAL.
      DATA lt_whohu TYPE /scwm/tt_whohu.
      SELECT * FROM /scwm/whohu INTO CORRESPONDING FIELDS OF TABLE lt_whohu
        FOR ALL ENTRIES IN lt_who
        WHERE lgnum = lcl_controller=>sv_lgnum AND
              who   = lt_who-who.
    ENDIF.

    "1. Stock still to be picked
    LOOP AT lt_who INTO DATA(ls_who).
      IF ls_who-status = wmegc_wo_open OR ls_who-status = wmegc_wo_on_hold.
        get_pick_open_who(
          EXPORTING
            is_who             = ls_who
            it_whohu           = lt_whohu
            it_ordim_o         = lt_ordim_o_all
            it_ordim_c         = lt_ordim_c_all
            it_huhdr           = lt_huhdr
          CHANGING
            ct_open_picking_hu = lt_open_picking_hu ).
      ELSE.
        READ TABLE lt_ordim_c_all INTO ls_ordim_c
          WITH KEY who = ls_who-who.
        IF sy-subrc <> 0.
          get_pick_open_who(
            EXPORTING
              is_who             = ls_who
              it_whohu           = lt_whohu
              it_ordim_o         = lt_ordim_o_all
              it_ordim_c         = lt_ordim_c_all
              it_huhdr           = lt_huhdr
            CHANGING
              ct_open_picking_hu = lt_open_picking_hu ).
        ELSE.
          READ TABLE lt_huhdr INTO DATA(ls_huhdr)
            WITH KEY huident = ls_ordim_c-nlenr.
          IF sy-subrc <> 0.
            get_pick_open_who(
              EXPORTING
                is_who             = ls_who
                it_whohu           = lt_whohu
                it_ordim_o         = lt_ordim_o_all
                it_ordim_c         = lt_ordim_c_all
                it_huhdr           = lt_huhdr
              CHANGING
                ct_open_picking_hu = lt_open_picking_hu ).
          ELSE.
            READ TABLE lt_huhdr INTO ls_huhdr_top
              WITH KEY guid_hu = ls_huhdr-guid_hu_top.
            IF sy-subrc <> 0.
              get_pick_open_who(
                EXPORTING
                  is_who             = ls_who
                  it_whohu           = lt_whohu
                  it_ordim_o         = lt_ordim_o_all
                  it_ordim_c         = lt_ordim_c_all
                  it_huhdr           = lt_huhdr
                CHANGING
                  ct_open_picking_hu = lt_open_picking_hu ).
            ELSE.
              APPEND VALUE #( huident = ls_huhdr_top-huident
                              hutype  = ls_huhdr_top-letyp
                              hutype_desc = get_hutype( is_huhdr = ls_huhdr_top )
                              lgpla   = ls_huhdr_top-lgpla
                              rsrc    = ls_huhdr_top-rsrc
                              who     = ls_who-who
                              whostatt = TEXT-009
                              aarea   = "COND #( WHEN ls_who-areawho <> mv_aarea THEN ls_who-areawho ELSE
                                                  get_real_aa( is_who     = ls_who
                                                               it_ordim_o = lt_ordim_o_all
                                                               it_ordim_c = lt_ordim_c_all ) )  TO lt_open_picking_hu.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    "2. Stock already picked
    DATA(lo_hu) = NEW /scwm/cl_dlv_prd2hum( ).

    TRY.
        lo_hu->get_hu_for_prd_fd(
          EXPORTING iv_docid  = iv_docid
                    iv_doccat = wmegc_doccat_pdo
          IMPORTING et_huhdr  = DATA(lt_huhdr_dlv) ).
      CATCH /scdl/cx_delivery.
        RETURN.
    ENDTRY.

    DELETE lt_huhdr_dlv WHERE lgtyp = mv_lgtyp.
    CLEAR lt_huhdr.

    LOOP AT lt_huhdr_dlv INTO ls_huhdr.
      IF ls_huhdr-top = abap_false.
        CONTINUE.
      ENDIF.

      IF is_hu_ready( iv_huident = ls_huhdr-huident ) = abap_true.
        CONTINUE.
      ENDIF.
      APPEND ls_huhdr TO lt_huhdr.
    ENDLOOP.

    LOOP AT lt_huhdr INTO ls_huhdr.
      APPEND VALUE #( huident = ls_huhdr-huident
                      hutype  = ls_huhdr-letyp
                      hutype_desc = get_hutype( is_huhdr = ls_huhdr )
                      lgpla   = ls_huhdr-lgpla
                      rsrc    = ls_huhdr-rsrc ) TO lt_open_picking_hu.
    ENDLOOP.

    CLEAR mt_open_picking_hu.
    mt_open_picking_hu = lt_open_picking_hu.

*    LOOP AT lt_ordim_o_all INTO ls_ordim_o WHERE vlenr IS NOT INITIAL.
*
*      READ TABLE lt_huhdr INTO ls_huhdr WITH KEY huident = ls_ordim_o-vlenr.
*      IF sy-subrc <> 0.
*        READ TABLE lt_huhdr INTO ls_huhdr WITH KEY huident = ls_ordim_o-nlenr.
*        IF sy-subrc <> 0.
*          CLEAR ls_huhdr.
*        ENDIF.
*      ENDIF.
*
*      IF ls_huhdr-lgtyp = 'SPED'.
*        CONTINUE.
*      ENDIF.
*
*      IF is_master_carton( is_huhdr = ls_huhdr ) = abap_true AND ls_huhdr-higher_guid IS NOT INITIAL.
*        READ TABLE lt_huhdr INTO ls_huhdr_top WITH KEY guid_hu = ls_huhdr-higher_guid.
*        IF sy-subrc = 0.
*          READ TABLE mt_open_picking_hu TRANSPORTING NO FIELDS
*            WITH KEY huident = ls_huhdr_top-huident.
*          IF sy-subrc = 0.
*            CONTINUE.
*          ENDIF.
*          ls_huhdr = ls_huhdr_top.
*          ls_ordim_o-vlenr = ls_huhdr-huident.
*        ENDIF.
*      ENDIF.
*
*      READ TABLE lt_who INTO ls_who WITH KEY who = ls_ordim_o-who.
*      IF sy-subrc <> 0.
*        CLEAR ls_who.
*      ENDIF.
*
*      CLEAR: lt_ordim_o, lt_ordim_c.
*
*      IF ls_who-who IS NOT INITIAL.
*        CALL FUNCTION '/SCWM/TO_READ_WHO'
*          EXPORTING
*            iv_lgnum   = lcl_controller=>sv_lgnum
*            iv_who     = ls_who-who
*            iv_flglock = abap_false
*          IMPORTING
*            et_ordim_o = lt_ordim_o
*            et_ordim_c = lt_ordim_c
*          EXCEPTIONS
*            OTHERS     = 1.
*        IF sy-subrc <> 0.
*          CLEAR: lt_ordim_o, lt_ordim_c.
*        ENDIF.
*      ENDIF.
*
*      APPEND VALUE #( huident = ls_ordim_o-vlenr
*                      hutype  = ls_huhdr-letyp
*                      hutype_desc = get_hutype( is_huhdr = ls_huhdr )
*                      lgpla   = ls_ordim_o-vlpla
*                      rsrc    = ls_who-rsrc
*                      who     = ls_who-who
*                      whostatt = COND #( WHEN lt_ordim_c IS INITIAL THEN TEXT-009 ELSE TEXT-010 )
*                      aarea   = ls_who-areawho ) TO mt_open_picking_hu.
*    ENDLOOP.
*
*    LOOP AT lt_ordim_c_all INTO ls_ordim_c WHERE nlenr IS NOT INITIAL AND tostat = wmegc_to_confirm.
*
*      READ TABLE lt_huhdr INTO ls_huhdr WITH KEY huident = ls_ordim_c-vlenr.
*      IF sy-subrc <> 0.
*        READ TABLE lt_huhdr INTO ls_huhdr WITH KEY huident = ls_ordim_c-nlenr.
*        IF sy-subrc <> 0.
*          CLEAR ls_huhdr.
*        ENDIF.
*      ENDIF.
*
*      IF ls_huhdr-lgtyp = 'SPED'.
*        CONTINUE.
*      ENDIF.
*
*      IF is_master_carton( is_huhdr = ls_huhdr ) = abap_true AND ls_huhdr-higher_guid IS NOT INITIAL.
*        CLEAR ls_huhdr_top.
*        READ TABLE lt_huhdr INTO ls_huhdr_top WITH KEY guid_hu = ls_huhdr-higher_guid.
*        IF sy-subrc = 0.
*          READ TABLE mt_open_picking_hu TRANSPORTING NO FIELDS
*            WITH KEY huident = ls_huhdr_top-huident.
*          IF sy-subrc = 0.
*            CONTINUE.
*          ENDIF.
*          ls_huhdr = ls_huhdr_top.
*          ls_ordim_c-nlenr = ls_huhdr-huident.
*        ENDIF.
*      ENDIF.
*
*      READ TABLE lt_who INTO ls_who WITH KEY who = ls_ordim_c-who.
*      IF sy-subrc <> 0.
*        CLEAR ls_who.
*      ENDIF.
*
*      CLEAR: lt_ordim_o, lt_ordim_c.
*
*      IF ls_who-who IS NOT INITIAL.
*        CALL FUNCTION '/SCWM/TO_READ_WHO'
*          EXPORTING
*            iv_lgnum   = lcl_controller=>sv_lgnum
*            iv_who     = ls_who-who
*            iv_flglock = abap_false
*          IMPORTING
*            et_ordim_o = lt_ordim_o
*            et_ordim_c = lt_ordim_c
*          EXCEPTIONS
*            OTHERS     = 1.
*        IF sy-subrc <> 0.
*          CLEAR: lt_ordim_o, lt_ordim_c.
*        ENDIF.
*      ENDIF.
*
*      READ TABLE mt_open_picking_hu TRANSPORTING NO FIELDS WITH KEY huident = ls_ordim_c-nlenr.
*      IF sy-subrc <> 0.
*        APPEND VALUE #( huident = ls_ordim_c-nlenr
*                        hutype  = ls_huhdr-letyp
*                        hutype_desc = get_hutype( is_huhdr = ls_huhdr )
*                        lgpla   = ls_ordim_c-nlpla
*                        rsrc    = ls_who-rsrc
*                        who     = ls_who-who
*                        whostatt = COND #( WHEN lt_ordim_c IS INITIAL THEN TEXT-009 ELSE TEXT-010 )
*                        aarea   = ls_who-areawho ) TO mt_open_picking_hu.
*      ENDIF.
*    ENDLOOP.

  ENDMETHOD.

  METHOD get_real_aa.

    DATA:
      lv_aarea   TYPE /scwm/de_aarea,
      lt_ordim_o TYPE /scwm/tt_ordim_o,
      lt_lagps   TYPE /scwm/tt_lagps.

    LOOP AT it_ordim_o INTO DATA(ls_ordim_o)
      WHERE who = is_who-who.
      APPEND ls_ordim_o TO lt_ordim_o.
    ENDLOOP.

    "read activity areas
    CALL FUNCTION '/SCWM/LAGPS_READ_MULTI'
      EXPORTING
        iv_lgnum       = lcl_controller=>sv_lgnum
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
      RETURN.
    ENDIF.

    " Andriyan Yordanov - new logic for aarea calculation
    LOOP AT lt_lagps ASSIGNING FIELD-SYMBOL(<ls_lagps>)
                                          WHERE act_type = zif_wme_c=>gs_act_typ-pick
                                              GROUP BY ( aarea = <ls_lagps>-aarea ).
      IF lv_aarea IS INITIAL.
        lv_aarea = <ls_lagps>-aarea.
      ENDIF.

      CHECK lv_aarea <> <ls_lagps>-aarea.

      ASSIGN lt_lagps[ act_type = c_actty ] TO FIELD-SYMBOL(<ls_sped_act>).

      IF sy-subrc = 0.
        rv_aarea = <ls_sped_act>-aarea.
      ELSE.
        rv_aarea = '*'.
      ENDIF.

      RETURN.
    ENDLOOP.

    rv_aarea = lv_aarea.
    "end Andriyan Yordanov

***    READ TABLE lt_lagps INTO DATA(ls_lagps_first) INDEX 1.
***    LOOP AT lt_lagps TRANSPORTING NO FIELDS WHERE aarea <> ls_lagps_first-aarea.
***      EXIT.
***    ENDLOOP.
***    IF sy-subrc = 0.
***      rv_aarea = '*'.
***    ELSE.
***      rv_aarea = ls_lagps_first-aarea.
***    ENDIF.

  ENDMETHOD.

  METHOD get_pick_open_who.

    DATA: lv_cnt TYPE int4.

    READ TABLE it_whohu INTO DATA(ls_whohu)
      WITH KEY who = is_who-who.

    IF sy-subrc <> 0.

      LOOP AT it_ordim_o INTO DATA(ls_ordim_o)
        WHERE who = is_who-who.
        READ TABLE it_huhdr INTO DATA(ls_huhdr) WITH KEY huident = ls_ordim_o-vlenr.
        IF sy-subrc <> 0.
          CLEAR ls_huhdr.
        ENDIF.

        APPEND VALUE #( huident = ls_ordim_o-vlenr
          hutype  = ls_huhdr-letyp
          hutype_desc = get_hutype( is_huhdr = ls_huhdr )
          who     = is_who-who
          whostatt = TEXT-009
          aarea   =   get_real_aa( is_who     = is_who
                                   it_ordim_o = it_ordim_o
                                   it_ordim_c = it_ordim_c ) "is_who-areawho  " Andriyan Yordanov - changed aarea logic ... should be like that or not ???
                         ) TO ct_open_picking_hu.
      ENDLOOP.

    ELSE.

      CLEAR lv_cnt.
      LOOP AT it_whohu INTO ls_whohu
        WHERE who = is_who-who.
        lv_cnt = lv_cnt + 1.
      ENDLOOP.
      IF lv_cnt = 1.
        get_hutype_for_pmat(
          EXPORTING
            iv_pmatid = ls_whohu-pmat_guid
          IMPORTING
            ev_hutype = DATA(lv_hutype)
            ev_descr  = DATA(lv_descr) ).

        APPEND VALUE #(
          hutype  = lv_hutype
          hutype_desc = lv_descr
          who     = is_who-who
          whostatt = TEXT-009
          aarea   = get_real_aa( is_who     = is_who
                                 it_ordim_o = it_ordim_o
                                 it_ordim_c = it_ordim_c ) "is_who-areawho  " Andriyan Yordanov - changed aarea logic ... should be like that or not ???
                  ) TO ct_open_picking_hu.
      ELSE.
        LOOP AT it_ordim_o INTO ls_ordim_o
          WHERE shiphuid IS NOT INITIAL AND
                shiphuid <> ls_whohu-huid.
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
          READ TABLE it_whohu INTO ls_whohu
            WITH KEY huid = ls_ordim_o-shiphuid.
          get_hutype_for_pmat(
            EXPORTING
              iv_pmatid = ls_whohu-pmat_guid
            IMPORTING
              ev_hutype = lv_hutype
              ev_descr  = lv_descr ).

          APPEND VALUE #(
            hutype      = lv_hutype
            hutype_desc = lv_descr
            who         = is_who-who
            whostatt    = TEXT-009
            aarea       = " COND #( WHEN is_who-areawho <> mv_aarea THEN is_who-areawho ELSE
                                         get_real_aa( is_who     = is_who
                                                      it_ordim_o = it_ordim_o
                                                      it_ordim_c = it_ordim_c ) ) TO ct_open_picking_hu.
        ELSE.
          CLEAR lv_descr.
          lv_descr = lv_cnt.

          CONCATENATE lv_descr space INTO lv_descr RESPECTING BLANKS.
          CONCATENATE lv_descr 'Master Cartons' INTO lv_descr.
          APPEND VALUE #(
            hutype_desc = TEXT-017"  lv_descr " Andriyan Yordanov
            who         = is_who-who
            whostatt    = TEXT-009
            aarea       = get_real_aa( is_who     = is_who
                                       it_ordim_o = it_ordim_o
                                       it_ordim_c = it_ordim_c ) ) TO ct_open_picking_hu.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD build_cons_huitem_table.

    DATA:
      lv_mc_count    TYPE /scwm/dl_counter,
      ls_mara        TYPE mara,
      lt_huitm_subhu TYPE /scwm/tt_huitm_int,
      ls_mat_global  TYPE /scwm/s_material_global,
      lt_mat_ean     TYPE /scwm/tt_mat_mean.

    DATA(lo_model) = lcl_model=>get_instance( ).

    CLEAR: ct_hu_content,
           et_hu_info_cont.

    READ TABLE lo_model->mt_cons_huhdr INTO DATA(ls_huhdr)
      WITH KEY huident = iv_huident.
    IF sy-subrc <> 0 AND iv_split = abap_false.
      RETURN.
    ENDIF.

    READ TABLE lo_model->mt_cons_hus INTO DATA(ls_cons_hu)
      WITH KEY huident = iv_huident.
    IF sy-subrc = 0.
      IF iv_split = abap_false.
        lo_model->ms_cons_hu = ls_cons_hu.
      ENDIF.
      " Changed by Andriyan Yordanov
***      APPEND INITIAL LINE TO ct_hu_content ASSIGNING FIELD-SYMBOL(<ls_hu_content>).
***
***      <ls_hu_content>-huident = ls_cons_hu-huident.

      LOOP AT it_huitem INTO DATA(ls_huitem) WHERE guid_parent = ls_huhdr-guid_hu.
        add_huitem(
          EXPORTING
            is_huitem = ls_huitem
            iv_split  = iv_split
          CHANGING
            ct_hu_content = ct_hu_content ).

        IF et_hu_info_cont IS REQUESTED.

          LOOP AT ct_hu_content ASSIGNING FIELD-SYMBOL(<ls_hu_content>).
            DO <ls_hu_content>-mc TIMES.
              APPEND INITIAL LINE TO et_hu_info_cont ASSIGNING  FIELD-SYMBOL(<ls_subhu_info_content>).
              <ls_subhu_info_content> = <ls_hu_content>.
              <ls_subhu_info_content>-guid_hu = ls_huhdr-guid_hu.
              <ls_subhu_info_content>-huident = ls_huhdr-huident.
            ENDDO.
          ENDLOOP.
        ENDIF.
      ENDLOOP.

      " The stock is directly on the HU we should recalculate the serial status
      LOOP AT ct_hu_content ASSIGNING FIELD-SYMBOL(<ls_hu_cont_pall>).

        CHECK <ls_hu_cont_pall>-huident = '-'.
        CHECK is_pallet( is_huhdr = ls_huhdr ) = abap_true AND
              <ls_hu_cont_pall>-serial_status <> space.

        CLEAR <ls_hu_cont_pall>-serial_status.
        DO <ls_hu_cont_pall>-mc TIMES.
          lv_mc_count += 1.
          DATA(lv_serial_status) = lo_model->get_serial_status( iv_huident       = ls_huhdr-huident
                                                                iv_pall_group_mc = lv_mc_count
                                                                iv_sn_screen_sub = abap_true ).

          IF <ls_hu_cont_pall>-serial_status IS INITIAL.
            <ls_hu_cont_pall>-serial_status = lv_serial_status.
          ELSEIF <ls_hu_cont_pall>-serial_status = icon_led_red AND
                lv_serial_status <> icon_led_red.
            <ls_hu_cont_pall>-serial_status = icon_led_yellow.
          ELSEIF <ls_hu_cont_pall>-serial_status = icon_led_green AND
                 lv_serial_status <> icon_led_green.
            <ls_hu_cont_pall>-serial_status = icon_led_yellow.
          ENDIF.
        ENDDO.

      ENDLOOP.

      "edn of change by Andriyan Yordanov

      IF ls_huhdr-top = abap_true AND
         ls_huhdr-bottom = abap_false AND
         is_pallet( is_huhdr = ls_huhdr ) = abap_false.

        LOOP AT lo_model->mt_cons_huhdr ASSIGNING FIELD-SYMBOL(<ls_huhdr_subhu>) WHERE higher_guid = ls_huhdr-guid_hu.
          LOOP AT it_huitem INTO ls_huitem WHERE guid_parent = <ls_huhdr_subhu>-guid_hu.
            add_huitem(
              EXPORTING
                is_huitem = ls_huitem
                iv_split  = iv_split
              CHANGING
                ct_hu_content = ct_hu_content ).

            IF et_hu_info_cont IS REQUESTED.
              LOOP AT ct_hu_content ASSIGNING <ls_hu_content>.
                DO <ls_hu_content>-mc TIMES.
                  APPEND INITIAL LINE TO et_hu_info_cont ASSIGNING <ls_subhu_info_content>.
                  <ls_subhu_info_content> = <ls_hu_content>.
                  <ls_subhu_info_content>-guid_hu = ls_huhdr-guid_hu.
                  <ls_subhu_info_content>-huident = ls_huhdr-huident.
                ENDDO.
              ENDLOOP.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

      ELSEIF ls_huhdr-top    = abap_true  AND
             ls_huhdr-bottom = abap_false AND
             is_pallet( is_huhdr = ls_huhdr ) = abap_true.

        LOOP AT lo_model->mt_cons_huhdr ASSIGNING <ls_huhdr_subhu>
                                                  WHERE higher_guid = ls_huhdr-guid_hu.

          CLEAR lt_huitm_subhu.
          "SubHU has only 1 product?
          "LOOP AT lo_model->mt_cons_huitm INTO DATA(ls_huitem_subhu) WHERE guid_parent = ls_huhdr_subhu-guid_hu.
          LOOP AT it_huitem INTO DATA(ls_huitem_subhu) WHERE guid_parent = <ls_huhdr_subhu>-guid_hu.
            READ TABLE lt_huitm_subhu TRANSPORTING NO FIELDS WITH KEY matid = ls_huitem_subhu-matid.
            IF sy-subrc <> 0.
              APPEND ls_huitem_subhu TO lt_huitm_subhu.
            ENDIF.
          ENDLOOP.

          IF get_hutype( is_huhdr = <ls_huhdr_subhu> ) = TEXT-017 AND
             lines( lt_huitm_subhu ) = 1 AND
             NOT line_exists( lo_model->mt_cons_hus[ huident = <ls_huhdr_subhu>-huident ] ). " Andriyan Yordanov - Amazon scenario

            DATA(lv_quan) = sum_hu_quantity( iv_guid_hu = <ls_huhdr_subhu>-guid_hu
                                             iv_matid   = lt_huitm_subhu[ 1 ]-matid ).

            READ TABLE ct_hu_content ASSIGNING <ls_hu_content>
              WITH KEY matid = lt_huitm_subhu[ 1 ]-matid
                       quan = lv_quan.

            IF sy-subrc = 0.
              "<ls_hu_content>-quan = <ls_hu_content>-quan + lv_quan.
              <ls_hu_content>-mc = <ls_hu_content>-mc + 1.
              IF et_hu_info_cont IS REQUESTED.
                APPEND INITIAL LINE TO et_hu_info_cont ASSIGNING <ls_subhu_info_content>.
                <ls_subhu_info_content> = <ls_hu_content>.
                <ls_subhu_info_content>-guid_hu = <ls_huhdr_subhu>-guid_hu.
                <ls_subhu_info_content>-huident = <ls_huhdr_subhu>-huident.
              ENDIF.
              CONTINUE.
            ENDIF.

            TRY.
                CLEAR: ls_mat_global, lt_mat_ean.
                CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
                  EXPORTING
                    iv_matid      = lt_huitm_subhu[ 1 ]-matid
                    iv_lgnum      = lcl_controller=>sv_lgnum
                  IMPORTING
                    es_mat_global = ls_mat_global
                    et_mat_mean   = lt_mat_ean.
              CATCH /scwm/cx_md.
                RETURN.
            ENDTRY.

            APPEND INITIAL LINE TO ct_hu_content ASSIGNING <ls_hu_content>.

            <ls_hu_content>-huident = '-'.
            <ls_hu_content>-descr = ls_mat_global-maktx.
            <ls_hu_content>-product = ls_mat_global-matnr.
            <ls_hu_content>-quan = lv_quan.
            <ls_hu_content>-matid = lt_huitm_subhu[ 1 ]-matid.
            <ls_hu_content>-uom = lt_huitm_subhu[ 1 ]-meins.
            <ls_hu_content>-serial_status = get_serial_status( iv_huident = <ls_huhdr_subhu>-huident ).
            <ls_hu_content>-higher_guid = <ls_huhdr_subhu>-higher_guid.
            <ls_hu_content>-mc = 1.

            CLEAR ls_mara.
            CALL FUNCTION 'MARA_SINGLE_READ'
              EXPORTING
                matnr  = ls_mat_global-matnr
              IMPORTING
                wmara  = ls_mara
              EXCEPTIONS
                OTHERS = 1.
            IF sy-subrc <> 0.
              CLEAR ls_mara.
            ENDIF.

            <ls_hu_content>-ean = ls_mara-ean11.
            <ls_hu_content>-mpn = ls_mara-mfrpn.

            IF et_hu_info_cont IS REQUESTED.
              APPEND INITIAL LINE TO et_hu_info_cont ASSIGNING <ls_subhu_info_content>.
              <ls_subhu_info_content> = <ls_hu_content>.
              <ls_subhu_info_content>-guid_hu = <ls_huhdr_subhu>-guid_hu.
              <ls_subhu_info_content>-huident = <ls_huhdr_subhu>-huident.
            ENDIF.

          ELSE.
            "1 line per HU
            READ TABLE ct_hu_content ASSIGNING <ls_hu_content>
              WITH KEY huident = <ls_huhdr_subhu>-huident.
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.

            CLEAR: ls_mat_global,
                   lt_mat_ean.

            "READ TABLE lo_model->mt_cons_huitm INTO ls_huitem_subhu WITH KEY guid_parent = ls_huhdr_subhu-guid_hu.
            READ TABLE it_huitem INTO ls_huitem_subhu WITH KEY guid_parent = <ls_huhdr_subhu>-guid_hu.
            IF sy-subrc <> 0.
              CONTINUE.
            ENDIF.

            IF get_hutype( is_huhdr = <ls_huhdr_subhu> ) = TEXT-016 OR " Andriyan Yordanov
               get_hutype( is_huhdr = <ls_huhdr_subhu> ) = TEXT-015.

              APPEND INITIAL LINE TO ct_hu_content ASSIGNING <ls_hu_content>.

              <ls_hu_content>-quan = sum_hu_quantity( iv_guid_hu = <ls_huhdr_subhu>-guid_hu ).

              <ls_hu_content>-huident = <ls_huhdr_subhu>-huident.
              <ls_hu_content>-uom     = ls_huitem_subhu-meins.

              IF get_hutype( is_huhdr = <ls_huhdr_subhu> ) = TEXT-016.
                <ls_hu_content>-serial_status = get_serial_status( iv_huident = <ls_huhdr_subhu>-huident ).
              ENDIF.

              IF et_hu_info_cont IS REQUESTED.
                APPEND INITIAL LINE TO et_hu_info_cont ASSIGNING  <ls_subhu_info_content>.
                <ls_subhu_info_content> = <ls_hu_content>.
                <ls_subhu_info_content>-guid_hu = <ls_huhdr_subhu>-guid_hu.
                <ls_subhu_info_content>-huident = <ls_huhdr_subhu>-huident.
              ENDIF.
              " End Andriyan Yordanov
            ELSE.

              TRY.
                  CLEAR: ls_mat_global,
                         lt_mat_ean.

                  CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
                    EXPORTING
                      iv_matid      = ls_huitem_subhu-matid
                      iv_lgnum      = lcl_controller=>sv_lgnum
                    IMPORTING
                      es_mat_global = ls_mat_global
                      et_mat_mean   = lt_mat_ean.
                CATCH /scwm/cx_md.
                  RETURN.
              ENDTRY.

              APPEND INITIAL LINE TO ct_hu_content ASSIGNING <ls_hu_content>.

              " Andriyan Yordano - change for Amazon scenario
              <ls_hu_content>-huident =  COND #( WHEN line_exists( lo_model->mt_cons_hus[ huident = <ls_huhdr_subhu>-huident ] )
                                                   THEN <ls_huhdr_subhu>-huident
                                                   ELSE '-' ).

              <ls_hu_content>-mc = 1. " in this cas e we are always in MC scenarion

              <ls_hu_content>-descr   = ls_mat_global-maktx.
              <ls_hu_content>-product = ls_mat_global-matnr.

              <ls_hu_content>-quan = sum_hu_quantity( iv_guid_hu = <ls_huhdr_subhu>-guid_hu
                                                      iv_matid   = ls_huitem_subhu-matid ).

              <ls_hu_content>-matid = ls_huitem_subhu-matid.
              <ls_hu_content>-uom = ls_huitem_subhu-meins.
              <ls_hu_content>-serial_status = get_serial_status( iv_huident = <ls_huhdr_subhu>-huident ).
              <ls_hu_content>-higher_guid = <ls_huhdr_subhu>-higher_guid.

              CLEAR ls_mara.
              CALL FUNCTION 'MARA_SINGLE_READ'
                EXPORTING
                  matnr  = ls_mat_global-matnr
                IMPORTING
                  wmara  = ls_mara
                EXCEPTIONS
                  OTHERS = 1.
              IF sy-subrc <> 0.
                CLEAR ls_mara.
              ENDIF.

              <ls_hu_content>-ean = ls_mara-ean11.
              <ls_hu_content>-mpn = ls_mara-mfrpn.

              IF et_hu_info_cont IS REQUESTED.
                APPEND INITIAL LINE TO et_hu_info_cont ASSIGNING  <ls_subhu_info_content>.
                <ls_subhu_info_content> = <ls_hu_content>.
                <ls_subhu_info_content>-guid_hu = <ls_huhdr_subhu>-guid_hu.
                <ls_subhu_info_content>-huident = <ls_huhdr_subhu>-huident.
              ENDIF.

            ENDIF.

          ENDIF.

        ENDLOOP.

      ENDIF.
    ELSE.
      READ TABLE lo_model->mt_cons_huhdr TRANSPORTING NO FIELDS
        WITH KEY huident = iv_huident.
      IF sy-subrc = 0 AND iv_split IS INITIAL.
        "HU belongs to a different delivery
        MESSAGE e344(/scwm/rf_en) WITH iv_huident.
      ELSE.
        "New HU
        APPEND INITIAL LINE TO ct_hu_content ASSIGNING <ls_hu_content>.

        <ls_hu_content>-huident = iv_huident.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD sum_hu_quantity.

    DATA: lt_matid_r   TYPE rseloption.

    IF iv_matid IS NOT INITIAL.

      lt_matid_r = VALUE #( ( sign   = wmegc_sign_inclusive
                              option = wmegc_option_eq
                              low    = iv_matid ) ).

    ENDIF.

    LOOP AT mt_cons_huitm INTO DATA(ls_huitem) WHERE guid_parent = iv_guid_hu AND
                                                     matid       IN lt_matid_r.
      rv_quan = rv_quan + ls_huitem-quan.
    ENDLOOP.

  ENDMETHOD.

  METHOD add_huitem.

    DATA:
      ls_mat_global TYPE /scwm/s_material_global,
      ls_mara       TYPE mara,
      lt_mat_ean    TYPE /scwm/tt_mat_mean,
      lt_mat_uom    TYPE /scwm/tt_material_uom.

    DATA(lo_model) = lcl_model=>get_instance( ).

    READ TABLE lo_model->mt_cons_huhdr INTO DATA(ls_huhdr)
      WITH KEY guid_hu = is_huitem-guid_parent.

    READ TABLE ct_hu_content ASSIGNING FIELD-SYMBOL(<ls_hu_content>)
      WITH KEY matid = is_huitem-matid.

    IF sy-subrc = 0 AND
       <ls_hu_content>-quan = is_huitem-quan.

      IF is_pallet( is_huhdr    =   ls_huhdr ).
        <ls_hu_content>-mc = <ls_hu_content>-mc + is_huitem-quana.
      ELSE.
        <ls_hu_content>-quan = <ls_hu_content>-quan + is_huitem-quan.
      ENDIF.

    ELSE.

      TRY.
          CLEAR: ls_mat_global, lt_mat_ean.
          CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
            EXPORTING
              iv_matid      = is_huitem-matid
              iv_lgnum      = lcl_controller=>sv_lgnum
            IMPORTING
              es_mat_global = ls_mat_global
              et_mat_mean   = lt_mat_ean
              et_mat_uom    = lt_mat_uom.
        CATCH /scwm/cx_md.
          RETURN.
      ENDTRY.

      APPEND INITIAL LINE TO ct_hu_content ASSIGNING <ls_hu_content>.

      <ls_hu_content>-huident = '-'.
      <ls_hu_content>-descr = ls_mat_global-maktx.
      <ls_hu_content>-product = ls_mat_global-matnr.

      READ TABLE lt_mat_uom INTO DATA(ls_mat_uom)
        WITH KEY meinh = is_huitem-altme.
      IF sy-subrc <> 0 OR ls_mat_uom-umrez <= 0 OR is_huitem-quan < ls_mat_uom-umrez.
        <ls_hu_content>-quan = is_huitem-quan.
        <ls_hu_content>-uom = is_huitem-meins.
      ELSEIF is_huitem-quan MOD ls_mat_uom-umrez = 0.
        <ls_hu_content>-quan = ls_mat_uom-umrez.
        <ls_hu_content>-uom =  is_huitem-meins. ""is_huitem-altme. Andriyan Yordanov
        <ls_hu_content>-mc = is_huitem-quan DIV ls_mat_uom-umrez.
      ELSE.
        <ls_hu_content>-quan = is_huitem-quan.
        <ls_hu_content>-uom = is_huitem-meins.
      ENDIF.

      <ls_hu_content>-matid = is_huitem-matid.
      <ls_hu_content>-serial_status = get_serial_status( iv_huident = ls_huhdr-huident ).

      IF is_master_carton( is_huhdr = ls_huhdr ) = abap_true.
        <ls_hu_content>-mc = <ls_hu_content>-mc + 1.
      ENDIF.

      CLEAR ls_mara.
      CALL FUNCTION 'MARA_SINGLE_READ'
        EXPORTING
          matnr  = ls_mat_global-matnr
        IMPORTING
          wmara  = ls_mara
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc <> 0.
        CLEAR ls_mara.
      ENDIF.

      <ls_hu_content>-ean = ls_mara-ean11.
      <ls_hu_content>-mpn = ls_mara-mfrpn.

    ENDIF.

    IF <ls_hu_content> IS ASSIGNED.
      IF ls_huhdr-higher_guid IS NOT INITIAL.
        <ls_hu_content>-higher_guid = ls_huhdr-higher_guid.
      ELSE.
        <ls_hu_content>-higher_guid = ls_huhdr-guid_hu.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD get_delivery_for_hu.

    DATA:
      lt_huitm TYPE /scwm/tt_huitm_int,
      lt_docid TYPE /scdl/dl_docid_tab.

    CALL FUNCTION '/SCWM/HU_READ'
      EXPORTING
        iv_appl    = wmegc_huappl_wme
        iv_lgnum   = lcl_controller=>sv_lgnum
        iv_huident = iv_huident
      IMPORTING
        et_huitm   = lt_huitm
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0 OR lt_huitm IS INITIAL.
      MESSAGE e040(/scwm/delivery_ui).
    ENDIF.

    LOOP AT lt_huitm INTO DATA(ls_huitm)
      WHERE qdocid IS NOT INITIAL.
      APPEND VALUE #( docid = ls_huitm-qdocid ) TO lt_docid.
    ENDLOOP.

    get_open_deliveries( it_docid = lt_docid ).

  ENDMETHOD.

  METHOD is_hu_ready.

    DATA:
      lt_prcesa TYPE /scwm/tt_tprcesa.

    READ TABLE mt_cons_huhdr INTO DATA(ls_huhdr) WITH KEY huident = iv_huident.
    IF sy-subrc <> 0.
      CALL FUNCTION '/SCWM/HUHEADER_READ'
        EXPORTING
          iv_appl     = wmegc_huappl_wme
          iv_huident  = iv_huident
        IMPORTING
          es_huheader = ls_huhdr
        EXCEPTIONS
          OTHERS      = 1.
      IF sy-subrc <> 0.
        CLEAR ls_huhdr.
      ENDIF.
    ENDIF.

    IF ls_huhdr-prces IS INITIAL OR ls_huhdr-procs IS INITIAL.
      rv_ready = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCWM/TPRCESA_READ_SINGLE'
      EXPORTING
        iv_lgnum   = lcl_controller=>sv_lgnum
        iv_prces   = ls_huhdr-prces
      IMPORTING
        et_tprcesa = lt_prcesa
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      rv_ready = abap_false.
      RETURN.
    ENDIF.

    READ TABLE lt_prcesa INTO DATA(ls_prcesa_curr)
      WITH KEY procs = ls_huhdr-procs.
    IF sy-subrc <> 0.
      rv_ready = abap_false.
      RETURN.
    ENDIF.

    READ TABLE lt_prcesa INTO DATA(ls_prcesa_sped)
      WITH KEY procs = mv_procs. " Andriyan Yordanov change hardcoded with new param
    IF sy-subrc <> 0.
      rv_ready = abap_false.
      RETURN.
    ENDIF.

    CASE ls_huhdr-procs.
      WHEN mv_procs.         " Andriyan Yordanov change hardcoded with new param
        IF ls_huhdr-copst = abap_true.
          rv_ready = abap_true.
        ELSE.
          rv_ready = abap_false.
        ENDIF.
      WHEN OTHERS.
        IF ls_prcesa_curr-seqnr >= ls_prcesa_sped-seqnr.
          rv_ready = abap_true.
        ELSE.
          rv_ready = abap_false.
        ENDIF.
    ENDCASE.

  ENDMETHOD.

  METHOD get_serials_for_hu.

    READ TABLE mt_cons_huhdr INTO DATA(ls_huhdr)
      WITH KEY huident = iv_huident.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lt_serial) = zcl_crud_ztcross_cap_nums=>select_multi_by_huid(
      iv_lgnum = lcl_controller=>sv_lgnum
      iv_huid  = ls_huhdr-guid_hu ).

    " Andriyan Yordanov get all subHU serial numbers
    IF ls_huhdr-top    = abap_true AND
       ls_huhdr-bottom = abap_false .

      LOOP AT mt_cons_huhdr ASSIGNING FIELD-SYMBOL(<ls_sub_hu_mc>)
                                     WHERE higher_guid = ls_huhdr-guid_hu.

        DATA(lt_mc_serial) = zcl_crud_ztcross_cap_nums=>select_multi_by_huid(
                                                                iv_lgnum = lcl_controller=>sv_lgnum
                                                                iv_huid  = <ls_sub_hu_mc>-guid_hu ).

        CHECK lt_mc_serial IS NOT INITIAL.

        APPEND LINES OF lt_mc_serial TO lt_serial.

      ENDLOOP.

    ELSEIF is_shipping_carton( is_huhdr = ls_huhdr ) = abap_true.

      " Shipping carton new requirment
      lt_mc_serial = zcl_crud_ztcross_cap_nums=>select_multi_by_huid(
                                                                     iv_lgnum = lcl_controller=>sv_lgnum
                                                                     iv_huid  = ls_huhdr-guid_hu ).


      LOOP AT lt_mc_serial ASSIGNING FIELD-SYMBOL(<ls_check_exist>).
        CHECK NOT line_exists( lt_serial[ full_serial_number =  <ls_check_exist>-full_serial_number ] ).
        APPEND <ls_check_exist> TO lt_serial.
      ENDLOOP.

    ENDIF.

    IF is_pallet( is_huhdr = ls_huhdr ) AND
       iv_group_mc IS NOT INITIAL.
      DELETE lt_serial WHERE group_mc <> iv_group_mc.
    ENDIF.
    "end Andriyan Yordanov

    IF lt_serial IS INITIAL.
      RETURN.
    ENDIF.

    SELECT * FROM ztcross_numbers INTO TABLE @DATA(lt_numbers)
      FOR ALL ENTRIES IN @lt_serial
      WHERE spras	 = @sy-langu AND
            lgnum  = @lcl_controller=>sv_lgnum AND
            selnum = @lt_serial-id_type.

    LOOP AT lt_serial INTO DATA(ls_serial).
      APPEND INITIAL LINE TO rt_serial ASSIGNING FIELD-SYMBOL(<ls_serial>).
      MOVE-CORRESPONDING ls_serial TO <ls_serial>.
      "<ls_serial>-serial = ls_serial-serial.

      READ TABLE lt_numbers INTO DATA(ls_number)
        WITH KEY selnum = ls_serial-id_type.
      IF sy-subrc = 0.
        <ls_serial>-descr = ls_number-number_description.
      ENDIF.

      IF ls_serial-itemno IS NOT INITIAL.
        READ TABLE mt_dlv_items INTO DATA(ls_dlv_item)
          WITH KEY itemno = ls_serial-itemno.
        IF sy-subrc = 0.
          NEW /scwm/cl_ui_stock_fields( )->get_matkey_by_id(
            EXPORTING
              iv_matid = ls_dlv_item-product-productid
            IMPORTING
              ev_matnr = <ls_serial>-matnr ).
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD insert_serials.
*    INSERT zewm_t_serial FROM TABLE it_serial.
*    IF sy-subrc <> 0.
*      MESSAGE e225(/scwm/serial).
*    ENDIF.
*
*    IF iv_commit = abap_true.
*      COMMIT WORK AND WAIT.
*    ENDIF.
  ENDMETHOD.

  METHOD update_serials.
*    UPDATE zewm_t_serial FROM TABLE it_serial.
*    IF sy-subrc <> 0.
*      MESSAGE e225(/scwm/serial).
*    ENDIF.
*
*    IF iv_commit = abap_true.
*      COMMIT WORK AND WAIT.
*    ENDIF.
  ENDMETHOD.

  METHOD delete_serials_for_hu.
*    DELETE FROM zewm_t_serial WHERE lgnum = lcl_controller=>sv_lgnum AND huident = iv_huident.
*    IF sy-subrc <> 0.
*      MESSAGE e225(/scwm/serial).
*    ENDIF.
*
*    IF iv_commit = abap_true.
*      COMMIT WORK AND WAIT.
*    ENDIF.
  ENDMETHOD.

ENDCLASS.
