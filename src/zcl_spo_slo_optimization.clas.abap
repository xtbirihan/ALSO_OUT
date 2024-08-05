CLASS zcl_spo_slo_optimization DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-DATA:
      BEGIN OF ms_report.
    CLASS-DATA: ucomm            TYPE sy-ucomm.
    CLASS-DATA: error            TYPE char1.
    CLASS-DATA: alv              TYPE bapirettab."ztt_who.
    CLASS-DATA: data             TYPE /scwm/tt_wo_det_mon.
    CLASS-DATA: r_alv            TYPE REF TO cl_salv_table.
    CLASS-DATA: r_columns        TYPE REF TO cl_salv_columns_table.
    CLASS-DATA: r_column         TYPE REF TO cl_salv_column.
    CLASS-DATA: r_events         TYPE REF TO cl_salv_events_table.
    CLASS-DATA: r_selections     TYPE REF TO cl_salv_selections.
    CLASS-DATA:
      END OF ms_report .
    CLASS-DATA mv_lgnum TYPE /scwm/lgnum .
    DATA:
      mt_cust       TYPE STANDARD TABLE OF ztout_wo_optmztn,
      mt_bapirettab TYPE bapirettab.

    TYPES: BEGIN OF ty_reduced_quan,
             rdoccat TYPE  /scwm/de_doccat,
             rdocid  TYPE  /scwm/de_docid,
             ritmid  TYPE  /scwm/de_itmid,
             vsolm   TYPE  /scwm/ltap_vsolm,
             nistm   TYPE  /scwm/ltap_nistm,
           END OF ty_reduced_quan,
           tty_reduced_quan TYPE STANDARD TABLE OF ty_reduced_quan WITH EMPTY KEY.

    METHODS constructor
      IMPORTING
        VALUE(iv_lgnum) TYPE /scwm/lgnum .
*
    METHODS fetch_data .
    METHODS who_merge
      IMPORTING
        it_ordim_o    TYPE zif_whse_order=>tty_to OPTIONAL
        it_ordim_c    TYPE zif_whse_order=>tty_to OPTIONAL
      CHANGING
        VALUE(ct_who) TYPE STANDARD TABLE.

    METHODS initialize_alv
      CHANGING
        VALUE(ct_table) TYPE STANDARD TABLE.

    METHODS get_log_messages
      RETURNING VALUE(rt_bapiret) TYPE bapirettab.
    METHODS save_log_messages
      IMPORTING it_bapiret TYPE bapirettab.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_customizing .
    METHODS fill_selection_parameters
      RETURNING
        VALUE(rt_selection_parameters) TYPE ztt_select_option .
    METHODS time_conversion
      IMPORTING
        !iv_time_in  TYPE auszt
        !iv_unit_in  TYPE meins
        !iv_unit_out TYPE meins
      EXPORTING
        !ev_time_out TYPE auszt .
    METHODS after_merge
      IMPORTING
        !it_merged_who TYPE /scwm/tt_who.
    METHODS reduce_delivery_quan
      IMPORTING
        !it_to               TYPE /scwm/tt_to_det_mon
      EXPORTING
        !et_reduced_delivery TYPE tty_reduced_quan .
    METHODS get_delivery_details
      IMPORTING
        !it_reduced_delivery     TYPE tty_reduced_quan
      RETURNING
        VALUE(et_delivery_items) TYPE /scwm/dlv_item_out_prd_tab .
    METHODS apply_rules
      IMPORTING
        !it_who              TYPE /scwm/tt_who
        !it_to               TYPE /scwm/tt_to_det_mon
        !it_reduced_delivery TYPE tty_reduced_quan
        !it_delivery_items   TYPE /scwm/dlv_item_out_prd_tab .
    METHODS create_who
      IMPORTING
        !iv_who TYPE /scwm/de_who
        !iv_wcr TYPE /scwm/de_wcr .
    METHODS get_date_time
      IMPORTING
        !it_to              TYPE /scwm/tt_to_det_mon
      EXPORTING
        !ev_wave_completion TYPE tzntstmps
        !ev_wt_creation     TYPE tzntstmps .
    METHODS get_timestamp
      IMPORTING
        !iv_wcr                   TYPE /scwm/de_wcr
      EXPORTING
        !ev_tstmp_wave_completion TYPE tzntstmpl
        !ev_tstmp_wt_creation     TYPE tzntstmpl .
    METHODS enable_layout_settings .
    METHODS optimize_column_width .
    METHODS set_toolbar .
    METHODS display_settings .
    METHODS set_hotspot_click .
    METHODS display_alv .

    METHODS fill_bapiret_messages_to_alv
      IMPORTING
        it_bapiret TYPE bapirettab OPTIONAL
        is_bapiret TYPE bapiret2 OPTIONAL.

    METHODS spool_message_details
      CHANGING ct_bapiret TYPE bapirettab.


ENDCLASS.



CLASS ZCL_SPO_SLO_OPTIMIZATION IMPLEMENTATION.


  METHOD after_merge.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    NEW zcl_whse_order(
      iv_lgnum = me->mv_lgnum
      )->to_data_select(
      EXPORTING
        it_who = it_merged_who
      IMPORTING
        et_to  = DATA(lt_to)
    ).

    me->reduce_delivery_quan(
      EXPORTING
        it_to               = lt_to
      IMPORTING
        et_reduced_delivery = DATA(lt_reduced_delivery)
    ).

    DATA(lt_delivery_items) = me->get_delivery_details( it_reduced_delivery = lt_reduced_delivery ).

    me->apply_rules(
      EXPORTING
        it_who              = it_merged_who
        it_to               = lt_to
        it_reduced_delivery = lt_reduced_delivery
        it_delivery_items   = lt_delivery_items
    ).
  ENDMETHOD.                                             "#EC CI_VALPAR


  METHOD apply_rules.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA: lv_rule_a TYPE abap_boolean,
          lv_rule_b TYPE abap_boolean,
          lv_rule_c TYPE abap_boolean.
    DATA: lt_to          TYPE STANDARD TABLE OF /scwm/s_to_det_mon
                    WITH NON-UNIQUE SORTED KEY who COMPONENTS who,
          lt_filtered_to TYPE  /scwm/tt_to_det_mon.

    lt_to = it_to.

    LOOP AT it_who ASSIGNING FIELD-SYMBOL(<ls_who>).
      lt_filtered_to = FILTER #( lt_to USING KEY who WHERE who = <ls_who>-who ).

      me->reduce_delivery_quan(
        EXPORTING
          it_to               = lt_filtered_to
        IMPORTING
          et_reduced_delivery = DATA(lt_reduced_delivery)
      ).

      DATA(lv_line) = lines( lt_reduced_delivery ).

      LOOP AT lt_reduced_delivery ASSIGNING FIELD-SYMBOL(<ls_reduced_delivery>).

        DATA(ls_total_reduced_delivery) = it_reduced_delivery[ rdocid = <ls_reduced_delivery>-rdocid
                                                               ritmid = <ls_reduced_delivery>-ritmid ].

        DATA(lv_quan) = VALUE #( it_delivery_items[ docid = ls_total_reduced_delivery-rdocid itemid = ls_total_reduced_delivery-ritmid ]-qty-qty OPTIONAL ).

        " a. the system should check if the sum of the quantities of the  WTs for a delivery in the WO is less than the delivery quantity – then the WO should stay blocked.
        IF ls_total_reduced_delivery-vsolm < lv_quan.
          "do nothing!
          lv_rule_a = abap_true.
          CLEAR: lv_rule_b, lv_rule_c.
          EXIT.
        ENDIF.

        " b.  there is more than one delivery in the WO and the quantity in the WTs for each delivery is equal to the delivery quantity,
        " i.e. the whole quantity of the delivery is in the WO then we should unblock the WO but only if either of the following is true:
        " i.  the WT with the earliest wave completion time taken from the wave is within the time interval for wave completion time or is in the past
        " ii.  the WT with the earlies WT creation time is within the time interval for WT creation time or is in the past

        IF lv_line > 1.
          IF ls_total_reduced_delivery-vsolm = lv_quan.
            lv_rule_b = abap_true.
          ELSE.
            lv_rule_b = abap_false.
            EXIT.
          ENDIF.
        ENDIF.

        " c.  there is only one delivery in the WO with its WTs quantity equal to the delivery quantity then we should pass it to WOCR “KSCH” but only if either of the following is true:
        " i.  the WT with the earliest wave completion time taken from the wave is within the time interval for wave completion time or is in the past
        " ii.  the WT with the earlies WT creation time is within the time interval for WT creation time or is in the past

        IF lv_line = 1.
          IF ls_total_reduced_delivery-vsolm = lv_quan.
            lv_rule_c = abap_true.
          ELSE.
            lv_rule_c = abap_false.
            EXIT.
          ENDIF.
        ENDIF.

        CLEAR: lv_quan, ls_total_reduced_delivery.
      ENDLOOP.

      IF lv_rule_a EQ abap_true.
        SET UPDATE TASK LOCAL.
        CALL FUNCTION '/SCWM/WHO_HOLD_UNHOLD'
          EXPORTING
            iv_lgnum = me->mv_lgnum
            iv_hold  = abap_true
            it_who   = VALUE /scwm/tt_whoid( ( who = <ls_who>-who ) )
          EXCEPTIONS
            OTHERS   = 99.

        IF sy-subrc EQ 0.
          me->fill_bapiret_messages_to_alv(
            EXPORTING
              is_bapiret = VALUE #( type   = zif_whse_order=>message_severity-success
                                    id     = zif_whse_order=>who_block_successfull-msgid
                                    number = zif_whse_order=>who_block_successfull-msgno
                                    message_v1 = | { <ls_who>-who } | ) ).
        ELSE.
          me->fill_bapiret_messages_to_alv(
            EXPORTING
              is_bapiret = VALUE #( type   = zif_whse_order=>message_severity-error
                                    id     = zif_whse_order=>who_block_error-msgid
                                    number = zif_whse_order=>who_block_error-msgno
                                    message_v1 = | { <ls_who>-who } | ) ).
        ENDIF.
      ENDIF.


      IF lv_rule_b EQ abap_true.

        me->get_date_time(
          EXPORTING
            it_to              = lt_filtered_to
          IMPORTING
            ev_wave_completion = DATA(lv_wave_completion)
            ev_wt_creation     = DATA(lv_wt_creation)
        ).

        me->get_timestamp(
          EXPORTING
            iv_wcr                   = <ls_who>-wcr
          IMPORTING
            ev_tstmp_wave_completion = DATA(lv_tstmp_wave_completion)
            ev_tstmp_wt_creation     = DATA(lv_tstmp_wt_creation)
        ).

        IF lv_wave_completion <= lv_tstmp_wave_completion OR
           lv_wt_creation <= lv_tstmp_wt_creation  .
          SET UPDATE TASK LOCAL.
          CALL FUNCTION '/SCWM/WHO_HOLD_UNHOLD'
            EXPORTING
              iv_lgnum  = me->mv_lgnum
              iv_unhold = abap_true
              it_who    = VALUE /scwm/tt_whoid( ( who = <ls_who>-who ) )
            EXCEPTIONS
              OTHERS    = 99.
          IF sy-subrc EQ 0.
            me->fill_bapiret_messages_to_alv(
              EXPORTING
                is_bapiret = VALUE #( type   = zif_whse_order=>message_severity-success
                                      id     = zif_whse_order=>who_unblock_successfull-msgid
                                      number = zif_whse_order=>who_unblock_successfull-msgno
                                      message_v1 = | { <ls_who>-who } | ) ).
          ELSE.
            me->fill_bapiret_messages_to_alv(
              EXPORTING
                is_bapiret = VALUE #( type   = zif_whse_order=>message_severity-error
                                      id     = zif_whse_order=>who_unblock_error-msgid
                                      number = zif_whse_order=>who_unblock_error-msgno
                                      message_v1 = | { <ls_who>-who } | ) ).
          ENDIF.


        ENDIF.
      ENDIF.


      IF lv_rule_c EQ abap_true.

        me->get_date_time(
          EXPORTING
            it_to              = lt_filtered_to
          IMPORTING
            ev_wave_completion = lv_wave_completion
            ev_wt_creation     = lv_wt_creation
        ).

        me->get_timestamp(
          EXPORTING
            iv_wcr                   = <ls_who>-wcr
          IMPORTING
            ev_tstmp_wave_completion = lv_tstmp_wave_completion
            ev_tstmp_wt_creation     = lv_tstmp_wt_creation
        ).

        IF lv_wave_completion <= lv_tstmp_wave_completion OR
           lv_wt_creation <= lv_tstmp_wt_creation.

          me->create_who(
            EXPORTING
              iv_who = <ls_who>-who
              iv_wcr = zif_whse_order=>wo_cr-kshc ).

        ENDIF.
      ENDIF.
      IF lv_rule_a IS INITIAL AND
         lv_rule_b IS INITIAL AND
         lv_rule_c IS INITIAL.

        me->fill_bapiret_messages_to_alv(
          EXPORTING
            is_bapiret = VALUE #( type   = zif_whse_order=>message_severity-error
                                  id     = zif_whse_order=>who_rule_error-msgid
                                  number = zif_whse_order=>who_rule_error-msgno ) ).

      ENDIF.

      CLEAR: lt_filtered_to, lt_reduced_delivery,
             lv_line, lv_tstmp_wave_completion, lv_tstmp_wt_creation,
             lv_rule_a, lv_rule_b, lv_rule_c.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    me->mv_lgnum = iv_lgnum.
    me->get_customizing( ).
  ENDMETHOD.


  METHOD create_who.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA: lt_range      TYPE rsds_frange_t,
          lt_ordim_o    TYPE /scwm/tt_ordim_o,
          ls_who_to     TYPE /scwm/s_ordim_o_int,
          lt_who_to     TYPE /scwm/tt_ordim_o_int,
          lt_to_new     TYPE /scwm/tt_ordim_o_int,
          lt_who_new    TYPE /scwm/tt_who_int,
          ls_who_new    TYPE /scwm/s_who_int,
          lt_whohu_new  TYPE /scwm/tt_whohu_int,
          lt_change_att TYPE /scwm/tt_to_change_att,
          lt_bapiret    TYPE bapirettab.

    APPEND VALUE rsds_frange(
           fieldname = zif_whse_order=>wo_mapping_fieldname-who
           selopt_t  = VALUE rsds_selopt_t( ( sign   = wmegc_sign_inclusive
                                              option = wmegc_option_eq
                                              low    = iv_who ) )
    ) TO lt_range.

    SET UPDATE TASK LOCAL.
    TRY.
        CALL FUNCTION '/SCWM/WHO_GET'
          EXPORTING
            iv_lgnum    = me->mv_lgnum
            iv_to       = abap_true
            iv_lock_who = abap_true
            iv_lock_to  = abap_true
            it_whorange = lt_range
          IMPORTING
            et_ordim_o  = lt_ordim_o.
        ##NO_HANDLER
      CATCH /scwm/cx_core.
    ENDTRY.


    LOOP AT lt_ordim_o ASSIGNING FIELD-SYMBOL(<ls_ordim_o>).
      MOVE-CORRESPONDING <ls_ordim_o> TO ls_who_to.
      CLEAR: ls_who_to-who, ls_who_to-wcr, ls_who_to-huid.
      APPEND ls_who_to TO lt_who_to.
    ENDLOOP.

    SET UPDATE TASK LOCAL.
    CALL FUNCTION '/SCWM/WHO_CREATE'
      EXPORTING
        iv_lgnum       = me->mv_lgnum
        iv_wcr         = iv_wcr
        iv_set_on_hold = ' '
        it_to          = lt_who_to
      IMPORTING
        et_to          = lt_to_new
        et_who         = lt_who_new
        et_whohu       = lt_whohu_new
      EXCEPTIONS
        input_error    = 1
        OTHERS         = 2.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_whse_order
        EXPORTING
          textid = zif_whse_order=>who_creation_error.
    ELSE.

      GET TIME STAMP FIELD DATA(lv_ts).

      ls_who_new-created_at = lv_ts.
      ls_who_new-created_by = sy-uname.
      MODIFY lt_who_new FROM ls_who_new TRANSPORTING created_at created_by WHERE updkz = zif_whse_order=>update_indicator-insert.

      CHECK NOT lt_whohu_new IS INITIAL.
      CALL FUNCTION '/SCWM/WHO_DB_UPDATE'
        IN UPDATE TASK
        EXPORTING
          it_who   = lt_who_new
          it_whohu = lt_whohu_new.

*     update TOs with new WHO number
      LOOP AT lt_to_new ASSIGNING FIELD-SYMBOL(<ls_to>).
        APPEND VALUE #( tanum = <ls_to>-tanum
                        tt_changed =
                          VALUE /scwm/tt_changed(
                          ( fieldname = zif_whse_order=>wo_att_fieldname-who            value_c = <ls_to>-who )
                          ( fieldname = zif_whse_order=>wo_att_fieldname-huid           value_c = <ls_to>-huid )
                          ( fieldname = zif_whse_order=>wo_att_fieldname-whoseq         value_c = <ls_to>-whoseq )
                          ( fieldname = zif_whse_order=>wo_att_fieldname-wcr            value_c = zif_whse_order=>wo_cr-kshc )
                          ( fieldname = zif_whse_order=>wo_att_fieldname-dlogpos_ext_wt value_c = <ls_to>-dlogpos_ext_wt )
                          ( fieldname = zif_whse_order=>wo_att_fieldname-dstgrp         value_c = <ls_to>-dstgrp )
                          )  ) TO lt_change_att.
      ENDLOOP.
      CALL FUNCTION '/SCWM/TO_CHANGE_ATT'
        EXPORTING
          iv_lgnum       = me->mv_lgnum
          iv_simulate    = VALUE /scwm/simulate( )
          iv_update_task = 'X'
          iv_commit_work = space
          it_change_att  = lt_change_att
        IMPORTING
          et_bapiret     = lt_bapiret.
      me->fill_bapiret_messages_to_alv(
        EXPORTING
          it_bapiret = lt_bapiret
      ).
      LOOP AT lt_bapiret ASSIGNING FIELD-SYMBOL(<ls_bapiret>) WHERE type CA 'EAX'.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0 .
        CALL FUNCTION 'DB_ROLLBACK'.
      ELSE.
        COMMIT WORK AND WAIT.
        CALL METHOD /scwm/cl_tm=>cleanup( ).
        LOOP AT lt_who_new ASSIGNING FIELD-SYMBOL(<ls_who>) WHERE updkz EQ  zif_whse_order=>update_indicator-insert.
          me->fill_bapiret_messages_to_alv(
            EXPORTING
              is_bapiret = VALUE #( type   = zif_whse_order=>message_severity-success
                                    id     = zif_whse_order=>who_creation_rule_successfull-msgid
                                    number = zif_whse_order=>who_creation_rule_successfull-msgno
                                    message_v1 = | { <ls_who>-who } | ) ).
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD display_alv.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    ms_report-r_alv->refresh( refresh_mode = if_salv_c_refresh=>full ).
    ms_report-r_alv->display( ).
  ENDMETHOD.


  METHOD display_settings.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA display_settings TYPE REF TO cl_salv_display_settings.
    DATA: lv_tanim TYPE text70.
    DATA: lv_line TYPE i.
    lv_line  = lines( ms_report-alv ).
    lv_tanim = TEXT-001 && | --> | && |{ lv_line } | && TEXT-002.

    display_settings =  ms_report-r_alv->get_display_settings( ).
    display_settings->set_striped_pattern( if_salv_c_bool_sap=>true ).
    display_settings->set_list_header( lv_tanim ).
  ENDMETHOD.


  METHOD enable_layout_settings.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA layout_settings TYPE REF TO cl_salv_layout.
    DATA layout_key      TYPE salv_s_layout_key.


    layout_settings =  ms_report-r_alv->get_layout( ).
    layout_key-report = sy-repid.
    layout_settings->set_key( layout_key ).
    layout_settings->set_save_restriction( if_salv_c_layout=>restrict_none ).

    ms_report-r_selections =  ms_report-r_alv->get_selections( ).
    ms_report-r_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).
  ENDMETHOD.


  METHOD fetch_data.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    NEW zcl_whse_order(
      iv_lgnum = me->mv_lgnum
      it_selection_parameters = me->fill_selection_parameters( )
      )->wo_data_select(
      IMPORTING
        et_who     = ms_report-data
        et_ordim_o = DATA(lt_ordim_o)
        et_ordim_c = DATA(lt_ordim_c)
    ).

    me->who_merge(
      EXPORTING
        it_ordim_o = lt_ordim_o
        it_ordim_c = lt_ordim_c
      CHANGING
        ct_who     = ms_report-data
    ).
    me->initialize_alv( CHANGING ct_table = ms_report-alv ).
    me->display_alv( ).
  ENDMETHOD.


  METHOD fill_bapiret_messages_to_alv.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    LOOP AT it_bapiret INTO DATA(ls_bapiret).
      APPEND VALUE symsg( msgid = ls_bapiret-id
                                   msgty = ls_bapiret-type
                                   msgno = ls_bapiret-number
                                   msgv1 = ls_bapiret-message_v1
                                   msgv2 = ls_bapiret-message_v2
                                   msgv3 = ls_bapiret-message_v3
                                   msgv4 = ls_bapiret-message_v4 ) TO ms_report-alv.
    ENDLOOP.

    IF is_bapiret IS NOT INITIAL.
      APPEND VALUE symsg( msgid = is_bapiret-id
                                   msgty = is_bapiret-type
                                   msgno = is_bapiret-number
                                   msgv1 = is_bapiret-message_v1
                                   msgv2 = is_bapiret-message_v2
                                   msgv3 = is_bapiret-message_v3
                                   msgv4 = is_bapiret-message_v4 ) TO ms_report-alv.
    ENDIF.

    DATA: lt_filter_tab TYPE SORTED TABLE OF bapi_mtype
                        WITH UNIQUE KEY table_line.


    lt_filter_tab = VALUE #( ( zif_whse_order=>message_severity-error ) ).
    DATA(lt_bapiret) = FILTER #( it_bapiret IN lt_filter_tab
                               WHERE type = table_line ).
    IF is_bapiret-type EQ zif_whse_order=>message_severity-error.
      APPEND is_bapiret TO mt_bapirettab.
    ENDIF.

    IF lt_bapiret IS NOT INITIAL.
      APPEND LINES OF lt_bapiret TO mt_bapirettab.
    ENDIF.
  ENDMETHOD.


  METHOD fill_selection_parameters.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    rt_selection_parameters = VALUE #(
      ( field = zif_whse_order=>wo_mapping_fieldname-wostho
        select_options = VALUE #( ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = abap_true ) ) ) ).

    rt_selection_parameters  = VALUE #( BASE rt_selection_parameters
                                      ( field = zif_whse_order=>wo_mapping_fieldname-wcr
                                        select_options = VALUE #( FOR ls_cust IN mt_cust (
                                        sign = wmegc_sign_inclusive option = wmegc_option_eq low = ls_cust-wcr
                                        ) ) ) ).

  ENDMETHOD.


  METHOD get_customizing.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    SELECT * FROM ztout_wo_optmztn
             INTO TABLE @mt_cust
             WHERE lgnum = @me->mv_lgnum.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_whse_order
        EXPORTING
          textid = zif_whse_order=>wo_no_customizing_found.
    ENDIF.
  ENDMETHOD.


  METHOD get_date_time.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA: lt_wavehdr  TYPE /scwm/tt_wavehdr_int,
          lt_bapiret  TYPE bapiret2_t.

    CALL FUNCTION '/SCWM/WAVE_SELECT_EXT'
      EXPORTING
        iv_lgnum    = me->mv_lgnum
        iv_rdoccat  = wmegc_doccat_pdo
        it_wave     = VALUE /scwm/tt_wave_no( FOR ls_to IN it_to ( lgnum = me->mv_lgnum wave = ls_to-wave ) )
        iv_flglock  = abap_true
      IMPORTING
        et_wavehdr  = lt_wavehdr
        et_bapiret  = lt_bapiret.

    me->fill_bapiret_messages_to_alv(
      EXPORTING
        it_bapiret = lt_bapiret
    ).

    SORT lt_wavehdr BY load_comp_dt.
    ev_wave_completion = VALUE #( lt_wavehdr[ 1 ]-load_comp_dt OPTIONAL ).

    DATA(lt_filtered_to) = it_to.
    SORT lt_filtered_to BY created_at.
    ev_wt_creation = lt_filtered_to[ 1 ]-created_at.


  ENDMETHOD.


  METHOD get_delivery_details.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA(ls_read_options) =
      VALUE /scwm/dlv_query_contr_str(
        data_retrival_only      = abap_true
        mix_in_object_instances = abap_true
        item_part_select        = abap_true  ).

    DATA(lt_docid) = VALUE /scwm/dlv_docid_item_tab( FOR ls_delivery IN it_reduced_delivery
                                                  (  doccat = ls_delivery-rdoccat
                                                     docid  = ls_delivery-rdocid
                                                     itemid = ls_delivery-ritmid  ) ).

    DATA(lo_bom_prd) = /scwm/cl_dlv_management_prd=>get_instance( ).

    TRY.
        CALL METHOD lo_bom_prd->query
          EXPORTING
            it_docid        = lt_docid
            is_include_data = VALUE #( )
            is_read_options = ls_read_options
          IMPORTING
            et_items        = et_delivery_items.

      CATCH /scdl/cx_delivery.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDTRY.
    SORT et_delivery_items BY docid itemid.
  ENDMETHOD.


  METHOD get_log_messages.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    rt_bapiret = me->mt_bapirettab.
  ENDMETHOD.


  METHOD get_timestamp.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA(ls_cust) = VALUE #( me->mt_cust[ lgnum = me->mv_lgnum wcr = iv_wcr ] OPTIONAL ).

    GET TIME STAMP FIELD DATA(lv_tstmp_wave_completion).

    me->time_conversion(
      EXPORTING
        iv_time_in  = CONV #( ls_cust-wave_completion_time )
        iv_unit_in  = CONV #( ls_cust-time_unit+0(1) )
        iv_unit_out = zif_whse_order=>wo_time_conversion-seconds
      IMPORTING
        ev_time_out = DATA(lv_sec)
    ).
    TRY.
        cl_abap_tstmp=>add(
          EXPORTING
            tstmp   = lv_tstmp_wave_completion
            secs    = lv_sec
          RECEIVING
            r_tstmp = ev_tstmp_wave_completion
        ).
        ##NO_HANDLER
      CATCH cx_parameter_invalid_type.
        ##NO_HANDLER
      CATCH cx_parameter_invalid_range.
    ENDTRY.


    GET TIME STAMP FIELD DATA(lv_tstmp_wt_creation).

    me->time_conversion(
      EXPORTING
        iv_time_in  = CONV #( ls_cust-wt_creation_time )
        iv_unit_in  = CONV #( ls_cust-time_unit+0(1) )
        iv_unit_out = zif_whse_order=>wo_time_conversion-seconds
      IMPORTING
        ev_time_out = lv_sec
    ).
    TRY.
        cl_abap_tstmp=>add(
          EXPORTING
            tstmp   = lv_tstmp_wt_creation
            secs    = lv_sec
          RECEIVING
            r_tstmp = ev_tstmp_wt_creation ).
        ##NO_HANDLER
      CATCH cx_parameter_invalid_type.  " Parameter with Invalid Type
        ##NO_HANDLER
      CATCH cx_parameter_invalid_range. " Parameter with invalid value range
    ENDTRY.

  ENDMETHOD.


  METHOD initialize_alv.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    me->save_log_messages( me->get_log_messages( ) ).

    me->spool_message_details(
      CHANGING
        ct_bapiret = ct_table
    ).

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = ms_report-r_alv
          CHANGING
            t_table      = ms_report-alv ).

        ms_report-r_columns =  ms_report-r_alv->get_columns( ).
        me->enable_layout_settings( ).
        me->optimize_column_width( ).
        me->set_toolbar( ).
        me->display_settings( ).
        me->set_hotspot_click( ).
        ##NO_HANDLER
      CATCH cx_salv_msg.
        " error handling
    ENDTRY.
  ENDMETHOD.                                             "#EC CI_VALPAR


  METHOD optimize_column_width.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    ms_report-r_columns->set_optimize( ).
  ENDMETHOD.


  METHOD reduce_delivery_quan.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    et_reduced_delivery = VALUE tty_reduced_quan(
                 FOR GROUPS <group_key> OF <g> IN it_to GROUP BY ( rdoccat = <g>-rdoccat rdocid = <g>-rdocid  )
                 LET coll_line = REDUCE #( INIT line TYPE ty_reduced_quan FOR <m> IN GROUP <group_key>
                                           NEXT line-rdoccat = <m>-rdoccat
                                                line-rdocid  = <m>-rdocid
                                                line-ritmid  = <m>-ritmid
                                                line-vsolm = line-vsolm + <m>-vsolm
                                                line-nistm = line-nistm + <m>-nistm )
                                 IN ( coll_line ) ) .

  ENDMETHOD.


  METHOD save_log_messages.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA: ls_log TYPE bal_s_log.

    ls_log-object    = zif_whse_order=>log-object.
    ls_log-subobject = zif_whse_order=>log-subobject.
    ls_log-extnumber = me->mv_lgnum.
    ls_log-aldate = sy-datum.
    ls_log-altime = sy-uzeit.
    ls_log-aluser = sy-uname.
    ls_log-alprog = sy-repid.

    CALL FUNCTION '/SCWM/APP_LOG_WRITE_V2'
      EXPORTING
        is_log     = ls_log
        it_bapiret = it_bapiret.
  ENDMETHOD.


  METHOD set_hotspot_click.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    ms_report-r_events =  ms_report-r_alv->get_event( ).
    DATA: event_handler TYPE REF TO lcl_handle_events.
    CREATE OBJECT event_handler.
    SET HANDLER event_handler->on_link_click   FOR  ms_report-r_events.
    SET HANDLER event_handler->on_user_command FOR  ms_report-r_events.
  ENDMETHOD.


  METHOD set_toolbar.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA functions TYPE REF TO cl_salv_functions_list.
    functions =  ms_report-r_alv->get_functions( ).
    functions->set_all( ).

    ms_report-r_alv->set_screen_status(
      pfstatus      = zif_whse_order=>wo_report-pfstatus
      report        = zif_whse_order=>wo_report-report
      set_functions = ms_report-r_alv->c_functions_all ).
  ENDMETHOD.


  METHOD spool_message_details.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    LOOP AT ct_bapiret ASSIGNING FIELD-SYMBOL(<ls_bapiret>).
      CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
        EXPORTING
          id         = <ls_bapiret>-id
          number     = <ls_bapiret>-number
          language   = sy-langu
          textformat = zif_whse_order=>log-textformat
          message_v1 = <ls_bapiret>-message_v1
          message_v2 = <ls_bapiret>-message_v2
          message_v3 = <ls_bapiret>-message_v3
          message_v4 = <ls_bapiret>-message_v4
        IMPORTING
          message    = <ls_bapiret>-message.
    ENDLOOP.
  ENDMETHOD.


  METHOD time_conversion.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA: lv_work_time           TYPE f.

    IF iv_unit_in <> zif_whse_order=>wo_time_conversion-seconds AND
       iv_unit_in <> zif_whse_order=>wo_time_conversion-minutes AND
       iv_unit_in <> zif_whse_order=>wo_time_conversion-hours.

      RAISE EXCEPTION TYPE zcx_whse_order
        EXPORTING
          textid = zif_whse_order=>invalid_time_unit.
    ENDIF.


    IF iv_unit_out <> zif_whse_order=>wo_time_conversion-seconds AND
       iv_unit_out <> zif_whse_order=>wo_time_conversion-minutes AND
       iv_unit_out <> zif_whse_order=>wo_time_conversion-hours.

      RAISE EXCEPTION TYPE zcx_whse_order
        EXPORTING
          textid = zif_whse_order=>invalid_time_unit.
    ENDIF.

" ... convert all time back to seconds
    CASE iv_unit_in.
      WHEN zif_whse_order=>wo_time_conversion-seconds.
        lv_work_time = iv_time_in.

      WHEN zif_whse_order=>wo_time_conversion-hours.
        lv_work_time = iv_time_in * 3600.    "Hours to seconds

      WHEN zif_whse_order=>wo_time_conversion-minutes.
        lv_work_time = iv_time_in * 60.      "Mins to seconds

    ENDCASE.

" ... now return time in UNIT_OUT
    CASE iv_unit_out.
      WHEN zif_whse_order=>wo_time_conversion-seconds.

        ev_time_out = lv_work_time.

      WHEN zif_whse_order=>wo_time_conversion-hours.
        ev_time_out  = lv_work_time / 3600.    "Hours to seconds

      WHEN zif_whse_order=>wo_time_conversion-minutes.
        ev_time_out = lv_work_time / 60.      "Mins to seconds

    ENDCASE.


  ENDMETHOD.


  METHOD who_merge.
********************************************************************
*& Key          : TBIRIHAN-Jan 24, 2024
*& Request No.  : GAP-063 SPO-SLO Warehouse Order Optimization
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA: lt_bapiret TYPE bapirettab,
          lt_who     TYPE /scwm/tt_who.
    DATA: lt_filter_tab  TYPE SORTED TABLE OF /scwm/who-wcr
                     WITH UNIQUE KEY table_line.

    DATA(lt_whoid) = VALUE /scwm/tt_whoid( FOR <ls_cust> IN ct_who
                                           LET ls_who = CORRESPONDING /scwm/who( <ls_cust> ) IN
                                           ( who =  ls_who-who ) ).
    IF lt_whoid IS INITIAL.
      RAISE EXCEPTION TYPE zcx_whse_order
        EXPORTING
          textid = zif_whse_order=>whoid_not_found.
    ENDIF.

    SET UPDATE TASK LOCAL.
    CALL FUNCTION 'Z_WHO_MERGE'
      EXPORTING
        iv_lgnum       = me->mv_lgnum
        iv_wcr         = VALUE /scwm/de_wcr( )
        iv_reason_code = VALUE char4( )
        iv_update      = abap_true
        iv_commit      = abap_true
        it_who         = lt_whoid
      IMPORTING
        et_bapiret     = lt_bapiret
        et_who         = lt_who.

    LOOP AT lt_bapiret TRANSPORTING NO FIELDS WHERE type CA 'EAX'.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      COMMIT WORK AND WAIT.
    ENDIF.

    me->fill_bapiret_messages_to_alv(
      EXPORTING
        it_bapiret = lt_bapiret
    ).

    lt_filter_tab = VALUE #( FOR ls_cust IN mt_cust ( ls_cust-wcr ) ).
    lt_who = FILTER #( lt_who IN lt_filter_tab
                              WHERE wcr = table_line ).
    IF lt_who IS INITIAL.
      RETURN.
    ENDIF.

    me->after_merge(
        it_merged_who = lt_who ).

  ENDMETHOD.                                             "#EC CI_VALPAR
ENDCLASS.
