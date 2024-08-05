CLASS zcl_out_who_flt_il_spo_slo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /scwm/if_ex_who_flt_il .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF gs_spo_slo,
        zznosplo TYPE zde_check_spo_slo VALUE '1',
        zznoslo  TYPE zde_check_spo_slo VALUE '2',
      END OF gs_spo_slo .

    CLASS-METHODS read_mat_lgnum
      IMPORTING
        !iv_lgnum           TYPE /scwm/lgnum
        !iv_matid           TYPE /scwm/de_matid
        !iv_entitled        TYPE /scwm/de_entitled
      RETURNING
        VALUE(rs_mat_lgnum) TYPE /scwm/s_material_lgnum .
    CLASS-METHODS find_vas_orders
      IMPORTING
        !iv_lgnum           TYPE /scwm/lgnum
        !iv_docno           TYPE /scwm/sp_docno_int
      RETURNING
        VALUE(rv_vas_exist) TYPE abap_bool .
    CLASS-METHODS get_open_wt_diff_areawho
      IMPORTING
        !iv_lgnum    TYPE /scwm/lgnum
        !iv_docid    TYPE /scdl/dl_docid
        !iv_areawho  TYPE /scwm/de_whoaa
      RETURNING
        VALUE(rt_to) TYPE /scwm/tt_to_det_mon .
    CLASS-METHODS is_putwall
      IMPORTING
        VALUE(it_wt_areawho) TYPE zcl_out_who_wt_areawho=>tt_wt_areawho
        VALUE(iv_docid)      TYPE /scdl/dl_docid
        VALUE(iv_ritmid)     TYPE /scwm/de_itmid
        VALUE(iv_areawho)    TYPE /scwm/de_whoaa
      RETURNING
        VALUE(rv_putwall)    TYPE abap_boolean .
    CLASS-METHODS read_delivery
      IMPORTING
        it_to      TYPE /scwm/tt_ordim_o_int
      EXPORTING
        et_items   TYPE /scwm/dlv_item_out_prd_tab
        et_headers TYPE /scwm/dlv_header_out_prd_tab.
ENDCLASS.



CLASS ZCL_OUT_WHO_FLT_IL_SPO_SLO IMPLEMENTATION.


  METHOD /scwm/if_ex_who_flt_il~filter_il.
**********************************************************************
*& Key           : RM-230407
*& Request No.   : GAP 57 Outbound: Custom Warehouse Order Creation
*&                 Rule filters for SPO/SLO flows
**********************************************************************
*& Description (short)
*&
**********************************************************************
    DATA:
      lt_dlv_headers  TYPE /scwm/dlv_header_out_prd_tab,
      lt_dlv_items    TYPE /scwm/dlv_item_out_prd_tab,
      lv_to_qtys      TYPE /scwm/ltap_vsolm,
      lv_dlv_qtys     TYPE /scdl/dl_quantity,
      lo_areawho      TYPE REF TO zcl_out_who_wt_areawho.

    IF zcl_switch=>get_switch_state( iv_lgnum = /scwm/cl_tm=>sv_lgnum
                                     iv_devid = zif_switch_const=>c_zout_002 ) EQ abap_false.
      RETURN.
    ENDIF.

    " Read Z Customizing table
    DATA(ls_whowcrfltr) = NEW zcl_crud_ztout_whowcrfltr( )->select_single_by_lgnum_fltid(
                                                      iv_lgnum    = is_filter-lgnum
                                                      iv_filterid = is_filter-filterid ).

    IF ls_whowcrfltr IS INITIAL.
      RETURN.
    ENDIF.

    read_delivery(
      EXPORTING
        it_to      = it_to
      IMPORTING
        et_items   = lt_dlv_items
        et_headers = lt_dlv_headers ).

    lo_areawho = zcl_out_who_wt_areawho=>get_inst( ).

    LOOP AT it_to ASSIGNING FIELD-SYMBOL(<ls_to>).
      CLEAR: lv_to_qtys, lv_dlv_qtys.

      DATA(ls_dlv_item) = VALUE /scwm/dlv_item_out_prd_str( ).
      DATA(lv_dlv_itm_lines) = 0.
      LOOP AT lt_dlv_items ASSIGNING FIELD-SYMBOL(<ls_itm>) WHERE docid  = <ls_to>-rdocid.

        lv_dlv_itm_lines += 1.

        lv_dlv_qtys += <ls_itm>-qty-qty.

        IF lv_dlv_itm_lines = 1.
          ls_dlv_item = <ls_itm>.
        ENDIF.
      ENDLOOP.

      IF lv_dlv_itm_lines = 0.
        APPEND <ls_to> TO ct_to_failed.
        DELETE ct_to_success WHERE tanum = <ls_to>-tanum.
        CONTINUE.
      ENDIF.

      " Check Max Delivery Lines
      IF ls_whowcrfltr-max_dlv_lines IS NOT INITIAL AND lv_dlv_itm_lines > ls_whowcrfltr-max_dlv_lines. ""lines( lt_items ) > ls_whowcrfltr-max_dlv_lines.
        APPEND <ls_to> TO ct_to_failed.
        DELETE ct_to_success WHERE tanum = <ls_to>-tanum.
        CONTINUE.
      ENDIF.

      " Check Max Delivery Qty                        " lt_items[ 1 ]
      IF ls_whowcrfltr-max_dlv_qty IS NOT INITIAL AND ls_dlv_item-qty-qty > ls_whowcrfltr-max_dlv_qty.
        APPEND <ls_to> TO ct_to_failed.
        DELETE ct_to_success WHERE tanum = <ls_to>-tanum.
        CONTINUE.
      ENDIF.

      " Check for SPO/SLO
      " If material master data field ZZNOSPO not initial - Single Pice Order not allowed
      " If material master data field ZZNOSLO not initial - Single Line Order not allowed
      IF ls_whowcrfltr-check_spo_slo IS NOT INITIAL.
        DATA(rs_mat_lgnum) = read_mat_lgnum( iv_lgnum    = <ls_to>-lgnum
                                             iv_matid    = <ls_to>-matid
                                             iv_entitled = <ls_to>-entitled ).

        IF rs_mat_lgnum IS INITIAL.
          APPEND <ls_to> TO ct_to_failed.
          DELETE ct_to_success WHERE tanum = <ls_to>-tanum.
          CONTINUE.
        ENDIF.

        CASE ls_whowcrfltr-check_spo_slo.
          WHEN gs_spo_slo-zznoslo.
            IF rs_mat_lgnum-zz1_noslo_whd IS NOT INITIAL.
              APPEND <ls_to> TO ct_to_failed.
              DELETE ct_to_success WHERE tanum = <ls_to>-tanum.
              CONTINUE.
            ENDIF.

          WHEN gs_spo_slo-zznosplo.
            IF rs_mat_lgnum-zz1_nospo_whd IS NOT INITIAL.
              APPEND <ls_to> TO ct_to_failed.
              DELETE ct_to_success WHERE tanum = <ls_to>-tanum.
              CONTINUE.
            ENDIF.
        ENDCASE.
      ENDIF.

      " Check VAS Orders
      IF ls_whowcrfltr-check_vas IS NOT INITIAL.
        DATA(ls_vas_exist) = find_vas_orders( iv_lgnum = <ls_to>-lgnum
                                              iv_docno = <ls_to>-rdocno ).

        IF ls_vas_exist = abap_true.
          APPEND <ls_to> TO ct_to_failed.
          DELETE ct_to_success WHERE tanum = <ls_to>-tanum.
          CONTINUE.
        ENDIF.
      ENDIF.

      " Check Min Delivery Qty                                                    " lt_items[ 1 ]
      IF ls_whowcrfltr-min_dlv_qty IS NOT INITIAL AND ls_whowcrfltr-min_dlv_qty > ls_dlv_item-qty-qty.
        APPEND <ls_to> TO ct_to_failed.
        DELETE ct_to_success WHERE tanum = <ls_to>-tanum.
        CONTINUE.
      ENDIF.

      " Multi AA check
      IF ls_whowcrfltr-multi_aa_check IS NOT INITIAL.

        " Check if the tasks for one document, that are to be created during the wave release,
        " have different activity areas
        READ TABLE lo_areawho->mt_wt_areawho WITH KEY lgnum  = <ls_to>-lgnum
                                                      rdocno = <ls_to>-rdocno
                                             INTO DATA(ls_wt_areawho).

        IF sy-subrc = 0 AND ls_wt_areawho-diff_areawho = abap_true.
          " WTs with different WHO AA exist
          APPEND <ls_to> TO ct_to_failed.
          DELETE ct_to_success WHERE tanum = <ls_to>-tanum.
          CONTINUE.
        ENDIF.

        DATA(lt_to_diff_areawho) = get_open_wt_diff_areawho( iv_lgnum   = <ls_to>-lgnum
                                                             iv_docid   = <ls_to>-rdocid
                                                             iv_areawho = <ls_to>-areawho ).

        IF lt_to_diff_areawho IS NOT INITIAL.
          " Open WTs with different WHO AA exist
          APPEND <ls_to> TO ct_to_failed.
          DELETE ct_to_success WHERE tanum = <ls_to>-tanum.
          CONTINUE.
        ENDIF.

      ENDIF.

      " Check Delivery Partial Quantity
      DATA(lv_dlv_stat_der) = VALUE #( lt_dlv_headers[ docid = <ls_to>-rdocid
            ]-status[ status_type = /scdl/if_dl_status_c=>sc_t_planning_picking ]-status_value OPTIONAL ).

      IF ls_whowcrfltr-dlv_partial_qty_check IS NOT INITIAL AND
         lv_dlv_stat_der <> /scdl/if_dl_status_c=>sc_v_finished.

        LOOP AT it_to ASSIGNING FIELD-SYMBOL(<ls_to_sum>) WHERE rdocid = <ls_to>-rdocid.
          lv_to_qtys += <ls_to_sum>-vsolm.
        ENDLOOP.

        IF lv_dlv_qtys > lv_to_qtys.
          APPEND <ls_to> TO ct_to_failed.
          DELETE ct_to_success WHERE tanum = <ls_to>-tanum.
          CONTINUE.
        ENDIF.
      ENDIF.

*-->added by tbirihan 12.05.2023 GAP-49
      IF ls_whowcrfltr-putwall_check EQ abap_true.
        IF zcl_out_who_flt_il_spo_slo=>is_putwall( it_wt_areawho = lo_areawho->mt_wt_areawho
                                                   iv_docid      = <ls_to>-rdocid
                                                   iv_ritmid     = <ls_to>-ritmid
                                                   iv_areawho    = <ls_to>-areawho  ) EQ abap_true.
          APPEND <ls_to> TO ct_to_failed.
          DELETE ct_to_success WHERE tanum = <ls_to>-tanum.
          CONTINUE.
        ENDIF.
      ENDIF.
*--<added by tbirihan 12.05.2023 GAP-49
    ENDLOOP.

  ENDMETHOD.


  METHOD find_vas_orders.
**********************************************************************
*& Key           : RM-230407
*& Request No.   : GAP 57 Outbound: Custom Warehouse Order Creation
*&                 Rule filters for SPO/SLO flows
**********************************************************************
*& Description (short)
*&
**********************************************************************
    DATA:
          lt_range TYPE rsds_frange_t.

    lt_range = VALUE #( ( fieldname = /scwm/cl_vas_db_selects=>gc_fieldname_delivery
                          selopt_t  = VALUE rsds_selopt_t( ( sign   = wmegc_sign_inclusive
                                                             option = wmegc_option_eq
                                                             low    = iv_docno ) ) )
                        ( fieldname = /scwm/cl_vas_db_selects=>gc_fieldname_vas_type
                          selopt_t  = VALUE rsds_selopt_t( ( sign   = wmegc_sign_inclusive
                                                             option = wmegc_option_eq
                                                             low    = /scwm/if_vas_c=>sc_type_outbound ) ) ) ). " VAS for Outbound Delivery                                                          ).

    /scwm/cl_vas_db_selects=>vas_order(
      EXPORTING
        iv_lgnum   = iv_lgnum
        it_frange  = lt_range
        iv_doccat  = /scdl/if_adf_c=>c_doccat_pdo
      IMPORTING
        et_vas_key = DATA(lt_vas_key) ).

    IF lt_vas_key IS NOT INITIAL.
      " VAS Order exist
      rv_vas_exist = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD get_open_wt_diff_areawho.
**********************************************************************
*& Key           : RM-230407
*& Request No.   : GAPs 57 Outbound: Custom Warehouse Order Creation
*&                 Rule filters for SPO/SLO flows
**********************************************************************
*& Description (short)
*&
**********************************************************************

    CALL FUNCTION '/SCWM/TO_GET_WIP'
      EXPORTING
        iv_lgnum   = iv_lgnum
        iv_open    = abap_true
        iv_conf    = abap_true
        is_selcrit = VALUE /scwm/s_to_selcrit_mon( r_rdocid = VALUE #( ( sign   = wmegc_sign_inclusive
                                                                         option = wmegc_option_eq
                                                                         low    = iv_docid ) )
                                                   r_tostat = VALUE #( ( sign   = wmegc_sign_inclusive
                                                                         option = wmegc_option_eq
                                                                         low    = wmegc_to_open )
                                                                       ( sign   = wmegc_sign_inclusive
                                                                         option = wmegc_option_eq
                                                                         low    = wmegc_to_confirmed ) ) )
      IMPORTING
        et_to      = rt_to.

    IF rt_to IS INITIAL.
      RETURN.
    ENDIF.

    " Delete the open WTs with the same Warehouse Order Activity Area
    DELETE rt_to WHERE areawho = iv_areawho.

  ENDMETHOD.


  METHOD is_putwall.
********************************************************************
*& Key          : <TBIRIHAN>-January 16, 2024
*& Request No.  : GAP-049
********************************************************************
*& Description  :
********************************************************************

    TYPES: BEGIN OF ty_combined_aarea,
             rdocid TYPE  /scwm/de_docid,
             ritmid TYPE  /scwm/de_itmid,
             aarea  TYPE  /scwm/de_aarea,
           END OF ty_combined_aarea,
           tty_combined_aarea TYPE STANDARD TABLE OF ty_combined_aarea WITH EMPTY KEY.
    DATA: lt_combined_aarea TYPE tty_combined_aarea,
          lr_aawho          TYPE /scwm/tt_areawho_r.

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = /scwm/cl_tm=>sv_lgnum
        iv_process   = zif_param_const=>c_zout_0002
        iv_parameter = zif_param_const=>c_putwall_aarea
      IMPORTING
        et_list      = DATA(lt_aarea_params) ).

    lr_aawho  = VALUE /scwm/tt_areawho_r( BASE lr_aawho FOR ls_aarea_params IN lt_aarea_params
                                          ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = ls_aarea_params ) ).

    DATA(lt_to_diff_areawho) = get_open_wt_diff_areawho( iv_lgnum   = /scwm/cl_tm=>sv_lgnum
                                                         iv_docid   = iv_docid
                                                         iv_areawho = iv_areawho ).
    lt_combined_aarea = VALUE tty_combined_aarea(
                          FOR GROUPS <group_key> OF <g1> IN lt_to_diff_areawho
                          GROUP BY ( rdoccat = <g1>-rdoccat rdocid = <g1>-rdocid  aarea = <g1>-aarea )
                          LET coll_line = REDUCE #( INIT line TYPE ty_combined_aarea FOR <m1> IN GROUP <group_key>
                                          NEXT   line-rdocid  = <m1>-rdocid
                                                 line-ritmid  = <m1>-ritmid
                                                 line-aarea   = <m1>-aarea )  IN ( coll_line ) ) .


    lt_combined_aarea = VALUE tty_combined_aarea(
                          BASE lt_combined_aarea
                          FOR GROUPS <group_key> OF <g2> IN it_wt_areawho
                          GROUP BY ( rdoccat = <g2>-rdoccat rdocid = <g2>-rdocid  aarea = <g2>-aarea )
                          LET coll_line = REDUCE #( INIT line TYPE ty_combined_aarea FOR <m2> IN GROUP <group_key>
                                          NEXT   line-rdocid  = <m2>-rdocid
                                                 line-ritmid  = <m2>-ritmid
                                                 line-aarea   = <m2>-aarea )  IN ( coll_line ) ) .


    LOOP AT lt_combined_aarea ASSIGNING FIELD-SYMBOL(<ls_combined_aarea>) WHERE aarea IN lr_aawho.
      LOOP AT lr_aawho ASSIGNING FIELD-SYMBOL(<ls_aarea>) WHERE low NE <ls_combined_aarea>-aarea.
        IF line_exists( lt_combined_aarea[ rdocid = <ls_combined_aarea>-rdocid aarea = <ls_aarea>-low ] ).
          rv_putwall  = abap_true.
        ENDIF.
        IF rv_putwall IS NOT INITIAL.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF rv_putwall IS NOT INITIAL.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD read_delivery.
********************************************************************
*& Key          : BSUGAREV-Dec 14, 2023
*& Request No.  : GAP 57 Outbound: Custom Warehouse Order Creation
********************************************************************
*& Description  :
*&
*&
********************************************************************
    TRY.
        /scwm/cl_dlv_management_prd=>get_instance( )->query(
          EXPORTING
            it_docid        = VALUE #( FOR <l> IN it_to ( doccat = <l>-rdoccat  docid = <l>-rdocid ) )
            is_read_options = VALUE #( data_retrival_only      = abap_true
                                       mix_in_object_instances = abap_true )
            is_include_data = VALUE #( head_status = abap_true
                                       item_status = abap_true
                                       head_status_dyn = abap_true
                                       head_status_dyn_detail = VALUE #(
                                          ( /scdl/if_dl_status_c=>sc_t_planning_picking ) ) )
          IMPORTING
            et_headers      = et_headers
            et_items        = et_items ).
        ##NO_HANDLER
      CATCH /scdl/cx_delivery.
    ENDTRY.
  ENDMETHOD.


  METHOD read_mat_lgnum.
**********************************************************************
*& Key           : RM-230407
*& Request No.   : GAPs 57 Outbound: Custom Warehouse Order Creation
*&                 Rule filters for SPO/SLO flows
**********************************************************************
*& Description (short)
*&
**********************************************************************

    TRY.

        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid     = iv_matid
            iv_entitled  = iv_entitled
            iv_lgnum     = iv_lgnum
          IMPORTING
            es_mat_lgnum = rs_mat_lgnum.

      CATCH /scwm/cx_md_interface
            /scwm/cx_md_material_exist
            /scwm/cx_md_lgnum_locid
            /scwm/cx_md                ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
