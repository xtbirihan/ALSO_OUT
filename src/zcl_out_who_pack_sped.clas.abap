CLASS zcl_out_who_pack_sped DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES /scwm/if_ex_who_packing .

    CLASS-METHODS get_pallet_pmats
      IMPORTING
        !iv_lgnum       TYPE /scwm/lgnum OPTIONAL
        !is_bp          TYPE zstr_ship_bp
      RETURNING
        VALUE(rt_pmats) TYPE /scwm/tt_who_pmat .
  PROTECTED SECTION.

private section.

  types:
    BEGIN OF ty_to_aarea_gr,
        aarea TYPE /scwm/de_aarea,
        tasks TYPE /scwm/tt_ordim_o_int,
      END OF ty_to_aarea_gr .
  types:
    tt_to_aarea_gr TYPE STANDARD TABLE OF ty_to_aarea_gr WITH EMPTY KEY .

  class-data MO_PACKMAT type ref to ZCL_PACKMMAT_ALGO .
  class-data MO_WHO_PACK type ref to ZCL_OUT_WHO_PACK_WT .
  class-data MV_LGNUM type /SCWM/LGNUM .

  class-methods GET_AAREA_FOR_TASK_SBIN
    importing
      !IT_TASKS type /SCWM/TT_ORDIM_O_INT
    returning
      value(RT_RESULT) type /SCWM/TT_LAGPS .
  class-methods GET_BP
    importing
      !IS_TASK type /SCWM/S_ORDIM_O_INT
    returning
      value(RS_BP) type ZSTR_SHIP_BP .
  class-methods GROUP_TASKS_BY_AAREA
    importing
      !IT_TASKS type /SCWM/TT_ORDIM_O_INT
    returning
      value(RT_RESULT) type TT_TO_AAREA_GR .
  class-methods PACK_CARTONS_IN_PALLET
    importing
      !IT_CARTON type /SCWM/TT_ORDIM_O_INT
      !IS_BP type ZSTR_SHIP_BP
    returning
      value(RT_PACKED) type /SCWM/TT_ORDIM_O_INT .
  class-methods PACK_TASKS_IN_CARTON
    importing
      !IT_TASKS type /SCWM/TT_ORDIM_O_INT
      !IT_CARTONS type ZCL_SHIP_PMAT_ALGORITHM_BASE=>TT_PMAT_QTY
    exporting
      !ET_PACKED type /SCWM/TT_ORDIM_O_INT
      !ET_TO_NEXT_WHO type /SCWM/TT_ORDIM_O_INT
      !EV_SPLIT type SY-TABIX .
  class-methods PACK_TASKS_IN_SHIPPMAT
    importing
      !IT_TASKS type /SCWM/TT_ORDIM_O_INT
      !IT_SHIPPING type ZCL_SHIP_PMAT_ALGORITHM_BASE=>TT_PMAT_QTY
    exporting
      !ET_PACKED type /SCWM/TT_ORDIM_O_INT
      !ET_TO_NEXT_WHO type /SCWM/TT_ORDIM_O_INT
      !EV_SPLIT type SY-TABIX .
  class-methods GROUP_ALL_TASKS
    importing
      !IT_TASKS type /SCWM/TT_ORDIM_O_INT
    returning
      value(RT_RESULT) type TT_TO_AAREA_GR .
ENDCLASS.



CLASS ZCL_OUT_WHO_PACK_SPED IMPLEMENTATION.


  METHOD /scwm/if_ex_who_packing~packing.
********************************************************************
*& Key          : <BSUGAREV>-06.04.2023 15:11:45
*& Request No.  : GAP-55 – SPED Orders Picking
********************************************************************
*& Description  : Pack WT for spedition
*&
********************************************************************
    DATA: lv_counter TYPE i.

    IF lines( it_to ) = 0.
      RETURN.
    ENDIF.

    " field level switch on/off
    DATA(lt_switch_fields) = VALUE ztt_switch_fields( ( field       = zif_switch_const=>c_packprofile
                                                        field_value = is_tpack-packprofile ) ).

    IF zcl_switch=>get_switch_state( iv_lgnum  = is_tpack-lgnum
                                     iv_devid  = zif_switch_const=>c_zout_006
                                     it_fields = lt_switch_fields ) EQ abap_false.
      RETURN.
    ENDIF.

    mv_lgnum = is_tpack-lgnum.
    mo_who_pack = NEW zcl_out_who_pack_wt( is_tpack-lgnum ).
    mo_packmat = NEW zcl_packmmat_algo( mv_lgnum ).

    " this call will be updated for SPED process
* UM Start 18.12.2023 - Change the new method to retrieve the dedicated pack-profile.
    DATA(lt_pmat_totes) = mo_packmat->get_pmat_totes_pick( ).
* UM End 18.12.2023 - Change the new method to retrieve the dedicated pack-profile.

    DATA(lt_cartons) = VALUE zcl_ship_pmat_algorithm_base=>tt_pmat_qty( ).
    DATA(lt_in_tasks_for_pack) = VALUE /scwm/tt_ordim_o_int( ).


* UM Start 09.02.2024 - Group only for Shipping Cartons.

*    " group tasks by AAREA of source bin
*    DATA(lt_tasks_group_by_aarea) = group_tasks_by_aarea( it_tasks = it_to ).

    IF abap_true = zcl_switch=>get_switch_state(
        iv_lgnum  = mv_lgnum
        iv_devid  = zif_switch_const=>c_zout_021
        it_fields = VALUE #( ( field = zif_switch_const=>c_wocr field_value = is_wcr-wcr ) ) ).

      " group tasks by AAREA of source bin
      DATA(lt_tasks_group_by_aarea) = group_tasks_by_aarea( it_tasks = it_to ).
    ELSE.

      lt_tasks_group_by_aarea = group_all_tasks( it_tasks = it_to ).

    ENDIF.
* UM Start 09.02.2024 - Group only for Shipping Cartons.


    lv_counter = 0.
    LOOP AT lt_tasks_group_by_aarea ASSIGNING FIELD-SYMBOL(<ls_task_group>).

      lv_counter += 1.
      DATA(ls_pack_output) = VALUE zcl_ship_pmat_algorithm_base=>ty_pack_output( ).

      " in case of conveyer packing must be done in TOTES. Conveyer is in AAREAs maintained in switch for ID 'ZOUT_016'
* UM Start 18.12.2023 - Add the condition on the WOCR.
      IF abap_true = zcl_switch=>get_switch_state(
          iv_lgnum  = mv_lgnum
          iv_devid  = zif_switch_const=>c_zout_016
          it_fields = VALUE #( ( field = zif_switch_const=>c_aarea field_value = <ls_task_group>-aarea )
                               ( field = zif_switch_const=>c_wocr field_value = is_wcr-wcr ) ) ).
* UM Start 18.12.2023 - Add the condition on the WOCR.

        NEW zcl_ship_pmat_real_algorithm( )->determine_pmat_for_tasks(
          EXPORTING
            it_to          = <ls_task_group>-tasks
            it_packmat     = VALUE #( FOR <pl> IN lt_pmat_totes ( pmat_guid = <pl>-matid ) )
            iv_use_only_supplied_pmats = abap_true
          IMPORTING
            es_pack_output = ls_pack_output ).

      ELSE.
        NEW zcl_ship_pmat_real_algorithm( )->determine_pmat_for_tasks(
          EXPORTING
            it_to          = <ls_task_group>-tasks
            it_packmat     = it_pmat
          IMPORTING
            es_pack_output = ls_pack_output ).
      ENDIF.


      ls_pack_output = mo_who_pack->filter_pack_result_by_wocr( iv_wocr = is_wcr-wcr
                                                                is_pack = ls_pack_output ).

      IF lines( ls_pack_output-carton ) > 0.
        APPEND LINES OF ls_pack_output-carton TO lt_cartons.
        APPEND LINES OF <ls_task_group>-tasks TO lt_in_tasks_for_pack.

      ELSEIF lines( lt_cartons ) = 0 AND lines( ls_pack_output-shipping ) > 0.
        pack_tasks_in_shippmat(
          EXPORTING
            it_tasks       = <ls_task_group>-tasks
            it_shipping    = ls_pack_output-shipping
          IMPORTING
            et_packed      = ct_packed
            et_to_next_who = ct_to
            ev_split       = ev_split ).

        " add tasks from next AAREA to CT_TO for the next WHO
        LOOP AT lt_tasks_group_by_aarea ASSIGNING FIELD-SYMBOL(<ls_tasks>) FROM ( lv_counter + 1 ).
          APPEND LINES OF <ls_tasks>-tasks TO ct_to.
        ENDLOOP.

        EXIT.
      ENDIF.
    ENDLOOP.

    IF lines( lt_cartons ) > 0.

      " Pack tasks to master cartons
      pack_tasks_in_carton(
        EXPORTING
          it_tasks       = lt_in_tasks_for_pack
          it_cartons     = lt_cartons
        IMPORTING
          et_packed      = DATA(lt_carton_packed)
          et_to_next_who = ct_to
          ev_split       = ev_split ).

      " Pack cartons in pallets
      ct_packed = pack_cartons_in_pallet( it_carton = lt_carton_packed
                                          is_bp     = get_bp( is_task = it_to[ 1 ] ) ).

      lv_counter = 0.
      DATA(lt_selop_delete_shiphuid) = VALUE rseloption( ).

      LOOP AT ct_packed ASSIGNING FIELD-SYMBOL(<ls_d>) GROUP BY ( shiphuid = <ls_d>-shiphuid )
                        ASSIGNING FIELD-SYMBOL(<lt_pallets>).
        lv_counter += 1.
        CHECK lv_counter > 1.

        lt_selop_delete_shiphuid = VALUE #( BASE lt_selop_delete_shiphuid
            ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <lt_pallets>-shiphuid ) ).

        LOOP AT GROUP <lt_pallets> INTO DATA(ls_pal_cont).

          DATA(ls_to) = REF #( ct_to[ tanum = ls_pal_cont-tanum ] OPTIONAL ).
          IF ls_to IS INITIAL.
            CLEAR: ls_pal_cont-huid, ls_pal_cont-shiphuid.
            APPEND ls_pal_cont TO ct_to.

          ELSE.
            ls_to->vsolm += ls_pal_cont-vsolm.
            ls_to->vsola += ls_pal_cont-vsola.
          ENDIF.

        ENDLOOP.

      ENDLOOP.

      IF lines( lt_selop_delete_shiphuid ) > 0.
        DELETE ct_packed WHERE shiphuid IN lt_selop_delete_shiphuid.
      ENDIF.
    ENDIF.

    /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = DATA(lo_pack) ).
    LOOP AT ct_packed ASSIGNING FIELD-SYMBOL(<ls_packed_upd>).

      DATA(lv_idx) = sy-tabix.

      IF line_exists( ct_whohu[ huid = <ls_packed_upd>-huid ] ).
        CONTINUE.
      ENDIF.

      lo_pack->get_hu(
        EXPORTING
          iv_guid_hu = <ls_packed_upd>-huid
        IMPORTING
          es_huhdr   = DATA(ls_huhdr_buffer)
        EXCEPTIONS
          not_found  = 1
          OTHERS     = 2 ).
      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lv_msg).
        CONTINUE.
      ENDIF.

      DATA(ls_whohu) = VALUE /scwm/s_whohu( lgnum     = is_wcr-lgnum
                                            hukng     = lv_idx "1
                                            pmat_guid = ls_huhdr_buffer-pmat_guid
                                            huid      = ls_huhdr_buffer-guid_hu
                                            huident   = ls_huhdr_buffer-huident
                                            prces     = is_wcr-prces
                                            wcr       = is_wcr-wcr
                                            updkz     = zif_wme_c=>gs_whohu_upd-create ).
      APPEND ls_whohu TO ct_whohu.
    ENDLOOP.

*    lv_idx = lines( ct_packed ). " UM 08.02.2024
    lv_idx = lines( ct_whohu ). " UM 08.02.2024
    LOOP AT ct_packed ASSIGNING <ls_packed_upd> WHERE shiphuid IS NOT INITIAL.

      " only if we have tasks with updated SHIPHUID
      DATA(lv_call_rfc) = abap_true.

      IF line_exists( ct_whohu[ huid = <ls_packed_upd>-shiphuid ] ).
        CONTINUE.
      ENDIF.

      lo_pack->get_hu(
        EXPORTING
          iv_guid_hu = <ls_packed_upd>-shiphuid
        IMPORTING
          es_huhdr   = ls_huhdr_buffer
        EXCEPTIONS
          not_found  = 1
          OTHERS     = 2 ).
      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
        CONTINUE.
      ENDIF.

      lv_idx = lv_idx + 1.

      ls_whohu = VALUE /scwm/s_whohu( lgnum     = is_wcr-lgnum
                                      hukng     = lv_idx "1
                                      pmat_guid = ls_huhdr_buffer-pmat_guid
                                      huid      = ls_huhdr_buffer-guid_hu
                                      huident   = ls_huhdr_buffer-huident
                                      prces     = is_wcr-prces
                                      wcr       = is_wcr-wcr
                                      updkz     = zif_wme_c=>gs_whohu_upd-create ).
      APPEND ls_whohu TO ct_whohu.
    ENDLOOP.

    " update SHIPHUID field of the tasks with the pallet ID
    IF lv_call_rfc = abap_true.
      DATA(lv_qname) = VALUE trfcqnam( ).
      lv_qname = |{ zif_wme_c=>gs_queue-ztoshpid }{ ct_packed[ 1 ]-tanum }{ sy-uzeit+4(2) }|.

      CALL FUNCTION 'TRFC_SET_QIN_PROPERTIES'
        EXPORTING
          qin_name = lv_qname.

      CALL FUNCTION 'Z_TO_UPDATE_SHIPHUID'
        IN BACKGROUND TASK AS SEPARATE UNIT
        EXPORTING
          iv_pallet_id = ls_huhdr_buffer-guid_hu
          it_packed    = ct_packed.
    ENDIF.

* UM Start 08.02.2024 - Fix dump
    LOOP AT it_to INTO DATA(ls_to_tmp).
      READ TABLE ct_to WITH KEY tanum = ls_to_tmp-tanum TRANSPORTING NO FIELDS.
      IF sy-subrc IS NOT INITIAL.
        READ TABLE ct_packed WITH KEY tanum = ls_to_tmp-tanum TRANSPORTING NO FIELDS.
        IF sy-subrc IS NOT INITIAL.
          READ TABLE ct_failed WITH KEY tanum = ls_to_tmp-tanum TRANSPORTING NO FIELDS.
          IF sy-subrc IS NOT INITIAL.
            APPEND ls_to_tmp TO ct_to.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
* UM End 08.02.2024 - Fix dump

  ENDMETHOD.


  METHOD get_aarea_for_task_sbin.
********************************************************************
*& Key          : <BSUGAREV>-Nov 28, 2023
*& Request No.  : GAP-55 – SPED Orders Picking
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA: lt_selopt_act_type TYPE rseloption,
          lt_selopt_storbin  TYPE rseloption.

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = mv_lgnum
        iv_process   = zif_param_const=>c_zout_0005
        iv_parameter = zif_param_const=>c_activity_pick
      IMPORTING
        ev_constant  = DATA(lv_act_pick) ).

    lt_selopt_act_type = VALUE #( ( sign   = wmegc_sign_inclusive
                                    option = wmegc_option_eq
                                    low    = lv_act_pick ) ).

    lt_selopt_storbin = VALUE #( FOR <t> IN it_tasks
                                  ( sign   = wmegc_sign_inclusive
                                    option = wmegc_option_eq
                                    low    = <t>-vlpla ) ).

    CALL FUNCTION '/SCWM/LAGPS_READ_MULTI'
      EXPORTING
        iv_lgnum          = mv_lgnum
        ir_activity_types = lt_selopt_act_type
        ir_storage_bin    = lt_selopt_storbin
      IMPORTING
        et_lagps          = rt_result
      EXCEPTIONS
        wrong_input       = 1
        not_found         = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDMETHOD.


  METHOD get_bp.
********************************************************************
*& Key          : <BSUGAREV>-Nov 28, 2023
*& Request No.  : GAP-55 – SPED Orders Picking
********************************************************************
*& Description  :
*&
*&
********************************************************************
    TRY.
        /scwm/cl_dlv_management_prd=>get_instance( )->query(
           EXPORTING
             it_docid        = VALUE #( ( doccat = is_task-rdoccat
                                          docid  = is_task-rdocid ) )
             is_read_options = VALUE #( data_retrival_only = abap_true
                                        mix_in_object_instances = abap_true
                                        item_part_select = abap_true )
             is_include_data = VALUE #( head_partyloc = abap_true
                                        item_partyloc = abap_true )
           IMPORTING
             et_headers      = DATA(lt_headers)
             et_items        = DATA(lt_items) ).
      CATCH /scdl/cx_delivery  ##NO_HANDLER.
    ENDTRY.

    rs_bp-carrier = VALUE #( lt_headers[ 1 ]-partyloc[ party_role = /scdl/if_dl_partyloc_c=>sc_party_role_carr ]-partyno OPTIONAL ).
    rs_bp-ship_to = VALUE #( lt_headers[ 1 ]-partyloc[ party_role = /scdl/if_dl_partyloc_c=>sc_party_role_stprt ]-partyno OPTIONAL ).
    rs_bp-sold_to = VALUE #( lt_items[ 1 ]-partyloc[
                        party_role = /scdl/if_dl_partyloc_c=>sc_party_role_sotprt ]-partyno OPTIONAL ).

  ENDMETHOD.


  METHOD get_pallet_pmats.
********************************************************************
*& Key          : BSUGAREV-Jan 5, 2024
*& Request No.  : GAP-55 – SPED Orders Picking
********************************************************************
*& Description  :
*&
*&
********************************************************************
    IF mv_lgnum IS INITIAL.
      mv_lgnum = iv_lgnum.
    ENDIF.

    IF mo_who_pack IS NOT BOUND.
      mo_who_pack = NEW zcl_out_who_pack_wt( mv_lgnum ).
    ENDIF.

    IF mo_packmat IS NOT BOUND.
      mo_packmat = NEW zcl_packmmat_algo( mv_lgnum ).
    ENDIF.

    " first need to determine list of pack materials from packaging specification
    DATA(lt_pmats) = mo_packmat->get_pmat_pallet_sped( ).
    DATA(lt_filtered_pmats) = mo_packmat->filter_pmats_by_bp( iv_sped  = abap_true
                                                              is_bp    = is_bp
                                                              it_pmats = lt_pmats ).
    IF lines( lt_filtered_pmats ) = 0.
      RETURN.
    ENDIF.

    rt_pmats = VALUE #( FOR <l> IN lt_filtered_pmats ( pmat_guid = <l>-pmatid ) ).

    mo_packmat->update_pmat_dimensions( CHANGING ct_packmat = rt_pmats ).

  ENDMETHOD.


  METHOD group_all_tasks.
********************************************************************
*& Key          : <BSUGAREV>-Nov 28, 2023
*& Request No.  : GAP-55 – SPED Orders Picking
********************************************************************
*& Description  :
*&
*&
********************************************************************

    DATA: lt_bin_aarea TYPE /scwm/tt_lagps,
          ls_bin_aarea TYPE LINE OF /scwm/tt_lagps.

    LOOP AT it_tasks INTO DATA(ls_task).
      ls_bin_aarea-aarea = ls_task-aarea.
      ls_bin_aarea-lgpla = ls_task-vlpla.
      APPEND ls_bin_aarea TO lt_bin_aarea.
    ENDLOOP.

    LOOP AT lt_bin_aarea ASSIGNING FIELD-SYMBOL(<ls_dummy>) GROUP BY ( aarea = <ls_dummy>-aarea )
                         ASSIGNING FIELD-SYMBOL(<ls_bins>).

      DATA(lt_selopt_bins) = VALUE rseloption( FOR <l> IN GROUP <ls_bins>
          ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <l>-lgpla ) ).

      rt_result = VALUE #( BASE rt_result
          ( aarea = <ls_bins>-aarea
            tasks = VALUE #( FOR <it> IN it_tasks WHERE ( vlpla IN lt_selopt_bins )
                           ( <it> ) ) ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD group_tasks_by_aarea.
********************************************************************
*& Key          : <BSUGAREV>-Nov 28, 2023
*& Request No.  : GAP-55 – SPED Orders Picking
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA(lt_bin_aarea) = get_aarea_for_task_sbin( it_tasks ).

    LOOP AT lt_bin_aarea ASSIGNING FIELD-SYMBOL(<ls_dummy>) GROUP BY ( aarea = <ls_dummy>-aarea )
                         ASSIGNING FIELD-SYMBOL(<ls_bins>).

      DATA(lt_selopt_bins) = VALUE rseloption( FOR <l> IN GROUP <ls_bins>
          ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <l>-lgpla ) ).

      rt_result = VALUE #( BASE rt_result
          ( aarea = <ls_bins>-aarea
            tasks = VALUE #( FOR <it> IN it_tasks WHERE ( vlpla IN lt_selopt_bins )
                           ( <it> ) ) ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD pack_cartons_in_pallet.
********************************************************************
*& Key          : <BSUGAREV>-Nov 28, 2023
*& Request No.  : GAP-55 – SPED Orders Picking
********************************************************************
*& Description  :
*&
*&
********************************************************************
    IF lines( it_carton ) = 0.
      RETURN.
    ENDIF.

    DATA(ls_whse_mdef) = zcl_crud_ztcrs_whse_mdef=>select_single_by_key( mv_lgnum ).

    rt_packed = it_carton.

    IF ls_whse_mdef-pal_mc_sped = abap_true.

      DATA(lt_pmats) = get_pallet_pmats( is_bp ).

      DATA(lt_pallet_packed) = mo_who_pack->pack_mc_in_pallet(
          is_hucrea_param = VALUE #( lgnum      = mv_lgnum
                                     no_huident = abap_true )
          it_pmats        = lt_pmats
          it_cartons      = it_carton ).

      IF lines( lt_pallet_packed ) > 0.
        rt_packed = lt_pallet_packed.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD pack_tasks_in_carton.
********************************************************************
*& Key          : <BSUGAREV>-Nov 28, 2023
*& Request No.  : GAP-55 – SPED Orders Picking
********************************************************************
*& Description  :
*&
*&
********************************************************************
    CLEAR: ev_split, et_packed, et_to_next_who.

    DATA(lt_tasks) = it_tasks.

    LOOP AT it_cartons ASSIGNING FIELD-SYMBOL(<ls_carton>).

      mo_who_pack->pack_tasks_in_carton(
        EXPORTING
          it_tasks       = lt_tasks
          is_carton      = <ls_carton>
        IMPORTING
          et_packed      = DATA(lt_packed_temp)
          et_to_next_who = DATA(lt_task_list_temp)
          ev_pmatid      = DATA(lv_pmatid)
          ev_split       = DATA(lv_split) ).

      CHECK lv_pmatid IS NOT INITIAL.

      lt_tasks = lt_task_list_temp.

      IF ev_split IS INITIAL AND lv_split IS NOT INITIAL.
        ev_split = lv_split.
      ENDIF.

      DATA(ls_huhdr) = mo_who_pack->create_ship_hu(
          is_param      = VALUE #( lgnum      = mv_lgnum
                                   no_huident = abap_true )
          is_huhdr_crea = VALUE #( pmat_guid  = lv_pmatid
                                   max_weight = REDUCE #( INIT x = 0 FOR <l> IN lt_packed_temp NEXT x += <l>-weight )
                                   unit_gw    = lt_packed_temp[ 1 ]-unit_w
                                   max_volume = REDUCE #( INIT x = 0 FOR <l> IN lt_packed_temp NEXT x += <l>-volum )
                                   unit_gv    = lt_packed_temp[ 1 ]-unit_v ) ).
      CHECK ls_huhdr IS NOT INITIAL.

      LOOP AT lt_packed_temp ASSIGNING FIELD-SYMBOL(<ls_packed>).
        <ls_packed>-huid = ls_huhdr-guid_hu.
      ENDLOOP.

      APPEND LINES OF lt_packed_temp TO et_packed. ""lt_carton_packed.
    ENDLOOP.

    et_to_next_who = lt_tasks.
  ENDMETHOD.


  METHOD pack_tasks_in_shippmat.
********************************************************************
*& Key          : <BSUGAREV>-Nov 28, 2023
*& Request No.  : GAP-55 – SPED Orders Picking
********************************************************************
*& Description  :
*&
*&
********************************************************************
    IF lines( it_shipping ) = 0.
      RETURN.
    ENDIF.

    mo_who_pack->pack_tasks_in_shippmat(
      EXPORTING
        it_tasks       = it_tasks
        it_shipping    = it_shipping
      IMPORTING
        et_packed      = DATA(lt_packed_temp)
        et_to_next_who = et_to_next_who
        ev_pmatid      = DATA(lv_pmatid)
        ev_split       = ev_split ).

    DATA(ls_huhdr) = mo_who_pack->create_ship_hu(
        is_param      = VALUE #( lgnum      = mv_lgnum
                                 no_huident = abap_true )
        is_huhdr_crea = VALUE #( pmat_guid  = lv_pmatid
                                 max_weight = REDUCE #( INIT x = 0 FOR <l> IN lt_packed_temp NEXT x += <l>-weight )
                                 unit_gw    = lt_packed_temp[ 1 ]-unit_w
                                 max_volume = REDUCE #( INIT x = 0 FOR <l> IN lt_packed_temp NEXT x += <l>-volum )
                                 unit_gv    = lt_packed_temp[ 1 ]-unit_v ) ).

    LOOP AT lt_packed_temp ASSIGNING FIELD-SYMBOL(<ls_packed_ship>).
      <ls_packed_ship>-huid = ls_huhdr-guid_hu.
    ENDLOOP.

    et_packed = lt_packed_temp.
  ENDMETHOD.
ENDCLASS.
