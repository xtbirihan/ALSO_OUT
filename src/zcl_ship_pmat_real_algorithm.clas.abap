CLASS zcl_ship_pmat_real_algorithm DEFINITION
  PUBLIC
  INHERITING FROM zcl_ship_pmat_algorithm_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_mats_by_aum,
             sbin TYPE /scwm/de_lgpla.
             INCLUDE TYPE ty_mat_details.
    TYPES: END OF ty_mats_by_aum.


    TYPES:
      BEGIN OF ty_bin_auom,
        bin  TYPE /scwm/de_lgpla,
        quan TYPE /scwm/s_quan-quan,
        uom  TYPE /scwm/s_quan-unit,
      END OF ty_bin_auom.

    TYPES: tt_mats_by_aum TYPE STANDARD TABLE OF ty_mats_by_aum WITH DEFAULT KEY,
           tt_bin_auom    TYPE STANDARD TABLE OF ty_bin_auom WITH DEFAULT KEY.

    METHODS determine_pmat_for_tasks
      IMPORTING
        !iv_full_pack               TYPE boole_d DEFAULT abap_true
        !iv_use_only_supplied_pmats TYPE boole_d OPTIONAL
        !it_to                      TYPE /scwm/tt_ordim_o_int
        !it_packmat                 TYPE /scwm/tt_who_pmat OPTIONAL
      EXPORTING
        !es_pack_output             TYPE ty_pack_output
        !et_bapiret                 TYPE bapirettab .

    METHODS pack_tasks_by_default_pmats
      IMPORTING
        !it_to          TYPE /scwm/tt_ordim_o_int
        !it_packmat     TYPE /scwm/tt_who_pmat OPTIONAL
        !iv_full_pack   TYPE boole_d DEFAULT abap_true
      EXPORTING
        !es_pack_output TYPE ty_pack_output
        !et_bapiret     TYPE bapirettab .

  PROTECTED SECTION.

private section.
ENDCLASS.



CLASS ZCL_SHIP_PMAT_REAL_ALGORITHM IMPLEMENTATION.


  METHOD determine_pmat_for_tasks.
********************************************************************
*& Key          : <BSUGAREV>-15.05.2023 14:29:02
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description
*&
********************************************************************
    TYPES: BEGIN OF ty_mats_by_aum,
             sbin TYPE /scwm/de_lgpla.
             INCLUDE    TYPE ty_mat_details.
    TYPES: END OF ty_mats_by_aum.

    DATA: lt_mats_aum TYPE STANDARD TABLE OF ty_mats_by_aum WITH DEFAULT KEY.

    BREAK-POINT ID zcg_ship_pmat_real_algorithm.

    CLEAR: es_pack_output, et_bapiret.

    MESSAGE i003(zmc_out) INTO mv_msg.
    add_message( ).

    IF lines( it_to ) = 0.
      " Process is cancelled. Empty list of tasks is supplied.
      MESSAGE w004(zmc_out) INTO mv_msg.
      add_message( ).

      RETURN.
    ENDIF.

    DATA(ls_task) = VALUE #( it_to[ 1 ] ).

    mv_lgnum = ls_task-lgnum.
    mv_entitled = ls_task-entitled.
    mv_full_pack = iv_full_pack.
    mv_use_only_supplied_pmats = iv_use_only_supplied_pmats.
    mo_packmat = NEW zcl_packmmat_algo( mv_lgnum ).
    mo_pack_who = NEW zcl_out_who_pack_wt( mv_lgnum ).

    TRY.
        /scwm/cl_dlv_management_prd=>get_instance( )->query(
           EXPORTING
             it_docid        = VALUE #( ( doccat = ls_task-rdoccat
                                          docid  = ls_task-rdocid ) )
             is_read_options = VALUE #( data_retrival_only = abap_true
                                        mix_in_object_instances = abap_true
                                        item_part_select = abap_true )
             is_include_data = VALUE #( head_partyloc = abap_true
                                        item_partyloc = abap_true )
           IMPORTING
             et_headers      = DATA(lt_headers)
             et_items        = DATA(lt_items) ).
      CATCH /scdl/cx_delivery.
    ENDTRY.

    IF lines( lt_items ) = 0.
      " Task &1 doesn't have a valid reference document ID.
      MESSAGE w005(zmc_out) WITH ls_task-tanum INTO mv_msg.
      add_message( ).

      RETURN.
    ENDIF.

    DATA(ls_header) = VALUE #( lt_headers[ 1 ] OPTIONAL ).

    " Packing will be executed for document &1.
    MESSAGE i006(zmc_out) WITH ls_header-docno INTO mv_msg.
    add_message( ).

    DATA(ls_bp) = get_header_bp( ls_header ).
    ls_bp-sold_to = VALUE #( lt_items[ 1 ]-partyloc[
                        party_role = /scdl/if_dl_partyloc_c=>sc_party_role_sotprt ]-partyno OPTIONAL ).

    " get materials UoM and dispatchable indicator
    DATA(lt_mat_data) = get_mat_details_for_packing( it_matid = VALUE #( FOR <l> IN it_to ( <l>-matid ) ) ).

    DATA(lo_conv) = NEW /scwm/cl_dlv_md_access( ).

    DATA(ls_doc_mat) = VALUE ty_docno_mats_map( docid = ls_task-rdocid ).

    DATA(lt_bin_auom) = mo_pack_who->get_bin_auom( it_to ).

    LOOP AT it_to ASSIGNING FIELD-SYMBOL(<ls_task>).

      DATA(ls_mat_data) = REF #( lt_mat_data[ matid = <ls_task>-matid ] OPTIONAL ).

      CHECK ls_mat_data IS NOT INITIAL.

      " convert QTY to the same UoM
      DATA(lv_task_qty) = <ls_task>-vsolm.
      IF ls_mat_data->meins <> <ls_task>-meins.
        TRY.
            lv_task_qty = lo_conv->/scdl/if_af_quantity_conversio~convert_unit_quantity_single(
                iv_productid = <ls_task>-matid
                iv_uom_from  = <ls_task>-meins
                iv_uom_to    = ls_mat_data->meins
                iv_qty_from  = <ls_task>-vsolm ).
          CATCH /scdl/cx_af_quantity_conversio.
        ENDTRY.
      ENDIF.

      DATA(ls_auom) = VALUE /scwm/s_quan( ).

      DATA(ls_bin_aum) = VALUE #( lt_bin_auom[ bin = <ls_task>-vlpla ] OPTIONAL ).
      IF ls_bin_aum IS NOT INITIAL.
        ls_auom = VALUE #( ls_mat_data->auom_tab[ unit = ls_bin_aum-uom ] OPTIONAL ).
      ENDIF.

      " get packing indicator by activity AAREA. No MC packing for contants maintain for switch ID ZOUT_014
      DATA(lv_no_mc_pack) = zcl_switch=>get_switch_state( iv_lgnum  = mv_lgnum
                                                          iv_devid  = zif_switch_const=>c_zout_014
                                                          it_fields = VALUE #( ( field = zif_switch_const=>c_aarea field_value = <ls_task>-aarea ) ) ).

      " sum QTYs for the same material or add the material if it is not in the list
      DATA(ls_mat_aum) = REF #( lt_mats_aum[ sbin       = <ls_task>-vlpla
                                             no_mc_pack = lv_no_mc_pack
                                             matid      = <ls_task>-matid ] OPTIONAL ).

      IF ls_mat_aum IS INITIAL.
        lt_mats_aum = VALUE #( BASE lt_mats_aum
            ( sbin     = <ls_task>-vlpla
              no_mc_pack = lv_no_mc_pack
              matid    = ls_mat_data->matid
              meins    = ls_mat_data->meins
              disp     = ls_mat_data->disp
              auom_tab = SWITCH #( ls_auom-unit WHEN space THEN VALUE #( )
                                                ELSE VALUE #( ( ls_auom ) )  )
              qty-quan = lv_task_qty
              qty-unit = ls_mat_data->meins ) ).
      ELSE.
        ls_mat_aum->qty-quan += lv_task_qty.
      ENDIF.

    ENDLOOP.

    " calculate pallets

    " calculate master cartons considering item qty and UoM MC
    " keep left quantity for the next step -> Cuboid alg. calculation
    ls_doc_mat-mats = VALUE #( FOR <mu> IN lt_mats_aum ( CORRESPONDING #( <mu> ) ) ).

    es_pack_output = determine_ship_pmats(
        is_bp      = ls_bp
        is_doc_mat = ls_doc_mat
        it_packmat = it_packmat ).

    et_bapiret = mt_bapiret.
  ENDMETHOD.


  METHOD pack_tasks_by_default_pmats.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    IF lines( it_to ) = 0.
      " Process is cancelled. Empty list of tasks is supplied.
      MESSAGE w004(zmc_out) INTO mv_msg.
      add_message( ).

      et_bapiret = mt_bapiret.
      RETURN.
    ENDIF.

    DATA(ls_task) = VALUE #( it_to[ 1 ] ).

    DATA(lt_pmat_ids) = NEW zcl_packmmat_algo( ls_task-lgnum )->get_pmat_planned_shipping( ).

    DATA(lt_packmat) = it_packmat.

    " L/W/H, Volume are determine in DETERMINE_PMAT_FOR_TASKS
    lt_packmat = VALUE #( BASE lt_packmat FOR <l> IN lt_pmat_ids ( pmat_guid = <l>-matid ) ).

    determine_pmat_for_tasks(
      EXPORTING
        it_to          = it_to
        it_packmat     = lt_packmat
        iv_full_pack   = abap_true
      IMPORTING
        es_pack_output = es_pack_output
        et_bapiret     = et_bapiret ).
  ENDMETHOD.
ENDCLASS.
