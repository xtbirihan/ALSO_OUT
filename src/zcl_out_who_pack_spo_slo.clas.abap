CLASS zcl_out_who_pack_spo_slo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES /scwm/if_ex_who_packing .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_matid_mfrnr,
        matid TYPE /scwm/de_matid,
        matnr TYPE /scwm/de_matnr,
        mfrnr TYPE mfrnr,
      END OF ty_matid_mfrnr .
  types:
    BEGIN OF ty_to_groups,
        key   TYPE string,
        tasks TYPE /scwm/tt_ordim_o_int,
      END OF ty_to_groups .
  types:
    tt_to_groups TYPE STANDARD TABLE OF ty_to_groups WITH KEY key .

  class-data GS_PACKMAT type /SCWM/S_WHO_PMAT .
  class-data GS_WHOPACKLIM type ZTOUT_WHO_PACKLI .
  class-data:
    gt_mat_mfrnr  TYPE STANDARD TABLE OF ty_matid_mfrnr WITH EMPTY KEY .
  class-data GT_PACK_GROUP type ZCL_CRUD_ZTOUT_WHO_PACKGR=>TT_ZTOUT_WHO_PACKGR .
  class-data GT_WHOPACKLIM type ZCL_CRUD_ZTOUT_WHOPACKLIM=>TT_ZTOUT_WHO_PACKLI .

  class-methods BUILD_LIST_MFRNR
    importing
      !IT_TO type /SCWM/TT_ORDIM_O_INT .
  class-methods CAPA_CHECK_FREE
    importing
      !IT_PACKED type /SCWM/TT_ORDIM_O_INT
    exporting
      !ET_PACKED type /SCWM/TT_ORDIM_O_INT
      !ET_FAILED type /SCWM/TT_ORDIM_O_INT .
*        is_packlim  TYPE ztout_who_packli
  class-methods CHECK_1DLV_CAPACITY
    importing
      !IS_GR_TASKS type TY_TO_GROUPS
    exporting
      !ES_GR_TASKS type TY_TO_GROUPS
      !ET_FAILED_TO type /SCWM/TT_ORDIM_O_INT .
  class-methods GROUP_TO
    importing
      !IS_GROUPING type ZTOUT_WHO_PACKGR
      !IT_TASKS type /SCWM/TT_ORDIM_O_INT
    returning
      value(RT_GROUPS) type TT_TO_GROUPS .
  class-methods PACK_TASKS
    importing
      !IS_WCR type /SCWM/TWCR
      !IS_PMAT type /SCWM/S_WHO_PMAT
      !IS_GR_TASKS type TY_TO_GROUPS
    exporting
      !ET_WHOHU type /SCWM/TT_WHOHU_INT
      !ET_TO type /SCWM/TT_ORDIM_O_INT
      !ET_PACKED type /SCWM/TT_ORDIM_O_INT
      !ET_FAILED type /SCWM/TT_ORDIM_O_INT .
  class-methods SET_GLOBAL_DATA
    importing
      !IS_TPACK type /SCWM/S_TWCRPACK
      !IT_TO type /SCWM/TT_ORDIM_O_INT
      !IT_PMAT type /SCWM/TT_WHO_PMAT .
ENDCLASS.



CLASS ZCL_OUT_WHO_PACK_SPO_SLO IMPLEMENTATION.


  METHOD /scwm/if_ex_who_packing~packing.
********************************************************************
*& Key          : <BSUGAREV>-06.04.2023 16:02:22
*& Request No.  : GAP-059 – Outbound Packing profile for SPO-SLO flows
********************************************************************
*& Description
*&
********************************************************************
    " Field level switch on/off
    DATA(lt_switch_fields) = VALUE ztt_switch_fields( ( field       = zif_switch_const=>c_packprofile
                                                        field_value = is_tpack-packprofile ) ).

    IF zcl_switch=>get_switch_state( iv_lgnum  = is_tpack-lgnum
                                     iv_devid  = zif_switch_const=>c_zout_008
                                     it_fields = lt_switch_fields ) EQ abap_false.
      RETURN.
    ENDIF.

    set_global_data( is_tpack = is_tpack
                     it_to    = it_to
                     it_pmat  = it_pmat ).

    IF gs_whopacklim-not_stock_mix = abap_true.
      DATA(ls_pack_group) = VALUE #( gt_pack_group[ lgnum = is_tpack-lgnum
                                                    whocr_pp = is_tpack-packprofile ] OPTIONAL ).

      DATA(lt_gr_tasks) = group_to( is_grouping = ls_pack_group
                                    it_tasks = it_to ).

      " only one group per WHO. Try to pack the first group, next TOs will be packed in the next WHO
      LOOP AT lt_gr_tasks ASSIGNING FIELD-SYMBOL(<ls_gr_tasks>) FROM 2.
        APPEND LINES OF <ls_gr_tasks>-tasks TO ct_to.
        DELETE lt_gr_tasks.
      ENDLOOP.
    ELSE.
      lt_gr_tasks = VALUE #( ( key = ''
                               tasks = it_to ) ).
    ENDIF.

    pack_tasks(
      EXPORTING
        is_wcr      = is_wcr
        is_pmat     = VALUE #( it_pmat[ 1 ] OPTIONAL )
        is_gr_tasks = VALUE #( lt_gr_tasks[ 1 ] OPTIONAL )
      IMPORTING
        et_whohu    = ct_whohu
        et_to       = DATA(lt_to)
        et_packed   = ct_packed
        et_failed   = ct_failed ).

    APPEND LINES OF lt_to TO ct_to.
  ENDMETHOD.


  METHOD build_list_mfrnr.
********************************************************************
*& Key          : BSUGAREV-Jan 5, 2024
*& Request No.  : GAP-059 – Outbound Packing profile for SPO-SLO flows
********************************************************************
*& Description  :
*&
*&
********************************************************************
    IF lines( it_to ) = 0.
      RETURN.
    ENDIF.

    DATA(lo_mat_det) = NEW /scwm/cl_ui_stock_fields( ).

    lo_mat_det->prefetch_matkey_by_id(
      EXPORTING
        it_matid        = VALUE #( FOR <l> IN it_to ( matid = <l>-matid ) )
      IMPORTING
        et_matid_extkey = DATA(lt_matnr) ).

    DATA(lt_selopt_matnr) = VALUE rseloption( FOR <mt> IN lt_matnr WHERE ( matnr IS NOT INITIAL )
        ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <mt>-matnr ) ).

    IF lines( lt_selopt_matnr ) = 0.
      RETURN.
    ENDIF.

    SELECT FROM mara
         FIELDS matnr, mfrnr
          WHERE matnr IN @lt_selopt_matnr
           INTO TABLE @DATA(lt_mfrnr).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_matnr ASSIGNING FIELD-SYMBOL(<ls_mat>).

      DATA(lr_mfrnr) = REF #( lt_mfrnr[ matnr = <ls_mat>-matnr ] OPTIONAL ).

      CHECK lr_mfrnr IS NOT INITIAL.

      gt_mat_mfrnr = VALUE #( BASE gt_mat_mfrnr ( matid = <ls_mat>-matid
                                                  matnr = <ls_mat>-matnr
                                                  mfrnr = lr_mfrnr->mfrnr ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD capa_check_free.
********************************************************************
*& Key          : BSUGAREV-Jan 5, 2024
*& Request No.  : GAP-059 – Outbound Packing profile for SPO-SLO flows
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA: lv_tasks_volume TYPE /scwm/de_quantity,
          lv_tasks_weight TYPE /scwm/de_quantity.

    CLEAR: et_failed, et_packed.

    IF gs_whopacklim-capa_check_free IS INITIAL OR lines( it_packed ) = 0.

      et_packed = it_packed.
      RETURN.
    ENDIF.

    DATA(ls_task) = VALUE #( it_packed[ 1 ] ).
    DATA(lv_free_lim_volume) = CONV /scwm/de_quantity( gs_packmat-max_volume -
              ( gs_packmat-max_volume * ( gs_whopacklim-capa_check_free / 100 ) ) ).

    DATA(lv_free_lim_weight) = CONV /scwm/de_quantity( gs_packmat-max_weight -
              ( gs_packmat-max_weight * ( gs_whopacklim-capa_check_free / 100 ) ) ).

    lv_tasks_volume = REDUCE #( INIT x = lv_tasks_volume FOR <l> IN it_packed NEXT x += <l>-volum ).
    lv_tasks_weight = REDUCE #( INIT x = lv_tasks_weight FOR <l> IN it_packed NEXT x += <l>-weight ).

    DATA(lo_converter) = /scmb/cl_md_access_mdl=>get_md_access( ).

    IF ls_task-unit_v <> gs_packmat-unit_v.

      TRY.
          lv_tasks_volume = lo_converter->prod_quan_conversion(
              iv_prodid    = ls_task-matid
              iv_uom_from  = ls_task-unit_v
              iv_uom_to    = gs_packmat-unit_v
              iv_quan      = lv_tasks_volume ).
          ##NO_HANDLER
        CATCH /scmb/cx_md_access.
      ENDTRY.

    ENDIF.

    IF ls_task-unit_w <> gs_packmat-unit_w.
      TRY.
          lv_tasks_weight = lo_converter->prod_quan_conversion(
              iv_prodid    = ls_task-matid
              iv_uom_from  = ls_task-unit_w
              iv_uom_to    = gs_packmat-unit_w
              iv_quan      = lv_tasks_weight ).
          ##NO_HANDLER
        CATCH /scmb/cx_md_access.
      ENDTRY.

    ENDIF.

    IF lv_tasks_volume <= lv_free_lim_volume AND lv_tasks_weight <= lv_free_lim_weight.
      APPEND LINES OF it_packed TO et_failed.
    ELSE.
      APPEND LINES OF it_packed TO et_packed.
    ENDIF.
  ENDMETHOD.


  METHOD check_1dlv_capacity.
********************************************************************
*& Key          : BSUGAREV-Jan 5, 2024
*& Request No.  : GAP-059 – Outbound Packing profile for SPO-SLO flows
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA: lv_docid              TYPE /scwm/de_docid,
          lt_selopt_to_failed   TYPE rseloption,
          lt_mat_for_cuboid_alg TYPE zcl_cuboid_algorithm=>tt_mat_cuboid_input.

    es_gr_tasks = is_gr_tasks.

    IF gs_whopacklim-capa_check_1dlv = abap_false.
      RETURN.
    ENDIF.

    DATA(lo_converter) = NEW /scwm/cl_dlv_md_access( ).

    " CAPA_CHECK_1DLV (Max capacity check for 1 DLV in 1 HU)
    CLEAR: lv_docid, lt_selopt_to_failed.

    LOOP AT es_gr_tasks-tasks ASSIGNING FIELD-SYMBOL(<ls_temp>) GROUP BY ( rdocid = <ls_temp>-rdocid )
                              ASSIGNING FIELD-SYMBOL(<ls_docid_group>).

      CLEAR: lt_mat_for_cuboid_alg.

      lv_docid = <ls_docid_group>-rdocid.

      LOOP AT GROUP <ls_docid_group> ASSIGNING FIELD-SYMBOL(<ls_doc_tasks>).

        DATA(ls_mat_for_cuboid) = REF #( lt_mat_for_cuboid_alg[ 1 ] OPTIONAL ).

        IF ls_mat_for_cuboid IS INITIAL.
          lt_mat_for_cuboid_alg = VALUE #( ( matid = <ls_doc_tasks>-matid
                                             quan  = <ls_doc_tasks>-vsolm
                                             meins = <ls_doc_tasks>-meins ) ).
          CONTINUE.
        ENDIF.

        IF ls_mat_for_cuboid->meins <> <ls_doc_tasks>-meins.

          TRY.
              <ls_doc_tasks>-vsolm = lo_converter->/scdl/if_af_quantity_conversio~convert_unit_quantity_single(
                  iv_productid = <ls_doc_tasks>-matid
                  iv_uom_from  = <ls_doc_tasks>-meins
                  iv_uom_to    = ls_mat_for_cuboid->meins
                  iv_qty_from  = <ls_doc_tasks>-vsolm ).
              ##NO_HANDLER
            CATCH /scdl/cx_af_quantity_conversio.
          ENDTRY.

        ENDIF.

        ls_mat_for_cuboid->quan += <ls_doc_tasks>-vsolm.

      ENDLOOP.

      TRY.
          DATA(lt_max_packed) = NEW zcl_cuboid_algorithm( gs_whopacklim-lgnum )->pack_by_best_pmat(
              it_materials   = lt_mat_for_cuboid_alg
              it_packmat     = VALUE #( ( gs_packmat ) ) ).
          ##NO_HANDLER
        CATCH zcx_core_exception.
      ENDTRY.

      IF lines( lt_max_packed ) = 0.
        CONTINUE.
      ENDIF.

      TRY.
          DATA(lv_max_pc_in_pmat) = VALUE #( lt_max_packed[ 1 ]-mat_details[ 1 ]-pc_max ).
          DATA(lv_packed_pc_in_pmat) = REDUCE int2( INIT x = 0 FOR <ppc> IN lt_max_packed[ 1 ]-mat_details NEXT x += <ppc>-pc_packed ).

        CATCH cx_sy_itab_line_not_found.
          CLEAR: lv_max_pc_in_pmat, lv_packed_pc_in_pmat.
      ENDTRY.

      " if DLV is packed in more then 1 HU -> this fails
      " if DLV packed qty is greater then max packing qty - 2 -> this fails
      IF lines( lt_max_packed ) > 1 OR ( lv_packed_pc_in_pmat > ( lv_max_pc_in_pmat - 2 ) ).
        LOOP AT es_gr_tasks-tasks ASSIGNING FIELD-SYMBOL(<ls_task_fail>) WHERE rdocid = lv_docid.
          lt_selopt_to_failed = VALUE #( BASE lt_selopt_to_failed
              ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <ls_task_fail>-tanum ) ).

          et_failed_to = VALUE #( BASE et_failed_to ( <ls_task_fail> ) ).
        ENDLOOP.
      ENDIF.

    ENDLOOP.

    IF lines( lt_selopt_to_failed ) > 0.
      DELETE es_gr_tasks-tasks WHERE tanum IN lt_selopt_to_failed.
    ENDIF.

  ENDMETHOD.


  METHOD group_to.
********************************************************************
*& Key          : BSUGAREV-Jan 5, 2024
*& Request No.  : GAP-059 – Outbound Packing profile for SPO-SLO flows
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA: lv_key       TYPE string.

    IF is_grouping IS INITIAL.
      RETURN.
    ENDIF.

    " Is there anything to group?
    IF is_grouping-stock_type = abap_false AND
       is_grouping-stock_usage = abap_false AND
       is_grouping-owner = abap_false AND
       is_grouping-entitled = abap_false AND
       is_grouping-mfrnr = abap_false.

      RETURN.
    ENDIF.

    " Compare key to the first
    LOOP AT it_tasks ASSIGNING FIELD-SYMBOL(<ls_to>).

      lv_key = |{ COND string( WHEN is_grouping-stock_type  = abap_true THEN <ls_to>-cat ) }+| &
               |{ COND string( WHEN is_grouping-stock_usage = abap_true THEN <ls_to>-stock_usage ) }+| &
               |{ COND string( WHEN is_grouping-owner       = abap_true THEN <ls_to>-owner  ) }+| &
               |{ COND string( WHEN is_grouping-entitled    = abap_true THEN <ls_to>-entitled ) }+| &
               |{ COND string( WHEN is_grouping-mfrnr       = abap_true THEN
                       VALUE #( gt_mat_mfrnr[ matid = <ls_to>-matid ]-mfrnr OPTIONAL ) ) }|.

      DATA(ls_exist) = REF #( rt_groups[ key = lv_key ] OPTIONAL ).
      IF ls_exist IS INITIAL.
        rt_groups = VALUE #( BASE rt_groups ( key = lv_key tasks = VALUE #( ( <ls_to> ) ) ) ).
      ELSE.
        APPEND <ls_to> TO ls_exist->tasks.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD pack_tasks.
********************************************************************
*& Key          : BSUGAREV-Jan 5, 2024
*& Request No.  : GAP-059 – Outbound Packing profile for SPO-SLO flows
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA: lv_full_tote        TYPE boole_d,
          ls_huhdr_out        TYPE /scwm/s_huhdr_int,
          lt_dlv_tasks        TYPE tt_to_groups,
          lt_materials_cuboid TYPE zcl_cuboid_algorithm=>tt_mat_cuboid_input,
          lt_selopt_tos       TYPE rseloption.

    FIELD-SYMBOLS <ls_dlv_task> TYPE ty_to_groups.

    check_1dlv_capacity(
      EXPORTING
        is_gr_tasks = is_gr_tasks
      IMPORTING
        et_failed_to = et_failed
        es_gr_tasks  = DATA(ls_gr_tasks) ).

    IF ls_gr_tasks IS INITIAL.
      RETURN.
    ENDIF.

    " take for max PC smaller value maintained in the limitation table
    DATA(lv_max_allowed_dlvs) = gs_whopacklim-pack_max.

    IF lv_max_allowed_dlvs IS INITIAL.
      lv_max_allowed_dlvs = 32767. " max allowed number by domain
    ENDIF.

    LOOP AT ls_gr_tasks-tasks ASSIGNING FIELD-SYMBOL(<ls_dummy>) GROUP BY ( rdocid = <ls_dummy>-rdocid )
                              ASSIGNING FIELD-SYMBOL(<ls_tasks_by_dlv>).

      APPEND INITIAL LINE TO lt_dlv_tasks ASSIGNING <ls_dlv_task>.
      <ls_dlv_task>-key = <ls_tasks_by_dlv>-rdocid.

      LOOP AT GROUP <ls_tasks_by_dlv> ASSIGNING FIELD-SYMBOL(<ls_task_dlv>).
        APPEND <ls_task_dlv> TO <ls_dlv_task>-tasks.
      ENDLOOP.

    ENDLOOP.

    DATA(lv_dlv_counter) = 0.
    LOOP AT lt_dlv_tasks ASSIGNING <ls_dlv_task>.

      lv_dlv_counter += 1.

      IF lv_max_allowed_dlvs IS NOT INITIAL AND lv_dlv_counter > lv_max_allowed_dlvs.
        EXIT.
      ENDIF.

      CLEAR: lt_selopt_tos.
      LOOP AT <ls_dlv_task>-tasks ASSIGNING FIELD-SYMBOL(<ls_task>).

        lt_selopt_tos = VALUE #( BASE lt_selopt_tos
            ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <ls_task>-tanum ) ).

        lt_materials_cuboid = VALUE #( BASE lt_materials_cuboid
            ( matid = <ls_task>-matid
              quan  = <ls_task>-vsolm
              meins = <ls_task>-meins ) ).

        TRY.
            DATA(lt_max_packed) = NEW zcl_cuboid_algorithm( gs_whopacklim-lgnum )->pack_by_best_pmat(
                it_materials = lt_materials_cuboid
                it_packmat   = VALUE #( ( gs_packmat ) ) ).
            ##NO_HANDLER
          CATCH zcx_core_exception.
        ENDTRY.

        IF lines( lt_max_packed ) = 1.
          APPEND <ls_task> TO et_packed.

          CONTINUE.
        ENDIF.

        " combination is packed in 2 totes, so we have to remove the last DLV tasks and try a combination with the next one
        IF gs_whopacklim-not_odo_split = abap_true.
          lv_dlv_counter -= 1.

          " SPLIT is not allowed -> remove all packed tasks from current DLV
          DELETE et_packed WHERE tanum IN lt_selopt_tos.

          " rebuild material list with already packed materials and try a new combination with tasks from next DLV
          lt_materials_cuboid = VALUE #( FOR <ls_p> IN et_packed
              ( matid = <ls_p>-matid
                quan  = <ls_p>-vsolm
                meins = <ls_p>-meins ) ).
        ELSE.
          lv_full_tote = abap_true.
        ENDIF.

        EXIT.
      ENDLOOP.

      IF lv_full_tote = abap_true.
        EXIT.
      ENDIF.

    ENDLOOP.

    " filter tasks for the next WHO
    LOOP AT ls_gr_tasks-tasks ASSIGNING FIELD-SYMBOL(<ls_task_next_who>) .

      IF NOT line_exists( et_packed[ tanum = <ls_task_next_who>-tanum ] ).
        APPEND <ls_task_next_who> TO et_to.
      ENDIF.

    ENDLOOP.

    " CAPA_CHECK_FREE
    capa_check_free(
      EXPORTING
        it_packed = et_packed
      IMPORTING
        et_failed = DATA(lt_failed)
        et_packed = DATA(lt_packed) ).

    et_packed = lt_packed.
    APPEND LINES OF lt_failed TO et_failed.

    IF lines( et_packed ) = 0.
      RETURN.
    ENDIF.

    " now create the pick HU, update tasks and WHOHU
    DATA(ls_param) = VALUE /scwm/hum_create_attributes_s(
        appl = wmegc_huappl_wme
        lgnum = gs_whopacklim-lgnum
        no_huident = abap_true
        no_update  = abap_true ).

    DATA(ls_huhdr) = VALUE /scwm/s_huhdr_create_int( pmat_guid = gs_packmat-pmat_guid ).

    CALL FUNCTION '/SCWM/HUHDR_CREATE'
      EXPORTING
        is_param            = ls_param
        is_hdr              = ls_huhdr
      IMPORTING
        es_huhdr            = ls_huhdr_out
      EXCEPTIONS
        no_packing_material = 1
        error               = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    DATA(ls_whohu) = VALUE /scwm/s_whohu( lgnum = is_wcr-lgnum
                                          hukng = 1
                                          pmat_guid = is_pmat-pmat_guid
                                          huid  = ls_huhdr_out-guid_hu
                                          prces = is_wcr-prces
                                          wcr    = is_wcr-wcr
                                          updkz  = zif_wme_c=>gs_whohu_upd-create ).
    APPEND ls_whohu TO et_whohu.

    LOOP AT et_packed ASSIGNING FIELD-SYMBOL(<ls_packed>).
      <ls_packed>-huid = ls_huhdr_out-guid_hu.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_global_data.
********************************************************************
*& Key          : BSUGAREV-Jan 5, 2024
*& Request No.  : GAP-059 – Outbound Packing profile for SPO-SLO flows
********************************************************************
*& Description  :
*&
*&
********************************************************************
    build_list_mfrnr( it_to = it_to ).

    " read the customizing
    IF lines( gt_pack_group ) = 0.
      gt_pack_group = NEW zcl_crud_ztout_who_packgr( )->select_multi_by_lgnum( is_tpack-lgnum ).
    ENDIF.

    " read the customizing
    IF lines( gt_whopacklim ) = 0.
      gt_whopacklim = NEW zcl_crud_ztout_whopacklim( )->select_multi_by_lgnum( is_tpack-lgnum ).
    ENDIF.

    gs_whopacklim = VALUE #( gt_whopacklim[ lgnum = is_tpack-lgnum
                                            whocr_pp = is_tpack-packprofile ] OPTIONAL ).

    gs_packmat = VALUE #( it_pmat[ 1 ] OPTIONAL ).
  ENDMETHOD.
ENDCLASS.
