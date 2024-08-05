CLASS zcl_out_who_pack_wt DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_out_who_pack_wt .

    ALIASES pack_mc_in_pallet
      FOR zif_out_who_pack_wt~pack_mc_in_pallet.
    ALIASES create_ship_hu
      FOR zif_out_who_pack_wt~create_ship_hu .
    ALIASES pack_tasks_in_carton
      FOR zif_out_who_pack_wt~pack_tasks_in_carton .
    ALIASES pack_tasks_in_shippmat
      FOR zif_out_who_pack_wt~pack_tasks_in_shippmat .

    METHODS constructor
      IMPORTING
        !iv_lgnum TYPE /scwm/lgnum .

    METHODS filter_pack_result_by_wocr
      IMPORTING
        iv_wocr            TYPE /scwm/de_wcr
        is_pack            TYPE zcl_ship_pmat_algorithm_base=>ty_pack_output
      RETURNING
        VALUE(rs_pack_out) TYPE zcl_ship_pmat_algorithm_base=>ty_pack_output.

    METHODS get_bin_auom
      IMPORTING
        it_to             TYPE /scwm/tt_ordim_o_int
      RETURNING
        VALUE(rt_bin_aum) TYPE zif_out_who_pack_wt=>tt_bin_auom.

  PROTECTED SECTION.

private section.

  data MO_CONVERTER type ref to /SCMB/CL_MD_ACCESS .
  data MO_PACK type ref to /SCWM/CL_WM_PACKING .
  data MV_LGNUM type /SCWM/LGNUM .

  methods SPLIT_TASK
    importing
      !IS_TASK type /SCWM/S_ORDIM_O_INT
      !IS_QUAN_FREE type /SCWM/S_QUAN
    exporting
      !ES_PACKED type /SCWM/S_ORDIM_O_INT
      !ES_TASK_SPLIT type /SCWM/S_ORDIM_O_INT .
  methods UPDATE_DIM
    importing
      !IV_BASE_UOM type BOOLE_D default ABAP_FALSE
    changing
      !CT_PACKED type /SCWM/TT_ORDIM_O_INT .
ENDCLASS.



CLASS ZCL_OUT_WHO_PACK_WT IMPLEMENTATION.


  METHOD constructor.
********************************************************************
*& Key          : BSUGAREV-Jan 5, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = mo_pack ).

    mo_pack->init(
      EXPORTING
        iv_lgnum               = iv_lgnum
      EXCEPTIONS
        error                  = 1
        OTHERS                 = 2 ).
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CALL FUNCTION '/SCWM/TO_INIT_NEW'
      EXPORTING
        iv_lgnum = iv_lgnum.

    mv_lgnum = iv_lgnum.

    mo_converter = /scmb/cl_md_access_mdl=>get_md_access( ).
  ENDMETHOD.


  METHOD filter_pack_result_by_wocr.
********************************************************************
*& Key          : <BSUGAREV>-21.06.2023 16:58:05
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  : filter pack result considering provided WOCR.
*&      In general stock must be packed in MC only if the WOCR is
*&      for master carton
********************************************************************
    rs_pack_out = is_pack.

    DATA(lt_fields) = VALUE ztt_switch_fields( ( field = zif_switch_const=>c_wocr
                                                 field_value = iv_wocr ) ).
    IF abap_true = zcl_switch=>get_switch_state( iv_lgnum  = mv_lgnum
                                                  iv_devid  = zif_switch_const=>c_zout_013
                                                  it_fields = lt_fields ).
      CLEAR: rs_pack_out-shipping.
    ELSE.
      CLEAR: rs_pack_out-carton.
    ENDIF.

  ENDMETHOD.


  METHOD get_bin_auom.
********************************************************************
*& Key          : BSUGAREV-Jan 5, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    IF lines( it_to ) = 0.
      RETURN.
    ENDIF.

    NEW /scwm/cl_mon_stock( mv_lgnum )->get_physical_stock(
      EXPORTING
        iv_skip_bin      = abap_false
        iv_skip_resource = abap_true
        iv_skip_tu       = abap_true
        it_cat_r         = VALUE #( FOR <l> IN it_to ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <l>-cat ) )
        it_lgpla_r       = VALUE #( FOR <l> IN it_to ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <l>-vlpla ) )
      IMPORTING
        et_stock_mon     = DATA(lt_phys_stock) ).

    SORT lt_phys_stock BY lgpla quan DESCENDING.

    LOOP AT lt_phys_stock ASSIGNING FIELD-SYMBOL(<ls_stock>).

      CHECK <ls_stock>-meins <> <ls_stock>-altme.

      DATA(ls_phys_stock) = REF #( rt_bin_aum[ bin = <ls_stock>-lgpla ] OPTIONAL ).

      CHECK ls_phys_stock IS INITIAL.

      rt_bin_aum = VALUE #( BASE rt_bin_aum ( bin = <ls_stock>-lgpla uom = <ls_stock>-altme ) ).

      DELETE lt_phys_stock WHERE lgpla = <ls_stock>-lgpla.
    ENDLOOP.
  ENDMETHOD.


  METHOD split_task.
********************************************************************
*& Key          : BSUGAREV-Jan 5, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA(lo_conv) = NEW /scwm/cl_dlv_md_access( ).
    es_packed = is_task.

    es_task_split = is_task.
    es_task_split-split += 1.

    es_packed-split = es_task_split-split + 1.

    DATA(lv_qty_aum) = is_quan_free-quan.
    IF is_task-altme <> is_quan_free-unit.
      TRY.
          lv_qty_aum = lo_conv->/scdl/if_af_quantity_conversio~convert_unit_quantity_single(
              iv_productid = is_task-matid
              iv_uom_from  = is_quan_free-unit
              iv_uom_to    = is_task-altme
              iv_qty_from  = is_quan_free-quan ).
          ##NO_HANDLER
        CATCH /scdl/cx_af_quantity_conversio.
      ENDTRY.
    ENDIF.

    es_task_split-vsolm = es_packed-vsolm - is_quan_free-quan.
    es_task_split-vsola = es_packed-vsola - lv_qty_aum.

    es_packed-vsolm = is_quan_free-quan.
    es_packed-vsola = lv_qty_aum.
  ENDMETHOD.


  METHOD update_dim.
********************************************************************
*& Key          : BSUGAREV-Jan 5, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA: lv_quan    TYPE /scwm/ltap_vsola,
          ls_mat_uom TYPE /scwm/s_material_uom.

    IF lines( ct_packed ) = 0.
      RETURN.
    ENDIF.

    DATA(lo_prod) = CAST /scwm/if_af_product( /scdl/cl_af_management=>get_instance(
        )->get_service( /scwm/if_af_product=>sc_me_as_service ) ).
    TRY.
        lo_prod->read_material_multiple(
          EXPORTING
            it_matid           = VALUE #( FOR <l> IN ct_packed ( <l>-matid ) )
          IMPORTING
            et_mat_uom         = DATA(lt_mat_uom) ).
        ##NO_HANDLER
      CATCH /scwm/cx_md_api_faulty_call.
        ##NO_HANDLER
      CATCH /scwm/cx_md_exception.
    ENDTRY.

    LOOP AT ct_packed ASSIGNING FIELD-SYMBOL(<ls_mat>).
      CLEAR: ls_mat_uom.

      IF iv_base_uom = abap_true.
        ls_mat_uom = VALUE #( lt_mat_uom[ matid = <ls_mat>-matid meinh = <ls_mat>-meins ] OPTIONAL ).
        lv_quan = <ls_mat>-vsolm.
      ELSE.
        ls_mat_uom = VALUE #( lt_mat_uom[ matid = <ls_mat>-matid meinh = <ls_mat>-altme ] OPTIONAL ).
        lv_quan = <ls_mat>-vsola.
      ENDIF.

      CHECK ls_mat_uom IS NOT INITIAL.

      <ls_mat>-volum = lv_quan * ls_mat_uom-volum.
      <ls_mat>-unit_v = ls_mat_uom-voleh.

      <ls_mat>-weight = lv_quan * ls_mat_uom-brgew.
      <ls_mat>-unit_w = ls_mat_uom-gewei.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_out_who_pack_wt~create_ship_hu.
********************************************************************
*& Key          : <BSUGAREV>-02.06.2023 15:11:45
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  : Create shipping HU
*&
********************************************************************
    DATA(ls_param) = is_param.
    IF ls_param-appl IS INITIAL.
      ls_param-appl = wmegc_huappl_wme.
    ENDIF.

    IF ls_param-lgpla IS NOT INITIAL.

      mo_pack->create_hu(
        EXPORTING
          iv_pmat      = is_huhdr_crea-pmat_guid
          i_location   = ls_param-lgpla
        RECEIVING
          es_huhdr     = rs_huhdr
        EXCEPTIONS
          error        = 1
          OTHERS       = 2 ).
      IF sy-subrc <> 0.
        ##NEEDED
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lv_msg).
      ENDIF.
    ENDIF.

    IF rs_huhdr IS INITIAL.
      CALL FUNCTION '/SCWM/HUHDR_CREATE'
        EXPORTING
          is_param            = ls_param
          is_hdr              = is_huhdr_crea
        IMPORTING
          es_huhdr            = rs_huhdr
        EXCEPTIONS
          no_packing_material = 1
          error               = 2
          OTHERS              = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_out_who_pack_wt~pack_mc_in_pallet.
********************************************************************
*& Key          : <BSUGAREV>-05.06.2023 10:07:05
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description
*&
********************************************************************
    DATA: lv_init_volum TYPE /scwm/de_maxv,
          ls_pallet_hu  TYPE /scwm/s_huhdr_int.

    IF lines( it_pmats ) = 0 OR lines( it_cartons ) = 0.
      RETURN.
    ENDIF.

    DATA(lt_pmats) = it_pmats.

    LOOP AT lt_pmats ASSIGNING FIELD-SYMBOL(<ls_pmat>).

      CHECK <ls_pmat>-unit_v <> zif_wme_c=>gs_uom-cdm.

      TRY.
          <ls_pmat>-max_volume = mo_converter->prod_quan_conversion(
              iv_prodid    = <ls_pmat>-pmat_guid
              iv_uom_from  = <ls_pmat>-unit_v
              iv_uom_to    = zif_wme_c=>gs_uom-cdm
              iv_quan      = CONV #( <ls_pmat>-max_volume ) ).

          <ls_pmat>-unit_v = zif_wme_c=>gs_uom-cdm.
          ##NO_HANDLER
        CATCH /scmb/cx_md_access.
      ENDTRY.
    ENDLOOP.

    SORT lt_pmats BY max_volume DESCENDING.
    DATA(ls_biggest_pmat) = VALUE #( lt_pmats[ 1 ] ).

    LOOP AT it_cartons INTO DATA(ls_carton).
      CLEAR: lv_init_volum.

      DATA(lv_packed_volum) = REDUCE /scwm/de_maxv( INIT v = lv_init_volum FOR <l> IN rt_pallet_packed WHERE ( shiphuid = ls_pallet_hu-guid_hu ) NEXT v += <l>-volum ).

      IF ls_pallet_hu-unit_gv IS NOT INITIAL AND ls_carton-unit_v <> ls_pallet_hu-unit_gv.
        TRY.
            ls_carton-volum = mo_converter->prod_quan_conversion(
                iv_prodid   = ls_carton-matid
                iv_uom_from = ls_carton-unit_v
                iv_uom_to   = ls_pallet_hu-unit_gv
                iv_quan     = CONV #( ls_carton-volum ) ).
            ##NO_HANDLER
          CATCH /scmb/cx_md_access.
        ENDTRY.
      ENDIF.

      IF ls_pallet_hu-max_volume <= ( lv_packed_volum + ls_carton-volum ).
        CLEAR: ls_pallet_hu.
      ENDIF.

      IF ls_pallet_hu IS INITIAL.
        ls_pallet_hu = create_ship_hu( is_param = is_hucrea_param
                                   is_huhdr_crea = VALUE #( pmat_guid  = ls_biggest_pmat-pmat_guid ) ).
      ENDIF.

      ls_carton-shiphuid = ls_pallet_hu-guid_hu.

      rt_pallet_packed = VALUE #( BASE rt_pallet_packed ( ls_carton ) ).

    ENDLOOP.

    SORT lt_pmats BY max_volume ASCENDING.
    LOOP AT rt_pallet_packed ASSIGNING FIELD-SYMBOL(<ls_dummy>) GROUP BY ( shiphuid = <ls_dummy>-shiphuid )
                             ASSIGNING FIELD-SYMBOL(<ls_pallet>).
      CLEAR: lv_init_volum.

      DATA(lv_packed_cart_volum) = REDUCE /scwm/de_maxv( INIT v = lv_init_volum FOR <l> IN GROUP <ls_pallet> NEXT v += <l>-volum ).

      LOOP AT lt_pmats ASSIGNING FIELD-SYMBOL(<ls_best_pmat>) WHERE max_volume >= lv_packed_cart_volum.
        DATA(ls_best_pmat) = <ls_best_pmat>.
        EXIT.
      ENDLOOP.

      CHECK ls_biggest_pmat-pmat_guid <> ls_best_pmat-pmat_guid.

      mo_pack->/scwm/if_pack_bas~change_huhdr(
        EXPORTING
          is_huhdr   = VALUE #( guid_hu    = <ls_pallet>-shiphuid
                                pmat_guid  = ls_best_pmat-pmat_guid
                                max_volume = ls_best_pmat-max_volume
                                unit_gv    = ls_best_pmat-unit_v
                                max_weight = ls_best_pmat-max_weight
                                unit_gw    = ls_best_pmat-unit_w )
        EXCEPTIONS
          error      = 1
          not_locked = 2
          OTHERS     = 3 ).
      IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

    ENDLOOP.

    mo_pack->save(
      EXPORTING
        iv_commit = abap_false
        iv_wait   = abap_false
      EXCEPTIONS
        error     = 1
        OTHERS    = 2 ).
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDMETHOD.


  METHOD zif_out_who_pack_wt~pack_tasks_in_carton.
********************************************************************
*& Key          : <BSUGAREV>-02.06.2023 15:11:45
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  : WHO packing implementation
*&
********************************************************************
    DATA: lv_packed_success TYPE boole_d,
          lv_init_qty       TYPE /scwm/s_quan-quan,
          lt_to_delete      TYPE /scwm/tt_ordim_o_int,
          lt_to_append      TYPE /scwm/tt_ordim_o_int.

    FIELD-SYMBOLS: <ls_to> TYPE /scwm/s_ordim_o_int.

    CLEAR: et_packed, et_to_next_who, ev_pmatid, ev_split.

    DATA(lt_bin_auom) = get_bin_auom( it_tasks ).

    et_to_next_who = it_tasks.
    SORT et_to_next_who BY vsolm DESCENDING.

    " each carton must have only one material, mixed materials are not possible
    IF lines( is_carton-mat_packed ) = 0.
      RETURN.
    ENDIF.

    DATA(ls_mat_packed) = is_carton-mat_packed[ 1 ].

    " first check if there is a task with qty for complete carton
    LOOP AT et_to_next_who ASSIGNING <ls_to> WHERE matid = ls_mat_packed-matid
                                                             AND vsolm = ls_mat_packed-quan.

*      IF NOT line_exists( lt_bin_auom[ bin = <ls_to>-vlpla ] ). "UM 09.02.2024 - Replaced with the following line: add the AUoM check
      IF NOT line_exists( lt_bin_auom[ bin = <ls_to>-vlpla uom = ls_mat_packed-auom ] ).
        CONTINUE.
      ENDIF.

      " get packing indicator by activity AAREA. No MC packing for contants maintain for switch ID ZOUT_014
      CHECK abap_false = zcl_switch=>get_switch_state(
                            iv_lgnum  = mv_lgnum
                            iv_devid  = zif_switch_const=>c_zout_014
                            it_fields = VALUE #( ( field = zif_switch_const=>c_aarea field_value = <ls_to>-aarea ) ) ).

      lv_packed_success = abap_true.

      APPEND <ls_to> TO et_packed.

      DELETE et_to_next_who.

      EXIT.
    ENDLOOP.

    IF lv_packed_success = abap_true.
      ev_pmatid = is_carton-pmatid.
      update_dim( CHANGING ct_packed = et_packed ).

      RETURN.
    ENDIF.

    DATA(lt_to_next_who) = et_to_next_who.

    " update AUoM if needed
    LOOP AT lt_to_next_who ASSIGNING <ls_to>.
      DATA(ls_sbin_auom) = VALUE #( lt_bin_auom[ bin = <ls_to>-vlpla ] OPTIONAL ).

      CHECK ls_sbin_auom IS NOT INITIAL.

      IF <ls_to>-altme <> ls_sbin_auom-uom.
        TRY.
            DATA(lv_conv_qty) = mo_converter->prod_quan_conversion(
                iv_prodid   = <ls_to>-matid
                iv_uom_from = <ls_to>-meins
                iv_uom_to   = ls_sbin_auom-uom
                iv_quan     = <ls_to>-vsolm ).

            <ls_to>-vsola = lv_conv_qty.
            <ls_to>-altme = ls_sbin_auom-uom.
            ##NO_HANDLER
          CATCH /scmb/cx_md_access.
        ENDTRY.
      ENDIF.
    ENDLOOP.

    SORT lt_to_next_who BY vsolm.
    DATA(lt_packed_temp) = VALUE /scwm/tt_ordim_o_int( ).

    LOOP AT lt_to_next_who ASSIGNING FIELD-SYMBOL(<dummy>) GROUP BY ( vlpla = <dummy>-vlpla
                                                                      matid = <dummy>-matid
                                                                      altme = <dummy>-altme
                                                                      aarea = <dummy>-aarea )
                           ASSIGNING FIELD-SYMBOL(<lt_to_group>).

      " validate tasks in the group have the same material and AUoM as the one packed into the carton
      CHECK <lt_to_group>-matid = ls_mat_packed-matid AND <lt_to_group>-altme = ls_mat_packed-auom.

      " get packing indicator by activity AAREA. No MC packing for constants maintain for switch ID ZOUT_014
      CHECK abap_false = zcl_switch=>get_switch_state(
                            iv_lgnum  = mv_lgnum
                            iv_devid  = zif_switch_const=>c_zout_014
                            it_fields = VALUE #( ( field = zif_switch_const=>c_aarea field_value = <lt_to_group>-aarea ) ) ).

      CLEAR: lv_init_qty.

      " calculate total qty of tasks in a group
      DATA(lv_qty_group) = REDUCE /scwm/s_quan-quan( INIT s = lv_init_qty FOR <s> IN GROUP <lt_to_group> NEXT s += <s>-vsola ).

      CHECK lv_qty_group >= 1.

      LOOP AT GROUP <lt_to_group> ASSIGNING <ls_to>.

        IF <ls_to>-vsolm <= ls_mat_packed-quan.
          ls_mat_packed-quan -= <ls_to>-vsolm.

          APPEND <ls_to> TO lt_packed_temp.

          " 20231002: remove current task from the list.
          " Check if quantity is already set to 0, in this case we should exit the packing
          APPEND <ls_to> TO lt_to_delete.

          IF ls_mat_packed-quan = 0.
            lv_packed_success = abap_true.
            EXIT.
          ENDIF.
        ELSE.
          split_task(
            EXPORTING
              is_task = <ls_to>
              is_quan_free = VALUE #( quan = ls_mat_packed-quan unit = ls_mat_packed-unit )
            IMPORTING
              es_packed     = DATA(ls_packed)
              es_task_split = DATA(ls_task_split) ).

          APPEND <ls_to> TO lt_to_delete.

          lv_packed_success = abap_true.

          APPEND ls_packed TO lt_packed_temp.
          IF ls_task_split-vsolm > 0.

            APPEND ls_task_split TO lt_to_append.
          ENDIF.

          ev_split = 1.

          EXIT.
        ENDIF.
      ENDLOOP.

      " pack only 1 carton per time
      EXIT.
    ENDLOOP.

    LOOP AT lt_to_delete ASSIGNING FIELD-SYMBOL(<ls_del>).
      DELETE TABLE lt_to_next_who FROM <ls_del>.
    ENDLOOP.

    IF lines( lt_to_append ) > 0.
      APPEND LINES OF lt_to_append TO lt_to_next_who.
    ENDIF.

    et_to_next_who = lt_to_next_who.

    IF lv_packed_success = abap_true.
      ev_pmatid = is_carton-pmatid.

      APPEND LINES OF lt_packed_temp TO et_packed.

      update_dim( CHANGING ct_packed = et_packed ).
    ELSE.
      et_to_next_who = it_tasks.
    ENDIF.

  ENDMETHOD.


  METHOD zif_out_who_pack_wt~pack_tasks_in_shippmat.
********************************************************************
*& Key          : <BSUGAREV>-02.06.2023 15:11:45
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  : WHO packing implementation
*&
********************************************************************
    DATA:  lv_aarea_delta TYPE /scwm/de_aarea.

    et_to_next_who = it_tasks.

    DATA(ls_shipping) = VALUE #( it_shipping[ 1 ] OPTIONAL ).
    IF ls_shipping IS INITIAL OR lines( ls_shipping-mat_packed ) = 0.
      RETURN.
    ENDIF.

    " choose tasks only by the same AAREA
    IF lines( it_bin_aarea ) > 0.
      DATA(lv_check_aarea) = abap_true.
    ENDIF.

    LOOP AT ls_shipping-mat_packed ASSIGNING FIELD-SYMBOL(<ls_mat>).

      DATA(lv_packed_success) = abap_false.
      LOOP AT et_to_next_who ASSIGNING FIELD-SYMBOL(<ls_to>) WHERE matid = <ls_mat>-matid
                                                               AND vsolm = <ls_mat>-quan.

        IF lv_check_aarea = abap_true AND lv_aarea_delta IS INITIAL.
          lv_aarea_delta = VALUE #( it_bin_aarea[ lgnum = <ls_to>-lgnum lgpla = <ls_to>-vlpla ]-aarea OPTIONAL ).
        ENDIF.

        lv_packed_success = abap_true.
        APPEND <ls_to> TO et_packed.
        DELETE et_to_next_who.
        EXIT.
      ENDLOOP.

      IF lv_packed_success = abap_true.
        DELETE ls_shipping-mat_packed.
      ENDIF.
    ENDLOOP.

    SORT et_to_next_who BY vsolm DESCENDING.

    DATA(lt_packed_temp) = VALUE /scwm/tt_ordim_o_int( ).

    LOOP AT ls_shipping-mat_packed ASSIGNING <ls_mat>.
      LOOP AT et_to_next_who ASSIGNING <ls_to> WHERE matid = <ls_mat>-matid.

        IF lv_check_aarea = abap_true.
          DATA(lv_task_sbin_aarea) = VALUE #( it_bin_aarea[ lgnum = <ls_to>-lgnum lgpla = <ls_to>-vlpla ]-aarea OPTIONAL ).
          IF lv_aarea_delta IS INITIAL.
            lv_aarea_delta = lv_task_sbin_aarea.
          ENDIF.

          CHECK lv_aarea_delta = lv_task_sbin_aarea.
        ENDIF.

        IF <ls_to>-vsolm <= <ls_mat>-quan.
          <ls_mat>-quan -= <ls_to>-vsolm.

          APPEND <ls_to> TO lt_packed_temp.
          DELETE et_to_next_who.

          IF <ls_mat>-quan = 0.
            EXIT.
          ENDIF.
        ELSE.
          split_task(
            EXPORTING
              is_task = <ls_to>
              is_quan_free = VALUE #( quan = <ls_mat>-quan unit = <ls_mat>-unit )
            IMPORTING
              es_packed     = DATA(ls_packed)
              es_task_split = DATA(ls_task_split) ).

          DELETE et_to_next_who.

          APPEND ls_packed TO lt_packed_temp.

          IF ls_task_split-vsolm > 0.
            APPEND ls_task_split TO et_to_next_who.
          ENDIF.

          ev_split = 1.
          EXIT.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

    APPEND LINES OF lt_packed_temp TO et_packed.

    ev_pmatid = ls_shipping-pmatid.

    update_dim( EXPORTING iv_base_uom = abap_true
                CHANGING ct_packed = et_packed ).
  ENDMETHOD.
ENDCLASS.
