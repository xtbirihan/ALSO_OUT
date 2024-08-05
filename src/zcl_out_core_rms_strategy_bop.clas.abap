class ZCL_OUT_CORE_RMS_STRATEGY_BOP definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_CORE_RMS_STRATEGY .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.

  data MV_LGNUM type /SCWM/LGNUM .

  methods AVAIL_STOCK
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_MATID type /SCWM/DE_MATID
      !IV_LGTYP type /SCWM/LGTYP
      !IV_STOCK_CAT type /LIME/STOCK_CATEGORY
    returning
      value(RT_AVAIL_STOCK) type /SCWM/TT_AQUA .
  methods CALC_AVAIL_QTY
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_MATID type /SCWM/DE_MATID
      !IV_STOCK_CAT type /LIME/STOCK_CATEGORY
      !IT_STTYPES type ZTT_PARAM_LIST
    returning
      value(RV_SUM_QTY) type /SCWM/DE_AVLQUAN .
  methods GET_ITEM_QTY
    importing
      !IV_DOCID type /SCWM/DE_DOCID
      !IV_ITEMID type /SCWM/DE_ITMID
      !IV_DOCCAT type /SCWM/DE_DOCCAT
    returning
      value(RV_ITEM_QTY) type /SCDL/DL_QUANTITY .
  methods GET_PARAMETER
    importing
      !IV_PARAMETER type ZDE_PARAM_ID2
    returning
      value(RV_CONST) type ZDE_PARAM_LOW .
  methods GET_PARAMETER_LIST
    importing
      !IV_PARAMETER type ZDE_PARAM_ID2
    returning
      value(RT_LIST) type ZTT_PARAM_LIST .
ENDCLASS.



CLASS ZCL_OUT_CORE_RMS_STRATEGY_BOP IMPLEMENTATION.


  METHOD /scwm/if_ex_core_rms_strategy~strategy.
**********************************************************************
*& Key           : RM-230607
*& Request No.   : GAP 35 Picking strategies
**********************************************************************
*& Description (short)
*&
**********************************************************************

    DATA:
      lv_dyn_qty  TYPE /scwm/de_avlquan,
      lv_stat_qty TYPE /scwm/de_avlquan.

    IF zcl_switch=>get_switch_state( iv_lgnum = is_ltap-lgnum
                                     iv_devid = zif_switch_const=>c_zout_009 ) EQ abap_false.
      RETURN.
    ENDIF.

    IF is_ltap-trart <> wmegc_trart_pick.
      RETURN.
    ENDIF.

    mv_lgnum = is_ltap-lgnum.

    IF is_mat_lgnum-zz1_disp_whd <> zif_wme_c=>gs_matdisp-can_dispatch AND
       is_mat_lgnum-zz1_disp_whd <> zif_wme_c=>gs_matdisp-dispatch.
      RETURN.
    ENDIF.

    DATA(lt_put_ctr_ind) = get_parameter_list( iv_parameter = zif_param_const=>c_put_ctr_ind ).

    DATA(lr_put_ctrl) = VALUE rseloption( FOR <ls_put_ctr_ind> IN lt_put_ctr_ind
                                          ( low    = <ls_put_ctr_ind>
                                            sign   = wmegc_sign_inclusive
                                            option = wmegc_option_eq ) ).

    IF is_mat_lgnum-put_stra NOT IN lr_put_ctrl.
      RETURN.
    ENDIF.

    DATA(lt_mat_uom) = it_mat_uom.
    SORT lt_mat_uom BY umrez DESCENDING.

    DATA(lv_biggest_qty) = VALUE #( lt_mat_uom[ 1 ]-umrez OPTIONAL ).

    DATA(lv_qty_to_chk) = CONV /scdl/dl_quantity( lv_biggest_qty * is_mat_lgnum-zz1_minnummcinpack_whd ).

    DATA(lv_total_qty) = get_item_qty( iv_docid  = is_ltap-rdocid
                                       iv_itemid = is_ltap-ritmid
                                       iv_doccat = is_ltap-rdoccat ).

    IF lv_qty_to_chk > lv_total_qty.
      RETURN.
    ENDIF.

    DATA(lt_stat_sttyp) = get_parameter_list( iv_parameter = zif_param_const=>c_pack_stat ).
    DATA(lt_dyn_sttyp)  = get_parameter_list( iv_parameter = zif_param_const=>c_pack_dyn ).

    lv_stat_qty = calc_avail_qty( iv_lgnum     = is_ltap-lgnum
                                  iv_matid     = is_mat_global-matid
                                  iv_stock_cat = is_ltap-cat
                                  it_sttypes   = lt_stat_sttyp ).

    IF lv_stat_qty > lv_total_qty.
      RETURN.
    ENDIF.

    lv_dyn_qty = calc_avail_qty( iv_lgnum     = is_ltap-lgnum
                                 iv_matid     = is_mat_global-matid
                                 iv_stock_cat = is_ltap-cat
                                 it_sttypes   = lt_dyn_sttyp ).

* UM 28.06.2023 - Changed the quantity to be checked
*    IF lv_dyn_qty < lv_total_qty.
    IF lv_dyn_qty < iv_anfml.
* UM 28.06.2023 - Changed the quantity to be checked

      RETURN.
    ENDIF.

    CASE iv_rem_sseq.
      WHEN get_parameter( iv_parameter = zif_param_const=>c_pick_def_stss_1 ).
        ev_rem_sseq = get_parameter( iv_parameter = zif_param_const=>c_pick_str_stss_1 ).

      WHEN get_parameter( iv_parameter = zif_param_const=>c_pick_def_stss_2 ).
        ev_rem_sseq = get_parameter( iv_parameter = zif_param_const=>c_pick_str_stss_2 ).
    ENDCASE.

  ENDMETHOD.


  METHOD avail_stock.
**********************************************************************
*& Key           : RM-230607
*& Request No.   : GAP 35 Picking strategies
**********************************************************************
*& Description (short)
*&
**********************************************************************

    DATA:
      lt_lagp       TYPE /scwm/tt_lagp,
      lt_lagp_block TYPE /scwm/tt_lgpla_r.

    CALL FUNCTION '/SCWM/AQUA_READ_MAT'
      EXPORTING
        iv_lgnum    = iv_lgnum
        is_stock    = VALUE /scwm/s_stock( matid = iv_matid
                                           cat   = iv_stock_cat )
        iv_lgtyp    = iv_lgtyp
      IMPORTING
        et_aqua     = rt_avail_stock
      EXCEPTIONS
        not_found   = 1
        wrong_input = 2
        OTHERS      = 99.
    IF sy-subrc <> 0 OR rt_avail_stock IS INITIAL.
      RETURN.
    ENDIF.

    " Check if bins are blocked
    CALL FUNCTION '/SCWM/LAGP_READ_MULTI'
      EXPORTING
        it_lgpla = VALUE /scwm/tt_lagp_key( FOR <ls_av_stock> IN rt_avail_stock
                                             ( lgnum = iv_lgnum
                                               lgpla = <ls_av_stock>-lgpla ) )
      IMPORTING
        et_lagp  = lt_lagp.

    lt_lagp_block = VALUE #( FOR <ls_lagp> IN lt_lagp
                             WHERE ( skzua = abap_true )
                                   ( option = wmegc_option_eq
                                     sign   = wmegc_sign_inclusive
                                     low    = <ls_lagp>-lgpla ) ).

    IF lt_lagp_block IS NOT INITIAL.
      DELETE rt_avail_stock WHERE lgpla IN lt_lagp_block.
    ENDIF.

  ENDMETHOD.


  METHOD calc_avail_qty.
**********************************************************************
*& Key           : RM-230607
*& Request No.   : GAP 35 Picking strategies
**********************************************************************
*& Description (short)
*&
**********************************************************************
    DATA:
      lv_sum_qty   TYPE /scwm/de_avlquan,
      lt_all_stock TYPE /scwm/tt_aqua.

    LOOP AT it_sttypes ASSIGNING FIELD-SYMBOL(<ls_chk_sttyp>).
      DATA(lt_chk_stock) = avail_stock(
        EXPORTING
          iv_lgnum = iv_lgnum
          iv_matid = iv_matid
          iv_lgtyp = CONV /scwm/lgtyp( <ls_chk_sttyp> )
          iv_stock_cat = iv_stock_cat ).

      APPEND LINES OF lt_chk_stock TO lt_all_stock.
    ENDLOOP.

    LOOP AT lt_all_stock ASSIGNING FIELD-SYMBOL(<ls_all_stock>).
      lv_sum_qty += <ls_all_stock>-quan.
    ENDLOOP.

    rv_sum_qty = lv_sum_qty.

  ENDMETHOD.


  METHOD get_item_qty.
**********************************************************************
*& Key           : RM-230607
*& Request No.   : GAP 35 Picking strategies
**********************************************************************
*& Description (short)
*&
**********************************************************************
    DATA:
      ls_read_options TYPE /scwm/dlv_query_contr_str,
      ls_include_data TYPE /scwm/dlv_query_incl_str_prd,
      lt_items        TYPE /scwm/dlv_item_out_prd_tab,
      lo_prd          TYPE REF TO /scwm/cl_dlv_management_prd.

    lo_prd = /scwm/cl_dlv_management_prd=>get_instance( ).

    ls_read_options-data_retrival_only      = abap_true.
    ls_read_options-mix_in_object_instances = abap_true.

    ls_include_data-head_refdoc = abap_true.
    ls_include_data-item_refdoc = abap_true.

    TRY.
        lo_prd->query(
          EXPORTING
            it_docid        = VALUE #( ( docid = iv_docid ) )
            iv_doccat       = iv_doccat
            is_read_options = ls_read_options
            is_include_data = ls_include_data
          IMPORTING
            et_items        = lt_items ).
        ##NO_HANDLER
      CATCH /scdl/cx_delivery.
    ENDTRY.

    rv_item_qty = VALUE #( lt_items[ itemid = iv_itemid ]-qty-qty OPTIONAL ).

  ENDMETHOD.


  METHOD get_parameter.
**********************************************************************
*& Key           : RM-230607
*& Request No.   : GAP 35 Picking strategies
**********************************************************************
*& Description (short)
*&
**********************************************************************

    zcl_param=>get_parameter(
          EXPORTING
            iv_lgnum     = mv_lgnum
            iv_process   = zif_param_const=>c_zout_0004
            iv_parameter = iv_parameter
          IMPORTING
            ev_constant  = rv_const ).

  ENDMETHOD.


  METHOD GET_PARAMETER_LIST.
**********************************************************************
*& Key           : RM-230607
*& Request No.   : GAP 35 Picking strategies
**********************************************************************
*& Description (short)
*&
**********************************************************************

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = mv_lgnum
        iv_process   = zif_param_const=>c_zout_0004
        iv_parameter = iv_parameter
      IMPORTING
        et_list      = rt_list ).

  ENDMETHOD.
ENDCLASS.
