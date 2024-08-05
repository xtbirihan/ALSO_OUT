class ZCL_OUT_FULL_PALET_PICKING definition
  public
  final
  create public .

public section.

  data MT_STOCK_MON type /SCWM/TT_STOCK_MON .

  methods CHECK_PARAMETERS
    importing
      value(IV_DEVID) type ZDE_DEVID optional
      value(IT_SWITCH_FIELDS) type ZTT_SWITCH_FIELDS optional
      value(IV_TRART) type /SCWM/LVS_TRART optional
      value(IV_ANFML) type /SCWM/DE_QUANTITY optional
    returning
      value(RV_CHECK) type XFELD .
  methods GET_PHYSICAL_STOCK
    importing
      value(IT_QMAT) type /SCWM/TT_AQUA_INT
    returning
      value(RT_STOCK_MON) type /SCWM/TT_STOCK_MON .
  class-methods GET_INSTANCE
    returning
      value(EO_INSTANCE) type ref to ZCL_OUT_FULL_PALET_PICKING .
  methods DELETE_PICKED_BIN
    importing
      value(IV_BIN) type /SCWM/LGPLA .
protected section.
private section.

  class-data MO_INSTANCE type ref to ZCL_OUT_FULL_PALET_PICKING .
ENDCLASS.



CLASS ZCL_OUT_FULL_PALET_PICKING IMPLEMENTATION.


  METHOD CHECK_PARAMETERS.

    " Switch on/off for sorter.
    IF zcl_switch=>get_switch_state( iv_lgnum = /scwm/cl_tm=>sv_lgnum
                                     iv_devid = iv_devid ) EQ abap_false.
      RETURN.
    ENDIF.

    IF zcl_switch=>get_switch_state( iv_lgnum  = /scwm/cl_tm=>sv_lgnum
                                     iv_devid  = iv_devid
                                     it_fields = it_switch_fields  ) EQ abap_false.
      RETURN.
    ENDIF.

    IF iv_trart  NE wmegc_trart_pick.
      RETURN.
    ENDIF.

    IF iv_anfml <= 0.
      RETURN.
    ENDIF.

    rv_check = abap_true.
  ENDMETHOD. "#EC CI_VALPAR


  METHOD DELETE_PICKED_BIN.
    DELETE mt_stock_mon WHERE lgpla = iv_bin.
  ENDMETHOD.


  METHOD GET_INSTANCE.
    eo_instance = COND #( WHEN mo_instance IS NOT BOUND THEN NEW #( ) ELSE mo_instance ).
    mo_instance = eo_instance.
  ENDMETHOD.


  METHOD GET_PHYSICAL_STOCK.

    DATA: ls_read_opt TYPE /scwm/s_read_opt_stock,
          lr_lgpla    TYPE /scwm/tt_lgpla_r.

    IF me->mt_stock_mon IS NOT INITIAL.
      rt_stock_mon = me->mt_stock_mon.
      RETURN.
    ENDIF.

    ls_read_opt-exclude_outcon = abap_true.
    ls_read_opt-exclude_texts  = abap_true.

    DATA(lo_mon_stock) = NEW /scwm/cl_mon_stock(  iv_lgnum  =  /scwm/cl_tm=>sv_lgnum is_read_options = ls_read_opt ).

    lr_lgpla = VALUE /scwm/tt_lgpla_r( BASE lr_lgpla FOR ls_qmat IN it_qmat
                                         WHERE ( lgpla IS NOT INITIAL )
                                               ( sign   = wmegc_sign_inclusive
                                                 option = wmegc_option_eq
                                                 low    = ls_qmat-lgpla ) ).

    lo_mon_stock->get_physical_stock(
        EXPORTING
          iv_skip_resource = abap_true
          iv_skip_tu       = abap_true
          it_lgpla_r       = lr_lgpla
      IMPORTING
        et_stock_mon     = me->mt_stock_mon
        ev_error         = DATA(lv_error)
    ).

    rt_stock_mon = me->mt_stock_mon.
  ENDMETHOD. "#EC CI_VALPAR
ENDCLASS.
