class ZCL_WAVE_SAVE definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_WAVE_SAVE .
  interfaces IF_BADI_INTERFACE .

  class-data MV_REPORT_NAME type PROGRAMM value 'Z_R_WAVE_RELEASE' ##NO_TEXT.
protected section.
private section.

  methods CHANGE_REPORT_NAME
    exporting
      !EV_CHANGED type XFELD
      !EV_REPORT type RALDB_REPO
    changing
      !CT_WAVEHDR type /SCWM/TT_WAVEHDR_INT
      !CT_WAVEITM type /SCWM/TT_WAVEITM_INT .
  methods CHECK_PARAMETERS
    importing
      !IV_DEVID type ZDE_DEVID optional
    returning
      value(RV_CHECK) type XFELD .
ENDCLASS.



CLASS ZCL_WAVE_SAVE IMPLEMENTATION.


  METHOD /scwm/if_ex_wave_save~save.
**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-062
*& Author        : Tugay Birihan
*& e-mail        : tugay.birihan@qinlox.com
*& Module Cons.  : Sevil Rasim
*& Date          : 04.04.2023
**********************************************************************
*& Description (short)
*& The new wave release functionality will be structured into the following steps:
*&  Wave release
*&  Triggering of order-related replenishment
*&  Warehouse Order optimization

**********************************************************************

    BREAK-POINT ID zcg_badi.
    BREAK-POINT ID zcg_ex_wave_save.


    me->change_report_name(
      IMPORTING
        ev_changed = EV_CHANGED
        ev_report  = EV_REPORT
      CHANGING
        ct_wavehdr = CT_WAVEHDR
        ct_waveitm = CT_WAVEITM
    ).

  ENDMETHOD.


  METHOD change_report_name.
**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-062
*& Author        : Tugay Birihan
*& e-mail        : tugay.birihan@qinlox.com
*& Module Cons.  : Sevil Rasim
*& Date          : 04.04.2023
**********************************************************************

    DATA: ls_wave_template TYPE /scwm/s_tmplt_data,
          lv_retry_release TYPE boole_d.

    IF me->check_parameters( iv_devid = zif_switch_const=>c_zout_004 ) EQ abap_false.
      RETURN.
    ENDIF.

    LOOP AT ct_wavehdr ASSIGNING FIELD-SYMBOL(<ls_wavehdr>).
      IF <ls_wavehdr>-status = 'E'.
        GET TIME STAMP FIELD DATA(lv_timestamp_now).
        "the retry interval is maintained at the wave template
        CALL FUNCTION '/SCWM/WAVE_TMPLT_READ_SINGLE'
          EXPORTING
            iv_lgnum      = <ls_wavehdr>-lgnum
            iv_tmplt      = <ls_wavehdr>-tmplt
          IMPORTING
            es_tmplt_data = ls_wave_template
          EXCEPTIONS
            OTHERS        = 0.

        DATA(lv_release_retry_interval) = ls_wave_template-release_retry_interval.

        "in order not to retry endlessly we stop 24 hours after the initial release of the wave
        IF lv_timestamp_now < cl_abap_tstmp=>add( tstmp = <ls_wavehdr>-released_at secs = 86400 ).
          lv_retry_release = abap_true.
        ELSE.
          lv_retry_release = abap_false.
        ENDIF.

        IF lv_retry_release = abap_true AND lv_release_retry_interval > 0.
          DATA(lv_change) = abap_true.
          EXIT.
        ENDIF.

      ENDIF.
    ENDLOOP.

    IF lv_change IS NOT INITIAL.
      ev_report  = mv_report_name.
      ev_changed = abap_true.
    ENDIF.


  ENDMETHOD.


  METHOD check_parameters.
**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-062
*& Author        : Tugay Birihan
*& e-mail        : tugay.birihan@qinlox.com
*& Module Cons.  : Sevil Rasim
*& Date          : 04.04.2023
**********************************************************************
    IF zcl_switch=>get_switch_state( iv_lgnum = /scwm/cl_tm=>sv_lgnum
                                     iv_devid = iv_devid ) EQ abap_false.
      RETURN.
    ENDIF.

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = /scwm/cl_tm=>sv_lgnum
        iv_process   = zif_param_const=>c_zout_0001
        iv_parameter = zif_param_const=>c_monitor
      IMPORTING
        ev_constant  = DATA(lv_monitor_param) ).

    GET PARAMETER ID zif_param_const=>c_monitor FIELD DATA(lv_monitor).
    IF lv_monitor <> lv_monitor_param.
      RETURN.
    ENDIF.
    rv_check  = abap_true.
  ENDMETHOD.
ENDCLASS.
