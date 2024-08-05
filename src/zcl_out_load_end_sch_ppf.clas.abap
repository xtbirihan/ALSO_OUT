class ZCL_OUT_LOAD_END_SCH_PPF definition
  public
  final
  create public .

public section.

  interfaces IF_EX_EVAL_SCHEDCOND_PPF .
protected section.
private section.
ENDCLASS.



CLASS ZCL_OUT_LOAD_END_SCH_PPF IMPLEMENTATION.


  method IF_EX_EVAL_SCHEDCOND_PPF~EVALUATE_SCHEDULE_CONDITION.
**********************************************************************
*& Key           : LH-101223
*& Request No.   : GAP-091 – "Loading via RF”
**********************************************************************
*& Description (short)
*& If Load ended is set then trigger the PPF
**********************************************************************
    ep_rc = 1.
    TRY.
        DATA(lo_tu_ppf) = CAST /scwm/cl_sr_tu_ppf( CAST /scwm/cl_sr_context_tuppf( io_context )->appl ).
      CATCH cx_sy_move_cast_error.
        MESSAGE e481(/scwm/shp_rcv) INTO DATA(lv_msg).
        cl_log_ppf=>add_message(
          EXPORTING
            ip_problemclass = sppf_pclass_1
            ip_handle       = ip_protocol ).
        RETURN.
    ENDTRY.

    DATA(lo_bo_tu) = /scwm/cl_sr_bom=>get_instance( )->get_bo_tu_by_key( VALUE #( tu_num = lo_tu_ppf->get_tu_num( )
                                                                                  tu_sr_act_num = lo_tu_ppf->get_tu_sr_act_num( ) )
                                                                        ).

    "check if status 'Goods Issue Posted" was just set
    TRY.
        IF lo_bo_tu->get_status_by_id( wmesr_status_load_end ) = abap_true.
          IF lo_bo_tu->get_status_change_by_id( wmesr_status_load_end ) = abap_true.
            ep_rc = 0.
          ENDIF.
        ENDIF.
      CATCH /scwm/cx_sr_error ##no_handler.
    ENDTRY.
  endmethod.
ENDCLASS.
