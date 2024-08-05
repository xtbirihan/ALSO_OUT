class ZCL_IM_BI_OUT_TU_MANIFEST definition
  public
  final
  create public .

public section.

  interfaces IF_EX_EXEC_METHODCALL_PPF .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_BI_OUT_TU_MANIFEST IMPLEMENTATION.


  METHOD if_ex_exec_methodcall_ppf~execute.
**********************************************************************
*& Key           : LH-111123
*& Request No.   : GAP-12 – "VCE Carrier integration”
**********************************************************************
*& Description (short)
*& Send manifest request
**********************************************************************
    "set status to incorrectly processed --------------------------------
    rp_status = sppf_status_error.


    TRY.
        DATA(lo_tu_ppf) = CAST /scwm/cl_sr_tu_ppf( io_appl_object ).
      CATCH cx_sy_move_cast_error.
        MESSAGE e481(/scwm/shp_rcv) INTO DATA(lv_msg) ##needed.
        cl_log_ppf=>add_message(
          EXPORTING
            ip_problemclass = sppf_pclass_1
            ip_handle       = ip_application_log ).
        RETURN.
    ENDTRY.

    IF zcl_out_tu_ppf_functions=>send_manifest(
         iv_tu_num        = lo_tu_ppf->get_tu_num( )                 " Internal Number of Transportation Unit
         iv_tu_sr_act_num = lo_tu_ppf->get_tu_sr_act_num( )                 " Shipping and Receiving Activity Number
         iv_lgnum         = lo_tu_ppf->get_lgnum( )
         iv_application_log = ip_application_log
       ).
      rp_status = sppf_status_processed.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
