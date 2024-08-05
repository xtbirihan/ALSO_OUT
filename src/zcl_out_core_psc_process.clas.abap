class ZCL_OUT_CORE_PSC_PROCESS definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_CORE_PSC_PROCESS .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_OUT_CORE_PSC_PROCESS IMPLEMENTATION.


  METHOD /scwm/if_ex_core_psc_process~process.
********************************************************************
*& Key          : RM-11.08.2023
*& Request No.  : GAP-54 – “Process-Oriented Storage Control custom settings”
********************************************************************
*& Description
*&
********************************************************************
    BREAK-POINT ID zcg_badi.
    BREAK-POINT ID zcg_psc_process.

    NEW zcl_out_core_psc_process_posc( )->/scwm/if_ex_core_psc_process~process(
    EXPORTING
      iv_lgnum         = iv_lgnum
      iv_vltyp         = iv_vltyp
      is_huhdr         = is_huhdr
      it_huitm         = it_huitm
      io_log           = io_log
      iv_row           = iv_row
      iv_prces         = iv_prces
      iv_sim           = iv_sim
      iv_wo_crea       = iv_wo_crea
      is_wt            = is_wt
    IMPORTING
      ev_not_this_step = ev_not_this_step
      ev_abort         = ev_abort
    CHANGING
      cv_procs         = cv_procs
      cv_nltyp         = cv_nltyp
      cv_nlber         = cv_nlber
      cv_nlpla         = cv_nlpla
      cv_dtu_num       = cv_dtu_num
      cv_procty        = cv_procty
      cs_crea_hu_cust  = cs_crea_hu_cust ).

  ENDMETHOD.
ENDCLASS.
