class ZCL_OUT_RSRC_PROC_WO definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces /SCWM/IF_EX_RSRC_PROC_WO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_OUT_RSRC_PROC_WO IMPLEMENTATION.


  METHOD /scwm/if_ex_rsrc_proc_wo~lsd_prio_update.
**********************************************************************
*& Key           : RM-230120
*& Request No.   : GAP-076 FD Priority settings
**********************************************************************
*& Description (short)
*& Pick the WO with highest prio and earliest LSD
**********************************************************************

    BREAK-POINT ID zcg_ex_rsrc_proc_sel.
    BREAK-POINT ID zcg_badi.

    NEW zcl_out_rsrc_proc_wo_prio( )->/scwm/if_ex_rsrc_proc_wo~lsd_prio_update(
      EXPORTING
        iv_lgnum      = iv_lgnum
        is_who        = is_who
        it_ordim_o    = it_ordim_o
        iv_mode       = iv_mode
      CHANGING
        ct_wo_rsrc_ty = ct_wo_rsrc_ty ).


  ENDMETHOD.
ENDCLASS.
