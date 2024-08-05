CLASS zcl_out_rsrc_proc_sel_prio DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES /scwm/if_ex_rsrc_proc_sel .

    ALIASES wo_select FOR /scwm/if_ex_rsrc_proc_sel~wo_select.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_OUT_RSRC_PROC_SEL_PRIO IMPLEMENTATION.


  METHOD /scwm/if_ex_rsrc_proc_sel~wo_select.
**********************************************************************
*& Key           : RM-230120
*& Request No.   : GAP-076 FD Priority settings
**********************************************************************
*& Description (short)
*& Pick the WO with highest prio and earliest LSD
**********************************************************************
    BREAK-POINT ID zcg_ex_rsrc_proc_sel.

    IF zcl_switch=>get_switch_state( iv_lgnum = iv_lgnum
                                     iv_devid = zif_switch_const=>c_zout_018 ) EQ abap_false.
      RETURN.
    ENDIF.

    SORT ct_wo_rsrc_ty BY priority ASCENDING lsd ASCENDING who ASCENDING.

  ENDMETHOD.
ENDCLASS.
