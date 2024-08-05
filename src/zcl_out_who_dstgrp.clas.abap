class ZCL_OUT_WHO_DSTGRP definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_WHO_DSTGRP .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_OUT_WHO_DSTGRP IMPLEMENTATION.


  METHOD /scwm/if_ex_who_dstgrp~dstgrp.
**********************************************************************
*& Key           : RM-230407
*& Request No.   : GAPs 57 Outbound: Custom Warehouse Order Creation
*&                 Rule filters for SPO/SLO flows
**********************************************************************
*& Description (short)
*&
**********************************************************************

    BREAK-POINT ID zcg_ex_who_flt_il.
    BREAK-POINT ID zcg_badi.

    zcl_out_who_dstgrp_spo_slo=>/scwm/if_ex_who_dstgrp~dstgrp(
      EXPORTING
        it_to = it_to
      CHANGING
        ct_to = ct_to ) .

  ENDMETHOD.
ENDCLASS.
