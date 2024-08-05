CLASS zcl_out_wo_creation_itm_filter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /scwm/if_ex_who_flt_il .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_OUT_WO_CREATION_ITM_FILTER IMPLEMENTATION.


  METHOD /scwm/if_ex_who_flt_il~filter_il.
**********************************************************************
*& Key           : RM-230407
*& Request No.   : GAPs 57 & 59 - Outbound: Custom Warehouse Order Creation
*&                 Rule filters for SPO/SLO flows
**********************************************************************
*& Description (short)
*&
**********************************************************************

    BREAK-POINT ID zcg_ex_who_flt_il.
    BREAK-POINT ID zcg_badi.

    zcl_out_who_flt_il_spo_slo=>/scwm/if_ex_who_flt_il~filter_il(
          EXPORTING
            is_wcr        = is_wcr           " Warehouse Order Creation Rule
            is_filter     = is_filter        " Filter Parameters
            it_to         = it_to            " WTs to Be Processed
          CHANGING
            ct_to_success = ct_to_success    " WTs That Were Filtered
            ct_to_failed  = ct_to_failed     " WTs That Were not Filtered
        ).

  ENDMETHOD.
ENDCLASS.
