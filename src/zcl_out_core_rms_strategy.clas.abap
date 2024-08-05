class ZCL_OUT_CORE_RMS_STRATEGY definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_CORE_RMS_STRATEGY .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_OUT_CORE_RMS_STRATEGY IMPLEMENTATION.


  METHOD /scwm/if_ex_core_rms_strategy~strategy.
**********************************************************************
*& Key           : RM-230608
*& Request No.   : GAP 35 Picking Strategy
**********************************************************************
*& Description (short)
*&
**********************************************************************

    BREAK-POINT ID zcg_pick_strat.
    BREAK-POINT ID zcg_badi.

    DATA:
         lo_pick_strat_bop TYPE REF TO zcl_out_core_rms_strategy_bop.

    lo_pick_strat_bop = NEW zcl_out_core_rms_strategy_bop( ).

    lo_pick_strat_bop->/scwm/if_ex_core_rms_strategy~strategy(
      EXPORTING
        iv_call         = iv_call          " Calling Application of Source Data Determination
        iv_rem_sseq     = iv_rem_sseq      " Storage Type Search Sequence: Stock Removal
        iv_rem_sseq_std = iv_rem_sseq_std  " Sequence w/o Influence of Add-On
        iv_rem_rule     = iv_rem_rule      " Stock Removal Rule
        iv_rem_rule_std = iv_rem_rule_std  " Stock Removal Rule w/o Influence of Add-On
        iv_anfml        = iv_anfml         " Qty field
        is_ltap         = is_ltap          " Warehouse Task Internal
        is_mat_global   = is_mat_global    " Material: Global Data
        is_mat_lgnum    = is_mat_lgnum     " Material: Warehouse-Number-Specific Data
        is_mat_hazard   = is_mat_hazard    " Material: Hazardous Material Data
        it_mat_uom      = it_mat_uom       " Material: Table Type for Units of Measure
        is_t333         = is_t333          " Warehouse Process Type
        io_log          = io_log           " Log
        iv_row          = iv_row           " Lines in parameter
      IMPORTING
        ev_rem_sseq     = ev_rem_sseq      " Storage Type Search Sequence: Stock Removal
        ev_rem_rule     = ev_rem_rule      " Stock Removal Rule
        ev_rem_rule_set = ev_rem_rule_set  " Checkbox
      CHANGING
        cs_ordim_cust   = cs_ordim_cust    " Customer Data in Warehouse Task
    ).

  ENDMETHOD.
ENDCLASS.
