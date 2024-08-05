CLASS zcl_out_who_packing DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /scwm/if_ex_who_packing .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_OUT_WHO_PACKING IMPLEMENTATION.


  METHOD /scwm/if_ex_who_packing~packing.
********************************************************************
*& Key          : <BSUGAREV>-06.04.2023 15:11:45
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  : WHO packing implementation. It is the main class
*&    for the BADI implementation and it is used from several GAPs

********************************************************************
    BREAK-POINT ID zcg_badi.
    BREAK-POINT ID zcg_who_packing.

    NEW zcl_out_who_pack_spo_slo( )->/scwm/if_ex_who_packing~packing(
      EXPORTING
        is_wcr    = is_wcr
        is_tpack  = is_tpack
        is_limit  = is_limit
        it_to     = it_to
        it_pmat   = it_pmat
      CHANGING
        cs_sum    = cs_sum
        ct_whohu  = ct_whohu
        ct_to     = ct_to
        ct_packed = ct_packed
        ct_failed = ct_failed
        ev_split  = ev_split ).

    NEW zcl_out_who_pack_ship_pmat_alg( )->/scwm/if_ex_who_packing~packing(
      EXPORTING
        is_wcr    = is_wcr
        is_tpack  = is_tpack
        is_limit  = is_limit
        it_to     = it_to
        it_pmat   = it_pmat
      CHANGING
        cs_sum    = cs_sum
        ct_whohu  = ct_whohu
        ct_to     = ct_to
        ct_packed = ct_packed
        ct_failed = ct_failed
        ev_split  = ev_split ).

    NEW zcl_out_who_pack_sped( )->/scwm/if_ex_who_packing~packing(
      EXPORTING
        is_wcr    = is_wcr
        is_tpack  = is_tpack
        is_limit  = is_limit
        it_to     = it_to
        it_pmat   = it_pmat
      CHANGING
        cs_sum    = cs_sum
        ct_whohu  = ct_whohu
        ct_to     = ct_to
        ct_packed = ct_packed
        ct_failed = ct_failed
        ev_split  = ev_split ).

    NEW zcl_out_who_pack_posc( )->/scwm/if_ex_who_packing~packing(
       EXPORTING
        is_wcr    = is_wcr
        is_tpack  = is_tpack
        is_limit  = is_limit
        it_to     = it_to
        it_pmat   = it_pmat
      CHANGING
        cs_sum    = cs_sum
        ct_whohu  = ct_whohu
        ct_to     = ct_to
        ct_packed = ct_packed
        ct_failed = ct_failed
        ev_split  = ev_split ).

  ENDMETHOD.
ENDCLASS.
