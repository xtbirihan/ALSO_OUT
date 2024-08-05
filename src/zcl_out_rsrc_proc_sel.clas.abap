CLASS zcl_out_rsrc_proc_sel DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_badi_interface .
    INTERFACES /scwm/if_ex_rsrc_proc_sel .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_OUT_RSRC_PROC_SEL IMPLEMENTATION.


  METHOD /scwm/if_ex_rsrc_proc_sel~wo_select.
********************************************************************
*& Key          : <BSUGAREV>-Oct 31, 2023
*& Request No.  : GAP-076 Outbound Priority settings
********************************************************************
*& Description
*&
*&
********************************************************************
    BREAK-POINT ID zcg_badi.

*    IF lines( ct_wo_rsrc_ty ) = 0.
*      DATA(lt_wo_rsrc) = NEW zcl_who_rsrc_picking( iv_lgnum )->rsrc_proc_wo_select( ).
*
*      APPEND LINES OF lt_wo_rsrc TO ct_wo_rsrc_ty.
*    ENDIF.

    NEW zcl_out_rsrc_proc_sel_prio( )->wo_select(
      EXPORTING
        iv_lgnum      = iv_lgnum
      CHANGING
        ct_wo_rsrc_ty = ct_wo_rsrc_ty ).

  ENDMETHOD.
ENDCLASS.
