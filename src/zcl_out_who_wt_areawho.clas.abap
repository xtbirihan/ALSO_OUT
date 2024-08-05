class ZCL_OUT_WHO_WT_AREAWHO definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ts_wt_areawho .
        INCLUDE TYPE /scwm/s_ordim_o_int.
    TYPES: diff_areawho TYPE abap_bool,
      END OF ts_wt_areawho .
  types:
    tt_wt_areawho TYPE TABLE OF ts_wt_areawho .

  class-data SO_INST type ref to ZCL_OUT_WHO_WT_AREAWHO .
  data MT_WT_AREAWHO type TT_WT_AREAWHO .

  class-methods GET_INST
    returning
      value(RO_INST) type ref to ZCL_OUT_WHO_WT_AREAWHO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_OUT_WHO_WT_AREAWHO IMPLEMENTATION.


  METHOD get_inst.
**********************************************************************
*& Key           : RM-230407
*& Request No.   : GAP 57 Outbound: Custom Warehouse Order Creation
*&                 Rule filters for SPO/SLO flows
**********************************************************************
*& Description (short)
*& This class will be used to store the WHO's WTs that are to be
*& creared during wave release
**********************************************************************

    IF so_inst IS NOT BOUND.
      so_inst = NEW #( ).
    ENDIF.

    ro_inst = so_inst.

  ENDMETHOD.
ENDCLASS.
