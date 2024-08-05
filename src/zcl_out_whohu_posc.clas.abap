class ZCL_OUT_WHOHU_POSC definition
  public
  final
  create public .

public section.

  class-data SO_INST type ref to ZCL_OUT_WHOHU_POSC .
  data MT_WHOHU type /SCWM/TT_WHOHU_INT .

  class-methods GET_INST
    returning
      value(RO_INST) type ref to ZCL_OUT_WHOHU_POSC .
protected section.
private section.
ENDCLASS.



CLASS ZCL_OUT_WHOHU_POSC IMPLEMENTATION.


  METHOD get_inst.
**********************************************************************
*& Key           : RM-2300818
*& Request No.   : GAP-54 – Process-Oriented Storage Control custom settings
**********************************************************************
*& Description (short)
*& This class will be used to store the Table of Warehouse Order HUs
*& creared during wave release to be used in BAdI /SCWM/EX_CORE_PSC_PROCESS
**********************************************************************

    IF so_inst IS NOT BOUND.
      so_inst = NEW #( ).
    ENDIF.

    ro_inst = so_inst.

  ENDMETHOD.
ENDCLASS.
