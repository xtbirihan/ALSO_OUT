CLASS zcl_outb_weight_scale DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_outb_weight_scale_intf.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_OUTB_WEIGHT_SCALE IMPLEMENTATION.


  METHOD zif_outb_weight_scale_intf~get_weigth_in_kg.
**********************************************************************
*& Key           : AD-231128
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Tiggers the weigth scale at the provided ip address and returns its
*& weigth value as kilogramm
**********************************************************************
    rv_weight_in_kg = 0.
  ENDMETHOD.
ENDCLASS.
