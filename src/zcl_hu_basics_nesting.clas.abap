class ZCL_HU_BASICS_NESTING definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces /SCWM/IF_EX_HU_BASICS_NESTING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_HU_BASICS_NESTING IMPLEMENTATION.


  method /SCWM/IF_EX_HU_BASICS_NESTING~CHECK.
    IF sy-uname = 'BWILLEMS'.
      BREAK-POINT.
    ENDIF.

  endmethod.
ENDCLASS.
