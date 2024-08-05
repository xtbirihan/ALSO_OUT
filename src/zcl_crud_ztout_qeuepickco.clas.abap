class ZCL_CRUD_ZTOUT_QEUEPICKCO definition
  public
  final
  create public .

public section.

  class-methods SELECT_SINGLE_BY_QUEUE
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_QUEUE type /SCWM/DE_QUEUE
    returning
      value(RS_PICK_CONTROL) type ZTOUT_QEUEPICKCO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_QEUEPICKCO IMPLEMENTATION.


  METHOD select_single_by_queue.
********************************************************************
*& Key          : <AYORDANOV> 20.10.2023
*& Request No.  :
********************************************************************
*& Description
*& REad cust table ZTOUT_QEUEPICKCO
********************************************************************

    SELECT SINGLE *
      FROM ztout_qeuepickco
       WHERE lgnum = @iv_lgnum AND
             queue = @iv_queue
      INTO @DATA(ls_pick_control).

    IF sy-subrc = 0.
      rs_pick_control = ls_pick_control.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
