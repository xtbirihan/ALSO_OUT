class ZCL_CRUD_ZTOUT_MC_PICK definition
  public
  final
  create public .

public section.

  types TS_MC_PICK type ZTOUT_MC_PICK .
  types:
    tt_mc_pick TYPE STANDARD TABLE OF ts_mc_pick .

  class-methods SELECT_MULTI_BY_QUEUE
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IT_QUEUE_R type /SCWM/TT_QUEUE_R optional
    exporting
      !ET_MC_PICK type ZCL_CRUD_ZTOUT_MC_PICK=>TT_MC_PICK .
  class-methods SELECT_SINGLE_BY_QUEUE
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_QUEUE type /SCWM/DE_QUEUE
    exporting
      !ES_MC_PICK type ZCL_CRUD_ZTOUT_MC_PICK=>TS_MC_PICK .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_MC_PICK IMPLEMENTATION.


  METHOD select_multi_by_queue.
********************************************************************
*& Key          : <AAHMEDOV>-26.06.2023
*& Request No.  : GAPs 17 - Picking WO Bundling
*& Description  : Managing Class for table ZTOUT_MC_PICK
********************************************************************

    BREAK-POINT ID zcg_db_crud.

    IF iv_lgnum IS INITIAL
      OR it_queue_r IS INITIAL.
      RETURN.
    ENDIF.

    SELECT * FROM
      ztout_mc_pick
      WHERE lgnum = @iv_lgnum
      AND queue IN @it_queue_r
      INTO TABLE @et_mc_pick.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztout_mc_pick' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.


  METHOD select_single_by_queue.
********************************************************************
*& Key          : <AAHMEDOV>-04.07.2023
*& Request No.  :
********************************************************************
*& Description
*& Managing Class for table ZTOUT_MC_PICK
********************************************************************

    BREAK-POINT ID zcg_db_crud.

    IF iv_lgnum IS INITIAL OR
      iv_queue IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM
      ztout_mc_pick
      WHERE lgnum = @iv_lgnum
      AND queue EQ @iv_queue
      INTO @es_mc_pick.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztout_mc_pick' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
