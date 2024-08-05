class ZCL_CRUD_ZTOUT_QUE_LY_MAP definition
  public
  final
  create public .

public section.

  types TS_QUE_LY_MAP type ZTOUT_QUE_LY_MAP .
  types:
    tt_que_ly_map TYPE STANDARD TABLE OF ts_que_ly_map .

  class-methods SELECT_SINGLE_BY_QUEUE
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_QUEUE type /SCWM/DE_QUEUE
    exporting
      !ES_QUE_LY_MAP type ZCL_CRUD_ZTOUT_QUE_LY_MAP=>TS_QUE_LY_MAP .
  class-methods SELECT_MULTI_BY_QUEUE
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IT_QUEUE_R type /SCWM/TT_QUEUE_R
    exporting
      !ET_QUE_LY_MAP type ZCL_CRUD_ZTOUT_QUE_LY_MAP=>TT_QUE_LY_MAP .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_QUE_LY_MAP IMPLEMENTATION.


  METHOD select_multi_by_queue.
*********************************************************************
*& Key           : <aahmedov>-06.07.2023
*& Request No.   : GAPs 17 - Picking WO Bundling
**********************************************************************
    BREAK-POINT ID zcg_db_crud.

    IF iv_lgnum IS INITIAL
      OR it_queue_r IS INITIAL.
      RETURN.
    ENDIF.

    SELECT *
      FROM ztout_que_ly_map
      WHERE lgnum = @iv_lgnum
      AND   queue IN @it_queue_r
      INTO TABLE @et_que_ly_map.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztout_que_ly_map' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.


  METHOD select_single_by_queue.
*********************************************************************
*& Key           : <aahmedov>-06.07.2023
*& Request No.   : GAPs 17 - Picking WO Bundling
**********************************************************************
    BREAK-POINT ID zcg_db_crud.

    IF iv_lgnum IS INITIAL
      OR iv_queue IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM ztout_que_ly_map
      WHERE lgnum = @iv_lgnum
      AND   queue = @iv_queue
      INTO @es_que_ly_map.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztout_que_ly_map' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
