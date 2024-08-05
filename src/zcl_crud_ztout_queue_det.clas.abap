CLASS zcl_crud_ztout_queue_det DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_queue_det TYPE TABLE OF ztout_queue_det .

    CLASS-METHODS select_multi_by_queue
      IMPORTING
        !iv_lgnum     TYPE /scwm/lgnum
        !it_queue_r   TYPE /scwm/tt_queue_r OPTIONAL
      EXPORTING
        !et_queue_det TYPE zcl_crud_ztout_queue_det=>tt_queue_det .

    CLASS-METHODS select_multi_by_outqueue
      IMPORTING
        !iv_lgnum     TYPE /scwm/lgnum
        !iv_queue     TYPE /scwm/de_queue
      EXPORTING
        !et_queue_det TYPE zcl_crud_ztout_queue_det=>tt_queue_det .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_QUEUE_DET IMPLEMENTATION.


  METHOD select_multi_by_outqueue.
********************************************************************
*& Key          : <BSUGAREV>-Oct 24, 2023
*& Request No.  :
********************************************************************
*& Description
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    IF iv_lgnum IS INITIAL OR iv_queue IS INITIAL.
      RETURN.
    ENDIF.

    SELECT * FROM ztout_queue_det
     WHERE lgnum    = @iv_lgnum
       AND outqueue = @iv_queue
      INTO TABLE @et_queue_det.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztout_queue_det' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.


  METHOD select_multi_by_queue.
********************************************************************
*& Key          : <AAHMEDOV>-15.06.2023
*& Request No.  :
********************************************************************
*& Description
*& Managing Class for table ZTOUT_QUEUE_DET
********************************************************************

    BREAK-POINT ID zcg_db_crud.

    IF iv_lgnum IS INITIAL.
      RETURN.
    ENDIF.

    SELECT * FROM
      ztout_queue_det
      WHERE lgnum = @iv_lgnum
      AND inqueue IN @it_queue_r
      ORDER BY inqueue, seqno ASCENDING
      INTO TABLE @et_queue_det.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztout_queue_det' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
