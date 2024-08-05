CLASS zcl_crud_ztout_buffer_bin DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: tt_ztout_buffer_bin TYPE STANDARD TABLE OF ztout_buffer_bin WITH EMPTY KEY.

    CLASS-METHODS select_single_by_key
      IMPORTING
        !iv_lgnum            TYPE /scwm/lgnum
      RETURNING
        VALUE(rs_buffer_bin) TYPE ztout_buffer_bin.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_BUFFER_BIN IMPLEMENTATION.


  METHOD select_single_by_key.
********************************************************************
*& Key          : <BSUGAREV>-Aug 28, 2023
*& Request No.  :
********************************************************************
*& Description  : Select single entry by key
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE * FROM ztout_buffer_bin
             INTO @rs_buffer_bin
            WHERE lgnum = @iv_lgnum.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ZTOUT_BUFFER_BIN' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
