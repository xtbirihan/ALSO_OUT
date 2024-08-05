CLASS zcl_crud_ztout_lgtyprndup DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: tt_ztout_lgtyprndup TYPE STANDARD TABLE OF ztout_lgtyprndup WITH EMPTY KEY.

    CLASS-METHODS select_single_by_key
      IMPORTING
        !iv_lgnum        TYPE /scwm/lgnum
        !iv_lgtyp        TYPE /scwm/lgtyp
      RETURNING
        VALUE(rs_result) TYPE ztout_lgtyprndup .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_LGTYPRNDUP IMPLEMENTATION.


  METHOD select_single_by_key.
********************************************************************
*& Key          : <BSUGAREV>-Nov 23, 2023
*& Request No.  :
********************************************************************
*& Description  :
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE * FROM ztout_lgtyprndup
     WHERE lgnum = @iv_lgnum
       AND lgtyp = @iv_lgtyp
      INTO @rs_result.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ZTOUT_LGTYPRNDUP' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
