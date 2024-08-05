CLASS zcl_crud_ztout_ship_pmat DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      tt_ztout_ship_pmat TYPE STANDARD TABLE OF ztout_ship_pmat WITH EMPTY KEY .

    CLASS-METHODS select_multi_by_lgnum
      IMPORTING
        iv_lgnum         TYPE /scwm/lgnum
      RETURNING
        VALUE(rt_result) TYPE tt_ztout_ship_pmat.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_SHIP_PMAT IMPLEMENTATION.


  METHOD select_multi_by_lgnum.
********************************************************************
*& Key          : <BSUGAREV>-04.05.2023 09:49:20
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description
*&
********************************************************************
    STATICS: sv_lgnum     TYPE /scwm/lgnum,
             st_ship_mats TYPE tt_ztout_ship_pmat.

    BREAK-POINT ID zcg_db_crud.

    IF sv_lgnum <> iv_lgnum.
      sv_lgnum = iv_lgnum.

      CLEAR: st_ship_mats.

      SELECT * FROM ztout_ship_pmat
        INTO TABLE @st_ship_mats
       WHERE lgnum = @iv_lgnum.
      IF sy-subrc <> 0.
        LOG-POINT ID zcg_db_crud FIELDS 'ZTOUT_SHIP_PMAT' sy-datum sy-uzeit.
      ENDIF.

    ENDIF.

    rt_result = st_ship_mats.

  ENDMETHOD.
ENDCLASS.
