CLASS zcl_crud_ztout_sped_pmat DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_ztout_sped_pmat TYPE STANDARD TABLE OF ztout_sped_pmat WITH EMPTY KEY .

    CLASS-METHODS select_multi_by_lgnum
      IMPORTING
        !iv_lgnum        TYPE /scwm/lgnum
      RETURNING
        VALUE(rt_result) TYPE tt_ztout_sped_pmat.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_SPED_PMAT IMPLEMENTATION.


  METHOD select_multi_by_lgnum.
********************************************************************
*& Key          : <BSUGAREV>-04.05.2023 09:49:20
*& Request No.  : GAP-55 – SPED Orders Picking
********************************************************************
*& Description
*&
********************************************************************
    STATICS: sv_lgnum     TYPE /scwm/lgnum,
             st_sped_mats TYPE tt_ztout_sped_pmat.

    BREAK-POINT ID zcg_db_crud.

    IF sv_lgnum <> iv_lgnum.
      sv_lgnum = iv_lgnum.

      CLEAR: st_sped_mats.

      SELECT * FROM ztout_sped_pmat
        INTO TABLE @st_sped_mats
       WHERE lgnum = @iv_lgnum.
      IF sy-subrc <> 0.
        LOG-POINT ID zcg_db_crud FIELDS 'ZTOUT_SPED_PMAT' sy-datum sy-uzeit.
      ENDIF.

    ENDIF.

    rt_result = st_sped_mats.

  ENDMETHOD.
ENDCLASS.
