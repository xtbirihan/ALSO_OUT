class ZCL_CRUD_ZTOUT_LAYOUT_TYP definition
  public
  final
  create public .

public section.

  types TS_LAYOUT_TYP type ZTOUT_LAYOUT_TYP .
  types:
    tt_layout_typ TYPE STANDARD TABLE OF ztout_layout_typ WITH EMPTY KEY.

  class-methods SELECT_SINGLE_BY_LAYOUT_ROW
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_LAYOUT type /SCWM/PCTLY_TYPE
      !IV_ROW_NUM type ZDE_PCTLY_ROW_NUM
    exporting
      !ES_LAYOUT_TYP type ZCL_CRUD_ZTOUT_LAYOUT_TYP=>TS_LAYOUT_TYP .
  class-methods SELECT_MULTI_BY_LAYOUT
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IT_LAYOUT_R type RSELOPTION
    exporting
      !ET_LAYOUT_TYP type ZCL_CRUD_ZTOUT_LAYOUT_TYP=>TT_LAYOUT_TYP .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_LAYOUT_TYP IMPLEMENTATION.


  METHOD select_multi_by_layout.
*********************************************************************
*& Key           : <aahmedov>-06.07.2023
*& Request No.   : GAPs 17 - Picking WO Bundling
**********************************************************************
    BREAK-POINT ID zcg_db_crud.

    IF iv_lgnum IS INITIAL
      OR it_layout_r IS INITIAL.
      RETURN.
    ENDIF.

    SELECT *
      FROM ztout_layout_typ
      WHERE lgnum = @iv_lgnum
      AND   type_id IN @it_layout_r
      INTO TABLE @et_layout_typ.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztout_layout_typ' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.


  METHOD select_single_by_layout_row.
*********************************************************************
*& Key           : <aahmedov>-06.07.2023
*& Request No.   : GAPs 17 - Picking WO Bundling
**********************************************************************
    BREAK-POINT ID zcg_db_crud.

    IF iv_lgnum IS INITIAL
      OR iv_layout IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM ztout_layout_typ
      WHERE lgnum = @iv_lgnum
      AND   type_id = @iv_layout
      AND row_num = @iv_row_num
      INTO @es_layout_typ.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztout_layout_typ' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
