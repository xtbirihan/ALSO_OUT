CLASS zcl_crud_ztout_tote_pmat DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_tote_pmat_map TYPE STANDARD TABLE OF ztout_tote_pmat WITH EMPTY KEY.

    CLASS-METHODS select_all
      IMPORTING
        !iv_lgnum         TYPE /scwm/lgnum
      EXPORTING
        !et_tote_pmat_map TYPE zcl_crud_ztout_tote_pmat=>tt_tote_pmat_map .
    CLASS-METHODS select_totes_for_pmat
      IMPORTING
        !iv_lgnum        TYPE /scwm/lgnum
        !iv_pmatid       TYPE /scwm/de_pmatid
      EXPORTING
        et_tote_pmat_map TYPE zcl_crud_ztout_tote_pmat=>tt_tote_pmat_map .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_TOTE_PMAT IMPLEMENTATION.


  METHOD select_all.
*********************************************************************
*& Key           : <aahmedov>-11.07.2023
*& Request No.   : GAPs 17 - Picking WO Bundling
**********************************************************************
    BREAK-POINT ID zcg_db_crud.

    IF iv_lgnum IS INITIAL.
      RETURN.
    ENDIF.

    SELECT *
      FROM ztout_tote_pmat
      WHERE lgnum = @iv_lgnum
      INTO TABLE @et_tote_pmat_map.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztout_tote_pmat' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.


  METHOD select_totes_for_pmat.
*********************************************************************
*& Key           : <aahmedov>-10.07.2023
*& Request No.   : GAPs 17 - Picking WO Bundling
**********************************************************************
    BREAK-POINT ID zcg_db_crud.

    CLEAR: et_tote_pmat_map.

    IF iv_lgnum IS INITIAL
          OR iv_pmatid IS INITIAL.
      RETURN.
    ENDIF.

    SELECT *
      FROM ztout_tote_pmat
      WHERE lgnum = @iv_lgnum
      AND   pmatid = @iv_pmatid
      INTO TABLE @et_tote_pmat_map.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztout_tote_pmat' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
