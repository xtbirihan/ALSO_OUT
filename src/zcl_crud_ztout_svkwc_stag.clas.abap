class ZCL_CRUD_ZTOUT_SVKWC_STAG definition
  public
  final
  create public .

public section.

  class-methods SELECT_SINGLE_BY_KEYS
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_STARE_A_GR type ZDE_STARE_A_GR
    returning
      value(RS_SVKWC_STAG) type ZTOUT_SVKWC_STAG .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_SVKWC_STAG IMPLEMENTATION.


  METHOD select_single_by_keys.
********************************************************************
*& Key          : RMANOVA-10.08.2023
*& Request No.  : GAP 54 Process-Oriented Storage Control custom settings
********************************************************************
*& Description
*&
********************************************************************

    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE * FROM ztout_svkwc_stag
             INTO @rs_svkwc_stag
            WHERE lgnum      = @iv_lgnum
              AND stare_a_gr = @iv_stare_a_gr.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztout_svkwc_stag' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
