class ZCL_CRUD_ZTOUT_VASARE_DET definition
  public
  final
  create public .

public section.

  class-methods SELECT_SINGLE_BY_KEYS
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_PROCS type /SCWM/DE_PROCS
    returning
      value(RS_VASARE_DET) type ZTOUT_VASARE_DET .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_VASARE_DET IMPLEMENTATION.


  METHOD select_single_by_keys.
********************************************************************
*& Key          : RMANOVA-10.08.2023
*& Request No.  : GAP 54 Process-Oriented Storage Control custom settings
********************************************************************
*& Description
*&
********************************************************************

    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE * FROM ztout_vasare_det
             INTO @rs_vasare_det
            WHERE lgnum = @iv_lgnum
              AND procs = @iv_procs.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
      FIELDS 'ztout_vasare_det' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
