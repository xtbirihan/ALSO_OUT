class ZCL_CRUD_ZTOUT_POSC_CMAP definition
  public
  final
  create public .

public section.

  class-methods SELECT_SINGLE_BY_KEYS
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_PRCES type /SCWM/DE_PRCES
      !IV_PROCS type /SCWM/DE_PROCS
    returning
      value(RS_POSC_CMAP) type ZTOUT_POSC_CMAP .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_POSC_CMAP IMPLEMENTATION.


  METHOD select_single_by_keys.
********************************************************************
*& Key          : RMANOVA-10.08.2023
*& Request No.  : GAP 54 Process-Oriented Storage Control custom settings
********************************************************************
*& Description
*&
********************************************************************

    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE * FROM ztout_posc_cmap
             INTO @rs_posc_cmap
            WHERE lgnum = @iv_lgnum
              AND prces = @iv_prces
              AND procs = @iv_procs.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztout_posc_cmap' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
