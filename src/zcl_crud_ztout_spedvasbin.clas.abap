class ZCL_CRUD_ZTOUT_SPEDVASBIN definition
  public
  final
  create public .

public section.

  class-methods SELECT_SINGLE_BY_KEY
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_CURR_STEP type ZDE_CURR_STEP
      !IV_CURR_BIN type ZDE_CURRENT_BIN
      !IV_NEXT_STEP type ZDE_NEXT_STEP
    returning
      value(RS_SPEDVASBIN) type ZTOUT_SPEDVASBIN .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_SPEDVASBIN IMPLEMENTATION.


  METHOD select_single_by_key.
**********************************************************************
*& Key           : RM-230818
*& Request No.   : GAP 54 - Process-Oriented Storage Control custom settings
**********************************************************************
*& Description (short)
*&
**********************************************************************

    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE * FROM ztout_spedvasbin
             INTO @rs_spedvasbin
            WHERE lgnum     = @iv_lgnum
              AND curr_step = @iv_curr_step
              AND curr_bin  = @iv_curr_bin
              AND next_step = @iv_next_step.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztout_spedvasbin' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
