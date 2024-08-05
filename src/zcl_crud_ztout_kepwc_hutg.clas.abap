class ZCL_CRUD_ZTOUT_KEPWC_HUTG definition
  public
  final
  create public .

public section.

  class-methods SELECT_SINGLE_BY_KEY
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_HUTYPGRP type /SCWM/DE_HUTYPGRP
    returning
      value(RS_KEPWC_HUTG) type ZTOUT_KEPWC_HUTG .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_KEPWC_HUTG IMPLEMENTATION.


  METHOD select_single_by_key.
**********************************************************************
*& Key           : RM-230810
*& Request No.   : GAP 54 - Process-Oriented Storage Control custom settings
**********************************************************************
*& Description (short)
*&
**********************************************************************

    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE * FROM ztout_kepwc_hutg
             INTO @rs_kepwc_hutg
            WHERE lgnum    = @iv_lgnum
              AND hutypgrp = @iv_hutypgrp.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztout_kepwc_hutg' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
