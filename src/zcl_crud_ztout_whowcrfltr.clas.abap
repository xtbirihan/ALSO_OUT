class ZCL_CRUD_ZTOUT_WHOWCRFLTR definition
  public
  final
  create public .

public section.

  types:
    tt_ztout_whowcrfltr TYPE STANDARD TABLE OF ztout_whowcrfltr WITH EMPTY KEY .

  methods SELECT_SINGLE_BY_LGNUM_FLTID
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_FILTERID type /SCWM/DE_WCRFILT
    returning
      value(RS_WHOWCRFLTR) type ZTOUT_WHOWCRFLTR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_WHOWCRFLTR IMPLEMENTATION.


  METHOD select_single_by_lgnum_fltid.
**********************************************************************
*& Key           : RM-230407
*& Request No.   : GAPs 57 & 59 - Outbound: Custom Warehouse Order Creation
*&                 Rule filters for SPO/SLO flows
**********************************************************************
*& Description (short)
*&
**********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE * FROM ztout_whowcrfltr
             INTO @rs_whowcrfltr
            WHERE lgnum    = @iv_lgnum
              AND filterid = @iv_filterid.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztout_whowcrfltr' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
