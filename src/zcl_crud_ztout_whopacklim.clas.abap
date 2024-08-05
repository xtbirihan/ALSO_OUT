class ZCL_CRUD_ZTOUT_WHOPACKLIM definition
  public
  final
  create public .

public section.

  types:
    tt_ztout_who_packli TYPE STANDARD TABLE OF ztout_who_packli WITH EMPTY KEY .

  methods SELECT_MULTI_BY_LGNUM
    importing
      !IV_LGNUM type /SCWM/LGNUM
    returning
      value(RT_RESULT) type TT_ZTOUT_WHO_PACKLI .
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_WHOPACKLIM IMPLEMENTATION.


  METHOD select_multi_by_lgnum.
**********************************************************************
*& Key           : RM-230309
*& Request No.   : GAPs 28 & 67 - Inbound: Custom Putaway Strategy
**********************************************************************
*& Description (short)
*&
**********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT * FROM ztout_who_packli
      INTO TABLE @rt_result
     WHERE lgnum = @iv_lgnum.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ZTOUT_WHO_PACKLI' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
