class ZCL_CRUD_ZTOUT_KEP_MSGCON definition
  public
  final
  create public .

public section.

  class-methods SELECT_SINGLE_BY_KEYS
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_LGBER type /SCWM/LGBER
    returning
      value(RS_KEP_MSGCON) type ZTOUT_KEP_MSGCON .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_KEP_MSGCON IMPLEMENTATION.


  METHOD select_single_by_keys.
********************************************************************
*& Key          : AYORDANOV-29.09.2023
*& Request No.  : GAP 51 RF Message control. Base on /SCWM/DE_STAREA
********************************************************************
*& Description
*&
********************************************************************

    SELECT SINGLE * FROM ztout_kep_msgcon
             INTO @DATA(ls_kep_msgcon)
            WHERE lgnum  = @iv_lgnum
              AND lgber  = @iv_lgber.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    rs_kep_msgcon = ls_kep_msgcon.

  ENDMETHOD.
ENDCLASS.
