class ZCL_CRUD_ZTOUT_KEP_PRINSN definition
  public
  final
  create public .

public section.

  class-methods SELECT_SINGLE_BY_LGNUM
    importing
      !IV_LGNUM type /SCWM/LGNUM
    exporting
      !ES_KEP_PRIN_SN_CONTROL type ZTOUT_KEP_PRINSN .
protected section.
private section.

  class-data SS_OUT_KEP_PRIN_SN_ACT type ZTOUT_KEP_PRINSN .
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_KEP_PRINSN IMPLEMENTATION.


  METHOD select_single_by_lgnum.
********************************************************************
*& Key          : <AYORDANOV> 10.01.2024
*& Request No.  :
********************************************************************
*& Description
*& REad cust table ZTOUT_KEP_PRINSN
********************************************************************

    CLEAR es_kep_prin_sn_control.

    IF ss_out_kep_prin_sn_act-lgnum = iv_lgnum.
      es_kep_prin_sn_control = ss_out_kep_prin_sn_act.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM ztout_kep_prinsn
      WHERE lgnum = @iv_lgnum
      INTO @DATA(ls_kep_print_sn).

    IF sy-subrc <> 0.
      RETURN.
    ELSE.
      ss_out_kep_prin_sn_act = ls_kep_print_sn.
    ENDIF.

    es_kep_prin_sn_control = ls_kep_print_sn.

  ENDMETHOD.
ENDCLASS.
