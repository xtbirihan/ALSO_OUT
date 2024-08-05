class ZCL_OUT_WHO_PACK_POSC definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_WHO_PACKING .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_OUT_WHO_PACK_POSC IMPLEMENTATION.


  METHOD /scwm/if_ex_who_packing~packing.
********************************************************************
*& Key          : rmanova-18.08.2023
*& Request No.  : GAP-54 – Process-Oriented Storage Control custom settings
********************************************************************
*& Description
*&
********************************************************************
    DATA:
          lo_impl_whohu TYPE REF TO zcl_out_whohu_posc.

    IF zcl_switch=>get_switch_state( iv_lgnum = is_tpack-lgnum
                                     iv_devid = zif_switch_const=>c_zout_017 ) EQ abap_false. "WOCR: POSC custom settings
      RETURN.
    ENDIF.

    lo_impl_whohu = zcl_out_whohu_posc=>get_inst( ).

    IF ct_whohu IS NOT INITIAL.
      MOVE-CORRESPONDING ct_whohu TO lo_impl_whohu->mt_whohu.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
