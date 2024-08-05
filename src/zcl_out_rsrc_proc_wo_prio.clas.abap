class ZCL_OUT_RSRC_PROC_WO_PRIO definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces /SCWM/IF_EX_RSRC_PROC_WO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_OUT_RSRC_PROC_WO_PRIO IMPLEMENTATION.


  METHOD /scwm/if_ex_rsrc_proc_wo~lsd_prio_update.
**********************************************************************
*& Key           : RM-230120
*& Request No.   : GAP-076 FD Priority settings
**********************************************************************
*& Description (short)
*& Pick the WO with highest prio and earliest LSD
**********************************************************************

    DATA:
      lv_prio    TYPE /scwm/de_prior,
      ls_ordim_o TYPE /scwm/s_ordim_o_int,
      lo_prio    TYPE REF TO zcl_out_who_priority.

    IF zcl_switch=>get_switch_state( iv_lgnum = iv_lgnum
                                     iv_devid = zif_switch_const=>c_zout_018 ) EQ abap_false.
      RETURN.
    ENDIF.

    READ TABLE it_ordim_o INTO ls_ordim_o INDEX 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF ls_ordim_o-flghuto = abap_true OR ls_ordim_o-trart <> wmegc_trart_pick.
      RETURN.
    ENDIF.

    lo_prio = NEW zcl_out_who_priority( ).

    lv_prio = lo_prio->who_find_prio( it_ordim_o = it_ordim_o ).

    IF lv_prio IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT ct_wo_rsrc_ty ASSIGNING FIELD-SYMBOL(<ls_wo_rsrc_ty>).
      <ls_wo_rsrc_ty>-priority =  lv_prio .
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
