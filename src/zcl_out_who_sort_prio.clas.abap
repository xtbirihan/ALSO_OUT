CLASS zcl_out_who_sort_prio DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES /scwm/if_ex_who_sort .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_OUT_WHO_SORT_PRIO IMPLEMENTATION.


  METHOD /scwm/if_ex_who_sort~sort.
**********************************************************************
*& Key           : RM-230120
*& Request No.   : GAP-076 FD Priority settings
**********************************************************************
*& Description (short)
*& Pick the WO with highest prio and earliest LSD
**********************************************************************

    DATA:
      lv_prio TYPE /scwm/de_prior,
      lt_to   TYPE /scwm/tt_ordim_o_int,
      lo_prio TYPE REF TO zcl_out_who_priority.

    BREAK-POINT ID zcg_ex_rsrc_proc_sel.
    BREAK-POINT ID zcg_badi.

    IF zcl_switch=>get_switch_state( iv_lgnum = ct_to[ 1 ]-lgnum
                                     iv_devid = zif_switch_const=>c_zout_018 ) EQ abap_false.
      RETURN.
    ENDIF.

    lt_to = VALUE #( FOR <ls_to> IN ct_to
                     WHERE ( flghuto = abap_false
                       AND   trart   = wmegc_trart_pick )
                           ( CORRESPONDING #( <ls_to> ) ) ) .

    lo_prio = NEW zcl_out_who_priority( ).

    lv_prio = lo_prio->who_find_prio( it_ordim_o = it_to ).

    IF lv_prio IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT ct_to ASSIGNING FIELD-SYMBOL(<ls_to_chng>).
      READ TABLE lt_to WITH KEY tanum = <ls_to_chng>-tanum TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      <ls_to_chng>-priority =  lv_prio .
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
