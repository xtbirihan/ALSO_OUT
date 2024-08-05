FUNCTION z_out_display_pack_inst.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_PACK_INST) TYPE  STRING
*"     REFERENCE(IV_WITH_HU) TYPE  FLAG
*"  EXPORTING
*"     REFERENCE(EV_HUIDENT) TYPE  /SCWM/HUIDENT
*"----------------------------------------------------------------------
  CLEAr ev_huident.
  IF iv_pack_inst IS INITIAL.
    RETURN.
  ENDIF.

  go_pack_instr_popup = NEW #( ).

  go_pack_instr_popup->set_data( iv_text = iv_pack_inst
                                 iv_with_hu = iv_with_hu ).

  CALL SCREEN 2000 STARTING AT 10 10.

  IF NOT go_pack_instr_popup->is_cancelled( ).
    ev_huident = go_pack_instr_popup->get_data( ).
  ENDIF.
ENDFUNCTION.
