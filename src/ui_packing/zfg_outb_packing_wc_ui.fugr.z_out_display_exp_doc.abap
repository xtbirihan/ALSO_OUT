FUNCTION z_out_display_exp_doc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_EXP_DOC) TYPE  STRING
*"     REFERENCE(IV_DEST_STOR_TYPE_T) TYPE  STRING
*"     REFERENCE(IV_WITH_HU) TYPE  FLAG
*"  EXPORTING
*"     REFERENCE(EV_HUIDENT) TYPE  /SCWM/HUIDENT
*"----------------------------------------------------------------------
  CLEAR ev_huident.

  go_pack_exp_doc = NEW #( ).

  go_pack_exp_doc->set_data(
    iv_with_hu = iv_with_hu
    iv_text = iv_exp_doc
    iv_dest_stor_type_t = iv_dest_stor_type_t ).

  CALL SCREEN 2100 STARTING AT 10 10.

  IF NOT go_pack_exp_doc->is_cancelled( ).
    ev_huident = go_pack_exp_doc->get_data( ).
  ENDIF.
ENDFUNCTION.
