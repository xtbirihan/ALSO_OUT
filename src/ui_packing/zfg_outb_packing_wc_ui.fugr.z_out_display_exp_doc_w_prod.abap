FUNCTION Z_OUT_DISPLAY_EXP_DOC_W_PROD.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_EXP_DOC) TYPE  STRING
*"  EXPORTING
*"     REFERENCE(EV_PROD) TYPE  ZSTR_OUT_UI_COMMON-PRODUCT_EAN_MPN
*"----------------------------------------------------------------------
  CLEAR ev_prod.

  go_pack_exp_doc_prod = NEW #( ).

  go_pack_exp_doc_prod->set_data(
    iv_text = iv_exp_doc
  ).

  CALL SCREEN 2110 STARTING AT 10 10.

  IF NOT go_pack_exp_doc_prod->is_cancelled( ).
    ev_prod = go_pack_exp_doc_prod->get_data( ).
  ENDIF.
ENDFUNCTION.
