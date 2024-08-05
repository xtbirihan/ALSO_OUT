**********************************************************************
*& Key           : AMOGYORODI-230814
*& Request No.   :
**********************************************************************
*& Description (short)
*&
*&
**********************************************************************
*----------------------------------------------------------------------*
***INCLUDE ZOUT_CONSOLIDATION_I05.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0500 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0500 OUTPUT.
  SET PF-STATUS 'STANDARD'.
  SET TITLEBAR 'TITLE_0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module PBO_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_0500 OUTPUT.

  CLEAR zstr_consolidation_scr_0500.
  MOVE-CORRESPONDING zstr_consolidation_scr_0300 TO zstr_consolidation_scr_0500.

  DATA(lo_container_exp_items) = NEW cl_gui_custom_container( container_name = 'CONT_EXP_ITEMS' ).
  DATA(lo_container_packed_items) = NEW cl_gui_custom_container( container_name = 'CONT_PACK_ITEMS' ).

  lo_view = lcl_view=>get_instance( ).
  lo_model = lcl_model=>get_instance( ).

  lo_view->mt_diff_hu_src = lo_model->mt_diff_hu_src.

  lo_view->show_alv_diff_hu_exp( lo_container_exp_items ).
  lo_view->show_alv_diff_hu_pack( lo_container_packed_items ).

ENDMODULE.
