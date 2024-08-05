*----------------------------------------------------------------------*
***INCLUDE ZOUT_CONSOLIDATION_O03.
*----------------------------------------------------------------------*
**********************************************************************
*& Key           : AMOGYORODI-230814
*& Request No.   :
**********************************************************************
*& Description (short)
*&
*&
**********************************************************************
*&---------------------------------------------------------------------*
*& Module STATUS_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0400 OUTPUT.
  SET PF-STATUS 'STANDARD'.
  SET TITLEBAR 'TITLE_0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module PBO_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_0400 OUTPUT.

  MOVE-CORRESPONDING zstr_consolidation_scr_0300 TO zstr_consolidation_scr_0400.

  DATA(lo_container_huitem_split) = NEW cl_gui_custom_container( container_name = 'CONT_HU_ITEMS' ).
  DATA(lo_container_huitem_dest) = NEW cl_gui_custom_container( container_name = 'CONT_HU_DEST' ).

  lo_model = lcl_model=>get_instance( ).

  lo_model->build_cons_hu_table( iv_docid = zstr_consolidation_scr_0400-docid ).

  lo_model->build_cons_huitem_table( EXPORTING iv_huident = zstr_consolidation_scr_0400-source_hu
                                               it_huitem  = lo_model->mt_cons_huitm
                                     CHANGING ct_hu_content = lo_model->mt_cons_hu_content ).

  lo_model->build_cons_huitem_table( EXPORTING iv_huident = zstr_consolidation_scr_0400-dest_hu
                                               iv_split   = abap_true
                                               it_huitem = lo_model->mt_cons_huitm
                                     CHANGING ct_hu_content = lo_model->mt_dest_hu_content ).

  lo_view = lcl_view=>get_instance( ).
  lo_view->show_alv_cons_hu_content( lo_container_huitem_split ).
  lo_view->show_alv_dest_hu_content( lo_container_huitem_dest ).

ENDMODULE.
