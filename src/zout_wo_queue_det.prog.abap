*&---------------------------------------------------------------------*
*& Report ZOUT_WO_QUEUE_DET
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
**********************************************************************
*& Key           : <AAHMEDOV>-130623
*& Request No.   : GAPs 17 - Picking WO Bundling
**********************************************************************
INCLUDE zout_wo_queue_det_top.
INCLUDE zout_wo_queue_det_cl_i.

START-OF-SELECTION.

  TRY.
      NEW lcl_wo_queue_det( p_lgnum )->main( ).
    CATCH zcx_core_exception. "#EC NO_HANDLER
  ENDTRY.
