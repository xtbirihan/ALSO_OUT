*&---------------------------------------------------------------------*
*& Include          ZOUT_WO_QUEUE_DET_SEL
*&---------------------------------------------------------------------*

TABLES:
  /scwm/s_rsrc_mon_f4.
*  /scwm/s_wo_to_mon_in.

*DATA: gc_dynnr TYPE dynnr VALUE 100.

SELECTION-SCREEN BEGIN OF BLOCK b1.
  PARAMETERS: p_lgnum TYPE /scwm/lgnum OBLIGATORY.
  SELECT-OPTIONS: so_que FOR /scwm/s_rsrc_mon_f4-queue.

SELECTION-SCREEN END OF BLOCK b1.
