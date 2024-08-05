*&---------------------------------------------------------------------*
*& Report Z_SPO_SLO_OPTIMIZATION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_spo_slo_optimization.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS p_lgnum LIKE /scwm/t300t-lgnum
                     MEMORY ID /scwm/lgn OBLIGATORY VALUE CHECK.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  TRY.
      DATA(lo_spo_slo_optimization) = NEW zcl_spo_slo_optimization( p_lgnum ).
      lo_spo_slo_optimization->fetch_data( ).
*      lo_spo_slo_optimization->save_log_messages( lo_spo_slo_optimization->get_log_messages( ) ).
    CATCH zcx_whse_order INTO DATA(lo_whse_order).
      DATA(lv_text) =  lo_whse_order->get_text( ).
      WRITE : |{ TEXT-002 } `: ` { lv_text }|.
  ENDTRY.
