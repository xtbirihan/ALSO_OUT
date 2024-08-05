FUNCTION z_out_start_ui.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------

  zcl_outb_packing_workcenter_ui=>get_def_parameters(
        IMPORTING
          ev_lgnum = /scwm/s_wrk_pack-lgnum
          ev_wrkst = /scwm/s_wrk_pack-workstation
          ev_lgpla = zstr_out_ui_common-stbin
      ).
  zstr_out_ui_common-meins_weight = 'KG'.
  IF zcl_outb_packing_workcenter_ui=>is_workcenter_mastercarton_rel(
       iv_lgnum      = /scwm/s_wrk_pack-lgnum                  " Warehouse Number/Warehouse Complex
       iv_workcenter = /scwm/s_wrk_pack-workstation                 " Work Center
     ).
    CALL SCREEN '0020'.
  ELSE.
    CALL SCREEN '0010'.
  ENDIF.
ENDFUNCTION.
