"Name: \TY:/SCWM/CL_SR_DO_TU\ME:SET_DATA\SE:END\EI
ENHANCEMENT 0 ZEI_OUT_TU_UI_DO_CREATE.
  IF is_bo_tu_data-tsp NE is_bo_tu_data-tsp_curr.
    ms_tu_hdr-zz_mix_sped_kep = zcl_out_tu_ui_enhance=>get_sped_kep_mix( iv_lgnum = /scwm/cl_tm=>sv_lgnum iv_tsp = is_bo_tu_data-tsp_curr ).
  ENDIF.
ENDENHANCEMENT.
