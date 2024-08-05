"Name: \TY:/SCWM/CL_SR_DO_TU\ME:CONSTRUCTOR\SE:END\EI
ENHANCEMENT 0 ZEI_OUT_TU_UI_DO_CREATE.
**********************************************************************
*& Key           : LH-111123
*& Request No.   : GAP-91 – "Load RF”
**********************************************************************
*& Description (short)
*& Get default value for Mixing SPED/KEP allowance
**********************************************************************
    IF is_do_tu_new IS NOT INITIAL.
      ms_tu_hdr-zz_mix_sped_kep = zcl_out_tu_ui_enhance=>get_sped_kep_mix( iv_lgnum = /scwm/cl_tm=>sv_lgnum iv_tsp = is_do_tu_new-tsp_curr ).
    ENDIF.
ENDENHANCEMENT.
