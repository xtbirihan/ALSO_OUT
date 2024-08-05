"Name: \TY:/SCWM/CL_SR_DO_TU\ME:SET_TU_DLV\SE:END\EI
ENHANCEMENT 0 ZEI_OUT_TU_UI_DO_CREATE.
**********************************************************************
*& Key           : LH-111123
*& Request No.   : GAP-12 – "VCE Carrier integration”
**********************************************************************
*& Description (short)
*& Get default values
**********************************************************************
  zcl_out_tu_ui_enhance=>get_tu_cmr(
    EXPORTING
      iv_chgind        = iv_chgind
      is_tu_dlv        = is_tu_dlv
      io_tudlv_manager = lo_tudlv_manager
    CHANGING
      cv_cmr           = ms_tu_hdr-zz_cmr
      cv_kep_sped      = ms_tu_hdr-zz_mix_sped_kep
  ).


ENDENHANCEMENT.
