"Name: \TY:/SCWM/CL_UI_FIELD_CONTROL\ME:GET_ASPECT_PROPERTY\SE:END\EI
ENHANCEMENT 0 ZEI_OUT_TU_UI_ASPECT.
**********************************************************************
*& Key           : LH-111123
*& Request No.   : GAP-12 – "VCE Carrier integration”
**********************************************************************
*& Description (short)
*& Add fields which can be edited
**********************************************************************
  IF iv_aspect EQ '/SCWM/S_ASP_TU'.
    APPEND LINES OF VALUE /scwm/tt_ui_property( ( name = 'ZZ_NO_CMR_PRINT' ) ) TO rt_property.
    APPEND LINES OF VALUE /scwm/tt_ui_property( ( name = 'ZZ_MIX_SPED_KEP' ) ) TO rt_property.
  ENDIF.
ENDENHANCEMENT.
