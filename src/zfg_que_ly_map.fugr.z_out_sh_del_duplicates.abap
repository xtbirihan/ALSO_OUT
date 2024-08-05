FUNCTION Z_OUT_SH_DEL_DUPLICATES.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) TYPE  DDSHF4CTRL
*"----------------------------------------------------------------------
*********************************************************************
*& Key           : <aahmedov>-25.07.2023
*& Request No.   : GAPs 17 - Picking WO Bundling
*& Description   : table ZTOUT_LAYOUT_TYP has several entries for the same layout type_id,
*                  and the purpose of this function module is to delete adjacent duplicates,
*                  so that the user does not see duplicate entries
* Example:
*  What the user would usually see:
*  4X4
*  4X4
*  4X4
*  4X4
*  What the user sees after this function module:
*  4X4
**********************************************************************

  IF record_tab[] IS INITIAL.
    RETURN.
  ENDIF.
*
  DELETE ADJACENT DUPLICATES FROM record_tab[].

ENDFUNCTION.
