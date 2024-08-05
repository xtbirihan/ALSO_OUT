*&---------------------------------------------------------------------*
*& Include          ZOUT_UI_PACK_GEN_PARAM_SEL
*&---------------------------------------------------------------------*
**********************************************************************
*& Key           : <AYORDANOV>-140723
*& Request No.   : GAPs 22 - Packing UI general program. This include
*&                 Content selection param of the UI pack appl.
**********************************************************************

*DATA: go_out_ui_pack_gen_prog TYPE REF TO lcl_out_ui_pack_gen_prog.

SELECTION-SCREEN BEGIN OF BLOCK workstation WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_lgnum TYPE /scwm/s_wrk_pack-lgnum       OBLIGATORY.
  PARAMETERS: p_wrkst TYPE /scwm/s_wrk_pack-workstation OBLIGATORY.
SELECTION-SCREEN END OF BLOCK workstation.

PARAMETERS: p_weight TYPE zstr_out_ui_common-scale_hu_weight MODIF ID wgh,
            p_unit   TYPE zstr_out_ui_common-meins_weight DEFAULT 'KG' MODIF ID wgh.

SELECTION-SCREEN BEGIN OF BLOCK filter WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_lgpla TYPE /scwm/lagp-lgpla OBLIGATORY.
  PARAMETERS: p_huid  TYPE /scwm/de_huident OBLIGATORY.
SELECTION-SCREEN END OF BLOCK filter.
