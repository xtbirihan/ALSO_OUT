**********************************************************************
*& Key           : AMOGYORODI-230814
*& Request No.   :
**********************************************************************
*& Description (short)
*&
*&
**********************************************************************
*&---------------------------------------------------------------------*
*& Include          ZOUT_CONSOLIDATION_SEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:
    p_lgnum TYPE /scwm/lgnum OBLIGATORY, "UM 07.11.2023 commented the memory id "MEMORY ID /SCWM/LGN.
    p_works TYPE /scwm/de_workstation OBLIGATORY,
    p_lost  TYPE zde_lostnfound_stbin OBLIGATORY,
    p_prin  TYPE rspopname OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
