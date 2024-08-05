*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_LAYOUT_TYP..................................*
TABLES: ZMV_LAYOUT_TYP, *ZMV_LAYOUT_TYP. "view work areas
CONTROLS: TCTRL_ZMV_LAYOUT_TYP
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_LAYOUT_TYP. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_LAYOUT_TYP.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_LAYOUT_TYP_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_LAYOUT_TYP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_LAYOUT_TYP_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_LAYOUT_TYP_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_LAYOUT_TYP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_LAYOUT_TYP_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_LAYOUT_TYP               .
