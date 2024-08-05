*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_QUE_LY_MAP..................................*
TABLES: ZMV_QUE_LY_MAP, *ZMV_QUE_LY_MAP. "view work areas
CONTROLS: TCTRL_ZMV_QUE_LY_MAP
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_QUE_LY_MAP. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_QUE_LY_MAP.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_QUE_LY_MAP_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_QUE_LY_MAP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_QUE_LY_MAP_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_QUE_LY_MAP_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_QUE_LY_MAP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_QUE_LY_MAP_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_QUE_LY_MAP               .
