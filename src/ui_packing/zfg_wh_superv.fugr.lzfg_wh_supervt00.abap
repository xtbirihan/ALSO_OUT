*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_WH_SUPERV...................................*
TABLES: ZMV_WH_SUPERV, *ZMV_WH_SUPERV. "view work areas
CONTROLS: TCTRL_ZMV_WH_SUPERV
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_WH_SUPERV. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_WH_SUPERV.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_WH_SUPERV_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_WH_SUPERV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_WH_SUPERV_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_WH_SUPERV_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_WH_SUPERV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_WH_SUPERV_TOTAL.

*.........table declarations:.................................*
TABLES: ZOUT_WH_SUPERV                 .
