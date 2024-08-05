*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_OUT_WGHT_DEV................................*
TABLES: ZMV_OUT_WGHT_DEV, *ZMV_OUT_WGHT_DEV. "view work areas
CONTROLS: TCTRL_ZMV_OUT_WGHT_DEV
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_OUT_WGHT_DEV. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_OUT_WGHT_DEV.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_OUT_WGHT_DEV_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_OUT_WGHT_DEV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_OUT_WGHT_DEV_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_OUT_WGHT_DEV_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_OUT_WGHT_DEV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_OUT_WGHT_DEV_TOTAL.

*.........table declarations:.................................*
TABLES: ZOUT_WEIGHT_DEV                .
