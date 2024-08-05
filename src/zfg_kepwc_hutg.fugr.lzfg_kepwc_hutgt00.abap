*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_KEPWC_HUTG..................................*
TABLES: ZMV_KEPWC_HUTG, *ZMV_KEPWC_HUTG. "view work areas
CONTROLS: TCTRL_ZMV_KEPWC_HUTG
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_KEPWC_HUTG. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_KEPWC_HUTG.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_KEPWC_HUTG_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_KEPWC_HUTG.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_KEPWC_HUTG_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_KEPWC_HUTG_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_KEPWC_HUTG.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_KEPWC_HUTG_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_KEPWC_HUTG               .
