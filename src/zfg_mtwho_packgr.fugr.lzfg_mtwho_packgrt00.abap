*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_WHO_PACKGR..................................*
TABLES: ZMV_WHO_PACKGR, *ZMV_WHO_PACKGR. "view work areas
CONTROLS: TCTRL_ZMV_WHO_PACKGR
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_WHO_PACKGR. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_WHO_PACKGR.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_WHO_PACKGR_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_WHO_PACKGR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_WHO_PACKGR_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_WHO_PACKGR_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_WHO_PACKGR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_WHO_PACKGR_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_WHO_PACKGR               .
