*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_SHIP_PMAT...................................*
TABLES: ZMV_SHIP_PMAT, *ZMV_SHIP_PMAT. "view work areas
CONTROLS: TCTRL_ZMV_SHIP_PMAT
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_SHIP_PMAT. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_SHIP_PMAT.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_SHIP_PMAT_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_SHIP_PMAT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_SHIP_PMAT_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_SHIP_PMAT_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_SHIP_PMAT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_SHIP_PMAT_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_SHIP_PMAT                .
