*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_SPMAT_PMAT..................................*
TABLES: ZMV_SPMAT_PMAT, *ZMV_SPMAT_PMAT. "view work areas
CONTROLS: TCTRL_ZMV_SPMAT_PMAT
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_SPMAT_PMAT. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_SPMAT_PMAT.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_SPMAT_PMAT_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_SPMAT_PMAT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_SPMAT_PMAT_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_SPMAT_PMAT_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_SPMAT_PMAT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_SPMAT_PMAT_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_SPMAT_PMAT               .
