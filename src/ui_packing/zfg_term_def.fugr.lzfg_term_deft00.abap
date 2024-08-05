*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_TERM_DEF....................................*
TABLES: ZMV_TERM_DEF, *ZMV_TERM_DEF. "view work areas
CONTROLS: TCTRL_ZMV_TERM_DEF
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_TERM_DEF. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_TERM_DEF.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_TERM_DEF_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_TERM_DEF.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_TERM_DEF_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_TERM_DEF_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_TERM_DEF.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_TERM_DEF_TOTAL.

*.........table declarations:.................................*
TABLES: ZOUT_TERM_DEF                  .
