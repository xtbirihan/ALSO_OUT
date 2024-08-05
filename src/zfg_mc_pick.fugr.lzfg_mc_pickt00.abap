*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_MC_PICK.....................................*
TABLES: ZMV_MC_PICK, *ZMV_MC_PICK. "view work areas
CONTROLS: TCTRL_ZMV_MC_PICK
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_MC_PICK. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_MC_PICK.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_MC_PICK_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_MC_PICK.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_MC_PICK_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_MC_PICK_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_MC_PICK.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_MC_PICK_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_MC_PICK                  .
