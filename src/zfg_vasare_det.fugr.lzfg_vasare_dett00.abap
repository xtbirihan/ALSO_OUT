*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_VASARE_DET..................................*
TABLES: ZMV_VASARE_DET, *ZMV_VASARE_DET. "view work areas
CONTROLS: TCTRL_ZMV_VASARE_DET
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_VASARE_DET. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_VASARE_DET.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_VASARE_DET_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_VASARE_DET.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_VASARE_DET_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_VASARE_DET_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_VASARE_DET.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_VASARE_DET_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_VASARE_DET               .
