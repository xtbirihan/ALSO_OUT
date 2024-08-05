*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_SVKWC_STAG..................................*
TABLES: ZMV_SVKWC_STAG, *ZMV_SVKWC_STAG. "view work areas
CONTROLS: TCTRL_ZMV_SVKWC_STAG
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_SVKWC_STAG. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_SVKWC_STAG.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_SVKWC_STAG_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_SVKWC_STAG.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_SVKWC_STAG_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_SVKWC_STAG_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_SVKWC_STAG.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_SVKWC_STAG_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_SVKWC_STAG               .
