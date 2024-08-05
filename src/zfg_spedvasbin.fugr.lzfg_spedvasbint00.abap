*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_SPEDVASBIN..................................*
TABLES: ZMV_SPEDVASBIN, *ZMV_SPEDVASBIN. "view work areas
CONTROLS: TCTRL_ZMV_SPEDVASBIN
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_SPEDVASBIN. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_SPEDVASBIN.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_SPEDVASBIN_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_SPEDVASBIN.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_SPEDVASBIN_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_SPEDVASBIN_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_SPEDVASBIN.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_SPEDVASBIN_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_SPEDVASBIN               .
