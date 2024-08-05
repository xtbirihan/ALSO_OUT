*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_LGTYPRNDUP..................................*
TABLES: ZMV_LGTYPRNDUP, *ZMV_LGTYPRNDUP. "view work areas
CONTROLS: TCTRL_ZMV_LGTYPRNDUP
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_LGTYPRNDUP. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_LGTYPRNDUP.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_LGTYPRNDUP_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_LGTYPRNDUP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_LGTYPRNDUP_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_LGTYPRNDUP_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_LGTYPRNDUP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_LGTYPRNDUP_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_LGTYPRNDUP               .
