*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_WHOWCRFLTR..................................*
TABLES: ZMV_WHOWCRFLTR, *ZMV_WHOWCRFLTR. "view work areas
CONTROLS: TCTRL_ZMV_WHOWCRFLTR
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_ZMV_WHOWCRFLTR. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_WHOWCRFLTR.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_WHOWCRFLTR_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_WHOWCRFLTR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_WHOWCRFLTR_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_WHOWCRFLTR_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_WHOWCRFLTR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_WHOWCRFLTR_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_WHOWCRFLTR               .
