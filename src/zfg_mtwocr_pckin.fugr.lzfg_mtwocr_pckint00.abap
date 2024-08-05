*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_WOCR_PCKIN..................................*
TABLES: ZMV_WOCR_PCKIN, *ZMV_WOCR_PCKIN. "view work areas
CONTROLS: TCTRL_ZMV_WOCR_PCKIN
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_WOCR_PCKIN. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_WOCR_PCKIN.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_WOCR_PCKIN_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_WOCR_PCKIN.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_WOCR_PCKIN_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_WOCR_PCKIN_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_WOCR_PCKIN.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_WOCR_PCKIN_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_WOCR_PCKIN               .
