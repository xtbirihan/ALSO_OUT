*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_QUEUE_DET...................................*
TABLES: ZMV_QUEUE_DET, *ZMV_QUEUE_DET. "view work areas
CONTROLS: TCTRL_ZMV_QUEUE_DET
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_QUEUE_DET. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_QUEUE_DET.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_QUEUE_DET_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_QUEUE_DET.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_QUEUE_DET_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_QUEUE_DET_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_QUEUE_DET.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_QUEUE_DET_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_QUEUE_DET                .
