*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_POSC_CMAP...................................*
TABLES: ZMV_POSC_CMAP, *ZMV_POSC_CMAP. "view work areas
CONTROLS: TCTRL_ZMV_POSC_CMAP
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_POSC_CMAP. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_POSC_CMAP.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_POSC_CMAP_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_POSC_CMAP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_POSC_CMAP_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_POSC_CMAP_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_POSC_CMAP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_POSC_CMAP_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_POSC_CMAP                .
