*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_WHOPACKLIM..................................*
TABLES: ZMV_WHOPACKLIM, *ZMV_WHOPACKLIM. "view work areas
CONTROLS: TCTRL_ZMV_WHOPACKLIM
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_WHOPACKLIM. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_WHOPACKLIM.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_WHOPACKLIM_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_WHOPACKLIM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_WHOPACKLIM_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_WHOPACKLIM_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_WHOPACKLIM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_WHOPACKLIM_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_WHO_PACKLI               .
