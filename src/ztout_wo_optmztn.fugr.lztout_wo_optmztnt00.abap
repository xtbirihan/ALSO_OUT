*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTOUT_WO_OPTMZTN................................*
DATA:  BEGIN OF STATUS_ZTOUT_WO_OPTMZTN              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTOUT_WO_OPTMZTN              .
CONTROLS: TCTRL_ZTOUT_WO_OPTMZTN
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTOUT_WO_OPTMZTN              .
TABLES: ZTOUT_WO_OPTMZTN               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
