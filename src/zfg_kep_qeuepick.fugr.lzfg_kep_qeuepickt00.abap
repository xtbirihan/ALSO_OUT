*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTOUT_QEUEPICKCO................................*
DATA:  BEGIN OF STATUS_ZTOUT_QEUEPICKCO              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTOUT_QEUEPICKCO              .
CONTROLS: TCTRL_ZTOUT_QEUEPICKCO
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTOUT_QEUEPICKCO              .
TABLES: ZTOUT_QEUEPICKCO               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
