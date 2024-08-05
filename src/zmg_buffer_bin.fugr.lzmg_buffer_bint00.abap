*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTOUT_BUFFER_BIN................................*
DATA:  BEGIN OF STATUS_ZTOUT_BUFFER_BIN              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTOUT_BUFFER_BIN              .
CONTROLS: TCTRL_ZTOUT_BUFFER_BIN
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTOUT_BUFFER_BIN              .
TABLES: ZTOUT_BUFFER_BIN               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
