*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTOUT_KEP_CBQUEU................................*
DATA:  BEGIN OF STATUS_ZTOUT_KEP_CBQUEU              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTOUT_KEP_CBQUEU              .
CONTROLS: TCTRL_ZTOUT_KEP_CBQUEU
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTOUT_KEP_CBQUEU              .
TABLES: ZTOUT_KEP_CBQUEU               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
