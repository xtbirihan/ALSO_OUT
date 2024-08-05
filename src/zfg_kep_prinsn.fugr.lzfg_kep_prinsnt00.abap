*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTOUT_KEP_PRINSN................................*
DATA:  BEGIN OF STATUS_ZTOUT_KEP_PRINSN              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTOUT_KEP_PRINSN              .
CONTROLS: TCTRL_ZTOUT_KEP_PRINSN
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTOUT_KEP_PRINSN              .
TABLES: ZTOUT_KEP_PRINSN               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
