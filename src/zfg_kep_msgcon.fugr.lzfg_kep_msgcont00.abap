*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTOUT_KEP_MSGCON................................*
DATA:  BEGIN OF STATUS_ZTOUT_KEP_MSGCON              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTOUT_KEP_MSGCON              .
CONTROLS: TCTRL_ZTOUT_KEP_MSGCON
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTOUT_KEP_MSGCON              .
TABLES: ZTOUT_KEP_MSGCON               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
