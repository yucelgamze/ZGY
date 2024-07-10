*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMM_001_T_MAILGR................................*
DATA:  BEGIN OF STATUS_ZMM_001_T_MAILGR              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_001_T_MAILGR              .
CONTROLS: TCTRL_ZMM_001_T_MAILGR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMM_001_T_MAILGR              .
TABLES: ZMM_001_T_MAILGR               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
