*&---------------------------------------------------------------------*
*& Include          ZMM_012_PRINT_TOP
*&---------------------------------------------------------------------*

CLASS lcl_class DEFINITION DEFERRED.
DATA:go_local TYPE REF TO lcl_class.
DATA:go_alv_grid  TYPE REF TO cl_gui_alv_grid,
     go_container TYPE REF TO cl_gui_custom_container,
     gt_fcat      TYPE lvc_t_fcat,
     gs_fcat      TYPE lvc_s_fcat,
     gs_layout    TYPE lvc_s_layo.

"Adobe Tanımlamaları
DATA: fm_name         TYPE rs38l_fnam,
      fp_docparams    TYPE sfpdocparams,
      fp_outputparams TYPE sfpoutputparams.

DATA:gv_fm_name TYPE fpname.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-000.
  PARAMETERS:rb_ham   RADIOBUTTON GROUP a DEFAULT 'X' USER-COMMAND cmd, "1
             rb_balya RADIOBUTTON GROUP a,                              "2
             rb_kimya RADIOBUTTON GROUP a,                              "3
             rb_spare RADIOBUTTON GROUP a,                              "4
             rb_cloth RADIOBUTTON GROUP a,                              "5
             rb_knit  RADIOBUTTON GROUP a.                              "6
SELECTION-SCREEN END OF BLOCK a.

SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE TEXT-001.
  PARAMETERS:p_mblnr TYPE mseg-mblnr MODIF ID x,                                   "1 "2 "3 "4 "5 "6
             p_charg TYPE mseg-charg MODIF ID a,                                   "1
             p_balya TYPE i  MODIF ID a.                                           "1
SELECTION-SCREEN END OF BLOCK b.

AT SELECTION-SCREEN OUTPUT.
  CASE abap_true.
    WHEN rb_ham.
      CLEAR:gv_fm_name.
      gv_fm_name = |ZMM_007_AF_HAMIPLIK|.     "Unfinished !!!
      LOOP AT SCREEN.
        IF screen-group1 EQ 'X'.
          screen-active = 1.
        ELSEIF screen-group1 EQ 'A'.
          screen-active = 1.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN rb_balya.
      CLEAR:gv_fm_name.
      gv_fm_name = |ZMM_006_AF_BALYA|.   "Done
      LOOP AT SCREEN.
        IF screen-group1 EQ 'X'.
          screen-active = 1.
        ELSEIF screen-group1 EQ 'A'.
          screen-active = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN rb_kimya.
      CLEAR:gv_fm_name.
      gv_fm_name = |ZMM_003_AF_ETIKET|.        "Unfinished !!!       " MBLNR DEĞİL IMPORT STRUCTURE OLARAK VERİLMİŞ
      LOOP AT SCREEN.
        IF screen-group1 EQ 'X'.
          screen-active = 1.
        ELSEIF screen-group1 EQ 'A'.
          screen-active = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN rb_spare.
      CLEAR:gv_fm_name.
      gv_fm_name = |ZMM_008_AF_SPARE|.  "Done
      LOOP AT SCREEN.
        IF screen-group1 EQ 'X'.
          screen-active = 1.
        ELSEIF screen-group1 EQ 'A'.
          screen-active = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN rb_cloth.                            "Unfinished !!!
      LOOP AT SCREEN.
        IF screen-group1 EQ 'X'.
          screen-active = 1.
        ELSEIF screen-group1 EQ 'A'.
          screen-active = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN rb_knit.                            "Unfinished !!!
      LOOP AT SCREEN.
        IF screen-group1 EQ 'X'.
          screen-active = 1.
        ELSEIF screen-group1 EQ 'A'.
          screen-active = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
  ENDCASE.
