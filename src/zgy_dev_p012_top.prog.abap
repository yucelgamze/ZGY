*&---------------------------------------------------------------------*
*& Include          ZGY_DEV_P012_TOP
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION DEFERRED.
DATA:go_local     TYPE REF TO lcl_class,
     go_alv_grid  TYPE REF TO cl_gui_alv_grid,
     go_container TYPE REF TO cl_gui_custom_container,
     gt_fcat      TYPE lvc_t_fcat,
     gs_fcat      TYPE lvc_s_fcat,
     gs_layout    TYPE lvc_s_layo.

"Adobe Tanımlamaları
DATA: fm_name         TYPE rs38l_fnam,
      fp_docparams    TYPE sfpdocparams,
      fp_outputparams TYPE sfpoutputparams.

SELECTION-SCREEN BEGIN OF BLOCK c WITH FRAME TITLE TEXT-002.
  PARAMETERS:rb_print RADIOBUTTON GROUP x DEFAULT 'X' USER-COMMAND cmd,
             rb_load  RADIOBUTTON GROUP x.
SELECTION-SCREEN END OF BLOCK c.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-000.
  PARAMETERS:p_file TYPE localfile MODIF ID a.
SELECTION-SCREEN END OF BLOCK a.

SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE TEXT-001.
  PARAMETERS:p_id TYPE int1 MODIF ID b.
SELECTION-SCREEN END OF BLOCK b.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'P_FILE'
    IMPORTING
      file_name     = p_file.

AT SELECTION-SCREEN OUTPUT.
  CASE abap_true.

    WHEN rb_print.
      LOOP AT SCREEN.
        IF screen-group1 EQ 'A'.
          screen-active = 0.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'B'.
          screen-active = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    WHEN rb_load.
      LOOP AT SCREEN.
        IF screen-group1 EQ 'A'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'B'.
          screen-active = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
  ENDCASE.




*  cl_gui_frontend_services=>file_open_dialog
