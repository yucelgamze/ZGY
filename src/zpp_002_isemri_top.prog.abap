*&---------------------------------------------------------------------*
*& Include          ZPP_002_ISEMRI_TOP
*&---------------------------------------------------------------------*
TABLES:caufv,resb,mara.
CLASS lcl_class DEFINITION DEFERRED.
DATA:go_local     TYPE REF TO lcl_class,
     go_alv_grid  TYPE REF TO cl_gui_alv_grid,            "Başlık
     go_alv_grid2 TYPE REF TO cl_gui_alv_grid,            "Kalem
     go_container TYPE REF TO cl_gui_custom_container,
     gt_fcat      TYPE lvc_t_fcat,
     gt_fcat2     TYPE lvc_t_fcat,
     gs_fcat      TYPE lvc_s_fcat,
     gs_layout    TYPE lvc_s_layo.

"ADOBE IMPORT PARAMETRELERİ
DATA:gs_data  TYPE zpp_002_s_isemri_h,
     gt_data  TYPE zpp_002_tt_isemri_h,
     gt_table TYPE TABLE OF zpp_002_s_isemri,
     gs_table TYPE zpp_002_s_isemri.

"Adobe Tanımlamaları
DATA: fm_name         TYPE rs38l_fnam,
      fp_docparams    TYPE sfpdocparams,
      fp_outputparams TYPE sfpoutputparams.

DATA:gv_length TYPE i,
     gs_brand  TYPE zpp_002_t_isemri,
     gv_info   TYPE xfeld,
     gv_image  TYPE xfeld,
     gv_pdf    TYPE xfeld.

*--------------------------------------------------------------------*
DATA: dbdiff_values TYPE ddfixvalues,
      dbdiff_val    TYPE ddfixvalue.

DATA: BEGIN OF help OCCURS 0,
        low    TYPE ddfixvalue-low,
        ddtext TYPE ddfixvalue-ddtext,
      END OF help.

DATA: return TYPE TABLE OF ddshretval WITH HEADER LINE.

*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-000.
  PARAMETERS:rb_print RADIOBUTTON GROUP a DEFAULT 'X' USER-COMMAND cmd,
             rb_image RADIOBUTTON GROUP a,
             rb_domv  RADIOBUTTON GROUP a.
*             rb_gos   RADIOBUTTON GROUP a.
SELECTION-SCREEN END OF BLOCK a.

SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE TEXT-001.
  PARAMETERS:p_aufnr  TYPE caufv-aufnr MODIF ID x,
             p_brand  TYPE char3 MODIF ID a.
*             p_plnbez TYPE caufv-plnbez MODIF ID b.
SELECTION-SCREEN END OF BLOCK b.

SELECTION-SCREEN BEGIN OF BLOCK c WITH FRAME TITLE TEXT-002.
  PARAMETERS:p_file TYPE localfile MODIF ID y.
SELECTION-SCREEN END OF BLOCK c.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'P_FILE'
    IMPORTING
      file_name     = p_file.

*--------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_brand.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname      = 'ZPP_002_T_ISEMRI'
      fieldname    = 'BRAND_ID'
      lfieldname   = 'BRAND_ID'
    TABLES
      fixed_values = dbdiff_values.

  CLEAR help.  REFRESH help.
  LOOP AT dbdiff_values INTO dbdiff_val.
    MOVE-CORRESPONDING dbdiff_val TO help.
    APPEND help.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'BRAND_ID'
      dynprofield = 'P_BRAND'
      dynpprog    = sy-cprog
      dynpnr      = sy-dynnr
      value_org   = 'S'
    TABLES
      value_tab   = help.
*--------------------------------------------------------------------*


AT SELECTION-SCREEN OUTPUT.
  CASE abap_true.
    WHEN rb_print.
      gv_info  = abap_false.
      gv_image = abap_false.
      gv_pdf   = abap_true.
      LOOP AT SCREEN.
        IF screen-group1 EQ 'X'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'Y'.
          screen-active = 0.
          MODIFY SCREEN.
*        ELSEIF screen-group1 EQ 'A'.
*          screen-active = 0.
*          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    WHEN rb_image.
      gv_info  = abap_false.
      gv_image = abap_true.
      gv_pdf   = abap_false.
      LOOP AT SCREEN.
        IF screen-group1 EQ 'Y'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'X'.
          screen-active = 0.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'B'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    WHEN rb_domv.
      gv_info  = abap_true.
      gv_image = abap_false.
      gv_pdf   = abap_false.
      LOOP AT SCREEN.
        IF screen-group1 EQ 'X'.
          screen-active = 0.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'Y'.
          screen-active = 0.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'B'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

*    WHEN rb_gos.
*      gv_info  = abap_false.
*      gv_image = abap_false.
*      gv_pdf   = abap_false.
*      LOOP AT SCREEN.
*        IF screen-group1 EQ 'X'.
*          screen-active = 0.
*          MODIFY SCREEN.
*        ELSEIF screen-group1 = 'Y'.
*          screen-active = 0.
*          MODIFY SCREEN.
*        ELSEIF screen-group1 EQ 'A'.
*          screen-active = 0.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDLOOP.
  ENDCASE.



  "f4_filename func alternatifi:

*  DATA:lt_file_table TYPE filetable,
*       lv_rc         TYPE i.
*
*  cl_gui_frontend_services=>file_open_dialog(
*    CHANGING
*      file_table              = lt_file_table    " Table Holding Selected Files
*      rc                      = lv_rc    " Return Code, Number of Files or -1 If Error Occurred
*    EXCEPTIONS
*      file_open_dialog_failed = 1
*      cntl_error              = 2
*      error_no_gui            = 3
*      not_supported_by_gui    = 4
*      OTHERS                  = 5
*  ).
*  IF sy-subrc <> 0.
**     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    MESSAGE |Yükleme esnasında hata oluştu!| TYPE 'I' DISPLAY LIKE 'E'.
*  ELSE.
*    READ TABLE lt_file_table ASSIGNING FIELD-SYMBOL(<lfs_file_table>) INDEX 1.
*    IF sy-subrc IS INITIAL.
*      p_path = <lfs_file_table>-filename.
*    ENDIF.
*  ENDIF.
