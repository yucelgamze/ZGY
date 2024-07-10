*&---------------------------------------------------------------------*
*& Include          ZMM_005_STOCK_TOP
*&---------------------------------------------------------------------*
TABLES:mseg,mkpf,lfa1,t001,t001l,mara.
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

DATA:gs_data TYPE zmm_005_s_data,
     gt_tab  TYPE TABLE OF zmm_005_s_tab,
     gs_tab  TYPE zmm_005_s_tab.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-000.
  PARAMETERS:p_mblnr TYPE mseg-mblnr.
SELECTION-SCREEN END OF BLOCK a.
