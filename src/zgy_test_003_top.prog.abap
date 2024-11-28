*&---------------------------------------------------------------------*
*& Include          ZGY_TEST_003_TOP
*&---------------------------------------------------------------------*
TABLES:ekko, ekpo, sscrfields.

CLASS lcl_class DEFINITION DEFERRED.
DATA:go_local     TYPE REF TO lcl_class,
     go_alv_grid  TYPE REF TO cl_gui_alv_grid,
     go_container TYPE REF TO cl_gui_custom_container,
     gt_fcat      TYPE lvc_t_fcat,
     gs_fcat      TYPE lvc_s_fcat,
     gs_layout    TYPE lvc_s_layo.

TYPES : BEGIN OF gty_excel,
          ebeln TYPE ebeln,
          ebelp TYPE ebelp,
        END OF gty_excel,
        gtt_excel TYPE TABLE OF gty_excel.

DATA: gt_excel TYPE gtt_excel,
      gs_excel TYPE gty_excel.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  PARAMETERS:p_file TYPE localfile.  "EXCEL to ITAB file
  PARAMETERS:p_fitab TYPE localfile. "ITAB to EXCEL file
SELECTION-SCREEN END OF BLOCK b1.

"şablon indirme butonu
DATA:gs_sel_button TYPE smp_dyntxt.
SELECTION-SCREEN FUNCTION KEY 1.


RANGES:gr_ebeln FOR ekpo-ebeln,
       gr_ebelp FOR ekpo-ebelp.

DATA:lt_excel_format TYPE TABLE OF gty_excel WITH HEADER LINE,
     ls_excel_format TYPE gty_excel.

DATA:gt_data TYPE gtt_excel,
     gt_xls  TYPE gtt_excel.

TYPES:BEGIN OF gty_header,
        header TYPE char50,
      END OF gty_header.

DATA:gt_header TYPE TABLE OF gty_header,
     gs_header TYPE gty_header.
