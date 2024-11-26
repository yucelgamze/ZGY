*&---------------------------------------------------------------------*
*& Include          ZSD_002_FLOW_REPORTXXX_TOP
*&---------------------------------------------------------------------*

TABLES:vbak,vbap,lips,likp,vbrk,vbkd,vbfa,prcd_elements.

CLASS lcl_class DEFINITION DEFERRED.
DATA:go_local     TYPE REF TO lcl_class,
     go_alv_grid  TYPE REF TO cl_gui_alv_grid,
     go_container TYPE REF TO cl_gui_custom_container,
     gt_fcat      TYPE lvc_t_fcat,
     gs_fcat      TYPE lvc_s_fcat,
     gs_layout    TYPE lvc_s_layo.

DATA:gt_alv TYPE TABLE OF zsd_002_s_flow_report,
     gs_alv TYPE zsd_002_s_flow_report.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:so_kunnr FOR vbak-kunnr,
                 so_audat FOR vbak-audat,
                 so_vbeln FOR vbap-vbeln,
                 so_posnr FOR vbap-posnr,
                 so_vkorg FOR vbak-vkorg,
                 so_vtweg FOR vbak-vtweg,
                 so_abgru FOR vbap-abgru,
                 so_augru FOR vbak-augru,
                 so_auart FOR vbak-auart,
                 so_matnr FOR vbap-matnr,
                 so_werks FOR lips-werks,
                 so_lgort FOR lips-lgort,
                 so_mta   FOR likp-kunnr,
                 so_tes   FOR likp-vbeln,
                 so_fat   FOR vbrk-vbeln.
SELECTION-SCREEN END OF BLOCK a.
