*&---------------------------------------------------------------------*
*& Include          ZGY_DEV_P001_TOP
*&---------------------------------------------------------------------*
TABLES:bkpf,bseg,t003t,tbslt.
CLASS lcl_class DEFINITION DEFERRED.
DATA:go_local     TYPE REF TO lcl_class,
     go_alv_grid  TYPE REF TO cl_gui_alv_grid, "header
     go_alv_grid2 TYPE REF TO cl_gui_alv_grid, "item
*     go_container TYPE REF TO cl_gui_custom_container,
     gt_fcat      TYPE lvc_t_fcat,
     gt_fcat2     TYPE lvc_t_fcat,
     gs_fcat      TYPE lvc_s_fcat,
     gs_layout    TYPE lvc_s_layo.

DATA:go_splitter  TYPE REF TO cl_gui_splitter_container,
     go_topofpage TYPE REF TO cl_gui_container,
     go_split1    TYPE REF TO cl_gui_container,
     go_split2    TYPE REF TO cl_gui_container.

DATA:go_docking   TYPE REF TO cl_gui_docking_container,
     go_container TYPE REF TO cl_gui_container.

DATA:go_document TYPE REF TO cl_dd_document.

TYPES:BEGIN OF gty_header,
        bukrs TYPE bkpf-bukrs,
        belnr TYPE bkpf-belnr,
        gjahr TYPE bkpf-gjahr,
        blart TYPE bkpf-blart,
        ltext TYPE t003t-ltext,
        budat TYPE bkpf-budat,
        bktxt TYPE bkpf-bktxt,
      END OF gty_header.

DATA:gt_header TYPE TABLE OF gty_header,
     gs_header TYPE gty_header.

TYPES:BEGIN OF gty_item,
        bukrs TYPE bseg-bukrs,
        belnr TYPE bseg-belnr,
        gjahr TYPE bseg-gjahr,
        buzei TYPE bseg-buzei,
        bschl TYPE bseg-bschl,
        ltext TYPE tbslt-ltext,
        dmbtr TYPE bseg-dmbtr,
        wrbtr TYPE bseg-wrbtr,
      END OF gty_item.

DATA:gt_item TYPE TABLE OF gty_item,
     gs_item TYPE gty_item.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-000.
  SELECT-OPTIONS:so_bukrs FOR bkpf-bukrs NO INTERVALS OBLIGATORY DEFAULT 1000,
                 so_belnr FOR bkpf-belnr,
                 so_gjahr FOR bkpf-gjahr NO INTERVALS.
SELECTION-SCREEN END OF BLOCK a.
