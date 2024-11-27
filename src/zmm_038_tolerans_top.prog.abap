*&---------------------------------------------------------------------*
*& Include          ZMM_038_TOLERANS_TOP
*&---------------------------------------------------------------------*

TABLES:ekko,ekpo,cdhdr,cdpos.

CLASS lcl_class DEFINITION DEFERRED.
DATA:go_local     TYPE REF TO lcl_class,
     go_alv_grid  TYPE REF TO cl_gui_alv_grid,
     go_container TYPE REF TO cl_gui_custom_container,
     gt_fcat      TYPE lvc_t_fcat,
     gs_fcat      TYPE lvc_s_fcat,
     gs_layout    TYPE lvc_s_layo.

TYPES:BEGIN OF gty_tab,
        ernam    TYPE ekko-ernam,
        ebeln    TYPE ekko-ebeln,
        ebelp    TYPE ekpo-ebelp,
*        uebto TYPE ekpo-uebto,
*        untto TYPE ekpo-untto,
        changenr TYPE  cdpos-changenr,
      END OF gty_tab.
