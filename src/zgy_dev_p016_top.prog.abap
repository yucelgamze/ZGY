*&---------------------------------------------------------------------*
*& Include          ZGY_DEV_P016_TOP
*&---------------------------------------------------------------------*
TABLES:bkpf, bseg, kna1, lfa1.
CLASS lcl_class DEFINITION DEFERRED.
DATA:go_local     TYPE REF TO lcl_class,
     go_alv_grid  TYPE REF TO cl_gui_alv_grid,
     go_container TYPE REF TO cl_gui_custom_container,
     gt_fcat      TYPE lvc_t_fcat,
     gs_fcat      TYPE lvc_s_fcat,
     gs_layout    TYPE lvc_s_layo.

TYPES:BEGIN OF gty_alv,
        bukrs TYPE bkpf-bukrs,
        belnr TYPE bseg-belnr,
        gjahr TYPE bseg-gjahr,
        no    TYPE char10, "kna1-kunnr (müşteri) ya da lfa1-lifnr (satıcı)
        isim  TYPE char70,
        pswsl TYPE bseg-pswsl,
        wrbtr TYPE bseg-wrbtr,
        bldat TYPE bkpf-bldat,
      END OF gty_alv.

DATA:gt_alv TYPE TABLE OF gty_alv,
     gs_alv TYPE gty_alv.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-000.
  PARAMETERS:p_bukrs TYPE bkpf-bukrs ."MATCHCODE OBJECT zgy_sh_0010.
  SELECT-OPTIONS:so_belnr FOR bkpf-belnr,
                 so_pswsl FOR bseg-pswsl.
SELECTION-SCREEN END OF BLOCK a.
