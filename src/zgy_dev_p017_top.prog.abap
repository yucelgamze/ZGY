*&---------------------------------------------------------------------*
*& Include          ZGY_DEV_P016_TOP
*&---------------------------------------------------------------------*
TABLES:bkpf,bseg,kna1,lfa1.

CLASS lcl_class DEFINITION DEFERRED.
DATA: go_local TYPE REF TO lcl_class.
DATA: go_alv_grid  TYPE REF TO cl_gui_alv_grid,
      go_container TYPE REF TO cl_gui_custom_container,
      gt_fcat      TYPE lvc_t_fcat,
      gs_fcat      TYPE lvc_s_fcat,
      gs_layout    TYPE lvc_s_layo.

TYPES:BEGIN OF gty_alv,
        no    TYPE char10,   "kunnr/lifnr (kna1-kunnr)/(lfa1-lifnr)
        isim  TYPE char70,  "(kna1-kunnr)/(lfa1-name1)
        pswsl TYPE bseg-pswsl,
*        bldat TYPE bkpf-bldat,
        spmon TYPE spmon,
        wrbtr TYPE bseg-wrbtr,
      END OF gty_alv.

DATA:gt_alv TYPE TABLE OF gty_alv,
     gs_alv TYPE gty_alv.

DATA: go_t_dynamic TYPE REF TO data,
      go_s_dynamic TYPE REF TO data.

FIELD-SYMBOLS: <dyn_table>   TYPE STANDARD TABLE,
               <gfs_s_table>,
               <gfs>.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-000.
  PARAMETERS:rb_sat RADIOBUTTON GROUP a DEFAULT 'X' USER-COMMAND cmd,
             rb_mus RADIOBUTTON GROUP a.
SELECTION-SCREEN END OF BLOCK a.

SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:so_lifnr FOR bseg-lifnr MODIF ID x,
                 so_kunnr FOR bseg-kunnr MODIF ID y,
                 so_bldat FOR bkpf-bldat ,
                 so_pswsl FOR bseg-pswsl.
SELECTION-SCREEN END OF BLOCK b.
