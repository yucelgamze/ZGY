*&---------------------------------------------------------------------*
*& Include          ZMM_001_CONTRACT_TOP
*&---------------------------------------------------------------------*
TABLES:ekko,ekpo.
CLASS lcl_class DEFINITION DEFERRED.
DATA: go_local TYPE REF TO lcl_class.
DATA: go_alv_grid  TYPE REF TO cl_gui_alv_grid,
      go_container TYPE REF TO cl_gui_custom_container,
      gt_fcat      TYPE lvc_t_fcat,
      gs_fcat      TYPE lvc_s_fcat,
      gs_layout    TYPE lvc_s_layo.

DATA:gt_alv TYPE TABLE OF zmm_001_t_cntrct.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-000.
  SELECT-OPTIONS:so_bsart FOR ekko-bsart,
                 so_ekgrp FOR ekko-ekgrp,
                 so_ekorg FOR ekko-ekorg,
                 so_bukrs FOR ekko-bukrs.
SELECTION-SCREEN END OF BLOCK a.
