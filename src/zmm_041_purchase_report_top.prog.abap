*&---------------------------------------------------------------------*
*& Include          ZMM_041_PURCHASE_REPORT_TOP
*&---------------------------------------------------------------------*
TABLES:eban,ekko,ekpo,t16fs,rbkp.
CLASS lcl_class DEFINITION DEFERRED.
DATA:go_local           TYPE REF TO lcl_class,
     go_alv_grid        TYPE REF TO cl_gui_alv_grid,
***     go_container       TYPE REF TO cl_gui_custom_container,
     gt_fcat            TYPE lvc_t_fcat,
     gs_fcat            TYPE lvc_s_fcat,
     gs_layout          TYPE lvc_s_layo,
     gs_variant         TYPE disvariant,
     gs_variant_default TYPE disvariant.

DATA:gs_alv TYPE zmm_041_s_report,
     gt_alv TYPE TABLE OF zmm_041_s_report.

DATA:gs_cell_color TYPE lvc_s_scol.

"docking container
DATA:go_docking   TYPE REF TO cl_gui_docking_container,
     go_container TYPE REF TO cl_gui_container.
"splitter container
DATA:go_splitter  TYPE REF TO cl_gui_splitter_container,
     go_topofpage TYPE REF TO cl_gui_container,
     go_split1    TYPE REF TO cl_gui_container,
     go_split2    TYPE REF TO cl_gui_container.
"document
DATA:go_document TYPE REF TO cl_dd_document.

TYPES:BEGIN OF gty_sat_ernam_manager,
        pernr     TYPE objektid,
        sat_uname TYPE sysid,
        manager   TYPE objektid,
        uname     TYPE sysid,
      END OF gty_sat_ernam_manager.

DATA:gt_sat_onay TYPE TABLE OF gty_sat_ernam_manager.

TYPES:BEGIN OF gty_sas_approvers,
        pernr     TYPE objektid,
        sas_uname TYPE sysid,
        begda     TYPE begda,
        endda     TYPE endda,
        manager   TYPE objektid,
        uname     TYPE sysid,
        plans     TYPE plans,
        orgeh     TYPE orgeh,
        stell     TYPE stell,
        stext     TYPE stext,
      END OF gty_sas_approvers.

DATA:gt_sas_onay TYPE TABLE OF gty_sas_approvers.

RANGES:gr_frgco FOR t16fs-frgc1.

TYPES:BEGIN OF gty_sas_stell,
        ebeln TYPE ekko-ebeln,
        frgco TYPE frgco,
        index TYPE sy-tabix,
        ernam TYPE ekko-ernam,
      END OF gty_sas_stell.

DATA:gt_sstell TYPE TABLE OF gty_sas_stell,
     gt_frgco  TYPE TABLE OF gty_sas_stell.

PARAMETERS:p_var TYPE disvariant-variant.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-000.
  SELECT-OPTIONS:so_matnr FOR eban-matnr,
                 so_matkl FOR eban-matkl,
                 so_werks FOR eban-werks,
                 so_banfn FOR eban-banfn,
                 so_ekgrp FOR eban-ekgrp,
                 so_ernam FOR eban-ernam, """"MATCHCODE OBJECT zmm_041_sh_sat_ernam,
                 so_badat FOR eban-badat,
                 so_bsart FOR eban-bsart,
                 so_ebeln FOR ekpo-ebeln,
                 so_sasbt FOR ekko-bsart,
                 so_sasby FOR ekko-ernam, """"MATCHCODE OBJECT zmm_041_sh_sas_ernam,
                 so_aedat FOR ekko-aedat,
                 so_pstyp FOR ekpo-pstyp,
                 so_knttp FOR ekpo-knttp,
                 so_dispo FOR eban-dispo,
                 so_lifnr FOR ekko-lifnr,
                 so_belnr FOR rbkp-belnr MATCHCODE OBJECT zmm_041_sh_belnr,
                 so_bldat FOR rbkp-bldat.
SELECTION-SCREEN END OF BLOCK a.
