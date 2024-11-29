*&---------------------------------------------------------------------*
*& Include          ZMM_032_PR_APPROVAL_TOP
*&---------------------------------------------------------------------*
TABLES:eban,cdhdr,cdpos.

CLASS lcl_class DEFINITION DEFERRED.
DATA:go_local TYPE REF TO lcl_class.
DATA:go_alv_grid  TYPE REF TO cl_gui_alv_grid,
     go_container TYPE REF TO cl_gui_custom_container,
     gt_fcat      TYPE lvc_t_fcat,
     gt_fcat_x    TYPE lvc_t_fcat,
     gt_fcat_a    TYPE lvc_t_fcat,
     gt_fcat_b    TYPE lvc_t_fcat,
     gt_fcat_c    TYPE lvc_t_fcat,
     gt_fcat_i    TYPE lvc_t_fcat,
     gs_fcat      TYPE lvc_s_fcat,
     gs_layout    TYPE lvc_s_layo.

TYPES:BEGIN OF gty_app,
        banfn     TYPE eban-banfn,
        bnfpo     TYPE eban-bnfpo,
        banpr     TYPE eban-banpr,
        badat     TYPE eban-badat,
        frgdt     TYPE eban-frgdt,
        ernam     TYPE eban-ernam,
        mtart     TYPE mara-mtart,
        matnr     TYPE eban-matnr,
**        maktx     TYPE makt-maktx,
        maktx     TYPE char250,"mara-zz1_uzuntanim_prd,
        matkl     TYPE eban-matkl,
        wgbez     TYPE t023t-wgbez,
        preis     TYPE eban-preis,
        peinh     TYPE eban-peinh,
        menge     TYPE eban-menge,
        meins     TYPE eban-meins,
        waers     TYPE eban-waers,
        lv_totc   TYPE char40,
        lv_menge  TYPE char20,
        lv_preis  TYPE char20,
        txz01     TYPE eban-txz01,  "mara matnr boş ise tanımı buradan alacağız!!
        lv_tot    TYPE decfloat34,
        lv_total  TYPE decfloat34,
        lv_totalc TYPE char40,
        ekgrp	    TYPE ekgrp,
        zperson	  TYPE zmm_032_de_person,
        sags      TYPE zmm_032_de_sag,
      END OF gty_app.

RANGES:gr_tabkey FOR cdpos-tabkey.
