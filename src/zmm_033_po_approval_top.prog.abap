*&---------------------------------------------------------------------*
*& Include          ZMM_033_PO_APPROVAL_TOP
*&---------------------------------------------------------------------*

TABLES:ekko,ekpo,cdhdr,cdpos,t052u,makt,t001w,t001l.

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
        aedat             TYPE ekko-aedat,
        ernam             TYPE ekko-ernam,
        name1             TYPE lfa1-name1,
        text1             TYPE t052u-text1,
        ebeln             TYPE ekko-ebeln,
        ebelp             TYPE ekpo-ebelp,
        matnr             TYPE ekpo-matnr,
*        maktx             TYPE makt-maktx,
        maktx             TYPE char250,"mara-zz1_uzuntanim_prd,
        matkl             TYPE ekpo-matkl,
        menge             TYPE ekpo-menge,
        meins             TYPE ekpo-meins,
        netpr             TYPE ekpo-netpr,
        waers             TYPE ekko-waers,
        werks             TYPE t001w-werks,
        name1_werks       TYPE t001w-name1,
        lgort             TYPE t001l-lgort,
        lgobe             TYPE t001l-lgobe,
        netwr             TYPE ekpo-netwr,
        frgke             TYPE ekko-frgke,
        zterm             TYPE ekko-zterm,
        lv_totc           TYPE char20,
        lv_menge          TYPE char20,
        lv_netpr          TYPE char20,
        lv_nwerks         TYPE char40,
        lv_llgort         TYPE char30,
        txz01             TYPE ekpo-txz01,
        lv_tot            TYPE i,
        lv_total          TYPE i,
        lv_totalc         TYPE char20,
        eindt             TYPE eindt,
        datumx            TYPE sy-datum,
        ernam_pr          TYPE eban-ernam,
        banfn             TYPE eban-banfn,
        zz1_uzuntanim_prd TYPE char250,"mara-zz1_uzuntanim_prd,
      END OF gty_app.
