*&---------------------------------------------------------------------*
*& Include          ZSD_002_FLOW_TOP
*&---------------------------------------------------------------------*
TABLES:vbak,vbap,lips,likp,vbrk,vbkd,vbfa,prcd_elements.
CLASS lcl_class DEFINITION DEFERRED.
DATA:go_local     TYPE REF TO lcl_class,
     go_alv_grid  TYPE REF TO cl_gui_alv_grid,
     go_container TYPE REF TO cl_gui_custom_container,
     gt_fcat      TYPE lvc_t_fcat,
     gs_fcat      TYPE lvc_s_fcat,
     gs_layout    TYPE lvc_s_layo.

TYPES:BEGIN OF gty_alv,
        ernam                TYPE vbak-ernam,
        kunnr                TYPE vbak-kunnr,
        name1                TYPE kna1-name1,
        name2                TYPE kna1-name2,
        erdat                TYPE vbak-erdat, """""  NEW """""
        vbeln                TYPE vbap-vbeln,
        posnr                TYPE vbap-posnr,
        vkorg                TYPE vbak-vkorg,
        vtweg                TYPE vbak-vtweg,
        auart                TYPE vbak-auart,
        bezei	               TYPE tvakt_bezei, """""  NEW """""
        zterm                TYPE vbkd-zterm,
        kdmat                TYPE vbap-kdmat,
        matnr                TYPE vbap-matnr,
        arktx                TYPE vbap-arktx,
        kwmeng               TYPE vbap-kwmeng,
        vrkme                TYPE vbap-vrkme,
        augru                TYPE zsd_002_de_siparis_nedeni, "vbak-augru,
        auart_x              TYPE zsd_002_de_siparis_turu, "vbak-auart, "sipariş türü
        netwr_x              TYPE zsd_002_de_kalem_net,    "vbap-netwr,
        waerk_x              TYPE zsd_002_de_kalem_para_birim, "vbap-waerk,
        mwsbp                TYPE vbap-mwsbp,
        netwr                TYPE zsd_002_de_satis_belge_tt, "vbak-netwr,
        vergi                TYPE zsd_002_de_vergi_toplam, " vbap-mwsbp + vbak-netwr
        waerk                TYPE vbak-waerk,
        kwert                TYPE zsd_002_de_navlun_degeri, " işlemli * prcd_elements-kwert,
        lgort                TYPE vbap-lgort,
        abgru                TYPE val_text, "vbap-abgru,
        gbstk_d              TYPE val_text, "vbak-gbstk,
        teslimat_erdat       TYPE lips-erdat, """""  NEW """""
        teslimat_belgesi     TYPE val_text, "vbfa-vbeln
        teslimat_miktari     TYPE zsd_002_de_teslimat_miktari, "lips-lfimg,
        olcu_birimi          TYPE lips-vrkme,
        teslimat_durumu      TYPE val_text,
        cekme_durumu         TYPE val_text,
        faturalama_belgesi   TYPE val_text,
        faturalama_durumu    TYPE val_text, "lips-fksta
        muhasebe_belgesi     TYPE val_text,   "mblnr çekilir ????
        lfgsa                TYPE vbap-lfgsa,
        wbsta                TYPE lips-wbsta,
        charg                TYPE lips-charg, "parti için
        matkl                TYPE lips-matkl, "Mal grubu
*        arktx               TYPE lips-arktx, "Müşteri sipariş kalemine ilişkin kısa metin
        faturalama_durumuxxx TYPE vbak-fksak,
        knumv                TYPE vbak-knumv,
        vbtyp_v              TYPE vbfa-vbtyp_v,
        vbtyp_n              TYPE vbfa-vbtyp_n,
        delete               TYPE char20,
        teslimat_kalemi      TYPE lips-posnr,
        vdatu                TYPE edatu_vbak,
      END OF gty_alv.

DATA:gt_alv TYPE TABLE OF gty_alv,
     gs_alv TYPE gty_alv,
     gt_sip TYPE TABLE OF gty_alv,
     gs_sip TYPE gty_alv,
     gt_tes TYPE TABLE OF gty_alv,
     gs_tes TYPE gty_alv,
     gt_fat TYPE TABLE OF gty_alv,
     gs_fat TYPE gty_alv.

TYPES:BEGIN OF gty_type,
        ernam                TYPE vbak-ernam,
        kunnr                TYPE vbak-kunnr,
        name1                TYPE kna1-name1,
        name2                TYPE kna1-name2,
        vbeln                TYPE vbap-vbeln,
        posnr                TYPE vbap-posnr,
        vkorg                TYPE vbak-vkorg,
        vtweg                TYPE vbak-vtweg,
        auart                TYPE vbak-auart,
        zterm                TYPE vbkd-zterm,
        kdmat                TYPE vbap-kdmat,
        matnr                TYPE vbap-matnr,
        arktx                TYPE vbap-arktx,
        kwmeng               TYPE vbap-kwmeng,
        vrkme                TYPE vbap-vrkme,
        augru                TYPE vbak-augru,
        auart_x              TYPE vbak-auart,
        netwr_x              TYPE vbap-netwr,
        waerk_x              TYPE vbap-waerk,
        mwsbp                TYPE vbap-mwsbp,
        netwr                TYPE vbak-netwr,
        waerk                TYPE vbak-waerk,
        kwert                TYPE prcd_elements-kwert,
        lgort                TYPE vbap-lgort,
        abgru                TYPE vbap-abgru,
        gbstk_d              TYPE vbak-gbstk,
        vbtyp_v              TYPE vbfa-vbtyp_v,
        vbtyp_n              TYPE vbfa-vbtyp_n,
        teslimat_belgesi     TYPE likp-vbeln,
        teslimat_durumu      TYPE likp-gbstk,
        cekme_durumu         TYPE likp-kostk,
        faturalama_belgesi   TYPE vbrp-vbeln,
        faturalama_durumu    TYPE lips-fksta,
        muhasebe_belgesi     TYPE vbrk-belnr,
        knumv                TYPE vbak-knumv,
        faturalama_durumuxxx TYPE vbak-fksak,
        lfgsa                TYPE vbap-lfgsa,
        teslimat_miktari     TYPE lips-lfimg,
        olcu_birimi          TYPE lips-vrkme,
        wbsta                TYPE lips-wbsta,
        charg                TYPE lips-charg, "parti için
        matkl                TYPE lips-matkl, "Mal grubu
*        arktx                TYPE lips-arktx, "Müşteri sipariş kalemine ilişkin kısa metin
        erdat                TYPE vbak-erdat,
        bezei                TYPE char20,
        teslimat_erdat       TYPE lips-erdat,
        teslimat_kalemi      TYPE lips-posnr,
        vdatu                TYPE edatu_vbak,
      END OF gty_type.
DATA: gs_all TYPE gty_type,
      gt_all TYPE TABLE OF gty_type.

DATA:gt_all_x TYPE TABLE OF gty_type.


DATA:lv_muh2 TYPE xfeld,
     lv_muh3 TYPE xfeld.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-000 .
  PARAMETERS:"p_open AS CHECKBOX USER-COMMAND cmd, "MODIF ID a "DEFAULT abap_true,
    p_done NO-DISPLAY. " AS CHECKBOX  USER-COMMAND cmd. "MODIF ID b. "//XISMAILB 12.06.2024 NO-DISPLAY
*  PARAMETERS:p_open RADIOBUTTON GROUP a,
*             p_done RADIOBUTTON GROUP a.
SELECTION-SCREEN END OF BLOCK a.

SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:so_kunnr  FOR vbak-kunnr,
                 so_audat  FOR vbak-audat,
                 so_sip    FOR vbap-vbeln,
                 so_posnr  FOR vbap-posnr,
                 so_vkorg  FOR vbak-vkorg,
                 so_vtweg  FOR vbak-vtweg,
                 so_abgru  FOR vbap-abgru,
                 so_augru  FOR vbak-augru,
                 so_auart  FOR vbak-auart,
                 so_matnr  FOR vbap-matnr,
                 so_werks  FOR lips-werks,
                 so_lgort  FOR lips-lgort,
                 so_malt   FOR likp-kunnr,
                 so_tes    FOR likp-vbeln,
                 so_fat    FOR vbrk-vbeln.
SELECTION-SCREEN END OF BLOCK b.

AT SELECTION-SCREEN OUTPUT.

*  CASE abap_true.
*    WHEN p_open.
*      p_done = abap_false.
*      MODIFY SCREEN.
*    WHEN p_done.
*      p_open = abap_false.
*      MODIFY SCREEN.
*  ENDCASE.
