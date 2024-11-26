*&---------------------------------------------------------------------*
*& Include          ZMM_041_PURCHASE_REPORT_CLS
*&---------------------------------------------------------------------*

CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS:
      get_data,
      modify_data,
      fullname,
      color_data,
      color_data_cell,
      on_value_req_for_pr_ernam,
      on_value_req_for_po_ernam,
      handle_hotspot_click                          "HOTSPOT_CLICK
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
          e_row_id
          e_column_id,

      handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid              "TOOLBAR
        IMPORTING
          e_object
          e_interactive,

      handle_top_of_page                            "TOP_OF_PAGE
        FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING
          e_dyndoc_id
          table_index,

      call_screen,
      pbo_0100,
      pai_0100,
      set_fcat,
      set_layout,
      display_alv.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD get_data.

    REFRESH:gt_alv,gt_sas_onay,gt_sat_onay,gt_frgco,gt_sstell.

    SELECT  DISTINCT eban~banfn,
                     eban~bnfpo,
**         coalesce( eban~banfn, @space ) AS banfn,
**         coalesce( eban~bnfpo, @space ) AS bnfpo,
                     eban~badat,
                     eban~bsart,
                     eban~ekgrp,
                     CAST( 0 AS CURR( 17,2 ) ) AS sat_toplam_deger,
                     eban~preis,
                     eban~peinh,
**                     ( eban~preis / eban~peinh * eban~menge  ) AS sat_toplam_deger,
                     eban~waers AS sat_para_birimi,
***                     SAT_KALEM_METNI -> read_text
                     eban~banpr AS sat_durum,
                     eban~ernam AS sat_ernam,
                     eban~lfdat AS sat_teslimat_tarih,
***                     sat_onayci    -> zhr
**                     eban~frgdt,
                     CAST( '00000000' AS DATS ) AS frgdt,
                     eban~matnr,
                     CASE WHEN eban~matnr IS INITIAL THEN eban~txz01
                          ELSE makt~maktx
                     END AS maktx,
*                     mara~zz1_uzuntanim_prd,
                     eban~meins,
                     eban~menge,
                     ( eban~menge - eban~bsmng ) AS sat_acik_miktar,
                     ekko~bsart                  AS sas_bsart,
                     ekko~ebeln,
                     ekpo~ebelp,
                     ekko~lifnr,
                     lfa1~name1,
                     ekko~procstat AS sas_durum,
                     ekko~ernam    AS sas_ernam,
                     ekko~ekorg,
                     ekko~aedat,
                     eket~eindt,
                     ekpo~menge AS sas_menge,
                     ekpo~meins AS sas_meins,
                     ekpo~netpr,
                     ekpo~peinh AS fiyat_birimi,
                     ekpo~netwr,
                     ekko~waers,
                     ekko~zterm,
                     t052u~text1 AS zterm_desc,
                     ( ekpo~menge - ekbe~menge ) AS sas_acik_miktar,
                     ekpo~kostl,
                     cskt~ltext,
                     ekkn~prctr,
                     cepct~ltext AS prctr_ltext,
                     ekpo~werks,
                     t001w~name1 AS werks_name1,
                     ekpo~lgort,
                     t001l~lgobe,
                     ekpo~knttp,
                     ekkn~anln1,
                     ekkn~ps_psp_pnr,
                     ekkn~sakto,
                     ekpo~konnr,
                     ekpo~anfnr,
                     ekpo~inco1,
                     tinct~bezei,
                     ekpo~inco2_l,
                     ekpo~inco2,
                     ekpo~inco3_l,
                     mseg~mblnr,
                     mkpf~cpudt,
                     mseg~menge AS mseg_menge,
                     mkpf~usnam,
                     mseg~bwart,
                     t156ht~btext,
                     mseg~ebeln AS mseg_ebeln,
                     CAST( ' ' AS CHAR( 10 ) )  AS belnr, "rbkp~belnr,
                     CAST( '00000000' AS DATS ) AS bldat, "rbkp~bldat,
                     CAST( ' ' AS CHAR( 12 ) )  AS rbkp_usnam,"rbkp~usnam AS rbkp_usnam,
                     CAST( ' ' AS CHAR( 12 ) )  AS bseg_anln1"bseg~anln1 AS bseg_anln1
    FROM eban
    LEFT  JOIN ekpo  ON eban~banfn   = ekpo~banfn
                    AND eban~bnfpo   = ekpo~bnfpo
    LEFT  JOIN ekko  ON ekpo~ebeln   = ekko~ebeln
    LEFT  JOIN mara  ON eban~matnr   = mara~matnr
    LEFT  JOIN makt  ON mara~matnr   = makt~matnr
                    AND makt~spras   = @sy-langu
    LEFT JOIN lfa1   ON ekko~lifnr   = lfa1~lifnr
    LEFT JOIN eket   ON ekpo~ebeln   = eket~ebeln
                    AND ekpo~ebelp   = eket~ebelp
    LEFT JOIN t052u  ON ekko~zterm   = t052u~zterm
                    AND t052u~spras  = @sy-langu
    LEFT JOIN ekbe   ON ekpo~ebeln   = ekbe~ebeln
                    AND ekpo~ebelp   = ekbe~ebelp
                    AND ekbe~vgabe   = '1'
    LEFT JOIN cskt   ON ekpo~kostl   = cskt~kostl
                    AND cskt~spras   = @sy-langu
    LEFT JOIN t001w  ON ekpo~werks   = t001w~werks
                    AND t001w~spras  = @sy-langu
    LEFT JOIN t001l  ON ekpo~werks   = t001l~werks
                    AND ekpo~lgort   = t001l~lgort
    LEFT JOIN mseg   ON ekpo~ebeln   = mseg~ebeln
                    AND ekpo~ebelp   = mseg~ebelp
    LEFT JOIN mkpf   ON mseg~mblnr   = mkpf~mblnr
    LEFT JOIN t156ht ON mseg~bwart   = t156ht~bwart
                    AND t156ht~spras = @sy-langu
    LEFT JOIN ekkn   ON ekpo~ebeln   = ekkn~ebeln
                    AND ekpo~ebelp   = ekkn~ebelp
    LEFT JOIN cepct  ON ekkn~prctr   = cepct~prctr
                    AND cepct~spras  = @sy-langu
    LEFT JOIN tinct  ON ekpo~inco1   = tinct~inco1
                    AND tinct~spras  = @sy-langu
***    LEFT JOIN rbkp   on ( ekbe~belnr = rbkp~belnr AND ekbe~vgabe = '2' )
***                    AND rbkp~gjahr   = mkpf~mjahr
***    LEFT JOIN bseg   ON bseg~ebeln   = ekpo~ebeln
***                    AND bseg~ebelp   = ekpo~ebelp
***                    AND bseg~bschl   = '70'
    WHERE  eban~matnr IN @so_matnr
    AND    eban~matkl IN @so_matkl
    AND    eban~werks IN @so_werks
    AND    eban~banfn IN @so_banfn
    AND    eban~ekgrp IN @so_ekgrp
    AND    eban~ernam IN @so_ernam
    AND    eban~badat IN @so_badat
    AND    eban~bsart IN @so_bsart
    AND    ekpo~ebeln IN @so_ebeln
    AND    ekko~bsart IN @so_sasbt
    AND    ekko~ernam IN @so_sasby
    AND    ekko~aedat IN @so_aedat
    AND    ekpo~pstyp IN @so_pstyp
    AND    ekpo~knttp IN @so_knttp
    AND    eban~dispo IN @so_dispo
    AND    ekko~lifnr IN @so_lifnr

    UNION
          SELECT  DISTINCT eban~banfn,"@space AS banfn,
                           eban~bnfpo, "CAST( '00000' AS NUMC( 5 ) ) AS bnfpo,
                           eban~badat, "CAST( '00000000' AS DATS )   AS badat,
                           eban~bsart,
                           eban~ekgrp,
                           CAST( 0 AS CURR( 17,2 ) ) AS sat_toplam_deger,
                           eban~preis,
                           eban~peinh,
                           eban~waers AS sat_para_birimi,
                           eban~banpr AS sat_durum,
                           eban~ernam AS sat_ernam,
                           eban~lfdat AS sat_teslimat_tarih,
                           CAST( '00000000' AS DATS ) AS frgdt,
                           ekpo~matnr,
                           CASE WHEN ekpo~matnr IS INITIAL THEN ekpo~txz01
                                ELSE makt~maktx
                            END AS maktx,
*                           mara~zz1_uzuntanim_prd,
                           CAST( ' ' AS UNIT( 3 ) )   AS meins,
                           CAST( 0 AS QUAN( 13, 3 ) ) AS menge,
                           CAST( 0 AS QUAN( 13, 3 ) ) AS sat_acik_miktar,
                           ekko~bsart                 AS sas_bsart,
                           ekko~ebeln,
                           ekpo~ebelp,
                           ekko~lifnr,
                           lfa1~name1,
                           ekko~procstat AS sas_durum,
                           ekko~ernam    AS sas_ernam,
                           ekko~ekorg,
                           ekko~aedat,
                           eket~eindt,
                           ekpo~menge AS sas_menge,
                           ekpo~meins AS sas_meins,
                           ekpo~netpr,
                           ekpo~peinh AS fiyat_birimi,
                           ekpo~netwr,
                           ekko~waers,
                           ekko~zterm,
                           t052u~text1 AS zterm_desc,
                           ( ekpo~menge - ekbe~menge ) AS sas_acik_miktar,
                           ekpo~kostl,
                           cskt~ltext,
                           ekkn~prctr,
                           cepct~ltext AS prctr_ltext,
                           ekpo~werks,
                           t001w~name1 AS werks_name1,
                           ekpo~lgort,
                           t001l~lgobe,
                           ekpo~knttp,
                           ekkn~anln1,
                           ekkn~ps_psp_pnr,
                           ekkn~sakto,
                           ekpo~konnr,
                           ekpo~anfnr,
                           ekpo~inco1,
                           tinct~bezei,
                           ekpo~inco2_l,
                           ekpo~inco2,
                           ekpo~inco3_l,
                           mseg~mblnr,
                           mkpf~cpudt,
                           mseg~menge AS mseg_menge,
                           mkpf~usnam,
                           mseg~bwart,
                           t156ht~btext,
                           mseg~ebeln AS mseg_ebeln,
                           CAST( ' ' AS CHAR( 10 ) )  AS belnr, "rbkp~belnr,
                           CAST( '00000000' AS DATS ) AS bldat, "rbkp~bldat,
                           CAST( ' ' AS CHAR( 12 ) )  AS rbkp_usnam,"rbkp~usnam AS rbkp_usnam,
                           CAST( ' ' AS CHAR( 12 ) )  AS bseg_anln1"bseg~anln1 AS bseg_anln1
      FROM ekpo
      LEFT JOIN eban   ON ekpo~banfn   = eban~banfn
                      AND ekpo~bnfpo   = eban~bnfpo
      LEFT  JOIN ekko  ON ekpo~ebeln   = ekko~ebeln
      LEFT  JOIN mara  ON ekpo~matnr   = mara~matnr
      LEFT  JOIN makt  ON mara~matnr   = makt~matnr
                      AND makt~spras   = @sy-langu
      LEFT JOIN lfa1   ON ekko~lifnr   = lfa1~lifnr
      LEFT JOIN eket   ON ekpo~ebeln   = eket~ebeln
                      AND ekpo~ebelp   = eket~ebelp
      LEFT JOIN t052u  ON ekko~zterm   = t052u~zterm
                      AND t052u~spras  = @sy-langu
      LEFT JOIN ekbe   ON ekpo~ebeln   = ekbe~ebeln
                      AND ekpo~ebelp   = ekbe~ebelp
                      AND ekbe~vgabe   = '1'
      LEFT JOIN cskt   ON ekpo~kostl   = cskt~kostl
                      AND cskt~spras   = @sy-langu
      LEFT JOIN t001w  ON ekpo~werks   = t001w~werks
                      AND t001w~spras  = @sy-langu
      LEFT JOIN t001l  ON ekpo~werks   = t001l~werks
                      AND ekpo~lgort   = t001l~lgort
      LEFT JOIN mseg   ON ekpo~ebeln   = mseg~ebeln
                      AND ekpo~ebelp   = mseg~ebelp
      LEFT JOIN mkpf   ON mseg~mblnr   = mkpf~mblnr
      LEFT JOIN t156ht ON mseg~bwart   = t156ht~bwart
                      AND t156ht~spras = @sy-langu
      LEFT JOIN ekkn   ON ekpo~ebeln   = ekkn~ebeln
                      AND ekpo~ebelp   = ekkn~ebelp
      LEFT JOIN cepct  ON ekkn~prctr   = cepct~prctr
                      AND cepct~spras  = @sy-langu
      LEFT JOIN tinct  ON ekpo~inco1   = tinct~inco1
                      AND tinct~spras  = @sy-langu
***      LEFT JOIN rbkp   ON ( ekbe~belnr   = rbkp~belnr AND ekbe~vgabe = '2' )
***                      AND rbkp~gjahr   = mkpf~mjahr
***      LEFT JOIN bseg   ON bseg~ebeln   = ekpo~ebeln
***                      AND bseg~ebelp   = ekpo~ebelp
***                      AND bseg~bschl   = '70'
        WHERE  ekpo~matnr IN @so_matnr
        AND    eban~matkl IN @so_matkl
        AND    eban~werks IN @so_werks
        AND    eban~banfn IN @so_banfn
        AND    eban~ekgrp IN @so_ekgrp
        AND    eban~ernam IN @so_ernam
        AND    eban~badat IN @so_badat
        AND    eban~bsart IN @so_bsart
        AND    ekpo~ebeln IN @so_ebeln
        AND    ekko~bsart IN @so_sasbt
        AND    ekko~ernam IN @so_sasby
        AND    ekko~aedat IN @so_aedat
        AND    ekpo~pstyp IN @so_pstyp
        AND    ekpo~knttp IN @so_knttp
        AND    eban~dispo IN @so_dispo
        AND    ekko~lifnr IN @so_lifnr
      ORDER BY banfn,bnfpo,ebeln,ebelp
      INTO TABLE @DATA(lt_data).

***    BREAK xgamzey.

    DELETE ADJACENT DUPLICATES FROM lt_data COMPARING banfn bnfpo ebeln ebelp mblnr. "belnr.

    SELECT DISTINCT rbkp~belnr,
                    rbkp~bldat,
                    rbkp~usnam AS rbkp_usnam,
                    bseg~anln1 AS bseg_anln1,
                    lt~ebeln,
                    lt~ebelp
     FROM @lt_data AS lt
     INNER JOIN ekbe  ON lt~ebeln   = ekbe~ebeln
                     AND lt~ebelp   = ekbe~ebelp
                     AND ekbe~vgabe = '2'
                     AND ekbe~bewtp IN ( 'Q', 'R' )
     INNER JOIN rbkp  ON rbkp~belnr = ekbe~belnr
                     AND rbkp~gjahr = ekbe~gjahr
     LEFT JOIN bseg  ON bseg~ebeln = ekbe~ebeln
                     AND bseg~ebelp = ekbe~ebelp
                     AND bseg~bschl = '70'
     WHERE rbkp~belnr IN @so_belnr
     AND   rbkp~bldat IN @so_bldat
     ORDER BY lt~ebeln,lt~ebelp
     INTO TABLE @DATA(lt_fatura).

    IF so_belnr IS NOT INITIAL OR so_bldat IS NOT INITIAL.
      LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lfs_delete>).
        LOOP AT lt_fatura ASSIGNING FIELD-SYMBOL(<lfs_invoice>) WHERE ebeln = <lfs_delete>-ebeln
                                                                AND   ebelp = <lfs_delete>-ebelp.
          EXIT.
        ENDLOOP.
        IF sy-subrc IS NOT INITIAL.
          DELETE lt_data WHERE ebeln = <lfs_delete>-ebeln
                         AND   ebelp = <lfs_delete>-ebelp.
        ENDIF.
      ENDLOOP.
    ENDIF.


    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lfs_q>).
      READ TABLE lt_fatura ASSIGNING FIELD-SYMBOL(<lfs_fatura>) WITH KEY ebeln = <lfs_q>-ebeln
                                                                         ebelp = <lfs_q>-ebelp BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_q> = VALUE #( BASE <lfs_q>
                                belnr      = <lfs_fatura>-belnr
                                bldat      = <lfs_fatura>-bldat
                                rbkp_usnam = <lfs_fatura>-rbkp_usnam
                                bseg_anln1 = <lfs_fatura>-bseg_anln1 ) .
      ENDIF.
    ENDLOOP.

    SELECT DISTINCT cdpos~objectid,
                    cdpos~tabkey,
                    cdpos~fname,
                    cdpos~value_new,
                    cdpos~value_old,
                    substring( cdpos~tabkey ,14,5 ) AS item,  "14. den başla 5 tane al
                    cdpos~changenr
    FROM cdpos
    INNER JOIN @lt_data AS lt ON cdpos~objectid = lt~banfn
                             AND cdpos~tabkey   = concat( concat( '100' , lt~banfn ) , lt~bnfpo )
    WHERE fname     = 'BANPR'
    AND   value_new = '05'
    AND   value_old <> '05'
    ORDER BY cdpos~objectid, cdpos~tabkey
    INTO TABLE @DATA(lt_banpr).


    SELECT cdhdr~objectid,
           lt~item AS item,
           cdhdr~objectclas,
           cdhdr~username,
           cdhdr~udate
    FROM cdhdr
    INNER JOIN @lt_banpr AS lt ON cdhdr~objectid = lt~objectid
                              AND cdhdr~changenr = lt~changenr
    WHERE cdhdr~objectclas = 'BANF'
    ORDER BY cdhdr~objectid,lt~item
    INTO TABLE @DATA(lt_id).

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lfs_data>).
      READ TABLE lt_id ASSIGNING FIELD-SYMBOL(<lfs_id>) WITH KEY objectid = <lfs_data>-banfn
                                                                 item     = <lfs_data>-bnfpo BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_data>-frgdt = <lfs_id>-udate.
      ENDIF.
    ENDLOOP.

***    BREAK xgamzey.

    SELECT DISTINCT pa0105~pernr,
                    lt~sat_ernam
    FROM pa0105
    INNER JOIN @lt_data AS lt ON pa0105~usrid = lt~sat_ernam
    WHERE pa0105~subty = '0001'
      AND pa0105~begda <= @sy-datum
      AND pa0105~endda >= @sy-datum
    INTO TABLE @DATA(lt_pernr).

    DATA:it_pernr   TYPE hrpernr_struc_list.
    DATA:it_manager TYPE hrpad_t_manager.
    REFRESH:it_pernr,it_manager,gt_sat_onay.

    it_pernr = CORRESPONDING #( lt_pernr ).

    cl_hrpad_manager_util=>get_manager(
      EXPORTING
        it_pernr    = it_pernr
        iv_begda    = sy-datum
        iv_endda    = sy-datum
      IMPORTING
        et_manager  = it_manager ).

**    BREAK xgamzey.

    gt_sat_onay = CORRESPONDING #( it_manager ).
    SORT gt_sat_onay BY pernr.

    SELECT DISTINCT pa0105~pernr,
                    pa0105~usrid
    FROM @gt_sat_onay AS itab
    INNER JOIN pa0105  ON pa0105~pernr = itab~pernr OR pa0105~pernr = itab~manager
                      AND pa0105~subty = '0001'
                      AND pa0105~begda <= @sy-datum
                      AND pa0105~endda >= @sy-datum
    ORDER BY pa0105~pernr
    INTO TABLE @DATA(lt_0105).


    LOOP AT gt_sat_onay ASSIGNING FIELD-SYMBOL(<lfs_manager>).
      READ TABLE lt_0105 ASSIGNING FIELD-SYMBOL(<lfs_0105>) WITH KEY pernr = <lfs_manager>-pernr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_manager>-sat_uname = <lfs_0105>-usrid.
      ENDIF.

      READ TABLE lt_0105 ASSIGNING <lfs_0105> WITH KEY pernr = <lfs_manager>-manager BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_manager>-uname = <lfs_0105>-usrid.
      ENDIF.
    ENDLOOP.

***    BREAK xgamzey.

    gt_alv = CORRESPONDING #( lt_data ).


    SELECT DISTINCT cdpos~objectid,
****                    substring( cdpos~tabkey ,14,5 ) AS item,  "14. den başla 5 tane al
                    cdpos~changenr,
                    pos2~objectid AS id2,
                    pos2~changenr AS ch2,
                    pos3~objectid AS id3,
                    pos3~changenr AS ch3,
                    pos4~objectid AS id4,
                    pos4~changenr AS ch4,
                    pos5~objectid AS id5,
                    pos5~changenr AS ch5,
                    lt~ebeln
    FROM  @gt_alv AS lt
    LEFT JOIN cdpos  ON cdpos~objectid  = lt~ebeln
****                             AND cdpos~tabkey   = concat( concat( '100' , lt~banfn ) , lt~bnfpo )
                    AND cdpos~tabkey    = concat( '100' , lt~ebeln )
                    AND cdpos~fname     = 'FRGZU'
                    AND cdpos~value_new = 'X'
    LEFT JOIN cdpos AS pos2 ON pos2~objectid  = lt~ebeln
                           AND pos2~tabkey    = concat( '100' , lt~ebeln )
                           AND pos2~fname     = 'FRGZU'
                           AND pos2~value_new = 'XX'
    LEFT JOIN cdpos AS pos3 ON pos3~objectid  = lt~ebeln
                           AND pos3~tabkey    = concat( '100' , lt~ebeln )
                           AND pos3~fname     = 'FRGZU'
                           AND pos3~value_new = 'XXX'
    LEFT JOIN cdpos AS pos4 ON pos4~objectid  = lt~ebeln
                           AND pos4~tabkey    = concat( '100' , lt~ebeln )
                           AND pos4~fname     = 'FRGZU'
                           AND pos4~value_new = 'XXXX'
    LEFT JOIN cdpos AS pos5 ON pos5~objectid  = lt~ebeln
                           AND pos5~tabkey    = concat( '100' , lt~ebeln )
                           AND pos5~fname     = 'FRGZU'
                           AND pos5~value_new = 'XXXXX'
    ORDER BY cdpos~objectid
    INTO TABLE @DATA(lt_frgzu).

***    BREAK xgamzey.

    SELECT lt~ebeln,
           lt~objectid,
           lt~changenr,
           cdhdr~udate,
*           cdhdr~username,
           lt~ch2,
           dr2~udate    AS dat2,
*           dr2~username AS usr2,
           lt~ch3,
           dr3~udate    AS dat3,
*           dr3~username AS usr3,
           lt~ch4,
           dr4~udate    AS dat4,
*           dr4~username AS usr4,
           lt~ch5,
           dr5~udate    AS dat5   ",
*           dr4~username AS usr5
    FROM  @lt_frgzu AS lt
    LEFT JOIN cdhdr        ON cdhdr~objectid   = lt~objectid
                          AND cdhdr~changenr   = lt~changenr
                          AND cdhdr~objectclas = 'EINKBELEG'
    LEFT JOIN cdhdr AS dr2 ON dr2~objectid     = lt~id2
                          AND dr2~changenr     = lt~ch2
                          AND dr2~objectclas   = 'EINKBELEG'
    LEFT JOIN cdhdr AS dr3 ON dr3~objectid     = lt~id3
                          AND dr3~changenr     = lt~ch3
                          AND dr3~objectclas   = 'EINKBELEG'
    LEFT JOIN cdhdr AS dr4 ON dr4~objectid     = lt~id4
                          AND dr4~changenr     = lt~ch4
                          AND dr4~objectclas   = 'EINKBELEG'
    LEFT JOIN cdhdr AS dr5 ON dr5~objectid     = lt~id5
                          AND dr5~changenr     = lt~ch5
                          AND dr5~objectclas   = 'EINKBELEG'
    ORDER BY cdhdr~objectid
    INTO TABLE @DATA(lt_sas_onay).

****    BREAK xgamzey.


    SELECT DISTINCT t16fs~frggr,
                    t16fs~frgsx,
                    gt~ebeln,
                    gt~sas_ernam,
                    t16fs~frgc1,
                    t16fs~frgc2,
                    t16fs~frgc3,
                    t16fs~frgc4,
                    t16fs~frgc5,
                    t16fs~frgc6,
                    t16fs~frgc7,
                    t16fs~frgc8
     FROM t16fs
     INNER JOIN ekko          ON t16fs~frggr = ekko~frggr
                             AND t16fs~frgsx = ekko~frgsx
     INNER JOIN @gt_alv AS gt ON ekko~ebeln  = gt~ebeln
     INTO TABLE @DATA(lt_frgco).

    LOOP AT lt_frgco ASSIGNING FIELD-SYMBOL(<lfs_frgco>).

      APPEND VALUE #( ebeln =  <lfs_frgco>-ebeln
                      ernam =  <lfs_frgco>-sas_ernam
                      frgco =  <lfs_frgco>-frgc1 ) TO gt_sstell.
      APPEND VALUE #( ebeln =  <lfs_frgco>-ebeln
                      ernam =  <lfs_frgco>-sas_ernam
                      frgco =  <lfs_frgco>-frgc2 ) TO gt_sstell.
      APPEND VALUE #( ebeln =  <lfs_frgco>-ebeln
                      ernam =  <lfs_frgco>-sas_ernam
                      frgco =  <lfs_frgco>-frgc3 ) TO gt_sstell.
      APPEND VALUE #( ebeln =  <lfs_frgco>-ebeln
                      ernam =  <lfs_frgco>-sas_ernam
                      frgco =  <lfs_frgco>-frgc4 ) TO gt_sstell.
      APPEND VALUE #( ebeln =  <lfs_frgco>-ebeln
                      ernam =  <lfs_frgco>-sas_ernam
                      frgco =  <lfs_frgco>-frgc5 ) TO gt_sstell.
      APPEND VALUE #( ebeln =  <lfs_frgco>-ebeln
                      ernam =  <lfs_frgco>-sas_ernam
                      frgco =  <lfs_frgco>-frgc6 ) TO gt_sstell.
      APPEND VALUE #( ebeln =  <lfs_frgco>-ebeln
                      ernam =  <lfs_frgco>-sas_ernam
                      frgco =  <lfs_frgco>-frgc7 ) TO gt_sstell.
      APPEND VALUE #( ebeln =  <lfs_frgco>-ebeln
                      ernam =  <lfs_frgco>-sas_ernam
                      frgco =  <lfs_frgco>-frgc8 ) TO gt_sstell.

      APPEND VALUE #( ebeln =  <lfs_frgco>-ebeln
                      ernam =  <lfs_frgco>-sas_ernam
                      frgco =  <lfs_frgco>-frgc1 ) TO gt_frgco.
      APPEND VALUE #( ebeln =  <lfs_frgco>-ebeln
                      ernam =  <lfs_frgco>-sas_ernam
                      frgco =  <lfs_frgco>-frgc2 ) TO gt_frgco.
      APPEND VALUE #( ebeln =  <lfs_frgco>-ebeln
                      ernam =  <lfs_frgco>-sas_ernam
                      frgco =  <lfs_frgco>-frgc3 ) TO gt_frgco.
      APPEND VALUE #( ebeln =  <lfs_frgco>-ebeln
                      ernam =  <lfs_frgco>-sas_ernam
                      frgco =  <lfs_frgco>-frgc4 ) TO gt_frgco.
      APPEND VALUE #( ebeln =  <lfs_frgco>-ebeln
                      ernam =  <lfs_frgco>-sas_ernam
                      frgco =  <lfs_frgco>-frgc5 ) TO gt_frgco.
      APPEND VALUE #( ebeln =  <lfs_frgco>-ebeln
                      ernam =  <lfs_frgco>-sas_ernam
                      frgco =  <lfs_frgco>-frgc6 ) TO gt_frgco.
      APPEND VALUE #( ebeln =  <lfs_frgco>-ebeln
                      ernam =  <lfs_frgco>-sas_ernam
                      frgco =  <lfs_frgco>-frgc7 ) TO gt_frgco.
      APPEND VALUE #( ebeln =  <lfs_frgco>-ebeln
                      ernam =  <lfs_frgco>-sas_ernam
                      frgco =  <lfs_frgco>-frgc8 ) TO gt_frgco.

    ENDLOOP.


**    DELETE gr_frgco[] WHERE low   IS INITIAL.
    DELETE gt_frgco   WHERE frgco IS INITIAL.
    DELETE gt_sstell  WHERE frgco IS INITIAL.

    LOOP AT gt_frgco ASSIGNING FIELD-SYMBOL(<lfs_fin>).
      <lfs_fin>-index = sy-tabix.
    ENDLOOP.

    LOOP AT gt_sstell ASSIGNING FIELD-SYMBOL(<lfs_stell_index>) GROUP BY ( ebeln = <lfs_stell_index>-ebeln )
                                                                ASSIGNING FIELD-SYMBOL(<lfs_stell_indexx>).
      DATA(lv_tabix) = sy-tabix.
      lv_tabix = 1.
      LOOP AT GROUP <lfs_stell_indexx> ASSIGNING FIELD-SYMBOL(<lfs_stell_indexxxx>) WHERE ebeln = <lfs_stell_indexx>-ebeln.
        <lfs_stell_indexxxx>-index = lv_tabix.
        lv_tabix += 1.
      ENDLOOP.
    ENDLOOP.

    SELECT DISTINCT gt~ebeln,
                    z~usnam,
                    z~frgco,
                    z~approver,
                    1 AS index,
                    gt~index AS sıra
    FROM zmm_000_t_approv AS z
    INNER JOIN @gt_frgco AS gt ON z~usnam = gt~ernam
                              AND z~frgco = gt~frgco
    ORDER BY gt~index
    INTO TABLE @DATA(lt_sas_approvers).

    LOOP AT lt_sas_approvers ASSIGNING FIELD-SYMBOL(<lfs_zz>) GROUP BY ( ebeln = <lfs_zz>-ebeln
                                                                         usnam = <lfs_zz>-usnam )
                                                             ASSIGNING FIELD-SYMBOL(<lfs_tt>).
      DATA(lv_index) = sy-tabix.
      lv_index = 1.
      LOOP AT GROUP <lfs_tt> ASSIGNING FIELD-SYMBOL(<lfs_gg>) WHERE ebeln = <lfs_tt>-ebeln
                                                                AND usnam = <lfs_tt>-usnam.
        <lfs_gg>-index = lv_index.
        lv_index += 1.
      ENDLOOP.
    ENDLOOP.

***    BREAK xgamzey.

    SELECT DISTINCT gt~ebeln,
                    zstell~frgco,
                    zstell~stell
    FROM zmm_000_t_stell AS zstell
    INNER JOIN @gt_frgco AS gt ON zstell~frgco = gt~frgco
    INTO TABLE @DATA(lt_stell_z).  "zli sas approvers boş ise stell e göre mapleme için

    SELECT DISTINCT pa0105~pernr,
                    gt~sas_ernam
    FROM pa0105
    INNER JOIN @gt_alv AS gt ON pa0105~usrid = gt~sas_ernam
    WHERE pa0105~subty = '0001'
      AND pa0105~begda <= @sy-datum
      AND pa0105~endda >= @sy-datum
    INTO TABLE @DATA(lt_pernr_sas).

    REFRESH:it_pernr,it_manager,gt_sas_onay.

    it_pernr = CORRESPONDING #( lt_pernr_sas ).

    DO 10 TIMES.

      cl_hrpad_manager_util=>get_manager(
        EXPORTING
          it_pernr    = it_pernr
          iv_begda    = sy-datum
          iv_endda    = sy-datum
        IMPORTING
          et_manager  = it_manager ).

**    BREAK xgamzey.

      gt_sas_onay = CORRESPONDING #( BASE ( gt_sas_onay ) it_manager ).

      REFRESH : it_pernr[] .

      READ TABLE it_manager INTO DATA(ls_manager) INDEX 1 .
      IF sy-subrc IS INITIAL.
        APPEND VALUE #( pernr = ls_manager-manager ) TO it_pernr .
      ENDIF.

      IF it_pernr[] IS INITIAL.
        EXIT.
      ENDIF.

      REFRESH:it_manager[].
      CLEAR:ls_manager.

    ENDDO.


    SORT gt_sas_onay BY pernr.

    SELECT DISTINCT pa0105~pernr,
                    pa0105~usrid
    FROM @gt_sas_onay AS itab
    INNER JOIN pa0105  ON pa0105~pernr = itab~pernr OR pa0105~pernr = itab~manager
                      AND pa0105~subty = '0001'
                      AND pa0105~begda <= @sy-datum
                      AND pa0105~endda >= @sy-datum
    ORDER BY pa0105~pernr
    INTO TABLE @DATA(lt_0105_sas).

    SELECT pa0001~pernr,
           pa0001~stell
    FROM pa0001
    INNER JOIN @gt_sas_onay AS itab ON itab~pernr = pa0001~pernr
                                   AND itab~plans = pa0001~plans
    WHERE pa0001~begda <= @sy-datum
    AND   pa0001~endda >= @sy-datum
    ORDER BY pa0001~pernr
    INTO TABLE @DATA(lt_stell).


    DATA: lv_sort_stell TYPE short_d,
          lv_stell      TYPE stext.
    CLEAR:lv_sort_stell,lv_stell.

    LOOP AT lt_stell ASSIGNING FIELD-SYMBOL(<lfs_stell>).
      CALL FUNCTION 'HRWPC_RFC_STELL_TEXT_GET'
        EXPORTING
          stell       = <lfs_stell>-stell
          begda       = sy-datum
          endda       = sy-datum
          langu       = sy-langu
        IMPORTING
          stell_text1 = lv_sort_stell
          stell_text2 = lv_stell.

      READ TABLE gt_sas_onay ASSIGNING FIELD-SYMBOL(<lfs_sass_onayy>) WITH KEY pernr = <lfs_stell>-pernr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_sass_onayy>-stell = <lfs_stell>-stell.
        <lfs_sass_onayy>-stext = lv_stell.
      ENDIF.
    ENDLOOP.


    LOOP AT gt_sas_onay ASSIGNING FIELD-SYMBOL(<lfs_manager_sas>).
      READ TABLE lt_0105_sas ASSIGNING FIELD-SYMBOL(<lfs_0105_sas>) WITH KEY pernr = <lfs_manager_sas>-pernr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_manager_sas>-sas_uname = <lfs_0105_sas>-usrid.
      ENDIF.

      READ TABLE lt_0105_sas ASSIGNING <lfs_0105_sas> WITH KEY pernr = <lfs_manager_sas>-manager BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_manager_sas>-uname = <lfs_0105_sas>-usrid.
      ENDIF.
    ENDLOOP.


**    BREAK xgamzey.

    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv>).

      LOOP AT gt_sat_onay ASSIGNING FIELD-SYMBOL(<lfs_sat_onayci>) WHERE sat_uname = <lfs_alv>-sat_ernam.
        EXIT.
      ENDLOOP.
      IF sy-subrc IS INITIAL.
        <lfs_alv>-sat_onayci = <lfs_sat_onayci>-uname.
      ENDIF.


      LOOP AT lt_sas_onay ASSIGNING FIELD-SYMBOL(<lfs_sas_onay>) WHERE ebeln = <lfs_alv>-ebeln.
        EXIT.
      ENDLOOP.
      IF sy-subrc IS INITIAL.
        <lfs_alv> = VALUE #( BASE <lfs_alv>
***                                  sas_onayci_1     = <lfs_sas_onay>-username
                                  sas_onayci_1_dat = <lfs_sas_onay>-udate
***                                  sas_onayci_2     = <lfs_sas_onay>-usr2
                                  sas_onayci_2_dat = <lfs_sas_onay>-dat2
***                                  sas_onayci_3     = <lfs_sas_onay>-usr3
                                  sas_onayci_3_dat = <lfs_sas_onay>-dat3
***                                  sas_onayci_4     = <lfs_sas_onay>-usr4
                                  sas_onayci_4_dat = <lfs_sas_onay>-dat4
***                                  sas_onayci_5     = <lfs_sas_onay>-usr5
                                  sas_onayci_5_dat = <lfs_sas_onay>-dat5
                                   ).
      ENDIF.

      IF lt_sas_approvers IS NOT INITIAL.
        LOOP AT lt_sas_approvers ASSIGNING FIELD-SYMBOL(<lfs_zzapp>) WHERE usnam = <lfs_alv>-sas_ernam AND ebeln = <lfs_alv>-ebeln.
          <lfs_alv> = VALUE #( BASE <lfs_alv>
                                    sas_onayci_1     = COND #( WHEN <lfs_zzapp>-index = 1 AND <lfs_alv>-sas_onayci_1 IS INITIAL THEN <lfs_zzapp>-approver
                                                               ELSE <lfs_alv>-sas_onayci_1 )
                                    sas_onayci_2     = COND #( WHEN <lfs_zzapp>-index = 2 AND <lfs_alv>-sas_onayci_2 IS INITIAL THEN <lfs_zzapp>-approver
                                                               ELSE <lfs_alv>-sas_onayci_2 )
                                    sas_onayci_3     = COND #( WHEN <lfs_zzapp>-index = 3 AND <lfs_alv>-sas_onayci_3 IS INITIAL THEN <lfs_zzapp>-approver
                                                               ELSE <lfs_alv>-sas_onayci_3 )
                                    sas_onayci_4     = COND #( WHEN <lfs_zzapp>-index = 4 AND <lfs_alv>-sas_onayci_4 IS INITIAL THEN <lfs_zzapp>-approver
                                                               ELSE <lfs_alv>-sas_onayci_4 )
                                    sas_onayci_5     = COND #( WHEN <lfs_zzapp>-index = 5 AND <lfs_alv>-sas_onayci_5 IS INITIAL THEN <lfs_zzapp>-approver
                                                               ELSE <lfs_alv>-sas_onayci_5 )
                               ).
        ENDLOOP.

      ELSE.
        LOOP AT gt_sstell ASSIGNING FIELD-SYMBOL(<lfs_fr>) WHERE ebeln = <lfs_alv>-ebeln.

          CASE <lfs_fr>-frgco.
            WHEN '10'.
              SORT gt_sas_onay BY sas_uname.
              READ TABLE gt_sas_onay ASSIGNING FIELD-SYMBOL(<lfs_x>) WITH KEY  sas_uname = <lfs_fr>-ernam BINARY SEARCH.
              IF sy-subrc IS INITIAL.

                CASE <lfs_fr>-index.
                  WHEN 1.
                    <lfs_alv>-sas_onayci_1 = <lfs_x>-uname.
                  WHEN 2.
                    <lfs_alv>-sas_onayci_2 = <lfs_x>-uname.
                  WHEN 3.
                    <lfs_alv>-sas_onayci_3 = <lfs_x>-uname.
                  WHEN 4.
                    <lfs_alv>-sas_onayci_4 = <lfs_x>-uname.
                  WHEN 5.
                    <lfs_alv>-sas_onayci_5 = <lfs_x>-uname.
                ENDCASE.
              ENDIF.
            WHEN '20'.

            WHEN '25'.

            WHEN OTHERS.
              LOOP AT lt_stell_z ASSIGNING FIELD-SYMBOL(<lfs_stell_z>) WHERE frgco = <lfs_fr>-frgco.
                LOOP AT gt_sas_onay ASSIGNING FIELD-SYMBOL(<lfs_sas_stell>) WHERE stell = <lfs_stell_z>-stell.
                  CASE <lfs_stell_z>-frgco.
                    WHEN '30'.
                      CASE <lfs_fr>-index.
                        WHEN 1.
                          <lfs_alv>-sas_onayci_1 = <lfs_sas_stell>-uname.
                        WHEN 2.
                          <lfs_alv>-sas_onayci_2 = <lfs_sas_stell>-uname.
                        WHEN 3.
                          <lfs_alv>-sas_onayci_3 = <lfs_sas_stell>-uname.
                        WHEN 4.
                          <lfs_alv>-sas_onayci_4 = <lfs_sas_stell>-uname.
                        WHEN 5.
                          <lfs_alv>-sas_onayci_5 = <lfs_sas_stell>-uname.
                      ENDCASE.
                    WHEN '40'.
                      CASE <lfs_fr>-index.
                        WHEN 1.
                          <lfs_alv>-sas_onayci_1 = <lfs_sas_stell>-uname.
                        WHEN 2.
                          <lfs_alv>-sas_onayci_2 = <lfs_sas_stell>-uname.
                        WHEN 3.
                          <lfs_alv>-sas_onayci_3 = <lfs_sas_stell>-uname.
                        WHEN 4.
                          <lfs_alv>-sas_onayci_4 = <lfs_sas_stell>-uname.
                        WHEN 5.
                          <lfs_alv>-sas_onayci_5 = <lfs_sas_stell>-uname.
                      ENDCASE.
                    WHEN '50'.
                      CASE <lfs_fr>-index.
                        WHEN 1.
                          <lfs_alv>-sas_onayci_1 = <lfs_sas_stell>-uname.
                        WHEN 2.
                          <lfs_alv>-sas_onayci_2 = <lfs_sas_stell>-uname.
                        WHEN 3.
                          <lfs_alv>-sas_onayci_3 = <lfs_sas_stell>-uname.
                        WHEN 4.
                          <lfs_alv>-sas_onayci_4 = <lfs_sas_stell>-uname.
                        WHEN 5.
                          <lfs_alv>-sas_onayci_5 = <lfs_sas_stell>-uname.
                      ENDCASE.
                    WHEN '60'.
                      CASE <lfs_fr>-index.
                        WHEN 1.
                          <lfs_alv>-sas_onayci_1 = <lfs_sas_stell>-uname.
                        WHEN 2.
                          <lfs_alv>-sas_onayci_2 = <lfs_sas_stell>-uname.
                        WHEN 3.
                          <lfs_alv>-sas_onayci_3 = <lfs_sas_stell>-uname.
                        WHEN 4.
                          <lfs_alv>-sas_onayci_4 = <lfs_sas_stell>-uname.
                        WHEN 5.
                          <lfs_alv>-sas_onayci_5 = <lfs_sas_stell>-uname.
                      ENDCASE.
                  ENDCASE.
                ENDLOOP.
              ENDLOOP.
          ENDCASE.

        ENDLOOP.



      ENDIF.
    ENDLOOP.


  ENDMETHOD.

  METHOD modify_data.

    DATA:lt_tline TYPE TABLE OF tline.

    DATA(lt_domain) = VALUE dd07v_tab( ).

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'BANPR'
        text           = 'X'
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_domain
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    DATA(lt_domain_sas) = VALUE dd07v_tab( ).

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'MEPROCSTATE'
        text           = 'X'
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_domain_sas
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.


    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_table>).

      <lfs_table>-line_color = 'C200'.

      CLEAR:gs_cell_color.
      gs_cell_color = VALUE #( BASE gs_cell_color
                                    fname     = 'BANFN'
                                    color-col = '4'
                                    color-int = '1'
                                    color-inv = '0').
      APPEND gs_cell_color TO <lfs_table>-cell_color.

      CLEAR:gs_cell_color.
      gs_cell_color = VALUE #( BASE gs_cell_color
                                    fname     = 'EBELN'
                                    color-col = '4'
                                    color-int = '1'
                                    color-inv = '0').
      APPEND gs_cell_color TO <lfs_table>-cell_color.

      CLEAR:gs_cell_color.
      gs_cell_color = VALUE #( BASE gs_cell_color
                                    fname     = 'MBLNR'
                                    color-col = '4'
                                    color-int = '1'
                                    color-inv = '0').
      APPEND gs_cell_color TO <lfs_table>-cell_color.

      CLEAR:gs_cell_color.
      gs_cell_color = VALUE #( BASE gs_cell_color
                                    fname     = 'BELNR'
                                    color-col = '4'
                                    color-int = '1'
                                    color-inv = '0').
      APPEND gs_cell_color TO <lfs_table>-cell_color.


      <lfs_table>-sat_toplam_deger = ( ( <lfs_table>-preis / <lfs_table>-peinh ) * <lfs_table>-menge ).

      DATA:lv_days TYPE i.
      CLEAR:lv_days.

      CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
        EXPORTING
          i_datum_bis             = <lfs_table>-cpudt
          i_datum_von             = <lfs_table>-eindt
        IMPORTING
          e_tage                  = lv_days
        EXCEPTIONS
          days_method_not_defined = 1
          OTHERS                  = 2.


***      <lfs_table>-gecikme = COND #( WHEN lv_days > 0 THEN lv_days
***                                    ELSE 0 ).

      <lfs_table>-gecikme = lv_days.

      REFRESH:lt_tline.

      IF <lfs_table>-banfn IS NOT INITIAL.

        DATA(lv_name) = |{ <lfs_table>-banfn }{ <lfs_table>-bnfpo }|.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            client                  = sy-mandt
            id                      = 'B01'
            language                = sy-langu
            name                    = CONV char70( lv_name )
            object                  = 'EBAN'
          TABLES
            lines                   = lt_tline
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.

        LOOP AT lt_tline ASSIGNING FIELD-SYMBOL(<lfs_td_lines>).
*****     lv_name+10(5)
          <lfs_table>-sat_kalem_metni &&= <lfs_td_lines>-tdline.
        ENDLOOP.

      ENDIF.


      READ TABLE lt_domain ASSIGNING FIELD-SYMBOL(<lfs_domain>) WITH KEY domvalue_l = <lfs_table>-sat_durum BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_table>-sat_durum = <lfs_domain>-ddtext.
      ENDIF.


      REFRESH:lt_tline.
      CLEAR:lv_name.

      IF <lfs_table>-ebeln IS NOT INITIAL.

        lv_name = |{ <lfs_table>-ebeln }{ <lfs_table>-ebelp }|.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            client                  = sy-mandt
            id                      = 'F01'
            language                = sy-langu
            name                    = CONV char70( lv_name )
            object                  = 'EKPO'
          TABLES
            lines                   = lt_tline
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.

        LOOP AT lt_tline ASSIGNING <lfs_td_lines>.
          <lfs_table>-sas_kalem_metni &&= <lfs_td_lines>-tdline.
        ENDLOOP.

      ENDIF.


      READ TABLE lt_domain_sas ASSIGNING FIELD-SYMBOL(<lfs_domain_sas>) WITH KEY domvalue_l = <lfs_table>-sas_durum BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_table>-sas_durum = <lfs_domain_sas>-ddtext.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD fullname.

    SELECT DISTINCT usr21~bname,
                adrp~name_text AS sat_ernam,
                u2~bname       AS b2,
                a2~name_text   AS sat_onayci,
                u3~bname       AS b3,
                a3~name_text   AS sas_ernam,
                u4~bname       AS b4,
                a4~name_text   AS sas_onayci_1,
                u5~bname       AS b5,
                a5~name_text   AS sas_onayci_2,
                u6~bname       AS b6,
                a6~name_text   AS sas_onayci_3,
                u7~bname       AS b7,
                a7~name_text   AS sas_onayci_4,
                u8~bname       AS b8,
                a8~name_text   AS sas_onayci_5,
                u9~bname       AS b9,
                a9~name_text   AS usnam,
                u10~bname      AS b10,
                a10~name_text  AS rbkp_usnam
    FROM @gt_alv  AS lt
    LEFT JOIN usr21 ON lt~sat_ernam          = usr21~bname
    LEFT JOIN adrp  ON usr21~persnumber      = adrp~persnumber
    LEFT JOIN usr21 AS u2 ON lt~sat_onayci   = u2~bname
    LEFT JOIN adrp  AS a2 ON u2~persnumber   = a2~persnumber
    LEFT JOIN usr21 AS u3 ON lt~sas_ernam    = u3~bname
    LEFT JOIN adrp  AS a3 ON u3~persnumber   = a3~persnumber
    LEFT JOIN usr21 AS u4 ON lt~sas_onayci_1 = u4~bname
    LEFT JOIN adrp  AS a4 ON u4~persnumber   = a4~persnumber
    LEFT JOIN usr21 AS u5 ON lt~sas_onayci_2 = u5~bname
    LEFT JOIN adrp  AS a5 ON u5~persnumber   = a5~persnumber
    LEFT JOIN usr21 AS u6 ON lt~sas_onayci_3 = u6~bname
    LEFT JOIN adrp  AS a6 ON u6~persnumber   = a6~persnumber
    LEFT JOIN usr21 AS u7 ON lt~sas_onayci_4 = u7~bname
    LEFT JOIN adrp  AS a7 ON u7~persnumber   = a7~persnumber
    LEFT JOIN usr21 AS u8 ON lt~sas_onayci_5 = u8~bname
    LEFT JOIN adrp  AS a8 ON u8~persnumber   = a8~persnumber
    LEFT JOIN usr21 AS u9 ON lt~usnam        = u9~bname
    LEFT JOIN adrp  AS a9 ON u9~persnumber   = a9~persnumber
    LEFT JOIN usr21 AS u10 ON lt~rbkp_usnam  = u10~bname
    LEFT JOIN adrp  AS a10 ON u10~persnumber = a10~persnumber
*****    ORDER BY usr21~bname,u2~bname,u3~bname,u4~bname,u5~bname,
*****             u6~bname,u7~bname,u8~bname,u9~bname,u10~bname
    INTO TABLE @DATA(lt_users).


    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv>).

      SORT lt_users BY bname.
      READ TABLE lt_users ASSIGNING FIELD-SYMBOL(<lfs_users>) WITH KEY bname = <lfs_alv>-sat_ernam BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_alv>-sat_ernam = <lfs_users>-sat_ernam.
      ENDIF.

      SORT lt_users BY b2.
      READ TABLE lt_users ASSIGNING <lfs_users> WITH KEY b2 = <lfs_alv>-sat_onayci BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_alv>-sat_onayci = <lfs_users>-sat_onayci.
      ENDIF.

      SORT lt_users BY b3.
      READ TABLE lt_users ASSIGNING <lfs_users> WITH KEY b3 = <lfs_alv>-sas_ernam BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_alv>-sas_ernam = <lfs_users>-sas_ernam.
      ENDIF.

      SORT lt_users BY b4.
      READ TABLE lt_users ASSIGNING <lfs_users> WITH KEY b4 = <lfs_alv>-sas_onayci_1 BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_alv>-sas_onayci_1 = <lfs_users>-sas_onayci_1.
      ENDIF.

      SORT lt_users BY b5.
      READ TABLE lt_users ASSIGNING <lfs_users> WITH KEY b5 = <lfs_alv>-sas_onayci_2 BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_alv>-sas_onayci_2 = <lfs_users>-sas_onayci_2.
      ENDIF.

      SORT lt_users BY b6.
      READ TABLE lt_users ASSIGNING <lfs_users> WITH KEY b6 = <lfs_alv>-sas_onayci_3 BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_alv>-sas_onayci_3 = <lfs_users>-sas_onayci_3.
      ENDIF.

      SORT lt_users BY b7.
      READ TABLE lt_users ASSIGNING <lfs_users> WITH KEY b7 = <lfs_alv>-sas_onayci_4 BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_alv>-sas_onayci_4 = <lfs_users>-sas_onayci_4.
      ENDIF.

      SORT lt_users BY b8.
      READ TABLE lt_users ASSIGNING <lfs_users> WITH KEY b8 = <lfs_alv>-sas_onayci_5 BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_alv>-sas_onayci_5 = <lfs_users>-sas_onayci_5.
      ENDIF.

      SORT lt_users BY b9.
      READ TABLE lt_users ASSIGNING <lfs_users> WITH KEY b9 = <lfs_alv>-usnam BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_alv>-usnam = <lfs_users>-usnam.
      ENDIF.

      SORT lt_users BY b10.
      READ TABLE lt_users ASSIGNING <lfs_users> WITH KEY b10 = <lfs_alv>-rbkp_usnam BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_alv>-rbkp_usnam = <lfs_users>-rbkp_usnam.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD color_data.

    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_color>).
      IF <lfs_color>-sat_durum = 'Onay tamamlandı'.
        <lfs_color>-line_color = 'C110'.
      ENDIF.

      IF <lfs_color>-sas_durum = 'Onay tamamlandı'.
        <lfs_color>-line_color = 'C710'.
      ENDIF.

      IF <lfs_color>-mblnr IS NOT INITIAL.
        <lfs_color>-line_color = 'C300'.
      ENDIF.

      IF <lfs_color>-belnr IS NOT INITIAL.
        <lfs_color>-line_color = 'C600'.
      ENDIF.

****      IF <lfs_color>-zterm IS NOT INITIAL.
****        <lfs_color>-line_color = 'C510'.
****      ENDIF.
    ENDLOOP.

    me->color_data_cell( ).

  ENDMETHOD.

  METHOD color_data_cell.

    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_color>).
      IF <lfs_color>-sat_durum = 'Onay tamamlandı'.
        CLEAR:gs_cell_color.
        gs_cell_color = VALUE #( BASE gs_cell_color
                                      fname     = 'SAT_DURUM'
                                      color-col = '1'
                                      color-int = '1'
                                      color-inv = '0').
        APPEND gs_cell_color TO <lfs_color>-cell_color.
      ENDIF.

      IF <lfs_color>-sas_durum = 'Onay tamamlandı'.
        CLEAR:gs_cell_color.
        gs_cell_color = VALUE #( BASE gs_cell_color
                                      fname     = 'SAS_DURUM'
                                      color-col = '7'
                                      color-int = '1'
                                      color-inv = '0').
        APPEND gs_cell_color TO <lfs_color>-cell_color.
      ENDIF.

      IF <lfs_color>-mblnr IS NOT INITIAL.
        CLEAR:gs_cell_color.
        gs_cell_color = VALUE #( BASE gs_cell_color
                                      fname     = 'MBLNR'
                                      color-col = '3'
                                      color-int = '0'
                                      color-inv = '0').
        APPEND gs_cell_color TO <lfs_color>-cell_color.
      ENDIF.

      IF <lfs_color>-cpudt IS NOT INITIAL.
        CLEAR:gs_cell_color.
        gs_cell_color = VALUE #( BASE gs_cell_color
                                      fname     = 'CPUDT'
                                      color-col = '3'
                                      color-int = '0'
                                      color-inv = '0').
        APPEND gs_cell_color TO <lfs_color>-cell_color.
      ENDIF.

      IF <lfs_color>-belnr IS NOT INITIAL.
        CLEAR:gs_cell_color.
        gs_cell_color = VALUE #( BASE gs_cell_color
                                      fname     = 'BELNR'
                                      color-col = '6'
                                      color-int = '0'
                                      color-inv = '0').
        APPEND gs_cell_color TO <lfs_color>-cell_color.
      ENDIF.

      IF <lfs_color>-bldat IS NOT INITIAL.
        CLEAR:gs_cell_color.
        gs_cell_color = VALUE #( BASE gs_cell_color
                                      fname     = 'BLDAT'
                                      color-col = '6'
                                      color-int = '0'
                                      color-inv = '0').
        APPEND gs_cell_color TO <lfs_color>-cell_color.
      ENDIF.

****      IF <lfs_color>-zterm IS NOT INITIAL.
****        CLEAR:gs_cell_color.
****        gs_cell_color = VALUE #( BASE gs_cell_color
****                                      fname     = 'ZTERM'
****                                      color-col = '5'
****                                      color-int = '1'
****                                      color-inv = '0').
****        APPEND gs_cell_color TO <lfs_color>-cell_color.
****      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD on_value_req_for_pr_ernam.
    DATA:lt_return          TYPE STANDARD TABLE OF ddshretval,
         lt_dynpfld_mapping TYPE STANDARD TABLE OF dselc,
         dyn_wa             TYPE dselc.

    SELECT DISTINCT ernam,
                    banfn
    FROM eban
    INTO TABLE @DATA(lt_values)
    ORDER BY ernam, banfn.

    lt_dynpfld_mapping = VALUE #( ( fldname   = 'SO_ERNAM'
                                    dyfldname = 'SO_ERNAM-LOW' )
                                  ( fldname   = 'SO_BANFN'
                                    dyfldname = 'SO_BANFN-LOW' )
                                  ).

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield         = 'SO_ERNAM'
        dynpprog         = sy-cprog
        dynpnr           = sy-dynnr
        dynprofield      = 'SO_ERNAM'
        window_title     = 'SAT Yaratan F4'
        value_org        = 'S'
        callback_program = sy-cprog
        callback_form    = 'CALLBACK_F4'
      TABLES
        value_tab        = lt_values
        return_tab       = lt_return
        dynpfld_mapping  = lt_dynpfld_mapping.
  ENDMETHOD.

  METHOD on_value_req_for_po_ernam.
    DATA:lt_return          TYPE STANDARD TABLE OF ddshretval,
         lt_dynpfld_mapping TYPE STANDARD TABLE OF dselc,
         dyn_wa             TYPE dselc.

    SELECT DISTINCT ernam,
                    ebeln
    FROM ekko
    INTO TABLE @DATA(lt_values)
    ORDER BY ernam, ebeln.

    lt_dynpfld_mapping = VALUE #( ( fldname   = 'SO_SASBY'
                                    dyfldname = 'SO_SASBY-LOW' )
                                  ( fldname   = 'SO_EBELN'
                                    dyfldname = 'SO_EBELN-LOW' )
                                ).

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield         = 'SO_SASBY'
        dynpprog         = sy-cprog
        dynpnr           = sy-dynnr
        dynprofield      = 'SO_SASBY'
        window_title     = 'SAS Yaratan F4'
        value_org        = 'S'
        callback_program = sy-cprog
        callback_form    = 'CALLBACK_F4_SAS'
      TABLES
        value_tab        = lt_values
        return_tab       = lt_return
        dynpfld_mapping  = lt_dynpfld_mapping.
  ENDMETHOD.

  METHOD handle_hotspot_click.

    READ TABLE gt_alv INTO gs_alv INDEX e_row_id-index.
    IF sy-subrc IS INITIAL.
      CASE e_column_id-fieldname.
        WHEN 'BANFN'.
          SET PARAMETER ID : 'BAN' FIELD gs_alv-banfn.
          CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.
        WHEN 'EBELN'.
          SET PARAMETER ID : 'BES' FIELD gs_alv-ebeln.
          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
        WHEN 'MBLNR'.
          SET PARAMETER ID : 'MBN' FIELD gs_alv-mblnr.
          CALL TRANSACTION 'MIGO'  AND SKIP FIRST SCREEN.
        WHEN 'BELNR'.
          SET PARAMETER ID : 'RBN' FIELD gs_alv-belnr.
          CALL TRANSACTION 'MIR4'  AND SKIP FIRST SCREEN.
      ENDCASE.
    ENDIF.

  ENDMETHOD.

  METHOD handle_toolbar.

****    DATA:ls_toolbar TYPE stb_button.
****
****    CLEAR:ls_toolbar.
****    ls_toolbar = VALUE #( BASE ls_toolbar
****                               function   = ' '
****                               text       = 'SAT:Turkuaz'
****                               icon       = '@01@'
****                               quickinfo  = 'SAT Onay'
****                               disabled   = abap_false ).
****
****    APPEND ls_toolbar TO e_object->mt_toolbar.
****

  ENDMETHOD.

  METHOD handle_top_of_page.

    DATA:lv_text TYPE sdydo_text_element.

****    CLEAR:lv_text.
****    lv_text = |Hotspot|.
****    CALL METHOD go_document->add_text
****      EXPORTING
****        text         = lv_text
****        sap_color    = cl_dd_document=>list_key_int
****        sap_fontsize = cl_dd_document=>medium.
****
****    CALL METHOD go_document->add_gap.

    CLEAR:lv_text.
    lv_text = |SAT Onay|.
    CALL METHOD go_document->add_text
      EXPORTING
        text         = lv_text
        sap_color    = cl_dd_document=>list_heading_int
        sap_fontsize = cl_dd_document=>medium.

***    CALL METHOD go_document->new_line.
    CALL METHOD go_document->add_gap.

    CLEAR:lv_text.
    lv_text = |SAS Onay|.
    CALL METHOD go_document->add_text
      EXPORTING
        text         = lv_text
        sap_color    = cl_dd_document=>list_group_int
        sap_fontsize = cl_dd_document=>medium.

    CALL METHOD go_document->add_gap.

    CLEAR:lv_text.
    lv_text = |Mal Girişi|.
    CALL METHOD go_document->add_text
      EXPORTING
        text         = lv_text
        sap_color    = cl_dd_document=>list_total
        sap_fontsize = cl_dd_document=>medium.

    CALL METHOD go_document->add_gap.

    CLEAR:lv_text.
    lv_text = |Fatura Girişi|.
    CALL METHOD go_document->add_text
      EXPORTING
        text         = lv_text
        sap_color    = cl_dd_document=>list_negative
        sap_fontsize = cl_dd_document=>medium.

    CALL METHOD go_document->display_document
      EXPORTING
        parent = go_topofpage.

  ENDMETHOD.

  METHOD call_screen.
    CALL SCREEN 0100.
  ENDMETHOD.

  METHOD pbo_0100.
    SET PF-STATUS '0100'.
    SET TITLEBAR '0100'.
  ENDMETHOD.

  METHOD pai_0100.
    CASE sy-ucomm.
      WHEN '&BACK'.
        SET SCREEN 0.
    ENDCASE.
  ENDMETHOD.

  METHOD set_fcat.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'ZMM_041_S_REPORT'
      CHANGING
        ct_fieldcat            = gt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<lfs_fcat>).
      CASE <lfs_fcat>-fieldname.
        WHEN 'BANFN'.
          <lfs_fcat>-hotspot   = abap_true.
          <lfs_fcat>-emphasize = 'C410'.
        WHEN 'PREIS'.
          <lfs_fcat>-no_out  = abap_true.
        WHEN 'PEINH'.
          <lfs_fcat>-no_out  = abap_true.
        WHEN 'EBELN'.
          <lfs_fcat>-hotspot = abap_true.
          <lfs_fcat>-emphasize = 'C410'.
        WHEN 'MBLNR'.
          <lfs_fcat>-hotspot = abap_true.
          <lfs_fcat>-emphasize = 'C410'.
        WHEN 'BELNR'.
          <lfs_fcat>-hotspot = abap_true.
          <lfs_fcat>-emphasize = 'C410'.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD set_layout.
    CLEAR:gs_layout.
    gs_layout =  VALUE #( col_opt    = abap_true
                          cwidth_opt = abap_true
                          zebra      = abap_false "abap_true
                          ctab_fname = 'CELL_COLOR'
                          stylefname = 'CELLSTYLE'
                          info_fname = 'LINE_COLOR' ).
  ENDMETHOD.

  METHOD display_alv.
    IF go_alv_grid IS INITIAL.

***      CREATE OBJECT go_container
***        EXPORTING
***          container_name = 'CC_ALV'.

      CREATE OBJECT go_docking
        EXPORTING
          repid = sy-cprog
          dynnr = sy-dynnr
          ratio = 95
          side  = cl_gui_docking_container=>dock_at_bottom
          name  = 'DOCK_CONT'.

      TRY.
          go_container ?= go_docking.

          CREATE OBJECT go_splitter
            EXPORTING
              parent  = go_container
              rows    = 2
              columns = 1.

          CALL METHOD go_splitter->get_container
            EXPORTING
              row       = 1
              column    = 1
            RECEIVING
              container = go_topofpage.

          CALL METHOD go_splitter->set_row_height
            EXPORTING
              id     = 1                 " Row ID
              height = 5.                 " Height

          CALL METHOD go_splitter->get_container
            EXPORTING
              row       = 2
              column    = 1
            RECEIVING
              container = go_split1.

          CREATE OBJECT go_document
            EXPORTING
              style = 'ALV_GRID'.

          CREATE OBJECT go_alv_grid
            EXPORTING
              i_parent = go_split1.
**          i_parent = go_container.
**          i_parent = cl_gui_custom_container=>screen0.


          SET HANDLER me->handle_hotspot_click FOR go_alv_grid.
          SET HANDLER me->handle_toolbar       FOR go_alv_grid.
          SET HANDLER me->handle_top_of_page   FOR go_alv_grid.


          CALL METHOD go_alv_grid->list_processing_events
            EXPORTING
              i_event_name = 'TOP_OF_PAGE'
              i_dyndoc_id  = go_document.

          gs_variant = VALUE #( BASE gs_variant
                                    report  = sy-repid
                                    variant = p_var ).


          CALL METHOD go_alv_grid->set_table_for_first_display
            EXPORTING
              is_variant      = gs_variant
              i_save          = 'A'              " ' ' , 'X' , 'U'
              is_layout       = gs_layout             " Layout
            CHANGING
              it_outtab       = gt_alv                " Output Table
              it_fieldcatalog = gt_fcat.                " Field Catalog

        CATCH cx_salv_msg .
      ENDTRY.
    ELSE.
      CALL METHOD go_alv_grid->refresh_table_display( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
