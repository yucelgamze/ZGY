*&---------------------------------------------------------------------*
*& Include          ZSD_002_FLOW_LCL
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor,
      get_data,
      teslimat,
      fatura,
      done,
      call_screen,
      pbo_0100,
      pai_0100 IMPORTING iv_ucomm TYPE sy-ucomm,
      set_fcat,
      set_layout,
      display_alv.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.

  METHOD get_data.

    DATA(lt_domain) = VALUE dd07v_tab( ).

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'STATV'
        text           = 'X'
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_domain
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    SELECT
    tvakt~auart,
    tvakt~bezei
    FROM tvakt
    INNER JOIN vbak ON vbak~auart = tvakt~auart
    WHERE tvakt~spras = 'T'
    INTO TABLE @DATA(lt_tvakt).

    SELECT
    tvagt~abgru,
    tvagt~bezei
    FROM tvagt
    INNER JOIN vbap ON vbap~abgru = tvagt~abgru
    WHERE   tvagt~spras = 'T'
    INTO TABLE @DATA(lt_abgru_d).             "Satış belgeleri ret nedeni açıklama metni tablosu

    SELECT
    vbak~vbeln,
    vbak~knumv,
    prcd_elements~kposn,
    prcd_elements~kwert,
    prcd_elements~kschl
    FROM vbak
    INNER JOIN vbap ON vbak~vbeln EQ vbap~vbeln
    INNER JOIN prcd_elements ON  prcd_elements~kschl = 'ZL10' AND vbap~posnr EQ  prcd_elements~kposn
    WHERE  prcd_elements~knumv EQ vbak~knumv
    INTO TABLE @DATA(lt_naval).

    SELECT
   vbak~ernam,
   vbak~kunnr,
   kna1~name1,
   kna1~name2,
   vbap~vbeln,
   vbap~posnr,
   vbak~vkorg,
   vbak~vtweg,
   vbak~auart,
   vbkd~zterm,
   vbap~kdmat,
   vbap~matnr,
   vbap~arktx,
   vbap~kwmeng,
   vbap~vrkme,
   vbak~augru,
   vbak~auart AS auart_x,
   vbap~netwr AS netwr_x,
   vbap~waerk AS waerk_x,
   vbap~mwsbp,
   vbak~netwr,
   vbak~waerk,
   prcd_elements~kwert,
   vbap~lgort,
   vbap~abgru,
   vbak~gbstk AS gbstk_d,
   vbak~knumv,
   vbak~fksak  AS faturalama_durumuxxx,
   vbap~lfgsa,
   vbak~erdat,
   vbak~auart AS bezei,
   vbak~vdatu
    FROM vbak
    INNER JOIN vbap ON vbak~vbeln EQ vbap~vbeln
    INNER JOIN vbkd ON vbkd~vbeln EQ vbap~vbeln
    LEFT  JOIN kna1 ON kna1~kunnr EQ vbak~kunnr
    LEFT JOIN prcd_elements ON  prcd_elements~kschl = 'ZL10' AND prcd_elements~knumv EQ vbak~knumv "AND prcd_elements~kposn EQ vbap~posnr
    WHERE
        vbak~kunnr IN @so_kunnr
    AND vbak~audat IN @so_audat
    AND vbap~vbeln IN @so_sip
    AND vbak~vkorg IN @so_vkorg
    AND vbak~vtweg IN @so_vtweg
    AND vbap~abgru IN @so_abgru
    AND vbak~augru IN @so_augru
    AND vbak~auart IN @so_auart
    AND vbap~matnr IN @so_matnr
    AND vbap~werks IN @so_werks
    AND vbap~lgort IN @so_lgort
    AND vbap~posnr IN @so_posnr
    INTO CORRESPONDING FIELDS OF TABLE @gt_all.

    SORT gt_all BY vbeln posnr.
    DELETE ADJACENT DUPLICATES FROM gt_all COMPARING vbeln posnr.

    SELECT
   vbak~ernam,
   vbak~kunnr,
   kna1~name1,
   kna1~name2,
   vbap~vbeln,
   vbap~posnr,
   vbak~vkorg,
   vbak~vtweg,
   vbak~auart,
   vbkd~zterm,
   vbap~kdmat,
   vbap~matnr,
   vbap~arktx,
   vbap~kwmeng,
   vbap~vrkme,
   vbak~augru,
   vbak~auart AS auart_x,
   vbap~netwr AS netwr_x,
   vbap~waerk AS waerk_x,
   vbap~mwsbp,
   vbak~netwr,
   vbak~waerk,
   prcd_elements~kwert,
   vbap~lgort,
   vbap~abgru,            "metin gelmeli !!!
   vbak~gbstk AS gbstk_d,

vbfa~vbtyp_v,
vbfa~vbtyp_n,
    likp~vbeln   AS teslimat_belgesi,
    likp~gbstk   AS teslimat_durumu,      "Teslimat varsa "statv
    likp~kostk   AS cekme_durumu,         "Teslimat varsa "statv
    vbrp~vbeln   AS faturalama_belgesi,
    lips~fksta   AS faturalama_durumu,    "Fatura varsa   "statv
    vbrk~belnr   AS muhasebe_belgesi,      "Fatura varsa
     vbak~knumv,
     vbak~fksak  AS faturalama_durumuxxx,
vbap~lfgsa,
lips~lfimg AS teslimat_miktari,
lips~vrkme AS olcu_birimi,
lips~wbsta,
lips~charg,
lips~matkl,
   vbak~erdat,
   vbak~auart AS bezei,
   lips~erdat AS teslimat_erdat,
   vbak~auart AS delete,
   lips~posnr AS teslimat_kalemi,
   vbak~vdatu
   FROM vbak
   INNER JOIN vbap ON vbak~vbeln EQ vbap~vbeln
   INNER JOIN vbkd ON vbkd~vbeln EQ vbap~vbeln
   LEFT  JOIN kna1 ON kna1~kunnr EQ vbak~kunnr
   LEFT JOIN prcd_elements ON  prcd_elements~kschl = 'ZL10' AND prcd_elements~knumv EQ vbak~knumv "AND prcd_elements~kposn EQ vbap~posnr
   LEFT JOIN vbfa ON vbak~vbeln EQ vbfa~vbelv AND vbfa~posnv EQ vbap~posnr
   LEFT JOIN likp ON likp~vbeln EQ vbfa~vbeln
   LEFT JOIN lips ON lips~vbeln EQ likp~vbeln AND lips~posnr EQ vbfa~posnn "vbap~posnr
   LEFT JOIN vbrk ON vbrk~vbeln EQ vbfa~vbeln
   LEFT JOIN vbrp ON vbrp~vbeln EQ vbfa~vbeln AND vbrp~posnr EQ vbap~posnr
   WHERE
       vbak~kunnr IN @so_kunnr
   AND vbak~audat IN @so_audat
   AND vbap~vbeln IN @so_sip
   AND vbak~vkorg IN @so_vkorg
   AND vbak~vtweg IN @so_vtweg
   AND vbap~abgru IN @so_abgru
   AND vbak~augru IN @so_augru
   AND vbak~auart IN @so_auart
   AND vbap~matnr IN @so_matnr
   AND vbap~werks IN @so_werks
   AND vbap~lgort IN @so_lgort
   AND likp~kunnr IN @so_malt
   AND likp~vbeln IN @so_tes
   AND vbrk~vbeln IN @so_fat
   AND vbap~posnr IN @so_posnr
   AND vbfa~vbtyp_v IN ( 'C','J', 'M', 'K', 'L' , 'I' , 'H' )
   AND vbfa~vbtyp_n IN ( 'C','J', 'M', 'O', 'P' , 'T' )
   INTO TABLE @DATA(lt_data).

    SORT lt_data BY vbeln posnr.


    DATA(lt_fat) = lt_data.
    SORT lt_fat BY vbeln posnr faturalama_belgesi muhasebe_belgesi.
    DELETE ADJACENT DUPLICATES FROM lt_fat COMPARING vbeln posnr faturalama_belgesi muhasebe_belgesi.
    DELETE lt_fat WHERE faturalama_belgesi IS INITIAL.

    DATA(lt_test) = lt_data.
    SORT lt_test BY vbeln posnr teslimat_belgesi teslimat_kalemi.
    DELETE ADJACENT DUPLICATES FROM lt_test COMPARING vbeln posnr teslimat_belgesi teslimat_kalemi .
    DELETE lt_test WHERE faturalama_belgesi IS NOT INITIAL.

    LOOP AT lt_test ASSIGNING FIELD-SYMBOL(<lfs_erdat>).
      IF <lfs_erdat>-vbtyp_n = 'M'.
        <lfs_erdat>-delete = |Dele|.
      ENDIF.
      IF <lfs_erdat>-teslimat_belgesi IS NOT INITIAL.
        IF <lfs_erdat>-teslimat_erdat IS INITIAL.
          <lfs_erdat>-delete = |Dele|.
        ENDIF.
      ENDIF.
    ENDLOOP.
    DELETE lt_test WHERE delete = |Dele|.

    LOOP AT lt_fat ASSIGNING FIELD-SYMBOL(<ls_x>).
      LOOP AT lt_test ASSIGNING FIELD-SYMBOL(<ls_y>) WHERE vbeln = <ls_x>-vbeln AND posnr = <ls_x>-posnr.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.
        <ls_x>-faturalama_durumu = 'C'.
        DATA(ls_add) = <ls_x>.
        APPEND ls_add TO lt_test.
      ENDIF.
    ENDLOOP.
    SORT lt_test BY vbeln posnr.
    data : ls_test like line of lt_test .
    LOOP AT gt_all ASSIGNING FIELD-SYMBOL(<lfs_all>).
      LOOP AT lt_test ASSIGNING FIELD-SYMBOL(<lfs_z>) WHERE vbeln = <lfs_all>-vbeln AND posnr = <lfs_all>-posnr .
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.
        gs_all = <lfs_all>.
*        APPEND gs_all  TO lt_test.
        APPEND gs_all TO gt_all_x.
*        lt_test = CORRESPONDING #( gt_all_x ).
        ls_test = CORRESPONDING #( gs_all ).
        append ls_test to lt_test .
      ENDIF.
    ENDLOOP.
    SORT lt_test BY vbeln posnr.


    LOOP AT lt_test ASSIGNING FIELD-SYMBOL(<lfs_fd>).
      IF <lfs_fd>-faturalama_durumuxxx IS NOT INITIAL.
        <lfs_fd>-faturalama_durumu = <lfs_fd>-faturalama_durumuxxx.
      ELSE.
        <lfs_fd>-faturalama_durumu = <lfs_fd>-faturalama_durumu .
      ENDIF.

      LOOP AT lt_fat ASSIGNING FIELD-SYMBOL(<lfs_fat>) WHERE vbeln = <lfs_fd>-vbeln AND posnr = <lfs_fd>-posnr.

        IF <lfs_fd>-faturalama_durumu = 'C'.
          <lfs_fd>-faturalama_belgesi = <lfs_fat>-faturalama_belgesi.
          <lfs_fd>-muhasebe_belgesi   = <lfs_fat>-muhasebe_belgesi.
        ENDIF.
      ENDLOOP.
    ENDLOOP.


    gt_alv = CORRESPONDING #( lt_test ).

    LOOP AT lt_naval ASSIGNING FIELD-SYMBOL(<lfs_naval>).
      LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_kwert>) WHERE vbeln = <lfs_naval>-vbeln AND posnr = <lfs_naval>-kposn.
        <lfs_kwert>-kwert = <lfs_naval>-kwert.
      ENDLOOP.
    ENDLOOP.

    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv>).
      <lfs_alv>-vergi = <lfs_alv>-mwsbp + <lfs_alv>-netwr.

      READ TABLE lt_domain INTO DATA(ls_domain) WITH KEY domvalue_l = <lfs_alv>-gbstk_d.
      IF sy-subrc IS INITIAL .
        <lfs_alv>-gbstk_d = |{ ls_domain-ddtext }|.
      ENDIF.

      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-teslimat_durumu.
      IF sy-subrc IS INITIAL.
        CASE ls_domain-domvalue_l.
          WHEN  'A'.
            <lfs_alv>-teslimat_durumu = |Teslimat oluşturuldu.|.
          WHEN 'B'.
            READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-wbsta.
            IF sy-subrc IS INITIAL.
              CASE ls_domain-domvalue_l.
                WHEN 'C'.
                  <lfs_alv>-teslimat_durumu = |Teslimat tamamlandı.|.
                WHEN OTHERS.
                  <lfs_alv>-teslimat_durumu = |Teslimat işlemededir.|.
              ENDCASE.
            ENDIF.

          WHEN 'C'.
            <lfs_alv>-teslimat_durumu = |Teslimat kalemleri tamamlandı.|.
          WHEN OTHERS.
            <lfs_alv>-teslimat_durumu =  |{ ls_domain-ddtext }|.
        ENDCASE.

      ENDIF.

      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-cekme_durumu.
      IF sy-subrc IS INITIAL.
        IF <lfs_alv>-teslimat_belgesi IS NOT INITIAL.
          CASE ls_domain-domvalue_l.
            WHEN ' '.
              READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-wbsta.
              IF sy-subrc IS INITIAL.
                <lfs_alv>-cekme_durumu = |{ ls_domain-ddtext }|.
              ENDIF.
            WHEN OTHERS.
              <lfs_alv>-cekme_durumu = |{ ls_domain-ddtext }|.
          ENDCASE.
        ELSE.
          <lfs_alv>-cekme_durumu = | |.
        ENDIF.
      ENDIF.

      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-faturalama_durumu.
      IF sy-subrc IS INITIAL.
        CASE ls_domain-domvalue_l.
          WHEN 'A'.
            <lfs_alv>-faturalama_durumu = |Fatura oluşturulmadı.|.
          WHEN 'B'.
            <lfs_alv>-faturalama_durumu = |Kısmi faturalama sağlandı.|.
          WHEN 'C'.
            <lfs_alv>-faturalama_durumu = |Fatura oluşturuldu.|.
          WHEN OTHERS.
*            <lfs_alv>-faturalama_durumu = |{ ls_domain-ddtext }|.
            <lfs_alv>-faturalama_durumu = | |.
        ENDCASE.

      ENDIF.


      IF <lfs_alv>-muhasebe_belgesi IS INITIAL AND <lfs_alv>-faturalama_belgesi IS NOT INITIAL.
        <lfs_alv>-muhasebe_belgesi = CONV #( |   | ).
      ENDIF.

      IF <lfs_alv>-teslimat_belgesi IS INITIAL AND <lfs_alv>-lfgsa IS NOT INITIAL.
        <lfs_alv>-teslimat_durumu = CONV #( | Teslimat belgesi oluşturulmadı. | ).
      ENDIF.


    ENDLOOP.

    LOOP AT lt_abgru_d ASSIGNING FIELD-SYMBOL(<lfs_abgru_d>).
      LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv_d>) WHERE abgru = <lfs_abgru_d>-abgru.
        <lfs_alv_d>-abgru = CONV #( <lfs_abgru_d>-bezei ).
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_tvakt ASSIGNING FIELD-SYMBOL(<lfs_tvakt>).
      LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv_bezei>) WHERE auart = <lfs_tvakt>-auart.
        <lfs_alv_bezei>-bezei = <lfs_tvakt>-bezei.
      ENDLOOP.
    ENDLOOP.

    SORT gt_alv BY vbeln posnr.

  ENDMETHOD.

  METHOD teslimat.
    DATA(lt_domain) = VALUE dd07v_tab( ).

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'STATV'
        text           = 'X'
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_domain
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    SELECT
    tvakt~auart,
    tvakt~bezei
    FROM tvakt
    INNER JOIN vbak ON vbak~auart = tvakt~auart
    WHERE tvakt~spras = 'T'
    INTO TABLE @DATA(lt_tvakt).


    SELECT
    vbak~vbeln,
    vbak~knumv,
    prcd_elements~kposn,
    prcd_elements~kwert,
    prcd_elements~kschl
    FROM vbak
    INNER JOIN vbap ON vbak~vbeln EQ vbap~vbeln
    INNER JOIN prcd_elements ON  prcd_elements~kschl = 'ZL10' AND vbap~posnr EQ  prcd_elements~kposn
    WHERE  prcd_elements~knumv EQ vbak~knumv
    INTO TABLE @DATA(lt_naval).

    SELECT
   vbak~ernam,
   vbak~kunnr,
   vbap~vbeln,
   vbap~posnr,
   vbak~vkorg,
   vbak~vtweg,
   vbak~auart,
   vbkd~zterm,
   vbap~kdmat,
   vbap~matnr,
   vbap~arktx,
   vbap~kwmeng,              "X
   vbap~vrkme,
   vbak~augru,
   vbak~auart AS auart_x,
   vbap~netwr AS netwr_x,     "X
   vbap~waerk AS waerk_x,
   vbap~mwsbp,
   vbak~netwr,
  "vergi = vbap-mwsbp + vbak-netwr
   vbak~waerk,
   prcd_elements~kwert,       "X
   vbap~lgort,
   vbap~abgru,            "metin gelmeli !!!
   vbak~gbstk AS gbstk_d,

vbfa~vbtyp_v,
vbfa~vbtyp_n,
*CASE vbfa~vbtyp_n
*                  WHEN 'M' THEN vbrk~vbeln "fatura
*                  WHEN 'J' THEN likp~vbeln "teslimat
*                  END AS belge_tipi,
*
    likp~vbeln   AS teslimat_belgesi,
    likp~gbstk   AS teslimat_durumu,      "Teslimat varsa "statv
    likp~kostk   AS cekme_durumu,         "Teslimat varsa "statv
    vbrp~vbeln   AS faturalama_belgesi,
    lips~fksta   AS faturalama_durumu,    "Fatura varsa   "statv
    vbrk~belnr   AS muhasebe_belgesi,      "Fatura varsa
     vbak~knumv,
     vbak~fksak  AS faturalama_durumuxxx,
vbap~lfgsa,
lips~lfimg AS teslimat_miktari,
lips~vrkme AS olcu_birimi,
lips~wbsta,
lips~charg,
lips~matkl,
   vbak~erdat,
   vbak~auart AS bezei,
   lips~erdat AS teslimat_erdat,
   lips~posnr AS teslimat_kalemi,
   vbak~vdatu
   FROM vbak
   INNER JOIN vbap ON vbak~vbeln EQ vbap~vbeln
   INNER JOIN vbkd ON vbkd~vbeln EQ vbap~vbeln
   LEFT JOIN prcd_elements ON  prcd_elements~kschl = 'ZL10' AND prcd_elements~knumv EQ vbak~knumv "AND prcd_elements~kposn EQ vbap~posnr
   LEFT JOIN vbfa ON vbak~vbeln EQ vbfa~vbelv AND vbfa~posnv EQ vbap~posnr
   LEFT JOIN likp ON likp~vbeln EQ vbfa~vbeln
   LEFT JOIN lips ON lips~vbeln EQ likp~vbeln AND lips~posnr EQ vbfa~posnn"vbap~posnr
   LEFT JOIN vbrk ON vbrk~vbeln EQ vbfa~vbeln
   LEFT JOIN vbrp ON vbrp~vbeln EQ vbfa~vbeln AND vbrp~posnr EQ vbap~posnr
   WHERE
       vbak~kunnr IN @so_kunnr
   AND vbak~audat IN @so_audat
   AND vbap~vbeln IN @so_sip
   AND vbak~vkorg IN @so_vkorg
   AND vbak~vtweg IN @so_vtweg
   AND vbap~abgru IN @so_abgru
   AND vbak~augru IN @so_augru
   AND vbak~auart IN @so_auart
   AND vbap~matnr IN @so_matnr
   AND vbap~werks IN @so_werks
   AND vbap~lgort IN @so_lgort
   AND likp~kunnr IN @so_malt
   AND likp~vbeln IN @so_tes
   AND vbrk~vbeln IN @so_fat
   AND vbap~posnr IN @so_posnr
   AND vbfa~vbtyp_v IN ( 'C','J', 'M', 'K', 'L' , 'I' , 'H' )
   AND vbfa~vbtyp_n IN ( 'C','J', 'M', 'O', 'P' , 'T' )
   INTO TABLE @DATA(lt_data).

    SORT lt_data BY vbeln posnr.

    DATA(lt_test) = lt_data.
    SORT lt_test BY vbeln posnr teslimat_belgesi teslimat_kalemi.
    DELETE ADJACENT DUPLICATES FROM lt_test COMPARING vbeln posnr teslimat_belgesi teslimat_kalemi.
    DELETE lt_test WHERE faturalama_belgesi IS NOT INITIAL.

    IF lt_test IS NOT INITIAL.

      SELECT
     vbak~ernam,
     vbak~kunnr,
     vbap~vbeln,
     vbap~posnr,
     vbak~vkorg,
     vbak~vtweg,
     vbak~auart,
     vbkd~zterm,
     vbap~kdmat,
     vbap~matnr,
     vbap~arktx,
     vbap~kwmeng,
     vbap~vrkme,
     vbak~augru,
     vbak~auart AS auart_x,
     vbap~netwr AS netwr_x,
     vbap~waerk AS waerk_x,
     vbap~mwsbp,
     vbak~netwr,
    "vergi = vbap-mwsbp + vbak-netwr
     vbak~waerk,
     prcd_elements~kwert,
     vbap~lgort,
     vbap~abgru,            "metin gelmeli !!!
     vbak~gbstk AS gbstk_d,

  vbfa~vbtyp_v,
  vbfa~vbtyp_n,
*CASE vbfa~vbtyp_n
*                  WHEN 'M' THEN vbrk~vbeln "fatura
*                  WHEN 'J' THEN likp~vbeln "teslimat
*                  END AS belge_tipi,
*
      likp~vbeln   AS teslimat_belgesi,
      likp~gbstk   AS teslimat_durumu,      "Teslimat varsa "statv
      likp~kostk   AS cekme_durumu,         "Teslimat varsa "statv
      vbrp~vbeln   AS faturalama_belgesi,
      lips~fksta   AS faturalama_durumu,    "Fatura varsa   "statv
      vbrk~belnr   AS muhasebe_belgesi,      "Fatura varsa
       vbak~knumv,
       vbak~fksak  AS faturalama_durumuxxx,
  vbap~lfgsa,
  lips~lfimg AS teslimat_miktari,
  lips~vrkme AS olcu_birimi,
  lips~wbsta,
  lips~charg,
  lips~matkl,
     vbak~erdat,
   vbak~auart AS bezei,
   lips~erdat AS teslimat_erdat,
   lips~posnr AS teslimat_kalemi,
   vbak~vdatu
     FROM vbak
     INNER JOIN vbap ON vbak~vbeln EQ vbap~vbeln
     INNER JOIN vbkd ON vbkd~vbeln EQ vbap~vbeln
     LEFT JOIN prcd_elements ON  prcd_elements~kschl = 'ZL10' AND prcd_elements~knumv EQ vbak~knumv "AND prcd_elements~kposn EQ vbap~posnr
     LEFT JOIN vbfa ON vbak~vbeln EQ vbfa~vbelv AND vbfa~posnv EQ vbap~posnr
     LEFT JOIN likp ON likp~vbeln EQ vbfa~vbeln
     LEFT JOIN lips ON lips~vbeln EQ likp~vbeln AND lips~posnr EQ vbfa~posnn"vbap~posnr
     LEFT JOIN vbrk ON vbrk~vbeln EQ vbfa~vbeln
     LEFT JOIN vbrp ON vbrp~vbeln EQ vbfa~vbeln AND vbrp~posnr EQ vbap~posnr
     FOR ALL ENTRIES IN @lt_test
     WHERE
     vbak~vbeln = @lt_test-vbeln
     AND vbap~posnr = @lt_test-posnr
     AND vbak~kunnr IN @so_kunnr
     AND vbak~audat IN @so_audat
     AND vbap~vbeln IN @so_sip
     AND vbak~vkorg IN @so_vkorg
     AND vbak~vtweg IN @so_vtweg
     AND vbap~abgru IN @so_abgru
     AND vbak~augru IN @so_augru
     AND vbak~auart IN @so_auart
     AND vbap~matnr IN @so_matnr
     AND vbap~werks IN @so_werks
     AND vbap~lgort IN @so_lgort
     AND likp~kunnr IN @so_malt
*   AND likp~vbeln IN @so_tes
     AND vbrk~vbeln IN @so_fat
     AND vbap~posnr IN @so_posnr
     AND vbfa~vbtyp_v IN ( 'C','J', 'M', 'K', 'L' , 'I' , 'H' )
     AND vbfa~vbtyp_n IN ( 'C','J', 'M', 'O', 'P' , 'T' )
     INTO TABLE @DATA(lt_fat).
    ENDIF.

*    DATA(lt_fat) = lt_data.
    SORT lt_fat BY vbeln posnr faturalama_belgesi muhasebe_belgesi.
    DELETE ADJACENT DUPLICATES FROM lt_fat COMPARING vbeln posnr faturalama_belgesi muhasebe_belgesi.
    DELETE lt_fat WHERE faturalama_belgesi IS INITIAL.

    SELECT
    tvagt~abgru,
    tvagt~bezei
    FROM tvagt
    INNER JOIN vbap ON vbap~abgru = tvagt~abgru
    WHERE
      tvagt~spras = 'T'
    INTO TABLE @DATA(lt_abgru_d).             "Satış belgeleri ret nedeni açıklama metni tablosu


    LOOP AT lt_test ASSIGNING FIELD-SYMBOL(<lfs_fd>).
      IF <lfs_fd>-faturalama_durumuxxx IS NOT INITIAL.
        <lfs_fd>-faturalama_durumu = <lfs_fd>-faturalama_durumuxxx.
      ELSE.
        <lfs_fd>-faturalama_durumu = <lfs_fd>-faturalama_durumu .
      ENDIF.

      LOOP AT lt_fat ASSIGNING FIELD-SYMBOL(<lfs_fat>) WHERE vbeln = <lfs_fd>-vbeln AND posnr = <lfs_fd>-posnr.

        IF <lfs_fd>-faturalama_durumu = 'C'.
          <lfs_fd>-faturalama_belgesi = <lfs_fat>-faturalama_belgesi.
          <lfs_fd>-muhasebe_belgesi   = <lfs_fat>-muhasebe_belgesi.
        ENDIF.
      ENDLOOP.
    ENDLOOP.


    gt_alv = CORRESPONDING #( lt_test ).

    LOOP AT lt_naval ASSIGNING FIELD-SYMBOL(<lfs_naval>).
      LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_kwert>) WHERE vbeln = <lfs_naval>-vbeln AND posnr = <lfs_naval>-kposn.
        <lfs_kwert>-kwert = <lfs_naval>-kwert.
      ENDLOOP.
    ENDLOOP.

    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv>).
      <lfs_alv>-vergi = <lfs_alv>-mwsbp + <lfs_alv>-netwr.

      READ TABLE lt_domain INTO DATA(ls_domain) WITH KEY domvalue_l = <lfs_alv>-gbstk_d.
      IF sy-subrc IS INITIAL .
        <lfs_alv>-gbstk_d = |{ ls_domain-ddtext }|.
      ENDIF.

      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-teslimat_durumu.
      IF sy-subrc IS INITIAL.
        CASE ls_domain-domvalue_l.
          WHEN  'A'.
            <lfs_alv>-teslimat_durumu = |Teslimat oluşturuldu.|.
          WHEN 'B'.
            READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-wbsta.
            IF sy-subrc IS INITIAL.
              CASE ls_domain-domvalue_l.
                WHEN 'C'.
                  <lfs_alv>-teslimat_durumu = |Teslimat tamamlandı.|.
                WHEN OTHERS.
                  <lfs_alv>-teslimat_durumu = |Teslimat işlemededir.|.
              ENDCASE.
            ENDIF.

          WHEN 'C'.
            <lfs_alv>-teslimat_durumu = |Teslimat kalemleri tamamlandı.|.
          WHEN OTHERS.
            <lfs_alv>-teslimat_durumu =  |{ ls_domain-ddtext }|.
        ENDCASE.

      ENDIF.

      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-cekme_durumu.
      IF sy-subrc IS INITIAL.
        IF <lfs_alv>-teslimat_belgesi IS NOT INITIAL.
          CASE ls_domain-domvalue_l.
            WHEN ' '.
              READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-wbsta.
              IF sy-subrc IS INITIAL.
                <lfs_alv>-cekme_durumu = |{ ls_domain-ddtext }|.
              ENDIF.
            WHEN OTHERS.
              <lfs_alv>-cekme_durumu = |{ ls_domain-ddtext }|.
          ENDCASE.
        ELSE.
          <lfs_alv>-cekme_durumu = | |.
        ENDIF.
      ENDIF.

      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-faturalama_durumu.
      IF sy-subrc IS INITIAL.
        CASE ls_domain-domvalue_l.
          WHEN 'A'.
            <lfs_alv>-faturalama_durumu = |Fatura oluşturulmadı.|.
          WHEN 'B'.
            <lfs_alv>-faturalama_durumu = |Kısmi faturalama sağlandı.|.
          WHEN 'C'.
            <lfs_alv>-faturalama_durumu = |Fatura oluşturuldu.|.
          WHEN OTHERS.
*            <lfs_alv>-faturalama_durumu = |{ ls_domain-ddtext }|.
            <lfs_alv>-faturalama_durumu = | |.
        ENDCASE.

      ENDIF.


      IF <lfs_alv>-muhasebe_belgesi IS INITIAL AND <lfs_alv>-faturalama_belgesi IS NOT INITIAL.
        <lfs_alv>-muhasebe_belgesi = CONV #( |   | ).
      ENDIF.

      IF <lfs_alv>-teslimat_belgesi IS INITIAL AND <lfs_alv>-lfgsa IS NOT INITIAL.
        <lfs_alv>-teslimat_durumu = CONV #( | Teslimat belgesi oluşturulmadı. | ).
      ENDIF.


    ENDLOOP.

    LOOP AT lt_abgru_d ASSIGNING FIELD-SYMBOL(<lfs_abgru_d>).
      LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv_d>) WHERE abgru = <lfs_abgru_d>-abgru.
        <lfs_alv_d>-abgru = CONV #( <lfs_abgru_d>-bezei ).
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_tvakt ASSIGNING FIELD-SYMBOL(<lfs_tvakt>).
      LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv_bezei>) WHERE auart = <lfs_tvakt>-auart.
        <lfs_alv_bezei>-bezei = <lfs_tvakt>-bezei.
      ENDLOOP.
    ENDLOOP.

    SORT gt_alv BY vbeln posnr.
  ENDMETHOD.

  METHOD fatura.

    DATA(lt_domain) = VALUE dd07v_tab( ).

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'STATV'
        text           = 'X'
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_domain
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    SELECT
    tvakt~auart,
    tvakt~bezei
    FROM tvakt
    INNER JOIN vbak ON vbak~auart = tvakt~auart
    WHERE tvakt~spras = 'T'
    INTO TABLE @DATA(lt_tvakt).

    SELECT
    vbak~vbeln,
    vbak~knumv,
    prcd_elements~kposn,
    prcd_elements~kwert,
    prcd_elements~kschl
    FROM vbak
    INNER JOIN vbap ON vbak~vbeln EQ vbap~vbeln
    INNER JOIN prcd_elements ON  prcd_elements~kschl = 'ZL10' AND vbap~posnr EQ  prcd_elements~kposn
    WHERE  prcd_elements~knumv EQ vbak~knumv
    INTO TABLE @DATA(lt_naval).

    SELECT
   vbak~ernam,
   vbak~kunnr,
   vbap~vbeln,
   vbap~posnr,
   vbak~vkorg,
   vbak~vtweg,
   vbak~auart,
   vbkd~zterm,
   vbap~kdmat,
   vbap~matnr,
   vbap~arktx,
   vbap~kwmeng,
   vbap~vrkme,
   vbak~augru,
   vbak~auart AS auart_x,
   vbap~netwr AS netwr_x,
   vbap~waerk AS waerk_x,
   vbap~mwsbp,
   vbak~netwr,
  "vergi = vbap-mwsbp + vbak-netwr
   vbak~waerk,
   prcd_elements~kwert,
   vbap~lgort,
   vbap~abgru,            "metin gelmeli !!!
   vbak~gbstk AS gbstk_d,

vbfa~vbtyp_v,
vbfa~vbtyp_n,
*CASE vbfa~vbtyp_n
*                  WHEN 'M' THEN vbrk~vbeln "fatura
*                  WHEN 'J' THEN likp~vbeln "teslimat
*                  END AS belge_tipi,
*
    likp~vbeln   AS teslimat_belgesi,
    likp~gbstk   AS teslimat_durumu,      "Teslimat varsa "statv
    likp~kostk   AS cekme_durumu,         "Teslimat varsa "statv
    vbrp~vbeln   AS faturalama_belgesi,
    lips~fksta   AS faturalama_durumu,    "Fatura varsa   "statv
    vbrk~belnr   AS muhasebe_belgesi,      "Fatura varsa
     vbak~knumv,
     vbak~fksak  AS faturalama_durumuxxx,
vbap~lfgsa,
lips~lfimg AS teslimat_miktari,
lips~vrkme AS olcu_birimi,
lips~wbsta,
lips~charg,
lips~matkl,
   vbak~erdat,
   vbak~auart AS bezei,
   lips~erdat AS teslimat_erdat,
   lips~posnr AS teslimat_kalemi,
   vbak~vdatu
   FROM vbak
   INNER JOIN vbap ON vbak~vbeln EQ vbap~vbeln
   INNER JOIN vbkd ON vbkd~vbeln EQ vbap~vbeln
   LEFT JOIN prcd_elements ON  prcd_elements~kschl = 'ZL10' AND prcd_elements~knumv EQ vbak~knumv "AND prcd_elements~kposn EQ vbap~posnr
   LEFT JOIN vbfa ON vbak~vbeln EQ vbfa~vbelv AND vbfa~posnv EQ vbap~posnr
   LEFT JOIN likp ON likp~vbeln EQ vbfa~vbeln
   LEFT JOIN lips ON lips~vbeln EQ likp~vbeln AND lips~posnr EQ vbfa~posnn"vbap~posnr
   LEFT JOIN vbrk ON vbrk~vbeln EQ vbfa~vbeln
   LEFT JOIN vbrp ON vbrp~vbeln EQ vbfa~vbeln AND vbrp~posnr EQ vbap~posnr
   WHERE
       vbak~kunnr IN @so_kunnr
   AND vbak~audat IN @so_audat
   AND vbap~vbeln IN @so_sip
   AND vbak~vkorg IN @so_vkorg
   AND vbak~vtweg IN @so_vtweg
   AND vbap~abgru IN @so_abgru
   AND vbak~augru IN @so_augru
   AND vbak~auart IN @so_auart
   AND vbap~matnr IN @so_matnr
   AND vbap~werks IN @so_werks
   AND vbap~lgort IN @so_lgort
   AND likp~kunnr IN @so_malt
   AND likp~vbeln IN @so_tes
   AND vbrk~vbeln IN @so_fat
   AND vbap~posnr IN @so_posnr
   AND vbfa~vbtyp_v IN ( 'C','J', 'M', 'K', 'L' , 'I' , 'H' )
   AND vbfa~vbtyp_n IN ( 'C','J', 'M', 'O', 'P' , 'T' )
   INTO TABLE @DATA(lt_data).

    SORT lt_data BY vbeln posnr.

    DATA(lt_fat) = lt_data.
    SORT lt_fat BY vbeln posnr faturalama_belgesi muhasebe_belgesi.
    DELETE ADJACENT DUPLICATES FROM lt_fat COMPARING vbeln posnr faturalama_belgesi muhasebe_belgesi.
    DELETE lt_fat WHERE faturalama_belgesi IS INITIAL.

    IF lt_fat IS NOT INITIAL.
      SELECT
     vbak~ernam,
     vbak~kunnr,
     vbap~vbeln,
     vbap~posnr,
     vbak~vkorg,
     vbak~vtweg,
     vbak~auart,
     vbkd~zterm,
     vbap~kdmat,
     vbap~matnr,
     vbap~arktx,
     vbap~kwmeng,
     vbap~vrkme,
     vbak~augru,
     vbak~auart AS auart_x,
     vbap~netwr AS netwr_x,
     vbap~waerk AS waerk_x,
     vbap~mwsbp,
     vbak~netwr,
    "vergi = vbap-mwsbp + vbak-netwr
     vbak~waerk,
     prcd_elements~kwert,
     vbap~lgort,
     vbap~abgru,            "metin gelmeli !!!
     vbak~gbstk AS gbstk_d,

  vbfa~vbtyp_v,
  vbfa~vbtyp_n,
*CASE vbfa~vbtyp_n
*                  WHEN 'M' THEN vbrk~vbeln "fatura
*                  WHEN 'J' THEN likp~vbeln "teslimat
*                  END AS belge_tipi,
*
      likp~vbeln   AS teslimat_belgesi,
      likp~gbstk   AS teslimat_durumu,      "Teslimat varsa "statv
      likp~kostk   AS cekme_durumu,         "Teslimat varsa "statv
      vbrp~vbeln   AS faturalama_belgesi,
      lips~fksta   AS faturalama_durumu,    "Fatura varsa   "statv
      vbrk~belnr   AS muhasebe_belgesi,      "Fatura varsa
       vbak~knumv,
       vbak~fksak  AS faturalama_durumuxxx,
  vbap~lfgsa,
  lips~lfimg AS teslimat_miktari,
  lips~vrkme AS olcu_birimi,
  lips~wbsta,
  lips~charg,
  lips~matkl,
     vbak~erdat,
   vbak~auart AS bezei,
   lips~erdat AS teslimat_erdat,
   lips~posnr AS teslimat_kalemi,
   vbak~vdatu
     FROM vbak
     INNER JOIN vbap ON vbak~vbeln EQ vbap~vbeln
     INNER JOIN vbkd ON vbkd~vbeln EQ vbap~vbeln
     LEFT JOIN prcd_elements ON  prcd_elements~kschl = 'ZL10' AND prcd_elements~knumv EQ vbak~knumv "AND prcd_elements~kposn EQ vbap~posnr
     LEFT JOIN vbfa ON vbak~vbeln EQ vbfa~vbelv AND vbfa~posnv EQ vbap~posnr
     LEFT JOIN likp ON likp~vbeln EQ vbfa~vbeln
     LEFT JOIN lips ON lips~vbeln EQ likp~vbeln AND lips~posnr EQ vbfa~posnn"vbap~posnr
     LEFT JOIN vbrk ON vbrk~vbeln EQ vbfa~vbeln
     LEFT JOIN vbrp ON vbrp~vbeln EQ vbfa~vbeln AND vbrp~posnr EQ vbap~posnr
     FOR ALL ENTRIES IN @lt_fat
     WHERE
     vbak~vbeln = @lt_fat-vbeln
     AND vbap~posnr = @lt_fat-posnr
     AND vbak~kunnr IN @so_kunnr
     AND vbak~audat IN @so_audat
     AND vbap~vbeln IN @so_sip
     AND vbak~vkorg IN @so_vkorg
     AND vbak~vtweg IN @so_vtweg
     AND vbap~abgru IN @so_abgru
     AND vbak~augru IN @so_augru
     AND vbak~auart IN @so_auart
     AND vbap~matnr IN @so_matnr
     AND vbap~werks IN @so_werks
     AND vbap~lgort IN @so_lgort
     AND likp~kunnr IN @so_malt
     AND likp~vbeln IN @so_tes
*     AND vbrk~vbeln IN @so_fat
     AND vbap~posnr IN @so_posnr
     AND vbfa~vbtyp_v IN ( 'C','J', 'M', 'K', 'L' , 'I' , 'H' )
     AND vbfa~vbtyp_n IN ( 'C','J', 'M', 'O', 'P' , 'T' )
     INTO TABLE @DATA(lt_test).
    ENDIF.

*    DATA(lt_test) = lt_data.
    SORT lt_test BY vbeln posnr teslimat_belgesi teslimat_kalemi.
    DELETE ADJACENT DUPLICATES FROM lt_test COMPARING vbeln posnr teslimat_belgesi teslimat_kalemi.
    DELETE lt_test WHERE faturalama_belgesi IS NOT INITIAL.


    SELECT
    tvagt~abgru,
    tvagt~bezei
    FROM tvagt
    INNER JOIN vbap ON vbap~abgru = tvagt~abgru
    WHERE
      tvagt~spras = 'T'
    INTO TABLE @DATA(lt_abgru_d).             "Satış belgeleri ret nedeni açıklama metni tablosu

    IF lt_test IS NOT INITIAL.

      LOOP AT lt_test ASSIGNING FIELD-SYMBOL(<lfs_fd>).
        IF <lfs_fd>-faturalama_durumuxxx IS NOT INITIAL.
          <lfs_fd>-faturalama_durumu = <lfs_fd>-faturalama_durumuxxx.
        ELSE.
          <lfs_fd>-faturalama_durumu = <lfs_fd>-faturalama_durumu .
        ENDIF.

        LOOP AT lt_fat ASSIGNING FIELD-SYMBOL(<lfs_fat>) WHERE vbeln = <lfs_fd>-vbeln AND posnr = <lfs_fd>-posnr.

          IF <lfs_fd>-faturalama_durumu = 'C'.
            <lfs_fd>-faturalama_belgesi = <lfs_fat>-faturalama_belgesi.
            <lfs_fd>-muhasebe_belgesi   = <lfs_fat>-muhasebe_belgesi.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      gt_alv = CORRESPONDING #( lt_test ).
    ELSE.
      LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lfs_fdx>).
        IF <lfs_fdx>-faturalama_durumuxxx IS NOT INITIAL.
          <lfs_fdx>-faturalama_durumu = <lfs_fdx>-faturalama_durumuxxx.
        ELSE.
          <lfs_fdx>-faturalama_durumu = <lfs_fdx>-faturalama_durumu .
        ENDIF.

        LOOP AT lt_fat ASSIGNING FIELD-SYMBOL(<lfs_fatx>) WHERE vbeln = <lfs_fdx>-vbeln AND posnr = <lfs_fdx>-posnr.

          IF <lfs_fdx>-faturalama_durumu = 'C'.
            <lfs_fdx>-faturalama_belgesi = <lfs_fatx>-faturalama_belgesi.
            <lfs_fdx>-muhasebe_belgesi   = <lfs_fatx>-muhasebe_belgesi.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      gt_alv = CORRESPONDING #( lt_fat ).
    ENDIF.

    LOOP AT lt_naval ASSIGNING FIELD-SYMBOL(<lfs_naval>).
      LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_kwert>) WHERE vbeln = <lfs_naval>-vbeln AND posnr = <lfs_naval>-kposn.
        <lfs_kwert>-kwert = <lfs_naval>-kwert.
      ENDLOOP.
    ENDLOOP.

    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv>).
      <lfs_alv>-vergi = <lfs_alv>-mwsbp + <lfs_alv>-netwr.

      READ TABLE lt_domain INTO DATA(ls_domain) WITH KEY domvalue_l = <lfs_alv>-gbstk_d.
      IF sy-subrc IS INITIAL .
        <lfs_alv>-gbstk_d = |{ ls_domain-ddtext }|.
      ENDIF.

      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-teslimat_durumu.
      IF sy-subrc IS INITIAL.
        CASE ls_domain-domvalue_l.
          WHEN  'A'.
            <lfs_alv>-teslimat_durumu = |Teslimat oluşturuldu.|.
          WHEN 'B'.
            READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-wbsta.
            IF sy-subrc IS INITIAL.
              CASE ls_domain-domvalue_l.
                WHEN 'C'.
                  <lfs_alv>-teslimat_durumu = |Teslimat tamamlandı.|.
                WHEN OTHERS.
                  <lfs_alv>-teslimat_durumu = |Teslimat işlemededir.|.
              ENDCASE.
            ENDIF.

          WHEN 'C'.
            <lfs_alv>-teslimat_durumu = |Teslimat kalemleri tamamlandı.|.
          WHEN OTHERS.
            <lfs_alv>-teslimat_durumu =  |{ ls_domain-ddtext }|.
        ENDCASE.

      ENDIF.

      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-cekme_durumu.
      IF sy-subrc IS INITIAL.
        IF <lfs_alv>-teslimat_belgesi IS NOT INITIAL.
          CASE ls_domain-domvalue_l.
            WHEN ' '.
              READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-wbsta.
              IF sy-subrc IS INITIAL.
                <lfs_alv>-cekme_durumu = |{ ls_domain-ddtext }|.
              ENDIF.
            WHEN OTHERS.
              <lfs_alv>-cekme_durumu = |{ ls_domain-ddtext }|.
          ENDCASE.
        ELSE.
          <lfs_alv>-cekme_durumu = | |.
        ENDIF.
      ENDIF.

      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-faturalama_durumu.
      IF sy-subrc IS INITIAL.
        CASE ls_domain-domvalue_l.
          WHEN 'A'.
            <lfs_alv>-faturalama_durumu = |Fatura oluşturulmadı.|.
          WHEN 'B'.
            <lfs_alv>-faturalama_durumu = |Kısmi faturalama sağlandı.|.
          WHEN 'C'.
            <lfs_alv>-faturalama_durumu = |Fatura oluşturuldu.|.
          WHEN OTHERS.
*            <lfs_alv>-faturalama_durumu = |{ ls_domain-ddtext }|.
            <lfs_alv>-faturalama_durumu = | |.
        ENDCASE.

      ENDIF.


      IF <lfs_alv>-muhasebe_belgesi IS INITIAL AND <lfs_alv>-faturalama_belgesi IS NOT INITIAL.
        <lfs_alv>-muhasebe_belgesi = CONV #( |   | ).
      ENDIF.

      IF <lfs_alv>-teslimat_belgesi IS INITIAL AND <lfs_alv>-lfgsa IS NOT INITIAL.
        <lfs_alv>-teslimat_durumu = CONV #( | Teslimat belgesi oluşturulmadı. | ).
      ENDIF.


    ENDLOOP.

    LOOP AT lt_abgru_d ASSIGNING FIELD-SYMBOL(<lfs_abgru_d>).
      LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv_d>) WHERE abgru = <lfs_abgru_d>-abgru.
        <lfs_alv_d>-abgru = CONV #( <lfs_abgru_d>-bezei ).
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_tvakt ASSIGNING FIELD-SYMBOL(<lfs_tvakt>).
      LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv_bezei>) WHERE auart = <lfs_tvakt>-auart.
        <lfs_alv_bezei>-bezei = <lfs_tvakt>-bezei.
      ENDLOOP.
    ENDLOOP.

    SORT gt_alv BY vbeln posnr.

  ENDMETHOD.

  METHOD done.

    DATA(lt_domain) = VALUE dd07v_tab( ).

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'STATV'
        text           = 'X'
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_domain
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    SELECT
    tvakt~auart,
    tvakt~bezei
    FROM tvakt
    INNER JOIN vbak ON vbak~auart = tvakt~auart
    WHERE tvakt~spras = 'T'
    INTO TABLE @DATA(lt_tvakt).

    SELECT
    tvagt~abgru,
    tvagt~bezei
    FROM tvagt
    INNER JOIN vbap ON vbap~abgru = tvagt~abgru
    WHERE   tvagt~spras = 'T'
    INTO TABLE @DATA(lt_abgru_d).             "Satış belgeleri ret nedeni açıklama metni tablosu

    SELECT
    vbak~vbeln,
    vbak~knumv,
    prcd_elements~kposn,
    prcd_elements~kwert,
    prcd_elements~kschl
    FROM vbak
    INNER JOIN vbap ON vbak~vbeln EQ vbap~vbeln
    INNER JOIN prcd_elements ON  prcd_elements~kschl = 'ZL10' AND vbap~posnr EQ  prcd_elements~kposn
    WHERE  prcd_elements~knumv EQ vbak~knumv
    INTO TABLE @DATA(lt_naval).

    SELECT
   vbak~ernam,
   vbak~kunnr,
   vbap~vbeln,
   vbap~posnr,
   vbak~vkorg,
   vbak~vtweg,
   vbak~auart,
   vbkd~zterm,
   vbap~kdmat,
   vbap~matnr,
   vbap~arktx,
   vbap~kwmeng,
   vbap~vrkme,
   vbak~augru,
   vbak~auart AS auart_x,
   vbap~netwr AS netwr_x,
   vbap~waerk AS waerk_x,
   vbap~mwsbp,
   vbak~netwr,
   vbak~waerk,
   prcd_elements~kwert,
   vbap~lgort,
   vbap~abgru,
   vbak~gbstk AS gbstk_d,
   vbak~knumv,
   vbak~fksak  AS faturalama_durumuxxx,
   vbap~lfgsa,
   vbak~erdat,
   vbak~auart AS bezei,
   vbak~vdatu
    FROM vbak
    INNER JOIN vbap ON vbak~vbeln EQ vbap~vbeln
    INNER JOIN vbkd ON vbkd~vbeln EQ vbap~vbeln
    LEFT JOIN prcd_elements ON  prcd_elements~kschl = 'ZL10' AND prcd_elements~knumv EQ vbak~knumv "AND prcd_elements~kposn EQ vbap~posnr
    WHERE
        vbak~kunnr IN @so_kunnr
    AND vbak~audat IN @so_audat
    AND vbap~vbeln IN @so_sip
    AND vbak~vkorg IN @so_vkorg
    AND vbak~vtweg IN @so_vtweg
    AND vbap~abgru IN @so_abgru
    AND vbak~augru IN @so_augru
    AND vbak~auart IN @so_auart
    AND vbap~matnr IN @so_matnr
    AND vbap~werks IN @so_werks
    AND vbap~lgort IN @so_lgort
    AND vbap~posnr IN @so_posnr
    AND ( vbak~gbstk = 'C' AND vbap~fkrel = @space  )
    INTO CORRESPONDING FIELDS OF TABLE @gt_all.

    SORT gt_all BY vbeln posnr.
    DELETE ADJACENT DUPLICATES FROM gt_all COMPARING vbeln posnr.


    SELECT
   vbak~ernam,
   vbak~kunnr,
   vbap~vbeln,
   vbap~posnr,
   vbak~vkorg,
   vbak~vtweg,
   vbak~auart,
   vbkd~zterm,
   vbap~kdmat,
   vbap~matnr,
   vbap~arktx,
   vbap~kwmeng,
   vbap~vrkme,
   vbak~augru,
   vbak~auart AS auart_x,
   vbap~netwr AS netwr_x,
   vbap~waerk AS waerk_x,
   vbap~mwsbp,
   vbak~netwr,
   vbak~waerk,
   prcd_elements~kwert,
   vbap~lgort,
   vbap~abgru,            "metin gelmeli !!!
   vbak~gbstk AS gbstk_d,

vbfa~vbtyp_v,
vbfa~vbtyp_n,
    likp~vbeln   AS teslimat_belgesi,
    likp~gbstk   AS teslimat_durumu,      "Teslimat varsa "statv
    likp~kostk   AS cekme_durumu,         "Teslimat varsa "statv
    vbrp~vbeln   AS faturalama_belgesi,
    lips~fksta   AS faturalama_durumu,    "Fatura varsa   "statv
    vbrk~belnr   AS muhasebe_belgesi,      "Fatura varsa
     vbak~knumv,
     vbak~fksak  AS faturalama_durumuxxx,
vbap~lfgsa,
lips~lfimg AS teslimat_miktari,
lips~vrkme AS olcu_birimi,
lips~wbsta,
lips~charg,
lips~matkl,
   vbak~erdat,
   vbak~auart AS bezei,
   lips~erdat AS teslimat_erdat,
   vbak~auart AS delete,
   vbap~fkrel,
   lips~posnr AS teslimat_kalemi,
   vbak~vdatu
   FROM vbak
   INNER JOIN vbap ON vbak~vbeln EQ vbap~vbeln
   INNER JOIN vbkd ON vbkd~vbeln EQ vbap~vbeln
   LEFT JOIN prcd_elements ON  prcd_elements~kschl = 'ZL10' AND prcd_elements~knumv EQ vbak~knumv "AND prcd_elements~kposn EQ vbap~posnr
   LEFT JOIN vbfa ON vbak~vbeln EQ vbfa~vbelv AND vbfa~posnv EQ vbap~posnr
   LEFT JOIN likp ON likp~vbeln EQ vbfa~vbeln
   LEFT JOIN lips ON lips~vbeln EQ likp~vbeln AND lips~posnr EQ vbfa~posnn"vbap~posnr
   LEFT JOIN vbrk ON vbrk~vbeln EQ vbfa~vbeln
   LEFT JOIN vbrp ON vbrp~vbeln EQ vbfa~vbeln AND vbrp~posnr EQ vbap~posnr
   WHERE
       vbak~kunnr IN @so_kunnr
   AND vbak~audat IN @so_audat
   AND vbap~vbeln IN @so_sip
   AND vbak~vkorg IN @so_vkorg
   AND vbak~vtweg IN @so_vtweg
   AND vbap~abgru IN @so_abgru
   AND vbak~augru IN @so_augru
   AND vbak~auart IN @so_auart
   AND vbap~matnr IN @so_matnr
   AND vbap~werks IN @so_werks
   AND vbap~lgort IN @so_lgort
   AND likp~kunnr IN @so_malt
   AND likp~vbeln IN @so_tes
   AND vbrk~vbeln IN @so_fat
   AND vbap~posnr IN @so_posnr
   AND vbfa~vbtyp_v IN ( 'C','J', 'M', 'K', 'L' , 'I' , 'H' )
   AND vbfa~vbtyp_n IN ( 'C','J', 'M', 'O', 'P' , 'T' )
   AND ( vbak~gbstk = 'C' AND vbap~fkrel = @space )
*   AND ( vbak~gbstk = 'C' AND vbak~fksak = 'C' AND vbap~fkrel NE @space )
*   AND ( vbak~fksak =  @space )
   INTO TABLE @DATA(lt_data).
    SORT lt_data BY vbeln posnr.


    DATA(lt_fat) = lt_data.
    SORT lt_fat BY vbeln posnr faturalama_belgesi muhasebe_belgesi.
    DELETE ADJACENT DUPLICATES FROM lt_fat COMPARING vbeln posnr faturalama_belgesi muhasebe_belgesi.
    DELETE lt_fat WHERE faturalama_belgesi IS INITIAL.

    DATA(lt_test) = lt_data.
    SORT lt_test BY vbeln posnr teslimat_belgesi teslimat_kalemi.
    DELETE ADJACENT DUPLICATES FROM lt_test COMPARING vbeln posnr teslimat_belgesi teslimat_kalemi.
    DELETE lt_test WHERE faturalama_belgesi IS NOT INITIAL.

    LOOP AT lt_test ASSIGNING FIELD-SYMBOL(<lfs_erdat>).
      IF <lfs_erdat>-vbtyp_n = 'M'.
        <lfs_erdat>-delete = |Dele|.
      ENDIF.
      IF <lfs_erdat>-teslimat_belgesi IS NOT INITIAL.
        IF <lfs_erdat>-teslimat_erdat IS INITIAL.
          <lfs_erdat>-delete = |Dele|.
        ENDIF.
      ENDIF.
    ENDLOOP.
    DELETE lt_test WHERE delete = |Dele|.

    LOOP AT lt_fat ASSIGNING FIELD-SYMBOL(<ls_x>).
      LOOP AT lt_test ASSIGNING FIELD-SYMBOL(<ls_y>) WHERE vbeln = <ls_x>-vbeln AND posnr = <ls_x>-posnr.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.
        <ls_x>-faturalama_durumu = 'C'.
        DATA(ls_add) = <ls_x>.
        APPEND ls_add TO lt_test.
      ENDIF.
    ENDLOOP.
    SORT lt_test BY vbeln posnr.

    LOOP AT gt_all ASSIGNING FIELD-SYMBOL(<lfs_all>).
      LOOP AT lt_test ASSIGNING FIELD-SYMBOL(<lfs_z>) WHERE vbeln = <lfs_all>-vbeln AND posnr = <lfs_all>-posnr .
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.
        gs_all = <lfs_all>.
*        APPEND gs_all  TO lt_test.
        APPEND gs_all TO gt_all_x.
        lt_test = CORRESPONDING #( gt_all_x ).
      ENDIF.
    ENDLOOP.
    SORT lt_test BY vbeln posnr.


    LOOP AT lt_test ASSIGNING FIELD-SYMBOL(<lfs_fd>).
      IF <lfs_fd>-faturalama_durumuxxx IS NOT INITIAL.
        <lfs_fd>-faturalama_durumu = <lfs_fd>-faturalama_durumuxxx.
      ELSE.
        <lfs_fd>-faturalama_durumu = <lfs_fd>-faturalama_durumu .
      ENDIF.

      LOOP AT lt_fat ASSIGNING FIELD-SYMBOL(<lfs_fat>) WHERE vbeln = <lfs_fd>-vbeln AND posnr = <lfs_fd>-posnr.

        IF <lfs_fd>-faturalama_durumu = 'C'.
          <lfs_fd>-faturalama_belgesi = <lfs_fat>-faturalama_belgesi.
          <lfs_fd>-muhasebe_belgesi   = <lfs_fat>-muhasebe_belgesi.
        ENDIF.
      ENDLOOP.
    ENDLOOP.



    SELECT
    vbak~ernam,
    vbak~kunnr,
    vbap~vbeln,
    vbap~posnr,
    vbak~vkorg,
    vbak~vtweg,
    vbak~auart,
    vbkd~zterm,
    vbap~kdmat,
    vbap~matnr,
    vbap~arktx,
    vbap~kwmeng,
    vbap~vrkme,
    vbak~augru,
    vbak~auart AS auart_x,
    vbap~netwr AS netwr_x,
    vbap~waerk AS waerk_x,
    vbap~mwsbp,
    vbak~netwr,
    vbak~waerk,
    prcd_elements~kwert,
    vbap~lgort,
    vbap~abgru,            "metin gelmeli !!!
    vbak~gbstk AS gbstk_d,

    vbfa~vbtyp_v,
    vbfa~vbtyp_n,
    likp~vbeln   AS teslimat_belgesi,
    likp~gbstk   AS teslimat_durumu,      "Teslimat varsa "statv
    likp~kostk   AS cekme_durumu,         "Teslimat varsa "statv
    vbrp~vbeln   AS faturalama_belgesi,
    lips~fksta   AS faturalama_durumu,    "Fatura varsa   "statv
    vbrk~belnr   AS muhasebe_belgesi,      "Fatura varsa
     vbak~knumv,
     vbak~fksak  AS faturalama_durumuxxx,
    vbap~lfgsa,
    lips~lfimg AS teslimat_miktari,
    lips~vrkme AS olcu_birimi,
    lips~wbsta,
    lips~charg,
    lips~matkl,
    vbak~erdat,
    vbak~auart AS bezei,
    lips~erdat AS teslimat_erdat,
    vbak~auart AS delete,
    vbap~fkrel,
    lips~posnr AS teslimat_kalemi,
    vbak~vdatu
    FROM vbak
    INNER JOIN vbap ON vbak~vbeln EQ vbap~vbeln
    INNER JOIN vbkd ON vbkd~vbeln EQ vbap~vbeln
    LEFT JOIN prcd_elements ON  prcd_elements~kschl = 'ZL10' AND prcd_elements~knumv EQ vbak~knumv "AND prcd_elements~kposn EQ vbap~posnr
    LEFT JOIN vbfa ON vbak~vbeln EQ vbfa~vbelv AND vbfa~posnv EQ vbap~posnr
    LEFT JOIN likp ON likp~vbeln EQ vbfa~vbeln
    LEFT JOIN lips ON lips~vbeln EQ likp~vbeln AND lips~posnr EQ vbfa~posnn"vbap~posnr
    LEFT JOIN vbrk ON vbrk~vbeln EQ vbfa~vbeln
    LEFT JOIN vbrp ON vbrp~vbeln EQ vbfa~vbeln AND vbrp~posnr EQ vbap~posnr
    WHERE
       vbak~kunnr IN @so_kunnr
    AND vbak~audat IN @so_audat
    AND vbap~vbeln IN @so_sip
    AND vbak~vkorg IN @so_vkorg
    AND vbak~vtweg IN @so_vtweg
    AND vbap~abgru IN @so_abgru
    AND vbak~augru IN @so_augru
    AND vbak~auart IN @so_auart
    AND vbap~matnr IN @so_matnr
    AND vbap~werks IN @so_werks
    AND vbap~lgort IN @so_lgort
    AND likp~kunnr IN @so_malt
    AND likp~vbeln IN @so_tes
    AND vbrk~vbeln IN @so_fat
    AND vbap~posnr IN @so_posnr
    AND vbfa~vbtyp_v IN ( 'C','J', 'M', 'K', 'L' , 'I' , 'H' )
    AND vbfa~vbtyp_n IN ( 'C','J', 'M', 'O', 'P' , 'T' )
*       AND ( vbak~gbstk = 'C' AND vbap~fkrel = @space )
    AND ( vbak~gbstk = 'C' AND vbak~fksak = 'C' AND vbap~fkrel NE @space )
    AND vbrk~buchk IN ( 'C', ' ' )
*       AND ( vbak~fksak =  @space )
    INTO TABLE @DATA(lt_data2).
    SORT lt_data2 BY vbeln posnr.

    SELECT
vbak~ernam,
vbak~kunnr,
vbap~vbeln,
vbap~posnr,
vbak~vkorg,
vbak~vtweg,
vbak~auart,
vbkd~zterm,
vbap~kdmat,
vbap~matnr,
vbap~arktx,
vbap~kwmeng,
vbap~vrkme,
vbak~augru,
vbak~auart AS auart_x,
vbap~netwr AS netwr_x,
vbap~waerk AS waerk_x,
vbap~mwsbp,
vbak~netwr,
vbak~waerk,
prcd_elements~kwert,
vbap~lgort,
vbap~abgru,            "metin gelmeli !!!
vbak~gbstk AS gbstk_d,

vbfa~vbtyp_v,
vbfa~vbtyp_n,
likp~vbeln   AS teslimat_belgesi,
likp~gbstk   AS teslimat_durumu,      "Teslimat varsa "statv
likp~kostk   AS cekme_durumu,         "Teslimat varsa "statv
vbrp~vbeln   AS faturalama_belgesi,
lips~fksta   AS faturalama_durumu,    "Fatura varsa   "statv
vbrk~belnr   AS muhasebe_belgesi,      "Fatura varsa
 vbak~knumv,
 vbak~fksak  AS faturalama_durumuxxx,
vbap~lfgsa,
lips~lfimg AS teslimat_miktari,
lips~vrkme AS olcu_birimi,
lips~wbsta,
lips~charg,
lips~matkl,
vbak~erdat,
vbak~auart AS bezei,
lips~erdat AS teslimat_erdat,
vbak~auart AS delete,
vbap~fkrel,
lips~posnr AS teslimat_kalemi,
vbak~vdatu
FROM vbak
INNER JOIN vbap ON vbak~vbeln EQ vbap~vbeln
INNER JOIN vbkd ON vbkd~vbeln EQ vbap~vbeln
LEFT JOIN prcd_elements ON  prcd_elements~kschl = 'ZL10' AND prcd_elements~knumv EQ vbak~knumv "AND prcd_elements~kposn EQ vbap~posnr
LEFT JOIN vbfa ON vbak~vbeln EQ vbfa~vbelv AND vbfa~posnv EQ vbap~posnr
LEFT JOIN likp ON likp~vbeln EQ vbfa~vbeln
LEFT JOIN lips ON lips~vbeln EQ likp~vbeln AND lips~posnr EQ vbfa~posnn"vbap~posnr
LEFT JOIN vbrk ON vbrk~vbeln EQ vbfa~vbeln
LEFT JOIN vbrp ON vbrp~vbeln EQ vbfa~vbeln AND vbrp~posnr EQ vbap~posnr
WHERE
   vbak~kunnr IN @so_kunnr
AND vbak~audat IN @so_audat
AND vbap~vbeln IN @so_sip
AND vbak~vkorg IN @so_vkorg
AND vbak~vtweg IN @so_vtweg
AND vbap~abgru IN @so_abgru
AND vbak~augru IN @so_augru
AND vbak~auart IN @so_auart
AND vbap~matnr IN @so_matnr
AND vbap~werks IN @so_werks
AND vbap~lgort IN @so_lgort
AND likp~kunnr IN @so_malt
AND likp~vbeln IN @so_tes
AND vbrk~vbeln IN @so_fat
AND vbap~posnr IN @so_posnr
AND vbfa~vbtyp_v IN ( 'C','J', 'M', 'K', 'L' , 'I' , 'H' )
AND vbfa~vbtyp_n IN ( 'C','J', 'M', 'O', 'P' , 'T' )
*       AND ( vbak~gbstk = 'C' AND vbap~fkrel = @space )
AND ( vbak~gbstk = 'C' AND vbak~fksak = 'C' AND vbap~fkrel NE @space )
*       AND ( vbak~fksak =  @space )
INTO TABLE @DATA(lt_data22).
    SORT lt_data2 BY vbeln posnr.


    DATA(lt_fat2) = lt_data2.
    SORT lt_fat2 BY vbeln posnr faturalama_belgesi muhasebe_belgesi.
    DELETE ADJACENT DUPLICATES FROM lt_fat2 COMPARING vbeln posnr faturalama_belgesi muhasebe_belgesi.
    DELETE lt_fat2 WHERE faturalama_belgesi IS INITIAL.

    DATA(lt_test2) = lt_data22.
    SORT lt_test2 BY vbeln posnr teslimat_belgesi.
    DELETE ADJACENT DUPLICATES FROM lt_test2 COMPARING vbeln posnr teslimat_belgesi.
    DELETE lt_test2 WHERE faturalama_belgesi IS NOT INITIAL.

    LOOP AT lt_test2 ASSIGNING FIELD-SYMBOL(<lfs_erdat2>).
      IF <lfs_erdat2>-vbtyp_n = 'M'.
        <lfs_erdat2>-delete = |Dele|.
      ENDIF.
      IF <lfs_erdat2>-teslimat_belgesi IS NOT INITIAL.
        IF <lfs_erdat2>-teslimat_erdat IS INITIAL.
          <lfs_erdat2>-delete = |Dele|.
        ENDIF.
      ENDIF.
    ENDLOOP.
    DELETE lt_test2 WHERE delete = |Dele|.

    LOOP AT lt_fat2 ASSIGNING FIELD-SYMBOL(<ls_x2>).
      LOOP AT lt_test2 ASSIGNING FIELD-SYMBOL(<ls_y2>) WHERE vbeln = <ls_x2>-vbeln AND posnr = <ls_x2>-posnr.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.
        <ls_x2>-faturalama_durumu = 'C'.
        DATA(ls_add2) = <ls_x2>.
        APPEND ls_add2 TO lt_test2.
      ENDIF.
    ENDLOOP.
    SORT lt_test2 BY vbeln posnr.

    LOOP AT lt_test2 ASSIGNING FIELD-SYMBOL(<lfs_fd2>).
      IF <lfs_fd2>-faturalama_durumuxxx IS NOT INITIAL.
        <lfs_fd2>-faturalama_durumu = <lfs_fd2>-faturalama_durumuxxx.
      ELSE.
        <lfs_fd2>-faturalama_durumu = <lfs_fd2>-faturalama_durumu .
      ENDIF.

      LOOP AT lt_fat2 ASSIGNING FIELD-SYMBOL(<lfs_fat2>) WHERE vbeln = <lfs_fd2>-vbeln AND posnr = <lfs_fd2>-posnr.

        IF <lfs_fd2>-faturalama_durumu = 'C'.
          <lfs_fd2>-faturalama_belgesi = <lfs_fat2>-faturalama_belgesi.
          <lfs_fd2>-muhasebe_belgesi   = <lfs_fat2>-muhasebe_belgesi.
        ENDIF.
      ENDLOOP.
    ENDLOOP.


    SELECT
    vbak~ernam,
    vbak~kunnr,
    vbap~vbeln,
    vbap~posnr,
    vbak~vkorg,
    vbak~vtweg,
    vbak~auart,
    vbkd~zterm,
    vbap~kdmat,
    vbap~matnr,
    vbap~arktx,
    vbap~kwmeng,
    vbap~vrkme,
    vbak~augru,
    vbak~auart AS auart_x,
    vbap~netwr AS netwr_x,
    vbap~waerk AS waerk_x,
    vbap~mwsbp,
    vbak~netwr,
    vbak~waerk,
    prcd_elements~kwert,
    vbap~lgort,
    vbap~abgru,            "metin gelmeli !!!
    vbak~gbstk AS gbstk_d,

    vbfa~vbtyp_v,
    vbfa~vbtyp_n,
    likp~vbeln   AS teslimat_belgesi,
    likp~gbstk   AS teslimat_durumu,      "Teslimat varsa "statv
    likp~kostk   AS cekme_durumu,         "Teslimat varsa "statv
    vbrp~vbeln   AS faturalama_belgesi,
    lips~fksta   AS faturalama_durumu,    "Fatura varsa   "statv
    vbrk~belnr   AS muhasebe_belgesi,      "Fatura varsa
     vbak~knumv,
     vbak~fksak  AS faturalama_durumuxxx,
    vbap~lfgsa,
    lips~lfimg AS teslimat_miktari,
    lips~vrkme AS olcu_birimi,
    lips~wbsta,
    lips~charg,
    lips~matkl,
    vbak~erdat,
    vbak~auart AS bezei,
    lips~erdat AS teslimat_erdat,
    vbak~auart AS delete,
    vbap~fkrel,
    lips~posnr AS teslimat_kalemi,
    vbak~vdatu
    FROM vbak
    INNER JOIN vbap ON vbak~vbeln EQ vbap~vbeln
    INNER JOIN vbkd ON vbkd~vbeln EQ vbap~vbeln
    LEFT JOIN prcd_elements ON  prcd_elements~kschl = 'ZL10' AND prcd_elements~knumv EQ vbak~knumv "AND prcd_elements~kposn EQ vbap~posnr
    LEFT JOIN vbfa ON vbak~vbeln EQ vbfa~vbelv AND vbfa~posnv EQ vbap~posnr
    LEFT JOIN likp ON likp~vbeln EQ vbfa~vbeln
    LEFT JOIN lips ON lips~vbeln EQ likp~vbeln AND lips~posnr EQ vbfa~posnn"vbap~posnr
    LEFT JOIN vbrk ON vbrk~vbeln EQ vbfa~vbeln
    LEFT JOIN vbrp ON vbrp~vbeln EQ vbfa~vbeln AND vbrp~posnr EQ vbap~posnr
    WHERE
       vbak~kunnr IN @so_kunnr
    AND vbak~audat IN @so_audat
    AND vbap~vbeln IN @so_sip
    AND vbak~vkorg IN @so_vkorg
    AND vbak~vtweg IN @so_vtweg
    AND vbap~abgru IN @so_abgru
    AND vbak~augru IN @so_augru
    AND vbak~auart IN @so_auart
    AND vbap~matnr IN @so_matnr
    AND vbap~werks IN @so_werks
    AND vbap~lgort IN @so_lgort
    AND likp~kunnr IN @so_malt
    AND likp~vbeln IN @so_tes
    AND vbrk~vbeln IN @so_fat
    AND vbap~posnr IN @so_posnr
    AND vbfa~vbtyp_v IN ( 'C','J', 'M', 'K', 'L' , 'I' , 'H' )
    AND vbfa~vbtyp_n IN ( 'C','J', 'M', 'O', 'P' , 'T' )
*       AND ( vbak~gbstk = 'C' AND vbap~fkrel = @space )
*       AND ( vbak~gbstk = 'C' AND vbak~fksak = 'C' AND vbap~fkrel NE @space )
    AND ( vbak~fksak =  @space )
    AND vbrk~buchk IN ( 'C', ' ' )
    INTO TABLE @DATA(lt_data3).
    SORT lt_data3 BY vbeln posnr.

    SELECT
    vbak~ernam,
    vbak~kunnr,
    vbap~vbeln,
    vbap~posnr,
    vbak~vkorg,
    vbak~vtweg,
    vbak~auart,
    vbkd~zterm,
    vbap~kdmat,
    vbap~matnr,
    vbap~arktx,
    vbap~kwmeng,
    vbap~vrkme,
    vbak~augru,
    vbak~auart AS auart_x,
    vbap~netwr AS netwr_x,
    vbap~waerk AS waerk_x,
    vbap~mwsbp,
    vbak~netwr,
    vbak~waerk,
    prcd_elements~kwert,
    vbap~lgort,
    vbap~abgru,            "metin gelmeli !!!
    vbak~gbstk AS gbstk_d,

    vbfa~vbtyp_v,
    vbfa~vbtyp_n,
    likp~vbeln   AS teslimat_belgesi,
    likp~gbstk   AS teslimat_durumu,      "Teslimat varsa "statv
    likp~kostk   AS cekme_durumu,         "Teslimat varsa "statv
    vbrp~vbeln   AS faturalama_belgesi,
    lips~fksta   AS faturalama_durumu,    "Fatura varsa   "statv
    vbrk~belnr   AS muhasebe_belgesi,      "Fatura varsa
     vbak~knumv,
     vbak~fksak  AS faturalama_durumuxxx,
    vbap~lfgsa,
    lips~lfimg AS teslimat_miktari,
    lips~vrkme AS olcu_birimi,
    lips~wbsta,
    lips~charg,
    lips~matkl,
    vbak~erdat,
    vbak~auart AS bezei,
    lips~erdat AS teslimat_erdat,
    vbak~auart AS delete,
    vbap~fkrel,
    lips~posnr AS teslimat_kalemi,
    vbak~vdatu
    FROM vbak
    INNER JOIN vbap ON vbak~vbeln EQ vbap~vbeln
    INNER JOIN vbkd ON vbkd~vbeln EQ vbap~vbeln
    LEFT JOIN prcd_elements ON  prcd_elements~kschl = 'ZL10' AND prcd_elements~knumv EQ vbak~knumv "AND prcd_elements~kposn EQ vbap~posnr
    LEFT JOIN vbfa ON vbak~vbeln EQ vbfa~vbelv AND vbfa~posnv EQ vbap~posnr
    LEFT JOIN likp ON likp~vbeln EQ vbfa~vbeln
    LEFT JOIN lips ON lips~vbeln EQ likp~vbeln AND lips~posnr EQ vbfa~posnn"vbap~posnr
    LEFT JOIN vbrk ON vbrk~vbeln EQ vbfa~vbeln
    LEFT JOIN vbrp ON vbrp~vbeln EQ vbfa~vbeln AND vbrp~posnr EQ vbap~posnr
    WHERE
       vbak~kunnr IN @so_kunnr
    AND vbak~audat IN @so_audat
    AND vbap~vbeln IN @so_sip
    AND vbak~vkorg IN @so_vkorg
    AND vbak~vtweg IN @so_vtweg
    AND vbap~abgru IN @so_abgru
    AND vbak~augru IN @so_augru
    AND vbak~auart IN @so_auart
    AND vbap~matnr IN @so_matnr
    AND vbap~werks IN @so_werks
    AND vbap~lgort IN @so_lgort
    AND likp~kunnr IN @so_malt
    AND likp~vbeln IN @so_tes
    AND vbrk~vbeln IN @so_fat
    AND vbap~posnr IN @so_posnr
    AND vbfa~vbtyp_v IN ( 'C','J', 'M', 'K', 'L' , 'I' , 'H' )
    AND vbfa~vbtyp_n IN ( 'C','J', 'M', 'O', 'P' , 'T' )
*       AND ( vbak~gbstk = 'C' AND vbap~fkrel = @space )
*       AND ( vbak~gbstk = 'C' AND vbak~fksak = 'C' AND vbap~fkrel NE @space )
    AND ( vbak~fksak =  @space )
    INTO TABLE @DATA(lt_data33).
    SORT lt_data3 BY vbeln posnr.


    DATA(lt_fat3) = lt_data3.
    SORT lt_fat3 BY vbeln posnr faturalama_belgesi muhasebe_belgesi.
    DELETE ADJACENT DUPLICATES FROM lt_fat3 COMPARING vbeln posnr faturalama_belgesi muhasebe_belgesi.
    DELETE lt_fat3 WHERE faturalama_belgesi IS INITIAL.

    DATA(lt_test3) = lt_data33.
    SORT lt_test3 BY vbeln posnr teslimat_belgesi.
    DELETE ADJACENT DUPLICATES FROM lt_test3 COMPARING vbeln posnr teslimat_belgesi.
    DELETE lt_test3 WHERE faturalama_belgesi IS NOT INITIAL.

    LOOP AT lt_test3 ASSIGNING FIELD-SYMBOL(<lfs_erdat3>).
      IF <lfs_erdat3>-vbtyp_n = 'M'.
        <lfs_erdat3>-delete = |Dele|.
      ENDIF.
      IF <lfs_erdat3>-teslimat_belgesi IS NOT INITIAL.
        IF <lfs_erdat3>-teslimat_erdat IS INITIAL.
          <lfs_erdat3>-delete = |Dele|.
        ENDIF.
      ENDIF.
    ENDLOOP.
    DELETE lt_test3 WHERE delete = |Dele|.

    LOOP AT lt_fat3 ASSIGNING FIELD-SYMBOL(<ls_x3>).
      LOOP AT lt_test3 ASSIGNING FIELD-SYMBOL(<ls_y3>) WHERE vbeln = <ls_x3>-vbeln AND posnr = <ls_x3>-posnr.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.
        <ls_x3>-faturalama_durumu = 'C'.
        DATA(ls_add3) = <ls_x3>.
        APPEND ls_add3 TO lt_test.
      ENDIF.
    ENDLOOP.
    SORT lt_test3 BY vbeln posnr.

    LOOP AT lt_test3 ASSIGNING FIELD-SYMBOL(<lfs_fd3>).
      IF <lfs_fd3>-faturalama_durumuxxx IS NOT INITIAL.
        <lfs_fd3>-faturalama_durumu = <lfs_fd3>-faturalama_durumuxxx.
      ELSE.
        <lfs_fd3>-faturalama_durumu = <lfs_fd3>-faturalama_durumu .
      ENDIF.

      LOOP AT lt_fat3 ASSIGNING FIELD-SYMBOL(<lfs_fat3>) WHERE vbeln = <lfs_fd3>-vbeln AND posnr = <lfs_fd3>-posnr.

        IF <lfs_fd3>-faturalama_durumu = 'C'.
          <lfs_fd3>-faturalama_belgesi = <lfs_fat3>-faturalama_belgesi.
          <lfs_fd3>-muhasebe_belgesi   = <lfs_fat3>-muhasebe_belgesi.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

**********************************************
    gt_alv = CORRESPONDING #( lt_test ).
    gt_alv = CORRESPONDING #( BASE ( gt_alv ) lt_test2 ).
    gt_alv = CORRESPONDING #( BASE ( gt_alv ) lt_test3 ).


    LOOP AT lt_naval ASSIGNING FIELD-SYMBOL(<lfs_naval>).
      LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_kwert>) WHERE vbeln = <lfs_naval>-vbeln AND posnr = <lfs_naval>-kposn.
        <lfs_kwert>-kwert = <lfs_naval>-kwert.
      ENDLOOP.
    ENDLOOP.

    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv>).
      <lfs_alv>-vergi = <lfs_alv>-mwsbp + <lfs_alv>-netwr.

      READ TABLE lt_domain INTO DATA(ls_domain) WITH KEY domvalue_l = <lfs_alv>-gbstk_d.
      IF sy-subrc IS INITIAL .
        <lfs_alv>-gbstk_d = |{ ls_domain-ddtext }|.
      ENDIF.

      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-teslimat_durumu.
      IF sy-subrc IS INITIAL.
        CASE ls_domain-domvalue_l.
          WHEN  'A'.
            <lfs_alv>-teslimat_durumu = |Teslimat oluşturuldu.|.
          WHEN 'B'.
            READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-wbsta.
            IF sy-subrc IS INITIAL.
              CASE ls_domain-domvalue_l.
                WHEN 'C'.
                  <lfs_alv>-teslimat_durumu = |Teslimat tamamlandı.|.
                WHEN OTHERS.
                  <lfs_alv>-teslimat_durumu = |Teslimat işlemededir.|.
              ENDCASE.
            ENDIF.

          WHEN 'C'.
            <lfs_alv>-teslimat_durumu = |Teslimat kalemleri tamamlandı.|.
          WHEN OTHERS.
            <lfs_alv>-teslimat_durumu =  |{ ls_domain-ddtext }|.
        ENDCASE.

      ENDIF.

      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-cekme_durumu.
      IF sy-subrc IS INITIAL.
        IF <lfs_alv>-teslimat_belgesi IS NOT INITIAL.
          CASE ls_domain-domvalue_l.
            WHEN ' '.
              READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-wbsta.
              IF sy-subrc IS INITIAL.
                <lfs_alv>-cekme_durumu = |{ ls_domain-ddtext }|.
              ENDIF.
            WHEN OTHERS.
              <lfs_alv>-cekme_durumu = |{ ls_domain-ddtext }|.
          ENDCASE.
        ELSE.
          <lfs_alv>-cekme_durumu = | |.
        ENDIF.
      ENDIF.

      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-faturalama_durumu.
      IF sy-subrc IS INITIAL.
        CASE ls_domain-domvalue_l.
          WHEN 'A'.
            <lfs_alv>-faturalama_durumu = |Fatura oluşturulmadı.|.
          WHEN 'B'.
            <lfs_alv>-faturalama_durumu = |Kısmi faturalama sağlandı.|.
          WHEN 'C'.
            <lfs_alv>-faturalama_durumu = |Fatura oluşturuldu.|.
          WHEN OTHERS.
*            <lfs_alv>-faturalama_durumu = |{ ls_domain-ddtext }|.
            <lfs_alv>-faturalama_durumu = | |.
        ENDCASE.

      ENDIF.


      IF <lfs_alv>-muhasebe_belgesi IS INITIAL AND <lfs_alv>-faturalama_belgesi IS NOT INITIAL.
        <lfs_alv>-muhasebe_belgesi = CONV #( |   | ).
      ENDIF.

      IF <lfs_alv>-teslimat_belgesi IS INITIAL AND <lfs_alv>-lfgsa IS NOT INITIAL.
        <lfs_alv>-teslimat_durumu = CONV #( | Teslimat belgesi oluşturulmadı. | ).
      ENDIF.


    ENDLOOP.

    LOOP AT lt_abgru_d ASSIGNING FIELD-SYMBOL(<lfs_abgru_d>).
      LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv_d>) WHERE abgru = <lfs_abgru_d>-abgru.
        <lfs_alv_d>-abgru = CONV #( <lfs_abgru_d>-bezei ).
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_tvakt ASSIGNING FIELD-SYMBOL(<lfs_tvakt>).
      LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv_bezei>) WHERE auart = <lfs_tvakt>-auart.
        <lfs_alv_bezei>-bezei = <lfs_tvakt>-bezei.
      ENDLOOP.
    ENDLOOP.

    SORT gt_alv BY vbeln posnr.


  ENDMETHOD.

  METHOD call_screen.
    CALL SCREEN 0100.
  ENDMETHOD.

  METHOD pbo_0100.
    SET PF-STATUS '0100'.
    SET TITLEBAR '0100'.
  ENDMETHOD.

  METHOD pai_0100.
    CASE iv_ucomm.
      WHEN '&BACK'.
        SET SCREEN 0.
    ENDCASE.
  ENDMETHOD.

  METHOD set_fcat.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZSD_002_S_ALV'
      CHANGING
        ct_fieldcat      = gt_fcat.

    CLEAR gs_fcat.
    gs_fcat =  VALUE #( col_pos    = 28
                        datatype   = 'CHAR'
                        intlen     = 60
                        fieldname  = 'GBSTK_D'
*                        scrtext_s  = 'Sipariş Durumu'
                        scrtext_m  = 'Sipariş Durumu'
                        scrtext_l  = 'Sipariş Durumu').
    APPEND gs_fcat TO gt_fcat.

    CLEAR gs_fcat.
    gs_fcat =  VALUE #( col_pos    = 29
                        datatype   = 'DATS'
                        intlen     = 8
                        fieldname  = 'TESLIMAT_ERDAT'
                        ref_table  = 'LIPS'
                        ref_field  = 'ERDAT'
                        scrtext_m  = 'Kayıt Tarihi'
                        scrtext_l  = 'Kayıt Tarihi').
    APPEND gs_fcat TO gt_fcat.

    CLEAR gs_fcat.
    gs_fcat =  VALUE #( col_pos    = 30
                        datatype   = 'CHAR'
                        intlen     = 60
                        fieldname  = 'TESLIMAT_BELGESI'
*                        scrtext_s  = 'Teslimat Belgesi'
                        scrtext_m  = 'Teslimat Belgesi'
                        scrtext_l  = 'Teslimat Belgesi').
    APPEND gs_fcat TO gt_fcat.

    CLEAR gs_fcat.
    gs_fcat =  VALUE #( col_pos    = 31
                        datatype   = 'CHAR'
                        intlen     = 10
                        fieldname  = 'TESLIMAT_KALEMI'
*                        scrtext_s  = 'Teslimat Belgesi'
                        scrtext_m  = 'Teslimat Kalemi'
                        scrtext_l  = 'Teslimat Kalemi').
    APPEND gs_fcat TO gt_fcat.


    CLEAR gs_fcat.
    gs_fcat =  VALUE #( col_pos    = 32
                        datatype   = 'CHAR'
                        intlen     = 60
                        fieldname  = 'TESLIMAT_MIKTARI'
                        ref_table  = 'LIPS'
                        ref_field  = 'LFIMG'
                        scrtext_m  = 'Teslimat Miktarı'
                        scrtext_l  = 'Teslimat Miktarı').
    APPEND gs_fcat TO gt_fcat.

    CLEAR gs_fcat.
    gs_fcat =  VALUE #( col_pos    = 33
                        datatype   = 'CHAR'
                        intlen     = 60
                        fieldname  = 'OLCU_BIRIMI'
                        ref_table  = 'LIPS'
                        ref_field  = 'VRKME').
    APPEND gs_fcat TO gt_fcat.
*
    CLEAR gs_fcat.
    gs_fcat =  VALUE #( col_pos    = 34
                        datatype   = 'CHAR'
                        intlen     = 10
                        fieldname  = 'CHARG'
                        ref_table  = 'LIPS'
                        ref_field  = 'CHARG').
    APPEND gs_fcat TO gt_fcat.

    CLEAR gs_fcat.
    gs_fcat =  VALUE #( col_pos    = 35"32
                        datatype   = 'CHAR'
                        intlen     = 60
                        fieldname  = 'TESLIMAT_DURUMU'
*                        scrtext_s  = 'Teslimat Durumu'
                        scrtext_m  = 'Teslimat Durumu'
                        scrtext_l  = 'Teslimat Durumu').
    APPEND gs_fcat TO gt_fcat.

    CLEAR gs_fcat.
    gs_fcat =  VALUE #( col_pos    = 36"33
                        datatype   = 'CHAR'
                        intlen     = 60
                        fieldname  = 'CEKME_DURUMU'
*                        scrtext_s  = 'Çekme Durumu'
                        scrtext_m  = 'Çekme Durumu'
                        scrtext_l  = 'Çekme Durumu').
    APPEND gs_fcat TO gt_fcat.

    CLEAR gs_fcat.
    gs_fcat =  VALUE #( col_pos    = 37"34
                        datatype   = 'CHAR'
                        intlen     = 60
                        fieldname  = 'FATURALAMA_BELGESI'
*                        scrtext_s  = 'Faturalama Belgesi'
                        scrtext_m  = 'Faturalama Belgesi'
                        scrtext_l  = 'Faturalama Belgesi').
    APPEND gs_fcat TO gt_fcat.

    CLEAR gs_fcat.
    gs_fcat =  VALUE #( col_pos    = 38"35
                        datatype   = 'CHAR'
                        intlen     = 60
                        fieldname  = 'FATURALAMA_DURUMU'
*                        scrtext_s  = 'Faturalama Durumu'
                        scrtext_m  = 'Faturalama Durumu'
                        scrtext_l  = 'Faturalama Durumu').
    APPEND gs_fcat TO gt_fcat.

    CLEAR gs_fcat.
    gs_fcat =  VALUE #( col_pos    = 39"36
                        datatype   = 'CHAR'
                        intlen     = 60
                        fieldname  = 'MUHASEBE_BELGESI'
*                        scrtext_s  = 'Muhasebe Belgesi'
                        scrtext_m  = 'Muhasebe Belgesi'
                        scrtext_l  = 'Muhasebe Belgesi').
    APPEND gs_fcat TO gt_fcat.
  ENDMETHOD.

  METHOD set_layout.
    gs_layout = VALUE #( zebra      = abap_true
                         col_opt    = abap_true
                         cwidth_opt = abap_true
                         sel_mode   = |A| ).
  ENDMETHOD.

  METHOD display_alv.
    IF  go_alv_grid IS INITIAL.
      CREATE OBJECT go_container
        EXPORTING
          container_name = 'CC_ALV'.

      CREATE OBJECT go_alv_grid
        EXPORTING
*         i_parent = go_container.
          i_parent = cl_gui_container=>screen0.

      CALL METHOD go_alv_grid->set_table_for_first_display
        EXPORTING
          is_layout       = gs_layout                 " Layout
          i_save          = 'A'
        CHANGING
          it_outtab       = gt_alv                 " Output Table
          it_fieldcatalog = gt_fcat.                " Field Catalog

    ELSE.
      CALL METHOD go_alv_grid->refresh_table_display( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
