*&---------------------------------------------------------------------*
*& Include          ZSD_002_FLOW_REPORT_LCL
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION.

  PUBLIC SECTION.
    METHODS:
      get_data,
      modify_data,
      call_screen,
      pbo_0100,
      pai_0100,
      set_fcat,
      set_layout,
      display_alv.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.

  METHOD get_data.

    SELECT DISTINCT vbak~ernam,
                    vbak~kunnr,
                    kna1~name1,
                    kna1~name2,
                    vbak~erdat,
                    vbap~vbeln,
                    vbap~posnr,
                    vbak~vkorg,
                    vbak~vtweg,
                    vbak~auart,
                    tvakt~bezei,
                    vbkd~zterm,
                    vbap~kdmat,
                    vbap~matnr,
                    vbap~arktx,
                    vbap~kwmeng,
                    vbap~vrkme,
                    vbap~abgru AS vbap_abgru,
                    vbak~augru,
                    vbak~auart AS auart_x,
                    vbap~netwr AS netwr_x,
                    vbap~waerk AS waerk_x,
                    vbap~mwsbp,
                    vbak~netwr,
                    vbak~waerk,
                    prcd_elements~kwert,
                    vbap~lgort,
                    tvagt~bezei AS abgru,
                    vbak~gbstk,
                    CAST(  vbap~mwsbp + vbak~netwr AS CURR( 17,2 ) ) AS vergi,
                    lips~erdat AS erdat_t,
                    lips~vbeln AS vbeln_vl,
                    lips~posnr AS posnr_vl,
                    lips~lfimg,
                    lips~vrkme AS vrkme_t,
                    likp~gbstk AS gbstk_t,
                    likp~kostk,
                    lips~charg,
                    lips~matkl,
                    vbrp~vbeln AS vbeln_vf,
                    vbrk~fkdat,
                    lips~fksta,
                    vbrk~belnr,
                    bkpf~bldat,
                    vbap~lfgsa,
                    lips~wbsta,
                    vbak~fksak,
                    CASE WHEN vbak~fksak IS NOT INITIAL
                         THEN vbak~fksak
                         ELSE lips~fksta
                    END AS faturalama_durumu,
                    vbak~knumv,
                    vbap~fkrel
    FROM vbak
    INNER JOIN vbap         ON vbak~vbeln          = vbap~vbeln
    INNER JOIN kna1         ON vbak~kunnr          = kna1~kunnr
    LEFT  JOIN tvakt        ON vbak~auart          = tvakt~auart
                           AND tvakt~spras         = @sy-langu
    LEFT  JOIN tvagt        ON vbap~abgru          = tvagt~abgru
                           AND tvagt~spras         = @sy-langu
    INNER JOIN vbkd         ON vbak~vbeln          = vbkd~vbeln
                            AND vbkd~posnr         = @space
    LEFT JOIN prcd_elements ON vbak~knumv          = prcd_elements~knumv
                           AND vbap~posnr          = prcd_elements~kposn
                           AND prcd_elements~kschl = 'ZL10'
    LEFT JOIN lips          ON vbap~vbeln          = lips~vgbel
                           AND vbap~posnr          = lips~vgpos
    LEFT JOIN likp          ON lips~vbeln          = likp~vbeln
    LEFT JOIN vbrp          ON vbap~vbeln          = vbrp~aubel
                        AND vbap~posnr             = vbrp~aupos
                 AND ( vbrp~vgbel = lips~vbeln AND vbrp~vgpos = lips~posnr )
    LEFT JOIN vbrk          ON vbrp~vbeln          = vbrk~vbeln
                           AND vbrk~fksto          = @space
    LEFT JOIN bkpf          ON vbrk~belnr          = bkpf~belnr
                           AND vbrk~gjahr          = bkpf~gjahr
                           AND vbrk~bukrs          = bkpf~bukrs
    WHERE vbak~kunnr IN @so_kunnr
    AND  vbak~audat  IN @so_audat
    AND  vbap~vbeln  IN @so_vbeln
    AND  vbap~posnr  IN @so_posnr
    AND  vbak~vkorg  IN @so_vkorg
    AND  vbak~vtweg  IN @so_vtweg
    AND  vbap~abgru  IN @so_abgru
    AND  vbak~augru  IN @so_augru
    AND  vbak~auart  IN @so_auart
    AND  vbap~matnr  IN @so_matnr
    AND  lips~werks  IN @so_werks
    AND  lips~lgort  IN @so_lgort
    AND  likp~kunnr  IN @so_mta
    AND  likp~vbeln  IN @so_tes
    AND  vbrk~vbeln  IN @so_fat

    UNION DISTINCT

    SELECT DISTINCT vbak~ernam,
                    vbak~kunnr,
                    kna1~name1,
                    kna1~name2,
                    vbak~erdat,
                    vbap~vbeln,
                    vbap~posnr,
                    vbak~vkorg,
                    vbak~vtweg,
                    vbak~auart,
                    tvakt~bezei,
                    vbkd~zterm,
                    vbap~kdmat,
                    vbap~matnr,
                    vbap~arktx,
                    vbap~kwmeng,
                    vbap~vrkme,
                    vbap~abgru AS vbap_abgru,
                    vbak~augru,
                    vbak~auart AS auart_x,
                    vbap~netwr AS netwr_x,
                    vbap~waerk AS waerk_x,
                    vbap~mwsbp,
                    vbak~netwr,
                    vbak~waerk,
                    prcd_elements~kwert,
                    vbap~lgort,
                    tvagt~bezei AS abgru,
                    vbak~gbstk,
                    CAST(  vbap~mwsbp + vbak~netwr AS CURR( 17,2 ) ) AS vergi,
                    lips~erdat AS erdat_t,
                    lips~vbeln AS vbeln_vl,
                    lips~posnr AS posnr_vl,
                    lips~lfimg,
                    lips~vrkme AS vrkme_t,
                    likp~gbstk AS gbstk_t,
                    likp~kostk,
                    lips~charg,
                    lips~matkl,
                    vbrp~vbeln AS vbeln_vf,
                    vbrk~fkdat,
                    lips~fksta,
                    vbrk~belnr,
                    bkpf~bldat,
                    vbap~lfgsa,
                    lips~wbsta,
                    vbak~fksak,
                    CASE WHEN vbak~fksak IS NOT INITIAL
                         THEN vbak~fksak
                         ELSE lips~fksta
                    END AS faturalama_durumu,
                    vbak~knumv,
                    vbap~fkrel
    FROM vbrp
    INNER JOIN vbrk          ON vbrp~vbeln         = vbrk~vbeln
                           AND vbrk~fksto          = @space
    INNER JOIN vbap         ON vbap~vbeln          = vbrp~aubel
                           AND vbap~posnr          = vbrp~aupos
    INNER JOIN vbak         ON vbak~vbeln          = vbap~vbeln
    INNER JOIN kna1         ON vbak~kunnr          = kna1~kunnr
    LEFT  JOIN tvakt        ON vbak~auart          = tvakt~auart
                           AND tvakt~spras         = @sy-langu
    LEFT  JOIN tvagt        ON vbap~abgru          = tvagt~abgru
                           AND tvagt~spras         = @sy-langu
    INNER JOIN vbkd         ON vbak~vbeln          = vbkd~vbeln
                           AND vbkd~posnr          = @space
    LEFT JOIN prcd_elements ON vbak~knumv          = prcd_elements~knumv
                           AND vbap~posnr          = prcd_elements~kposn
                           AND prcd_elements~kschl = 'ZL10'
    LEFT JOIN lips          ON vbap~vbeln          = lips~vgbel
                           AND vbap~posnr          = lips~vgpos
                    AND ( vbrp~vgbel = lips~vbeln AND vbrp~vgpos = lips~posnr )
    LEFT JOIN likp          ON lips~vbeln          = likp~vbeln
    LEFT JOIN bkpf          ON vbrk~belnr          = bkpf~belnr
                           AND vbrk~gjahr          = bkpf~gjahr
                           AND vbrk~bukrs          = bkpf~bukrs
    WHERE vbak~kunnr IN @so_kunnr
    AND  vbak~audat  IN @so_audat
    AND  vbap~vbeln  IN @so_vbeln
    AND  vbap~posnr  IN @so_posnr
    AND  vbak~vkorg  IN @so_vkorg
    AND  vbak~vtweg  IN @so_vtweg
    AND  vbap~abgru  IN @so_abgru
    AND  vbak~augru  IN @so_augru
    AND  vbak~auart  IN @so_auart
    AND  vbap~matnr  IN @so_matnr
    AND  lips~werks  IN @so_werks
    AND  lips~lgort  IN @so_lgort
    AND  likp~kunnr  IN @so_mta
    AND  likp~vbeln  IN @so_tes
    AND  vbrk~vbeln  IN @so_fat
    INTO TABLE @DATA(lt_tab).

    SORT lt_tab BY vbeln_vl posnr_vl vbeln_vf.
    DELETE ADJACENT DUPLICATES FROM lt_tab COMPARING vbeln posnr vbeln_vl posnr_vl.

    SELECT DISTINCT  vbrp~vbeln AS vbeln_vf,
                     vbrk~fkdat,
                     vbrk~belnr,
                     bkpf~bldat,
                     lt_tab~vbeln,
                     lt_tab~posnr
    FROM @lt_tab AS lt_tab
    INNER JOIN vbrp  ON lt_tab~vbeln = vbrp~aubel
                    AND lt_tab~posnr = vbrp~aupos
    INNER JOIN vbrk  ON vbrp~vbeln   = vbrk~vbeln
                    AND vbrk~fksto   = @space
    LEFT JOIN bkpf  ON vbrk~belnr    = bkpf~belnr
                   AND vbrk~gjahr    = bkpf~gjahr
                   AND vbrk~bukrs    = bkpf~bukrs
    WHERE lt_tab~vbeln_vl = @space
    AND   lt_tab~posnr_vl = @space
    ORDER BY  lt_tab~vbeln,lt_tab~posnr,vbeln_vf
    INTO TABLE @DATA(lt_billing).


    LOOP AT lt_tab ASSIGNING FIELD-SYMBOL(<lfs_tab>).
      READ TABLE lt_billing ASSIGNING FIELD-SYMBOL(<lfs_billing>) WITH KEY vbeln = <lfs_tab>-vbeln
                                                                           posnr = <lfs_tab>-posnr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_tab> = VALUE #( BASE <lfs_tab>
                             vbeln_vf = <lfs_billing>-vbeln_vf
                             fkdat    = <lfs_billing>-fkdat
                             belnr    = <lfs_billing>-belnr
                             bldat    = <lfs_billing>-bldat ).
      ENDIF.
    ENDLOOP.

**    BREAK xgamzey.
    gt_alv = CORRESPONDING #( lt_tab ).
    SORT gt_alv BY vbeln posnr vbeln_vl posnr_vl vbeln_vf.

  ENDMETHOD.

  METHOD modify_data.

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

    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv>).

      READ TABLE lt_domain INTO DATA(ls_domain) WITH KEY domvalue_l = <lfs_alv>-gbstk BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_alv>-gbstk = |{ ls_domain-ddtext }|.
      ENDIF.

      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-gbstk_t BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        CASE ls_domain-domvalue_l.
          WHEN 'A'.
            <lfs_alv>-gbstk_t = TEXT-002.
          WHEN 'B'.
            READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-wbsta BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              CASE ls_domain-domvalue_l.
                WHEN 'C'.
                  <lfs_alv>-gbstk_t = TEXT-003.
                WHEN OTHERS.
                  <lfs_alv>-gbstk_t = TEXT-004.
              ENDCASE.
            ENDIF.
          WHEN 'C'.
            <lfs_alv>-gbstk_t = TEXT-005.
          WHEN OTHERS.
            <lfs_alv>-gbstk_t = |{ ls_domain-ddtext }|.
        ENDCASE.
      ENDIF.

      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-kostk BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        IF <lfs_alv>-vbeln_vl IS NOT INITIAL.
          CASE ls_domain-domvalue_l.
            WHEN ' '.
              READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-wbsta BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                <lfs_alv>-kostk = |{ ls_domain-ddtext }|.
              ENDIF.
            WHEN OTHERS.
              <lfs_alv>-kostk = |{ ls_domain-ddtext }|.
          ENDCASE.
        ELSE.
          <lfs_alv>-kostk = | |.
        ENDIF.
      ENDIF.

      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-faturalama_durumu BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        CASE ls_domain-domvalue_l.
          WHEN 'A'.
            <lfs_alv>-faturalama_durumu = TEXT-006.
          WHEN 'B'.
            <lfs_alv>-faturalama_durumu = TEXT-007.
          WHEN 'C'.
            <lfs_alv>-faturalama_durumu = TEXT-008.
          WHEN OTHERS.
            <lfs_alv>-faturalama_durumu = | |.
        ENDCASE.
      ENDIF.

      IF <lfs_alv>-vbeln_vl IS INITIAL AND <lfs_alv>-lfgsa IS NOT INITIAL.
        <lfs_alv>-gbstk_t = TEXT-009.
      ENDIF.

    ENDLOOP.

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
        i_structure_name       = 'ZSD_002_S_FLOW'
      CHANGING
        ct_fieldcat            = gt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<lfs_fcat>).
      CASE <lfs_fcat>-fieldname.
        WHEN 'LFGSA'.
          <lfs_fcat>-no_out = abap_true.
        WHEN 'WBSTA'.
          <lfs_fcat>-no_out = abap_true.
        WHEN 'FKSAK'.
          <lfs_fcat>-no_out = abap_true.
        WHEN 'FKSTA'.
          <lfs_fcat>-no_out = abap_true.
        WHEN 'POSNR_VF'.
          <lfs_fcat>-no_out = abap_true.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_layout.
    gs_layout = VALUE #( zebra      = abap_true
                         col_opt    = abap_true
                         cwidth_opt = abap_true
                         ctab_fname = 'CELL_COLOR'
                         stylefname = 'CELLSTYLE'
                         info_fname = 'LINE_COLOR'
                         sel_mode   = |A| ).
  ENDMETHOD.

  METHOD display_alv.
    IF  go_alv_grid IS INITIAL.
****      CREATE OBJECT go_container
****        EXPORTING
****          container_name = 'CC_ALV'.

      gs_variant = VALUE #( BASE gs_variant
                                 report  = sy-repid
                                 variant = p_var ).
      CREATE OBJECT go_alv_grid
        EXPORTING
*         i_parent = go_container.
          i_parent = cl_gui_container=>screen0.

      CALL METHOD go_alv_grid->set_table_for_first_display
        EXPORTING
          is_layout       = gs_layout
          is_variant      = gs_variant
          i_save          = 'A'              " ' ' , 'X' , 'U'
        CHANGING
          it_outtab       = gt_alv
          it_fieldcatalog = gt_fcat.

    ELSE.
      CALL METHOD go_alv_grid->refresh_table_display( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
