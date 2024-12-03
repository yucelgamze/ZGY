*&---------------------------------------------------------------------*
*& Include          ZSD_002_FLOW_REPORTXXX_LCL
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
                    vbak~augru,
                    vbap~netwr AS netwr_vbap,
                    vbap~waerk,
                    vbap~mwsbp,
                    vbak~netwr AS netwr_vbak,
                    prcd_elements~kwert,
                    vbap~lgort,
                    tvagt~bezei AS abgru, "vbap~abgru,
                    vbak~gbstk,
                    lips~erdat AS erdat_t,
                    lips~vbeln AS vbeln_vl,
                    lips~posnr AS posnr_vl,
                    lips~lfimg,
                    lips~vrkme AS vrkme_lips,
                    likp~gbstk AS gbstk_t,
                    likp~kostk,
                    vbrp~vbeln AS vbeln_vf,
                    lips~fksta,
                    vbrk~belnr,
                    vbap~lfgsa,
                    lips~wbsta,
                    vbak~fksak,
                    vbak~knumv
     FROM vbak
     INNER JOIN vbap         ON vbak~vbeln          = vbap~vbeln
     LEFT  JOIN tvakt        ON vbak~auart          = tvakt~auart
                            AND tvakt~spras         = @sy-langu
     LEFT  JOIN tvagt        ON vbap~abgru          = tvagt~abgru
                            AND tvagt~spras         = @sy-langu
     INNER JOIN vbkd         ON vbak~vbeln          = vbkd~vbeln
                            AND vbkd~posnr          = @space
     LEFT JOIN prcd_elements ON vbak~knumv          = prcd_elements~knumv
                            AND vbap~posnr          = prcd_elements~kposn
                            AND prcd_elements~kschl = 'ZA00'
     LEFT JOIN lips          ON vbap~vbeln          = lips~vgbel
                            AND vbap~posnr          = lips~vgpos
     LEFT JOIN likp          ON lips~vbeln          = likp~vbeln
     LEFT JOIN vbrp          ON vbap~vbeln          = vbrp~aubel
                            AND vbap~posnr          = vbrp~aupos
     LEFT JOIN vbrk          ON vbrp~vbeln          = vbrk~vbeln
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

      UNION

    SELECT DISTINCT ekko~ernam AS ernam,
                    ekpo~werks AS kunnr,
                    ekko~aedat AS erdat,
                    ekpo~ebeln AS vbeln,
                    CAST( concat( '0' ,ekpo~ebelp ) AS NUMC( 6 ) ) AS  posnr,
                    ekko~ekorg                AS vkorg,
                    CAST( ' ' AS CHAR( 2 ) )  AS vtweg,
                    ekko~bsart                AS auart,
                    t161t~batxt               AS bezei,
                    CAST( ' ' AS CHAR( 4 ) )  AS zterm,
                    CAST( ' ' AS CHAR( 40 ) ) AS kdmat,
                    CASE WHEN mara~matnr IS INITIAL  THEN ekpo~txz01
                         ELSE mara~matnr
                    END AS matnr,
                    ekpo~txz01               AS arktx,
                    ekpo~menge               AS kwmeng,
                    CAST( ' ' AS UNIT( 3 ) ) AS vrkme,
                    CAST( ' ' AS CHAR( 4 ) ) AS augru,
                    ekpo~netwr               AS netwr_vbap,
                    ekko~waers               AS waerk,
                    CAST( 0 AS CURR( 13,2 ) )  AS mwsbp,
                    CAST( 0 AS CURR( 15,2 ) )  AS netwr_vbak,
                    CAST( 0 AS CURR( 15,2 ) )  AS kwert,
                    lips~lgort,
                    CAST( ' ' AS CHAR( 60 ) ) AS abgru,
                    CAST( 'X' AS CHAR( 1 ) ) AS gbstk,
                    lips~erdat AS erdat_t,
                    lips~vbeln AS vbeln_vl,
                    lips~posnr AS posnr_vl,
                    lips~lfimg,
                    lips~vrkme AS vrkme_lips,
                    likp~gbstk AS gbstk_t,
                    likp~kostk,
                    vbrp~vbeln AS vbeln_vf,
                    lips~fksta,
                    vbrk~belnr,
                    CAST( 'X' AS CHAR( 1 ) )  AS lfgsa,
                    lips~wbsta,
                    CAST( 'X' AS CHAR( 1 ) )  AS fksak,
                    CAST( ' ' AS CHAR( 10 ) ) AS knumv
     FROM ekko
     INNER JOIN ekpo         ON ekko~ebeln                = ekpo~ebeln
     LEFT  JOIN t161t        ON ekko~bsart                = t161t~bsart
                            AND ekko~bstyp                = t161t~bstyp
                            AND t161t~spras               = @sy-langu
     LEFT JOIN mara          ON ekpo~matnr                = mara~matnr
     LEFT JOIN lips          ON ekpo~ebeln                = lips~vgbel
                            AND concat( '0', ekpo~ebelp ) = lips~vgpos
     LEFT JOIN likp          ON lips~vbeln                = likp~vbeln
     LEFT JOIN vbrp          ON ekpo~ebeln                = vbrp~aubel
                            AND concat( '0', ekpo~ebelp ) = vbrp~aupos
     LEFT JOIN vbrk          ON vbrp~vbeln                = vbrk~vbeln
     WHERE ekpo~werks IN @so_kunnr
     AND  ekpo~ebeln  IN @so_vbeln
     AND  ekpo~ebelp  IN @so_posnr
     AND  ekko~ekorg  IN @so_vkorg
     AND  ekko~bsart  IN @so_auart
     AND  ekpo~matnr  IN @so_matnr
     AND  lips~werks  IN @so_werks
     AND  lips~lgort  IN @so_lgort
     AND  likp~kunnr  IN @so_mta
     AND  likp~vbeln  IN @so_tes
     AND  vbrk~vbeln  IN @so_fat
     ORDER BY vbeln, posnr
     INTO TABLE @DATA(lt_data).

    gt_alv = CORRESPONDING #( lt_data ).

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

      <lfs_alv>-vergi = <lfs_alv>-mwsbp + <lfs_alv>-netwr_vbak.

      READ TABLE lt_domain INTO DATA(ls_domain) WITH KEY domvalue_l = <lfs_alv>-gbstk BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_alv>-gbstk = |{ ls_domain-ddtext }|.
        IF <lfs_alv>-gbstk = 'X'.
          <lfs_alv>-gbstk =  ' '.
        ENDIF.
      ENDIF.
      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-gbstk_t BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_alv>-gbstk_t = |{ ls_domain-ddtext }|.
      ENDIF.
      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-kostk BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_alv>-kostk = |{ ls_domain-ddtext }|.
      ENDIF.
      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-fksta BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_alv>-fksta = |{ ls_domain-ddtext }|.
      ENDIF.
      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-fksak BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_alv>-fksak = |{ ls_domain-ddtext }|.
        IF <lfs_alv>-fksak = 'X'.
          <lfs_alv>-fksak =  ' '.
        ENDIF.
      ENDIF.
      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-lfgsa BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_alv>-lfgsa = |{ ls_domain-ddtext }|.
        IF <lfs_alv>-lfgsa = 'X'.
          <lfs_alv>-lfgsa =  ' '.
        ENDIF.
      ENDIF.
      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_alv>-wbsta BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_alv>-wbsta = |{ ls_domain-ddtext }|.
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
        i_structure_name       = 'ZSD_002_S_FLOW_REPORT'
      CHANGING
        ct_fieldcat            = gt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
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

      CREATE OBJECT go_alv_grid
        EXPORTING
*         i_parent = go_container.
          i_parent = cl_gui_container=>screen0.

      CALL METHOD go_alv_grid->set_table_for_first_display
        EXPORTING
          is_layout       = gs_layout                 " Layout
        CHANGING
          it_outtab       = gt_alv                 " Output Table
          it_fieldcatalog = gt_fcat.                " Field Catalog

    ELSE.
      CALL METHOD go_alv_grid->refresh_table_display( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
