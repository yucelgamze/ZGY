*&---------------------------------------------------------------------*
*& Include          ZGY_DEV_P016_LCL
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS:
      get_data,
      call_screen,
      pbo_0100,
      pai_0100 IMPORTING iv_ucomm TYPE sy-ucomm,
      set_fcat,
      set_layout,
      display_alv.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD get_data.
    SELECT
    bkpf~bukrs,
    bkpf~belnr,
    bseg~gjahr,
    kna1~kunnr,
    lfa1~lifnr,
    kna1~name1,
    kna1~name2,
    lfa1~name1 AS lname1,
    lfa1~name2 AS lname2,
    bseg~pswsl,
    SUM( bseg~wrbtr ) AS w,
    bkpf~bldat,
    bseg~koart,
    bseg~shkzg
    FROM bkpf
    LEFT JOIN bseg ON bkpf~belnr EQ bseg~belnr
    LEFT JOIN kna1 ON bseg~kunnr EQ kna1~kunnr
    LEFT JOIN lfa1 ON bseg~lifnr EQ lfa1~lifnr
    WHERE bkpf~bukrs EQ @p_bukrs  AND
          bkpf~belnr IN @so_belnr AND
          bseg~pswsl IN @so_pswsl AND
          bseg~koart IN ( 'D', 'K' )  AND
          bseg~shkzg IN ( 'H', 'S' )
*        ( bseg~koart EQ 'D' OR bseg~koart EQ 'K')  AND "D müşteri kna1    K satıcı lfa1
*        ( bseg~shkzg EQ 'H' OR bseg~shkzg EQ 'S')      "H + borç          S - alacak
   GROUP BY
   bkpf~bukrs,
   bkpf~belnr,
   bseg~gjahr,
   kna1~kunnr,
   lfa1~lifnr,
   kna1~name1,
   kna1~name2,
   lfa1~name1,
   lfa1~name2,
   bseg~pswsl,
   bkpf~bldat,
   bseg~koart,
   bseg~shkzg
  INTO TABLE @DATA(lt_tab).

    LOOP AT lt_tab ASSIGNING FIELD-SYMBOL(<lfs_tab>).
      gs_alv = CORRESPONDING #( <lfs_tab> ).

      IF <lfs_tab>-koart EQ 'D'.
        gs_alv = VALUE #( BASE gs_alv
                          no    = <lfs_tab>-kunnr
                          isim  = |{ <lfs_tab>-name1 } { <lfs_tab>-name2 }| ).
      ELSEIF    <lfs_tab>-koart EQ 'K'.
        gs_alv = VALUE #( BASE gs_alv
                          no    = <lfs_tab>-lifnr
                          isim  = |{ <lfs_tab>-lname1 } { <lfs_tab>-lname2 }| ).
      ENDIF.

      IF <lfs_tab>-shkzg EQ 'H'.
        gs_alv-wrbtr = <lfs_tab>-w.
      ELSEIF <lfs_tab>-shkzg EQ 'S'.
        gs_alv-wrbtr = - <lfs_tab>-w.
      ENDIF.

      COLLECT gs_alv INTO gt_alv.
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
    CASE iv_ucomm.
      WHEN '&BACK'.
        SET SCREEN 0.
    ENDCASE.
  ENDMETHOD.

  METHOD set_fcat.
    CLEAR:gs_fcat.
    gs_fcat = VALUE #( col_pos   = 1
                       fieldname = 'BUKRS'
                       scrtext_l = 'BUKRS'
                       scrtext_m = 'BUKRS'
                       scrtext_s = 'BUKRS' ).
    APPEND gs_fcat TO gt_fcat.

    CLEAR:gs_fcat.
    gs_fcat = VALUE #( col_pos   = 2
                       fieldname = 'BELNR'
                       scrtext_l = 'BELNR'
                       scrtext_m = 'BELNR'
                       scrtext_s = 'BELNR' ).
    APPEND gs_fcat TO gt_fcat.

    CLEAR:gs_fcat.
    gs_fcat = VALUE #( col_pos   = 3
                       fieldname = 'GJAHR'
                       scrtext_l = 'GJAHR'
                       scrtext_m = 'GJAHR'
                       scrtext_s = 'GJAHR' ).
    APPEND gs_fcat TO gt_fcat.

    CLEAR:gs_fcat.
    gs_fcat = VALUE #( col_pos   = 4
                       fieldname = 'NO'
                       scrtext_l = 'KUNNR/LIFNR'
                       scrtext_m = 'KUNNR/LIFNR'
                       scrtext_s = 'KUNNR/LIFNR' ).
    APPEND gs_fcat TO gt_fcat.

    CLEAR:gs_fcat.
    gs_fcat = VALUE #( col_pos   = 5
                       fieldname = 'ISIM'
                       scrtext_l = 'ISIM'
                       scrtext_m = 'ISIM'
                       scrtext_s = 'ISIM' ).
    APPEND gs_fcat TO gt_fcat.

    CLEAR:gs_fcat.
    gs_fcat = VALUE #( col_pos   = 6
                       fieldname = 'PSWSL'
                       scrtext_l = 'PSWSL'
                       scrtext_m = 'PSWSL'
                       scrtext_s = 'PSWSL' ).
    APPEND gs_fcat TO gt_fcat.

    CLEAR:gs_fcat.
    gs_fcat = VALUE #( col_pos   = 7
                       fieldname = 'WRBTR'
                       scrtext_l = 'WRBTR'
                       scrtext_m = 'WRBTR'
                       scrtext_s = 'WRBTR' ).
    APPEND gs_fcat TO gt_fcat.

    CLEAR:gs_fcat.
    gs_fcat = VALUE #( col_pos   = 8
                       fieldname = 'BLDAT'
                       scrtext_l = 'BLDAT'
                       scrtext_m = 'BLDAT'
                       scrtext_s = 'BLDAT' ).
    APPEND gs_fcat TO gt_fcat.
  ENDMETHOD.

  METHOD set_layout.
    gs_layout = VALUE #( zebra      = abap_true
                         cwidth_opt = abap_true
                         col_opt    = abap_true ).
  ENDMETHOD.

  METHOD display_alv.
    IF go_alv_grid IS INITIAL.
      CREATE OBJECT go_container
        EXPORTING
          container_name = 'CC_ALV'.  " Name of the Screen CustCtrl Name to Link Container To
      CREATE OBJECT go_alv_grid
        EXPORTING
*         i_parent = go_container   " Parent Container
          i_parent = cl_gui_custom_container=>screen0.   " Parent Container
      CALL METHOD go_alv_grid->set_table_for_first_display
        EXPORTING
          is_layout       = gs_layout    " Layout
        CHANGING
          it_outtab       = gt_alv   " Output Table
          it_fieldcatalog = gt_fcat. " Field Catalog
    ENDIF.
  ENDMETHOD.
ENDCLASS.
