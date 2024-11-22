*&---------------------------------------------------------------------*
*& Include          ZGY_DEV_P017_LCL
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

    SELECT DISTINCT kna1~kunnr,
                    lfa1~lifnr,
                    kna1~name1,
                    kna1~name2,
                    lfa1~name1        AS lname1,
                    lfa1~name2        AS lname2,
                    bseg~pswsl,
                    bseg~koart,
                    SUM( bseg~wrbtr ) AS w,
                    substring( bkpf~bldat , 1 , 6 ) AS month,
                    CASE WHEN bseg~shkzg  = 'H' THEN SUM( bseg~wrbtr )     "H borç      pozitif
                         WHEN bseg~shkzg  = 'S' THEN SUM( - bseg~wrbtr )   "S alacak    negatif
                    END AS w2,
                    CASE WHEN bseg~koart = 'K' THEN lfa1~lifnr
                         WHEN bseg~koart = 'D' THEN kna1~kunnr
                    END  AS number,
                    CASE WHEN bseg~koart = 'K' THEN concat( concat( lfa1~name1, ' ' ),lfa1~name2 ) "K satıcı
                         WHEN bseg~koart = 'D' THEN concat( concat( kna1~name1, ' ' ),kna1~name2 ) "D müşteri
                    END  AS name,
                    bseg~shkzg,
                    bkpf~bldat
    FROM bseg
    LEFT JOIN kna1 ON bseg~kunnr EQ kna1~kunnr
    LEFT JOIN lfa1 ON bseg~lifnr EQ lfa1~lifnr
    LEFT JOIN bkpf ON bseg~belnr EQ bkpf~belnr
    WHERE lfa1~lifnr IN @so_lifnr
    AND   kna1~kunnr IN @so_kunnr
    AND   bkpf~bldat IN @so_bldat
    AND   bseg~koart IN ( 'D', 'K' )
    AND   bseg~shkzg IN ( 'H', 'S' )
    GROUP BY
    kna1~kunnr,lfa1~lifnr,kna1~name1,kna1~name2,lfa1~name1,
    lfa1~name2,bseg~pswsl,bseg~koart,bseg~shkzg,bkpf~bldat,bseg~wrbtr
    INTO TABLE @DATA(lt_tab).


    LOOP AT lt_tab ASSIGNING FIELD-SYMBOL(<lfs_tab>).
      gs_alv = VALUE #( BASE gs_alv
                             pswsl = <lfs_tab>-pswsl
                             spmon = <lfs_tab>-month
                             no    = <lfs_tab>-number
                             isim  = <lfs_tab>-name
                             wrbtr = <lfs_tab>-w2 ).
      COLLECT gs_alv INTO gt_alv.
    ENDLOOP.

    SORT gt_alv BY no spmon.

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

    DATA:lv_count TYPE int4 VALUE 4.

    CLEAR gs_fcat.
    gs_fcat =  VALUE #( col_pos   = 1
                        datatype  = 'CHAR'
                        intlen    = 10
                        fieldname = 'NO'
                        scrtext_s = 'KUNNR/LIFNR'
                        scrtext_m = 'KUNNR/LIFNR'
                        scrtext_l = 'KUNNR/LIFNR').

    APPEND gs_fcat TO gt_fcat.
    CLEAR gs_fcat.
    gs_fcat =  VALUE #( col_pos   = 2
                        datatype  = 'CHAR'
                        intlen    = 70
                        fieldname = 'ISIM'
                        scrtext_s = 'NAME1'
                        scrtext_m = 'NAME1'
                        scrtext_l = 'NAME1').

    APPEND gs_fcat TO gt_fcat.
    CLEAR gs_fcat.
    gs_fcat =  VALUE #( col_pos   = 3
                        fieldname = 'PSWSL'
                        scrtext_s = 'PSWSL'
                        scrtext_m = 'PSWSL'
                        scrtext_l = 'PSWSL').
    APPEND gs_fcat TO gt_fcat.

    DATA(lt_domain) = VALUE dd07v_tab( ).

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'ZGY_DO_0032'
        text           = 'X'
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_domain
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    CHECK gs_alv-spmon IS NOT INITIAL.
*    IF gs_alv-spmon IS NOT INITIAL.

    LOOP AT gt_alv INTO gs_alv GROUP BY ( spmon = gs_alv-spmon ) ASCENDING ASSIGNING FIELD-SYMBOL(<lfs_dat>).
      IF <lfs_dat> IS ASSIGNED.
        "YYYYMM

        READ TABLE lt_domain INTO DATA(ls_domain) WITH KEY domvalue_l = <lfs_dat>-spmon+4(2) BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          DATA(ay) = CONV string( ls_domain-ddtext ).
        ENDIF.
        CLEAR gs_fcat.
        gs_fcat = VALUE #(
                          col_pos   = lv_count
                          datatype  = 'CHAR'
                          intlen    = 20
                          fieldname = |{ ay }{ <lfs_dat>-spmon+0(4) }|
                          scrtext_s = |{ ay } { <lfs_dat>-spmon+0(4) }|
                          scrtext_m = |{ ay } { <lfs_dat>-spmon+0(4) }|
                          scrtext_l = |{ ay } { <lfs_dat>-spmon+0(4) }|
*                          cfieldname = 'WAERS'
                          ).
        APPEND gs_fcat TO gt_fcat.

        lv_count = lv_count + 1.
      ENDIF.
    ENDLOOP.
*    ENDIF.

    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = gt_fcat   " Field Catalog
      IMPORTING
        ep_table        = go_t_dynamic.   " Pointer to Dynamic Data Table

    ASSIGN go_t_dynamic->* TO <dyn_table>.

    CREATE DATA go_s_dynamic LIKE LINE OF <dyn_table>.
    ASSIGN go_s_dynamic->* TO <gfs_s_table>.

    LOOP AT gt_alv INTO DATA(ls_alv) GROUP BY ( no       = ls_alv-no
                                                pswsl    = ls_alv-pswsl
                                                isim     = ls_alv-isim )
      ASCENDING ASSIGNING FIELD-SYMBOL(<lfs_group>).

      APPEND INITIAL LINE TO <dyn_table> ASSIGNING <gfs_s_table>.

      IF <gfs_s_table> IS ASSIGNED.

        ASSIGN COMPONENT 'NO' OF STRUCTURE <gfs_s_table> TO <gfs>.
        IF <gfs> IS ASSIGNED.
          <gfs> = <lfs_group>-no.
          UNASSIGN <gfs>.
        ENDIF.

        ASSIGN COMPONENT 'PSWSL' OF STRUCTURE <gfs_s_table> TO <gfs>.
        IF <gfs> IS ASSIGNED.
          <gfs> = <lfs_group>-pswsl.
          UNASSIGN <gfs>.
        ENDIF.

        ASSIGN COMPONENT 'ISIM' OF STRUCTURE <gfs_s_table> TO <gfs>.
        IF <gfs> IS ASSIGNED.
          <gfs> = <lfs_group>-isim.
          UNASSIGN <gfs>.
        ENDIF.

      ENDIF.

      LOOP AT GROUP <lfs_group> ASSIGNING FIELD-SYMBOL(<lfs_s_group>) .

        READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = <lfs_s_group>-spmon+4(2) BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          ay = CONV string( ls_domain-ddtext ).
        ENDIF.
        DATA(lv_field) = |{ ay }{ <lfs_s_group>-spmon+0(4) }|.

        IF <gfs_s_table> IS ASSIGNED.
          ASSIGN COMPONENT lv_field OF STRUCTURE <gfs_s_table> TO <gfs>.
          IF <gfs> IS ASSIGNED.
            <gfs> = CONV #( <lfs_s_group>-wrbtr ).
            CONDENSE <gfs>.
            UNASSIGN <gfs>.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_layout.
    CLEAR:gs_layout.
    gs_layout = VALUE #( BASE gs_layout
                              zebra      = abap_true
                              cwidth_opt = abap_true
                              col_opt    = abap_true ).
  ENDMETHOD.

  METHOD display_alv.
    IF go_alv_grid IS INITIAL.
      CREATE OBJECT go_container
        EXPORTING
          container_name = 'CC_ALV'.   " Name of the Screen CustCtrl Name to Link Container To
      CREATE OBJECT go_alv_grid
        EXPORTING
*         i_parent = go_container.  " Parent Container
          i_parent = cl_gui_container=>screen0.  " Parent Container
      CALL METHOD go_alv_grid->set_table_for_first_display
        EXPORTING
          is_layout       = gs_layout   " Layout
        CHANGING
          it_outtab       = <dyn_table>   " Output Table
          it_fieldcatalog = gt_fcat.   " Field Catalog
    ELSE.
      CALL METHOD go_alv_grid->refresh_table_display.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
