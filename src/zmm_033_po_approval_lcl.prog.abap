*&---------------------------------------------------------------------*
*& Include          ZMM_033_PO_APPROVAL_LCL
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

    DATA:lt_items TYPE TABLE OF gty_app,
         lt_app   TYPE TABLE OF gty_app,
         lt_tabx  TYPE TABLE OF gty_app,
         ls_app   TYPE gty_app,
         ls_tabx  TYPE gty_app.

    DATA:lv_total TYPE decfloat34 , "i ? BWERT ?
         lv_tot   TYPE decfloat34 . " BWERT

    DATA:lt_zlog TYPE TABLE OF zmm_033_t_log,
         ls_zlog TYPE  zmm_033_t_log.

    DATA:lv_mail_flag TYPE xfeld.

    DATA:lv_content     TYPE string,
         lv_subject     TYPE so_obj_des,
         lt_recipients  TYPE bcsy_smtpa,
         lt_pdf_name    TYPE zabap_001_tt_fpname,
         lt_pdf_nast    TYPE zabap_001_tt_fpname,
         lv_tabname     TYPE tabname,
         ls_nast        TYPE nast,
         lv_return      TYPE string,
         lv_return_type TYPE char1.

*********************************************
    REFRESH:lt_items,lt_app,lt_zlog.
    CLEAR:ls_app,ls_zlog.
*********************************************

    SELECT objectid,
           objectclas,
           udate
    FROM cdhdr
    WHERE objectclas = 'EINKBELEG'
    AND   udate      = @sy-datum
    INTO TABLE @DATA(lt_id).

    CHECK lt_id IS NOT INITIAL.

    SELECT DISTINCT lt~objectid,
                    cdpos~tabkey,
                    cdpos~fname,
                    cdpos~value_new,
                    cdpos~value_old
***    substring( cdpos~tabkey ,14,5 ) AS item  "14. den başla 5 tane al ( sas onayında tabkey de item yer almıyor
    FROM cdpos
    INNER JOIN @lt_id AS lt ON cdpos~objectid = lt~objectid
    WHERE fname = 'FRGKE'
    AND   value_new = 'G'
    AND   value_old <> 'G'
    ORDER BY lt~objectid, cdpos~tabkey
    INTO TABLE @DATA(lt_frgke).

    CHECK lt_frgke IS NOT INITIAL.


    SELECT DISTINCT ekko~aedat,
                    ekko~ernam,
                    lfa1~name1,
                    ekpo~ebeln,
                    ekpo~ebelp,
                    ekpo~matnr,
                    CASE WHEN ekpo~matnr             IS INITIAL THEN ekpo~txz01
**                         WHEN mara~zz1_uzuntanim_prd IS INITIAL THEN makt~maktx
**                         ELSE mara~zz1_uzuntanim_prd
            ELSE makt~maktx
                    END AS maktx,
                    ekpo~matkl,
                    ekpo~menge,
                    ekpo~meins,
                    ekpo~netpr,
                    ekko~waers,
                    t001w~werks,
                    t001w~name1 AS name1_werks,
                    t001l~lgort,
                    t001l~lgobe,
                    ekpo~netwr,
                    ekko~frgke,
                    ekko~zterm,
                    ekpo~txz01,
                    eket~eindt,
                    @sy-datum  AS datumx,
                    eban~ernam AS ernam_pr,
                    eban~banfn
    FROM ekko
    INNER JOIN ekpo ON ekko~ebeln = ekpo~ebeln
    INNER JOIN eket ON eket~ebeln = ekpo~ebeln
                   AND eket~ebelp = ekpo~ebelp
    LEFT JOIN eban  ON ekpo~banfn = eban~banfn
                   AND ekpo~bnfpo = eban~bnfpo
    INNER JOIN @lt_frgke AS f ON ekko~ebeln = f~objectid
                             AND ekko~frgke = f~value_new
    INNER JOIN lfa1 ON ekko~lifnr = lfa1~lifnr
    LEFT JOIN t001w ON ekpo~werks = t001w~werks
    LEFT JOIN t001l ON ekpo~lgort = t001l~lgort AND ekpo~werks = t001l~werks
    LEFT JOIN mara  ON  ekpo~matnr = mara~matnr
    LEFT JOIN makt  ON ekpo~matnr = makt~matnr  AND makt~spras = @sy-langu
    WHERE ekko~bstyp = 'F'
    ORDER BY ekpo~ebeln,ekpo~ebelp
    INTO CORRESPONDING FIELDS OF TABLE @lt_items.

    CHECK lt_items IS NOT INITIAL.

    SELECT log~ebeln,
           log~ebelp,
           log~frgke,
           uname,
           datum,
           uzeit
    FROM zmm_033_t_log AS log
    INNER JOIN @lt_items AS check ON log~ebeln = check~ebeln
                                 AND log~ebelp = check~ebelp
      AND log~frgke = check~frgke
    INTO TABLE @DATA(lt_log).

    IF sy-subrc IS INITIAL.
      LOOP AT lt_log ASSIGNING FIELD-SYMBOL(<lfs_log>).
        DELETE lt_items WHERE ebeln = <lfs_log>-ebeln
                          AND ebelp = <lfs_log>-ebelp
                          AND frgke = <lfs_log>-frgke.
      ENDLOOP.
    ENDIF.


    CHECK lt_items IS NOT INITIAL.

    SELECT data~zterm,
           t052u~text1
    FROM t052u
    INNER JOIN @lt_items AS data ON data~zterm = t052u~zterm
    WHERE t052u~spras = @sy-langu
    INTO TABLE @DATA(lt_extra).


    LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<lfs_item>).

      IF ( <lfs_item>-frgke EQ 'G' ) .

        lv_mail_flag = abap_true.

        CLEAR:ls_app,
              lv_tot.

        LOOP AT lt_extra ASSIGNING FIELD-SYMBOL(<lfs_extra>) WHERE zterm = <lfs_item>-zterm.
          <lfs_item> = VALUE #( BASE <lfs_item>
                             text1 = <lfs_extra>-text1
                             ).
        ENDLOOP.

        lv_tot = <lfs_item>-netwr.
**        lv_total += <lfs_item>-netwr.

        DATA:lv_totc TYPE char40,
             lv_totx TYPE char40,
             lv_toty TYPE char40,
             lv_totz TYPE char40.

        CALL FUNCTION 'HRCM_AMOUNT_TO_STRING_CONVERT'
          EXPORTING
            betrg  = lv_tot
          IMPORTING
            string = lv_totc.

        CONDENSE lv_totc.
        SPLIT lv_totc AT ',' INTO lv_totx lv_toty.
        SPLIT lv_toty AT '0' INTO lv_totz lv_toty.

        IF lv_totz IS INITIAL.
          lv_totc = |{ lv_totx }|.
        ELSE.
          lv_totc = |{ lv_totx },{ lv_totz }|.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            input          = <lfs_item>-meins
            language       = sy-langu
          IMPORTING
            output         = <lfs_item>-meins
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.

        DATA:lv_menge TYPE char20,
             lv_a     TYPE char20,
             lv_b     TYPE char20,
             lv_c     TYPE char20.

        lv_menge = CONV #( <lfs_item>-menge ).

        CALL FUNCTION 'HRCM_AMOUNT_TO_STRING_CONVERT'
          EXPORTING
            betrg  = <lfs_item>-menge
          IMPORTING
            string = lv_menge.

        CONDENSE lv_menge.
        SPLIT lv_menge AT ',' INTO lv_a lv_b.
        SPLIT lv_b AT '0' INTO lv_c lv_b.

        IF lv_c IS INITIAL.
          lv_menge = |{ lv_a }|.
        ELSE.
          lv_menge = |{ lv_a },{ lv_c }|.
        ENDIF.

***        lv_menge = |{ lv_menge } { <lfs_item>-meins }|.
        lv_menge = lv_menge .

        DATA:lv_netpr TYPE char20,
             lv_x     TYPE char20,
             lv_y     TYPE char20,
             lv_z     TYPE char20.
        CALL FUNCTION 'HRCM_AMOUNT_TO_STRING_CONVERT'
          EXPORTING
            betrg  = <lfs_item>-netpr
          IMPORTING
            string = lv_netpr.

        CONDENSE lv_netpr.
        SPLIT lv_netpr AT ',' INTO lv_x lv_y.
        SPLIT lv_y AT '0' INTO lv_z lv_y.

        IF lv_z IS INITIAL.
          lv_netpr = |{ lv_x }|.
        ELSE.
          lv_netpr = |{ lv_x },{ lv_z }|.
        ENDIF.

        ls_app = CORRESPONDING #( <lfs_item> ).

        ls_app = VALUE #( BASE ls_app
                               lv_totc   = lv_totc
                               lv_menge  = lv_menge
                               lv_netpr  = lv_netpr
                               lv_nwerks = |{ ls_app-name1_werks }({ ls_app-werks })|
                               lv_llgort = |{ ls_app-lgobe }({ ls_app-lgort })|
                               lv_tot    = lv_tot ).
        APPEND ls_app TO lt_app.
      ELSE.
        lv_mail_flag = abap_false.

      ENDIF.
    ENDLOOP.

    "çoklu belge için
    lt_tabx = lt_app.
****************

    LOOP AT lt_app ASSIGNING FIELD-SYMBOL(<lfs_sum>) GROUP BY ( ebeln = <lfs_sum>-ebeln )
                                                    ASCENDING ASSIGNING FIELD-SYMBOL(<lfs_summa>).
      CLEAR:lv_total.
      LOOP AT GROUP <lfs_summa> ASSIGNING FIELD-SYMBOL(<lfs_total>) WHERE ebeln = <lfs_summa>-ebeln.
        lv_total += <lfs_total>-netwr.
        <lfs_total>-lv_total = lv_total.

        DATA:lv_total_c TYPE char40,
             lv_total_x TYPE char40,
             lv_total_y TYPE char40,
             lv_total_z TYPE char40.

        CALL FUNCTION 'HRCM_AMOUNT_TO_STRING_CONVERT'
          EXPORTING
            betrg  = lv_total
          IMPORTING
            string = lv_total_c.

        CONDENSE lv_total_c.
        SPLIT lv_total_c AT ',' INTO lv_total_x lv_total_y.
        SPLIT lv_total_y AT '0' INTO lv_total_z lv_total_y.

        IF lv_total_z IS INITIAL.
          lv_total_c = |{ lv_total_x }|.
        ELSE.
          lv_total_c = |{ lv_total_x },{ lv_total_z }|.
        ENDIF.

        <lfs_total>-lv_totalc = lv_total_c.

        ls_tabx-lv_totalc = lv_total_c.
        MODIFY lt_tabx FROM ls_tabx TRANSPORTING lv_totalc WHERE ebeln = <lfs_total>-ebeln.
      ENDLOOP.

    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM lt_tabx COMPARING ebeln.

    IF lv_mail_flag EQ abap_true.

      LOOP AT lt_tabx ASSIGNING FIELD-SYMBOL(<lfs_po>).

        "YYYYMMDD   -> DD.MM.YYYY
        DATA(lv_aedat) = |{ <lfs_po>-aedat+6(2) }.{ <lfs_po>-aedat+4(2) }.{ <lfs_po>-aedat+0(4) }|.

        DATA(lv_datumx) = |{ <lfs_po>-datumx+6(2) }.{ <lfs_po>-datumx+4(2) }.{ <lfs_po>-datumx+0(4) }|.

        DATA:lt_smtp   TYPE TABLE OF bapiadsmtp,
             ls_smtp   TYPE bapiadsmtp,
             lt_return TYPE TABLE OF bapiret2,
             ls_return TYPE bapiret2.

        CALL FUNCTION 'BAPI_USER_GET_DETAIL'
          EXPORTING
            username = <lfs_po>-ernam_pr
          TABLES
            return   = lt_return
            addsmtp  = lt_smtp.

        LOOP AT lt_smtp INTO ls_smtp.
          DATA(lv_recipient) = ls_smtp-e_mail.
        ENDLOOP.

        lt_recipients = VALUE #( ( lv_recipient ) ).

        lv_subject = TEXT-000.

        DATA:lines TYPE TABLE OF  tline.
        CLEAR:lv_content,lines.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            client   = sy-mandt
            id       = 'ST'
            language = sy-langu
            name     = 'ZMM_033_STR_PO_APPROVAL_PR'       "'ZMM_033_STR_SAS_ONAY'
            object   = 'TEXT'
          TABLES
            lines    = lines.

        LOOP AT lines ASSIGNING FIELD-SYMBOL(<lfs_lines>).
          lv_content &&= <lfs_lines>-tdline.
        ENDLOOP.

**        REPLACE ALL OCCURRENCES OF '<AEDAT>' IN lv_content WITH lv_aedat.
        REPLACE ALL OCCURRENCES OF '<BANFN>'  IN lv_content  WITH <lfs_po>-banfn.
        REPLACE ALL OCCURRENCES OF '<EBELN>'  IN lv_content  WITH <lfs_po>-ebeln.
        REPLACE ALL OCCURRENCES OF '<DATUMX>' IN lv_content  WITH lv_datumx.
        REPLACE ALL OCCURRENCES OF '<ERNAM>'  IN lv_content  WITH <lfs_po>-ernam.
        REPLACE ALL OCCURRENCES OF '<NAME1>'  IN lv_content  WITH <lfs_po>-name1.
**        REPLACE ALL OCCURRENCES OF '<TEXT1>'  IN lv_content  WITH <lfs_po>-text1.
**        REPLACE ALL OCCURRENCES OF '<TOTAL>'  IN lv_content  WITH <lfs_po>-lv_totalc."CONV string( lv_total_c ).

        DATA:lv_splitxx TYPE string,
             lv_splityy TYPE string,
             lv_splitzz TYPE string,
             lv_split   TYPE string.

**        SPLIT lv_content AT 'Toplam Tutar </th></tr>' INTO lv_split lv_splityy.
        SPLIT lv_content AT 'Teslimat Tarihi </th></tr>' INTO lv_split lv_splityy.
**        lv_split &&= |Toplam Tutar </th></tr>|.
        lv_split &&= |Teslimat Tarihi </th></tr>|.

        LOOP AT lt_app INTO ls_app WHERE ebeln = <lfs_po>-ebeln.

**          SPLIT lv_content AT 'Toplam Tutar </th></tr>' INTO lv_splitxx lv_splityy.
          SPLIT lv_content AT 'Teslimat Tarihi </th></tr>' INTO lv_splitxx lv_splityy.
          SPLIT lv_splityy AT '</table>' INTO lv_splityy lv_splitzz.

          DATA(lv_eindt) =  |{ ls_app-eindt+6(2) }.{ ls_app-eindt+4(2) }.{ ls_app-eindt+0(4) }|.

**          REPLACE ALL OCCURRENCES OF '<EBELN>' IN lv_splityy WITH ls_app-ebeln.
**          REPLACE ALL OCCURRENCES OF '<EBELP>' IN lv_splityy WITH ls_app-ebelp.
          REPLACE ALL OCCURRENCES OF '<MATNR>' IN lv_splityy WITH ls_app-matnr.
          REPLACE ALL OCCURRENCES OF '<MAKTX>' IN lv_splityy WITH ls_app-maktx.
**          REPLACE ALL OCCURRENCES OF '<MATKL>' IN lv_splityy WITH ls_app-matkl.
          REPLACE ALL OCCURRENCES OF '<MENGE>' IN lv_splityy WITH CONV string( ls_app-lv_menge ).
          REPLACE ALL OCCURRENCES OF '<MEINS>' IN lv_splityy WITH CONV string( ls_app-meins ).
**          REPLACE ALL OCCURRENCES OF '<NETPR>' IN lv_splityy WITH CONV string( ls_app-lv_netpr ).
**          REPLACE ALL OCCURRENCES OF '<WAERS>' IN lv_splityy WITH ls_app-waers.
**          REPLACE ALL OCCURRENCES OF '<NAME1_WERKS>' IN lv_splityy WITH ls_app-lv_nwerks.
          REPLACE ALL OCCURRENCES OF '<LGOBE>' IN lv_splityy WITH ls_app-lv_llgort.
          REPLACE ALL OCCURRENCES OF '<EINDT>' IN lv_splityy WITH lv_eindt.
**          REPLACE ALL OCCURRENCES OF '<TOT>'   IN lv_splityy WITH CONV string( ls_app-lv_totc ).

          lv_split &&= |{ lv_splityy }|.
        ENDLOOP.

        lv_split &&= |</table>{ lv_splitzz }|.

        lv_content = lv_split.


*        CLEAR:ls_nast.
*        lt_pdf_nast = VALUE #( ( 'ZMM_009_AF_PO_FORM' ) ).
*        ls_nast-objky = <lfs_po>-ebeln.
*        ls_nast-spras = sy-langu.

        IF lt_recipients IS NOT INITIAL.

          CALL FUNCTION 'ZABAP_001_FM_SENDMAIL' "IN BACKGROUND TASK
****          CALL FUNCTION 'ZABAP_001_FM_SENDMAIL_NAST'
            EXPORTING
              iv_content       = lv_content
              iv_subject       = lv_subject
              it_recipients    = lt_recipients
              it_pdf_name      = lt_pdf_name
              iv_tab_for_excel = lv_tabname
***              it_pdf_nast      = lt_pdf_nast
***              is_nast          = ls_nast
            IMPORTING
              ev_return        = lv_return
              ev_return_type   = lv_return_type.


          IF lv_return_type EQ 'S'.
            LOOP AT lt_app ASSIGNING FIELD-SYMBOL(<lfs_app_l>) WHERE ebeln = <lfs_po>-ebeln.
              ls_zlog = VALUE #( BASE ls_zlog
                                 ebeln = <lfs_app_l>-ebeln
                                 ebelp = <lfs_app_l>-ebelp
                                 frgke = <lfs_app_l>-frgke
                                 uname = sy-uname
                                 datum = sy-datum
                                 uzeit = sy-uzeit ).
              APPEND ls_zlog TO lt_zlog.
            ENDLOOP.

            MODIFY zmm_033_t_log FROM TABLE lt_zlog.
          ENDIF.

        ENDIF.

      ENDLOOP.

      LOOP AT lt_tabx ASSIGNING FIELD-SYMBOL(<lfs_poo>).

*        DATA:lt_smtp   TYPE TABLE OF bapiadsmtp,
*             ls_smtp   TYPE bapiadsmtp,
*             lt_return TYPE TABLE OF bapiret2,
*             ls_return TYPE bapiret2.
        REFRESH:lt_smtp.

        CALL FUNCTION 'BAPI_USER_GET_DETAIL'
          EXPORTING
            username = <lfs_poo>-ernam
          TABLES
            return   = lt_return
            addsmtp  = lt_smtp.

        LOOP AT lt_smtp INTO ls_smtp.
          lv_recipient = ls_smtp-e_mail.
        ENDLOOP.

        lt_recipients = VALUE #( ( lv_recipient ) ).

        lv_subject = TEXT-000.

        CLEAR:lines,lv_content.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            client   = sy-mandt
            id       = 'ST'
            language = sy-langu
            name     = 'ZMM_033_STR_PO_APPROVAL'       "'ZMM_033_STR_SAS_ONAY'
            object   = 'TEXT'
          TABLES
            lines    = lines.

        LOOP AT lines ASSIGNING FIELD-SYMBOL(<lfs_liness>).
          lv_content &&= <lfs_liness>-tdline.
        ENDLOOP.


        REPLACE ALL OCCURRENCES OF '<EBELN>'  IN lv_content  WITH <lfs_poo>-ebeln.
        REPLACE ALL OCCURRENCES OF '<NAME1>'  IN lv_content  WITH <lfs_poo>-name1.


        CLEAR:ls_nast.
        lt_pdf_nast = VALUE #( ( 'ZMM_009_AF_PO_FORM' ) ).
        ls_nast-objky = <lfs_poo>-ebeln.
        ls_nast-spras = sy-langu.

        IF lt_recipients IS NOT INITIAL.

****************          CALL FUNCTION 'ZABAP_001_FM_SENDMAIL' "IN BACKGROUND TASK

          CALL FUNCTION 'ZABAP_001_FM_SENDMAIL_NAST'
            EXPORTING
              iv_content       = lv_content
              iv_subject       = lv_subject
              it_recipients    = lt_recipients
              it_pdf_name      = lt_pdf_name
              iv_tab_for_excel = lv_tabname
              it_pdf_nast      = lt_pdf_nast
              is_nast          = ls_nast
            IMPORTING
              ev_return        = lv_return
              ev_return_type   = lv_return_type.


          IF lv_return_type EQ 'S'.
            LOOP AT lt_app ASSIGNING FIELD-SYMBOL(<lfs_app_lo>) WHERE ebeln = <lfs_poo>-ebeln.
              ls_zlog = VALUE #( BASE ls_zlog
                                 ebeln = <lfs_app_lo>-ebeln
                                 ebelp = <lfs_app_lo>-ebelp
                                 frgke = <lfs_app_lo>-frgke
                                 uname = sy-uname
                                 datum = sy-datum
                                 uzeit = sy-uzeit ).
              APPEND ls_zlog TO lt_zlog.
            ENDLOOP.

            MODIFY zmm_033_t_log FROM TABLE lt_zlog.
          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDIF.

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
*    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
*      EXPORTING
*        i_structure_name = ''
*      CHANGING
*        ct_fieldcat      = gt_fcat.
  ENDMETHOD.

  METHOD set_layout.
    gs_layout = VALUE #( zebra      = abap_true
                         cwidth_opt = abap_true
                         col_opt    = abap_true
                         sel_mode   = 'B' ).
  ENDMETHOD.

  METHOD display_alv.
    IF go_alv_grid IS INITIAL.
*      CREATE OBJECT go_container
*        EXPORTING
*          container_name = 'CC_ALV'.
*
*      CREATE OBJECT go_alv_grid
*        EXPORTING
**         i_parent = go_container.
*          i_parent = cl_gui_custom_container=>screen0.
*      CALL METHOD go_alv_grid->set_table_for_first_display
*        EXPORTING
*          is_layout       = gs_layout   " Layout
*        CHANGING
*          it_outtab       = gt_tab   " Output Table
*          it_fieldcatalog = gt_fcat.   " Field Catalog
*    ELSE.
*      CALL METHOD go_alv_grid->refresh_table_display.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
