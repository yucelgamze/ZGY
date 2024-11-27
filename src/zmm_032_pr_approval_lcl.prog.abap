*&---------------------------------------------------------------------*
*& Include          ZMM_032_PR_APPROVAL_LCL
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

    DATA:lv_total TYPE i,
         lv_tot   TYPE i.

    DATA:lt_zlog TYPE TABLE OF zmm_032_t_log,
         ls_zlog TYPE  zmm_032_t_log.

    DATA:lv_mail_flag TYPE xfeld.

    DATA:lv_content     TYPE string,
         lv_subject     TYPE so_obj_des,
         lt_recipients  TYPE bcsy_smtpa,
         lt_pdf_name    TYPE zabap_001_tt_fpname,
         lv_tabname     TYPE tabname,
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
    WHERE objectclas = 'BANF'
    AND   udate      = @sy-datum
    INTO TABLE @DATA(lt_id).

    CHECK lt_id IS NOT INITIAL.

    SELECT DISTINCT lt~objectid,
                    cdpos~tabkey,
                    cdpos~fname,
                    cdpos~value_new,
                    cdpos~value_old,
                    substring( cdpos~tabkey ,14,5 ) AS item  "14. den başla 5 tane al
    FROM cdpos
    INNER JOIN @lt_id AS lt ON cdpos~objectid = lt~objectid
    WHERE fname     = 'BANPR'
    AND   value_new = '05'
    AND   value_old <> '05'
    ORDER BY lt~objectid, cdpos~tabkey
    INTO TABLE @DATA(lt_banpr).

*    LOOP AT lt_banpr ASSIGNING FIELD-SYMBOL(<lfs_r>) .
*      gr_tabkey = VALUE #( sign   = 'I'
*                           option = 'CP'
*                           low    = |{ <lfs_r>-tabkey+13(5) }| ).
*      APPEND gr_tabkey TO gr_tabkey.
*    ENDLOOP.

    CHECK lt_banpr IS NOT INITIAL.

    SELECT DISTINCT eban~banfn,
                    eban~bnfpo,
                    eban~banpr,
                    eban~badat,
                    eban~frgdt,
                    eban~ernam,
                    eban~matnr,
                    eban~matkl,
                    eban~preis,
                    eban~peinh,
                    eban~menge,
                    eban~meins,
                    eban~waers,
                    eban~txz01,
                    eban~ekgrp,
                    zmm_032_t_ss~zperson,
*                    concat( concat( eban~ekgrp, ' (' ), concat( zmm_032_t_ss~zperson, ')' ) ) AS sags
                    zmm_032_t_ss~zperson AS sags
    FROM eban
    INNER JOIN @lt_banpr AS b ON eban~banfn = b~objectid
                             AND eban~banpr = b~value_new
                             AND eban~bnfpo = b~item
    LEFT JOIN zmm_032_t_ss    ON eban~ekgrp = zmm_032_t_ss~ekgrp
    ORDER BY banfn,bnfpo
    INTO CORRESPONDING FIELDS OF TABLE @lt_items.

    CHECK lt_items IS NOT INITIAL.

    SELECT log~banfn,
           log~bnfpo,
           log~banpr,
           uname,
           datum,
           uzeit
    FROM zmm_032_t_log AS log
    INNER JOIN @lt_items AS check ON log~banfn = check~banfn
                                 AND log~bnfpo = check~bnfpo
      AND log~banpr = check~banpr
    INTO TABLE @DATA(lt_log).

    IF sy-subrc IS INITIAL.
      LOOP AT lt_log ASSIGNING FIELD-SYMBOL(<lfs_log>).
        DELETE lt_items WHERE banfn = <lfs_log>-banfn
                          AND bnfpo = <lfs_log>-bnfpo
                          AND banpr = <lfs_log>-banpr.
      ENDLOOP.
    ENDIF.

    CHECK lt_items IS NOT INITIAL.

    SELECT
**    makt~maktx,
          mara~mtart,
          data~matnr,
          CASE WHEN data~matnr             IS INITIAL THEN data~txz01
***               WHEN mara~zz1_uzuntanim_prd IS INITIAL THEN makt~maktx
***               ELSE mara~zz1_uzuntanim_prd
      ELSE makt~maktx
            END AS maktx,
          t023t~wgbez,
          t023t~matkl
    FROM @lt_items AS data
    LEFT JOIN mara  ON data~matnr  = mara~matnr
    LEFT JOIN makt  ON mara~matnr  = makt~matnr  AND makt~spras  = @sy-langu
    LEFT JOIN t023t ON t023t~matkl = data~matkl  AND t023t~spras = @sy-langu
    INTO TABLE @DATA(lt_extra).


    LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<lfs_item>).

      IF ( <lfs_item>-banpr EQ '05' ) .

        lv_mail_flag = abap_true.

        CLEAR:ls_app,
              lv_tot.

        LOOP AT lt_extra ASSIGNING FIELD-SYMBOL(<lfs_extra>) WHERE matnr = <lfs_item>-matnr
                                                             AND   matkl = <lfs_item>-matkl.
          <lfs_item> = VALUE #( BASE <lfs_item>
                                  maktx = <lfs_extra>-maktx
                                  mtart = <lfs_extra>-mtart
                                  matkl = <lfs_extra>-matkl
                                  wgbez = <lfs_extra>-wgbez ).
        ENDLOOP.

        lv_tot = ( <lfs_item>-preis / <lfs_item>-peinh ) * <lfs_item>-menge.
***        lv_total += ( <lfs_item>-preis / <lfs_item>-peinh ) * <lfs_item>-menge.

        DATA:lv_totc TYPE char20,
             lv_totx TYPE char20,
             lv_toty TYPE char20,
             lv_totz TYPE char20.

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

**        lv_menge = |{ lv_menge } { <lfs_item>-meins }|.
        lv_menge = lv_menge .

        DATA:lv_preis TYPE char20,
             lv_x     TYPE char20,
             lv_y     TYPE char20,
             lv_z     TYPE char20.
        CALL FUNCTION 'HRCM_AMOUNT_TO_STRING_CONVERT'
          EXPORTING
            betrg  = <lfs_item>-preis
          IMPORTING
            string = lv_preis.

        CONDENSE lv_preis.
        SPLIT lv_preis AT ',' INTO lv_x lv_y.
        SPLIT lv_y AT '0' INTO lv_z lv_y.

        IF lv_z IS INITIAL.
          lv_preis = |{ lv_x }|.
        ELSE.
          lv_preis = |{ lv_x },{ lv_z }|.
        ENDIF.

        ls_app = CORRESPONDING #( <lfs_item> ).

        ls_app = VALUE #( BASE ls_app
                               lv_totc  = lv_totc
                               lv_menge = lv_menge
                               lv_preis = lv_preis
                               lv_tot   = lv_tot ).
        APPEND ls_app TO lt_app.
      ELSE.
        lv_mail_flag = abap_false.
      ENDIF.

    ENDLOOP.

    "çoklu belge için
    lt_tabx = lt_app.
****************
    LOOP AT lt_app ASSIGNING FIELD-SYMBOL(<lfs_sum>) GROUP BY ( banfn = <lfs_sum>-banfn )
                                                     ASCENDING ASSIGNING FIELD-SYMBOL(<lfs_summa>).
      CLEAR:lv_total.
      LOOP AT GROUP <lfs_summa> ASSIGNING FIELD-SYMBOL(<lfs_total>) WHERE banfn = <lfs_summa>-banfn.

        lv_total += <lfs_total>-lv_tot.
        <lfs_total>-lv_total = lv_total.

        DATA:lv_total_c TYPE char20,
             lv_total_x TYPE char20,
             lv_total_y TYPE char20,
             lv_total_z TYPE char20.

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
        MODIFY lt_tabx FROM ls_tabx TRANSPORTING lv_totalc WHERE banfn = <lfs_total>-banfn.

      ENDLOOP.
    ENDLOOP.


*    SORT DESCENDING BY lt_tabx.
    DELETE ADJACENT DUPLICATES FROM lt_tabx COMPARING banfn.


    IF lv_mail_flag EQ abap_true.

      LOOP AT lt_tabx ASSIGNING FIELD-SYMBOL(<lfs_pr>).  "çoklu belge onayı durumu

        "YYYYMMDD   -> DD.MM.YYYY
        DATA(lv_badat) = |{ <lfs_pr>-badat+6(2) }.{ <lfs_pr>-badat+4(2) }.{ <lfs_pr>-badat+0(4) }|.

        DATA(lv_frgdt) = |{ <lfs_pr>-frgdt+6(2) }.{ <lfs_pr>-frgdt+4(2) }.{ <lfs_pr>-frgdt+0(4) }|.


        DATA:lt_smtp   TYPE TABLE OF bapiadsmtp,
             ls_smtp   TYPE bapiadsmtp,
             lt_return TYPE TABLE OF bapiret2,
             ls_return TYPE bapiret2.

        CALL FUNCTION 'BAPI_USER_GET_DETAIL'
          EXPORTING
            username = <lfs_pr>-ernam
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
            name     = 'ZMM_032_STR_PR_APPROVAL'           "     'ZMM_000_STR_ONAY'
            object   = 'TEXT'
          TABLES
            lines    = lines.

        LOOP AT lines ASSIGNING FIELD-SYMBOL(<lfs_lines>).
          lv_content &&= <lfs_lines>-tdline.
        ENDLOOP.

        REPLACE ALL OCCURRENCES OF '<BANFN>' IN lv_content WITH <lfs_pr>-banfn.
        REPLACE ALL OCCURRENCES OF '<BADAT>' IN lv_content WITH lv_badat.
        REPLACE ALL OCCURRENCES OF '<FRGDT>' IN lv_content WITH lv_frgdt.
        REPLACE ALL OCCURRENCES OF '<SAGS>'  IN lv_content WITH <lfs_pr>-sags.
**        REPLACE ALL OCCURRENCES OF '<ERNAM>' IN lv_content WITH <lfs_pr>-ernam.
**        REPLACE ALL OCCURRENCES OF '<BANFN>' IN lv_content WITH <lfs_pr>-banfn.
**        REPLACE ALL OCCURRENCES OF '<TOTAL>' IN lv_content WITH <lfs_pr>-lv_totalc."CONV string( lv_total_c ).

        DATA:lv_splitxx TYPE string,
             lv_splityy TYPE string,
             lv_splitzz TYPE string,
             lv_split   TYPE string.

**        SPLIT lv_content AT 'Toplam Tutar </th></tr>' INTO lv_split lv_splityy.
**        lv_split &&= |Toplam Tutar </th></tr>|.

        SPLIT lv_content AT 'Ölçü Birimi </th></tr>' INTO lv_split lv_splityy.
        lv_split &&= |Ölçü Birimi </th></tr>|.

        LOOP AT lt_app INTO ls_app WHERE banfn EQ <lfs_pr>-banfn.

**          SPLIT lv_content AT 'Toplam Tutar </th></tr>' INTO lv_splitxx lv_splityy.
          SPLIT lv_content AT 'Ölçü Birimi </th></tr>' INTO lv_splitxx lv_splityy.
          SPLIT lv_splityy AT '</table>' INTO lv_splityy lv_splitzz.

**          REPLACE ALL OCCURRENCES OF '<MTART>' IN lv_splityy WITH ls_app-mtart.
          REPLACE ALL OCCURRENCES OF '<MATNR>' IN lv_splityy WITH ls_app-matnr.
          REPLACE ALL OCCURRENCES OF '<MAKTX>' IN lv_splityy WITH ls_app-maktx.
**          REPLACE ALL OCCURRENCES OF '<WGBEZ>' IN lv_splityy WITH ls_app-wgbez.
**          REPLACE ALL OCCURRENCES OF '<PREIS>' IN lv_splityy WITH CONV string( ls_app-lv_preis ).
          REPLACE ALL OCCURRENCES OF '<MENGE>' IN lv_splityy WITH CONV string( ls_app-lv_menge ).
          REPLACE ALL OCCURRENCES OF '<MEINS>' IN lv_splityy WITH CONV string( ls_app-meins ).
**          REPLACE ALL OCCURRENCES OF '<WAERS>' IN lv_splityy WITH ls_app-waers.
**          REPLACE ALL OCCURRENCES OF '<TOT>'   IN lv_splityy WITH CONV string( ls_app-lv_totc ).

          lv_split &&= |{ lv_splityy }|.
        ENDLOOP.

        lv_split &&= |</table>{ lv_splitzz }|.

        lv_content = lv_split.

        IF lt_recipients IS NOT INITIAL.

          CALL FUNCTION 'ZABAP_001_FM_SENDMAIL' """IN BACKGROUND TASK
            EXPORTING
              iv_content       = lv_content
              iv_subject       = lv_subject
              it_recipients    = lt_recipients
              it_pdf_name      = lt_pdf_name
              iv_tab_for_excel = lv_tabname
            IMPORTING
              ev_return        = lv_return
              ev_return_type   = lv_return_type.


          IF lv_return_type EQ 'S'.
            CLEAR:ls_zlog. REFRESH:lt_zlog.
            LOOP AT lt_app ASSIGNING FIELD-SYMBOL(<lfs_app_l>) WHERE banfn = <lfs_pr>-banfn
                                                               .
              ls_zlog = VALUE #( BASE ls_zlog
                                 banfn = <lfs_app_l>-banfn
                                 bnfpo = <lfs_app_l>-bnfpo
                                 banpr = <lfs_app_l>-banpr
                                 uname = sy-uname
                                 datum = sy-datum
                                 uzeit = sy-uzeit ).
              APPEND ls_zlog TO lt_zlog.
            ENDLOOP.

            MODIFY zmm_032_t_log FROM TABLE lt_zlog.
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
