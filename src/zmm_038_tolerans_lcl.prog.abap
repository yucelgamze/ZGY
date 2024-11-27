*&---------------------------------------------------------------------*
*& Include          ZMM_038_TOLERANS_LCL
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

    DATA:lt_tabx TYPE TABLE OF gty_tab,
         ls_tabx TYPE gty_tab.

    DATA:lt_zlog TYPE TABLE OF zmm_038_t_log,
         ls_zlog TYPE zmm_038_t_log.

    DATA:lv_mail_flag TYPE xfeld.

    DATA:lv_content     TYPE string,
         lv_subject     TYPE so_obj_des,
         lt_recipients  TYPE bcsy_smtpa,
         lt_pdf_name    TYPE zabap_001_tt_fpname,
         lv_tabname     TYPE tabname,
         lv_return      TYPE string,
         lv_return_type TYPE char1.

**************************************
    REFRESH:lt_zlog.
    CLEAR:ls_zlog.
**************************************

    SELECT objectid,
           objectclas,
           udate,
           changenr
    FROM cdhdr
    WHERE objectclas = 'EINKBELEG'
    AND   udate      = @sy-datum
    INTO TABLE @DATA(lt_id).

    CHECK lt_id IS NOT INITIAL.

    SELECT DISTINCT lt~objectid,
                    cdpos~tabkey,
                    substring( cdpos~tabkey ,14,5 ) AS item,  "14. den başla 5 tane al
                    cdpos~fname,
                    cdpos~value_new,
                    cdpos~value_old,
                    cdpos~changenr
    FROM cdpos
    INNER JOIN @lt_id AS lt ON cdpos~objectid = lt~objectid
                           AND cdpos~changenr = lt~changenr
    WHERE
     ( fname = 'UEBTO' OR  fname = 'UNTTO' )
    ORDER BY lt~objectid, cdpos~tabkey
    INTO TABLE @DATA(lt_changenr).


    CHECK lt_changenr IS NOT INITIAL.

    SELECT ekko~ernam,
           ekko~ebeln,
           ekpo~ebelp,
           ekpo~uebto,
           ekpo~untto,
           f~changenr
    FROM ekko
    INNER JOIN ekpo ON ekko~ebeln = ekpo~ebeln
    INNER JOIN @lt_changenr AS f ON ekko~ebeln = f~objectid
                                AND ekpo~ebelp = f~item
    WHERE ekko~bstyp = 'F'
    ORDER BY ekpo~ebeln,ekpo~ebelp
    INTO TABLE @DATA(lt_tab).

    DELETE ADJACENT DUPLICATES FROM lt_tab COMPARING changenr ebeln ebelp.

    CHECK lt_tab IS NOT INITIAL.

    SELECT log~ebeln,
           log~ebelp,
           log~changenr,
           uname,
           datum,
           uzeit
    FROM zmm_038_t_log AS log
    INNER JOIN @lt_tab AS check ON log~ebeln    = check~ebeln
                               AND log~ebelp    = check~ebelp
    AND log~changenr = check~changenr
    INTO TABLE @DATA(lt_log).

    IF sy-subrc IS INITIAL.
      LOOP AT lt_log ASSIGNING FIELD-SYMBOL(<lfs_log>).
        DELETE lt_tab WHERE ebeln = <lfs_log>-ebeln AND ebelp    = <lfs_log>-ebelp
                                                    AND ebelp    = <lfs_log>-ebelp
                                                    AND changenr = <lfs_log>-changenr.
      ENDLOOP.
    ENDIF.

    lt_tabx = CORRESPONDING #( lt_tab ).
    DELETE ADJACENT DUPLICATES FROM lt_tabx COMPARING ebeln.


    LOOP AT lt_tabx ASSIGNING FIELD-SYMBOL(<lfs_tab>).

      lv_subject = TEXT-000.

      DATA:lines TYPE TABLE OF  tline.

      CLEAR:lv_content,lines,ls_zlog.
      REFRESH:lt_zlog.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          client   = sy-mandt
          id       = 'ST'
          language = sy-langu
          name     = 'ZMM_000_STR_TOLERANS'
          object   = 'TEXT'
        TABLES
          lines    = lines.

      LOOP AT lines ASSIGNING FIELD-SYMBOL(<lfs_lines>).
        lv_content &&= <lfs_lines>-tdline.
      ENDLOOP.

      REPLACE ALL OCCURRENCES OF '<EBELN>' IN lv_content WITH <lfs_tab>-ebeln.

      DATA:lv_splitxx TYPE string,
           lv_splityy TYPE string,
           lv_splitzz TYPE string,
           lv_split   TYPE string.

      SPLIT lv_content AT 'Kalem No</th></tr>' INTO lv_split lv_splityy.
      lv_split &&= |Kalem No</th></tr>|.

      LOOP AT lt_tab INTO DATA(ls_tab) WHERE ebeln = <lfs_tab>-ebeln.

        SPLIT lv_content AT 'Kalem No</th></tr>' INTO lv_splitxx lv_splityy.
        SPLIT lv_splityy AT '</table>' INTO lv_splityy lv_splitzz.

        REPLACE ALL OCCURRENCES OF '<EBELN>' IN lv_splityy WITH ls_tab-ebeln.
        REPLACE ALL OCCURRENCES OF '<EBELP>' IN lv_splityy WITH ls_tab-ebelp.

        lv_split &&= |{ lv_splityy }|.
      ENDLOOP.

      lv_split &&= |</table>{ lv_splitzz }|.

      lv_content = lv_split.

      DATA:lt_smtp   TYPE TABLE OF bapiadsmtp,
           ls_smtp   TYPE bapiadsmtp,
           lt_return TYPE TABLE OF bapiret2,
           ls_return TYPE bapiret2.

      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
        EXPORTING
          username = <lfs_tab>-ernam
        TABLES
          return   = lt_return
          addsmtp  = lt_smtp.

      LOOP AT lt_smtp INTO ls_smtp.
        DATA(lv_recipient) = ls_smtp-e_mail.
      ENDLOOP.

      lt_recipients = VALUE #( ( lv_recipient ) ).


      IF lt_recipients IS NOT INITIAL.

        CALL FUNCTION 'ZABAP_001_FM_SENDMAIL'
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
          LOOP AT lt_tab ASSIGNING FIELD-SYMBOL(<lfs_tab_l>) WHERE changenr = <lfs_tab>-changenr.
            ls_zlog = VALUE #( BASE ls_zlog
                              ebeln    = <lfs_tab_l>-ebeln
                              ebelp    = <lfs_tab_l>-ebelp
                              changenr = <lfs_tab_l>-changenr
                              uname    = sy-uname
                              datum    = sy-datum
                              uzeit    = sy-uzeit ).
            APPEND ls_zlog TO lt_zlog.
          ENDLOOP.

          MODIFY zmm_038_t_log FROM TABLE lt_zlog.
        ENDIF.

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
