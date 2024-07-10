*&---------------------------------------------------------------------*
*& Include          ZMM_001_CONTRACT_LCL
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS:
      get_data,
      send_mail,
      call_screen,
      pbo_0100,
      pai_0100 IMPORTING iv_ucomm TYPE sy-ucomm,
      set_fcat,
      set_layout,
      display_alv.
ENDCLASS.
CLASS lcl_class IMPLEMENTATION.
  METHOD get_data.

    DATA:lv_date TYPE sy-datum.
    lv_date = sy-datum + 30.

    SELECT
    ekko~ebeln,
    ekpo~ebelp,
    ekko~bsart,
    ekko~ekgrp,
    ekko~ekorg,
    ekko~bukrs,
    ekko~loekz,
    ekko~bstyp,
    ekko~lifnr,
    ekko~waers,
    ekko~kdatb,
    ekko~kdate,
    ekpo~matnr,
    ekpo~txz01,
    ekpo~werks,
    ekpo~ktmng,
    ekpo~meins,
    ekpo~netpr,
    ekpo~peinh,
    ekpo~brtwr
    FROM ekko
    LEFT JOIN ekpo ON ekpo~ebeln = ekko~ebeln AND ekpo~loekz = @space
    INTO  TABLE @DATA(lt_alv)
    WHERE ekko~bsart IN @so_bsart AND
          ekko~ekgrp IN @so_ekgrp AND
          ekko~ekorg IN @so_ekorg AND
          ekko~bukrs IN @so_bukrs AND
          ekko~loekz EQ @space    AND
          ekko~bstyp EQ 'K'       AND
          ekko~kdate BETWEEN  @sy-datum  AND @lv_date .

    IF sy-subrc EQ 0.

      gt_alv = CORRESPONDING #( lt_alv ).

      DATA(lines) = lines( lt_alv ).
      DATA:i TYPE i VALUE 1.
      DATA:lv_meins TYPE meins.

      LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv>).
*        FOR i = 1 while i <= lines ( lt_alv )  let in ( )( ).
        <lfs_alv>-sozno   = lt_alv[ i ]-ebeln .
        <lfs_alv>-kalem   = lt_alv[ i ]-ebelp .
        <lfs_alv>-malzeme = | { lt_alv[ i ]-matnr } { lt_alv[ i ]-txz01 } |.

        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            input          = lt_alv[ i ]-meins
            language       = 'T'
          IMPORTING
            output         = lv_meins
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.

        <lfs_alv>-miktar  = | { lt_alv[ i ]-ktmng } { lv_meins } |. "&& CONV bstme( lt_alv[ i ]-meins ) .
        <lfs_alv>-birim   = | { lt_alv[ i ]-netpr / lt_alv[ i ]-peinh } { lt_alv[ i ]-waers } |.
        <lfs_alv>-brut    = | { lt_alv[ i ]-brtwr } { lt_alv[ i ]-waers } |.
        IF i NE lines.
          i = i + 1.
        ENDIF.

*        LOOP AT lt_alv ASSIGNING FIELD-SYMBOL(<lfs_x>).
*          <lfs_alv>-sozno   = | { <lfs_x>-ebeln }{ <lfs_x>-ebelp } |.
*          <lfs_alv>-malzeme = | { <lfs_x>-matnr } { <lfs_x>-txz01 } |.
*          <lfs_alv>-miktar  = | { <lfs_x>-ktmng } { <lfs_x>-meins } |.
*          <lfs_alv>-birim   = | { <lfs_x>-netpr / <lfs_x>-peinh } { lt_alv[ i ]-waers } |.
*          <lfs_alv>-brut    = | { <lfs_x>-brtwr } { <lfs_x>-waers } |.
*        ENDLOOP.

      ENDLOOP.
    ENDIF.

    MODIFY zmm_001_t_cntrct FROM TABLE gt_alv.

  ENDMETHOD.

  METHOD send_mail.

    DATA:lv_content     TYPE string,
         lv_subject     TYPE so_obj_des,
         lt_recipients  TYPE bcsy_smtpa,
         lt_pdf_name    TYPE zabap_001_tt_fpname,
         lv_tabname     TYPE tabname,
         lv_return      TYPE string,
         lv_return_type TYPE char1.

    DATA:lt_mail_group TYPE TABLE OF zmm_001_t_mailgr,
         ls_mail_group TYPE zmm_001_t_mailgr.

    SELECT
    zmail
    FROM zmm_001_t_mailgr INTO CORRESPONDING FIELDS OF TABLE lt_mail_group
    WHERE zgrupno = '1' OR zgrupno = '001'.

    lv_subject = |Sözleşme Bitiş Tarihleri Hk.|.

*    lv_content = |Sayın Kullanıcı Merhaba,{ cl_abap_char_utilities=>newline }Ekteki excelde belirtilen sözleşmelerin bitiş tarihleri 1 aydan az kalmıştır. Kontrol ediniz.|.

    lv_content =   '<!DOCTYPE html>                                            '
                && '<html>                                                     '
                && '  <head>                                                   '
                && '     <meta charset="utf-8"                                 '
                && '  </head>                                                  '
                && '  <body>                                                   '
                && '    <p><b>Sayın Kullanıcı Merhaba,</b></p><br>                        '
                && '    <p><b>                                                 '
                && ' Ekteki excelde belirtilen sözleşmelerin bitiş tarihleri 1 aydan az kalmıştır. '
                && '    </b></p><br>                                           '
                && '    <p><b>Kontrol ediniz.</b></p>                                '
                && '  </body>                                                  '
                && '</html>                                                    '.

    LOOP AT lt_mail_group INTO ls_mail_group.
      lt_recipients = VALUE #( BASE lt_recipients ( ls_mail_group-zmail ) ).
    ENDLOOP.

    lv_tabname = |ZMM_001_T_CNTRCT|.

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


    DATA(lv_msg) = | { lv_return_type } -> { lv_return } |.
    MESSAGE lv_msg TYPE 'I' DISPLAY LIKE lv_return_type.
    IF lv_return_type EQ 'S'.
      DELETE FROM zmm_001_t_cntrct .
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
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZMM_001_T_CNTRCT'
      CHANGING
        ct_fieldcat      = gt_fcat.
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
          container_name = 'CC_ALV'.   " Name of the Screen CustCtrl Name to Link Container To
      CREATE OBJECT go_alv_grid
        EXPORTING
*         i_parent = go_container.  " Parent Container
          i_parent = cl_gui_custom_container=>screen0.  " Parent Container
      CALL METHOD go_alv_grid->set_table_for_first_display
        EXPORTING
          is_layout       = gs_layout   " Layout
        CHANGING
          it_outtab       = gt_alv   " Output Table
          it_fieldcatalog = gt_fcat.   " Field Catalog
    ELSE.
      CALL METHOD go_alv_grid->refresh_table_display.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
