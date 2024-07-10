class ZCL_IM_MM_000_PR_MAIL definition
  public
  final
  create public .

public section.

  interfaces IF_EX_ME_PROCESS_REQ_CUST .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_MM_000_PR_MAIL IMPLEMENTATION.


  method IF_EX_ME_PROCESS_REQ_CUST~CHECK.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~CLOSE.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~FIELDSELECTION_HEADER.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~FIELDSELECTION_HEADER_REFKEYS.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~FIELDSELECTION_ITEM.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~FIELDSELECTION_ITEM_REFKEYS.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~INITIALIZE.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~OPEN.
  endmethod.


  METHOD if_ex_me_process_req_cust~post.

    "ME53N
    IF sy-uname EQ 'GAMZEY' OR sy-uname EQ 'TALHAC' OR sy-uname EQ 'OKTAYC'.

      DATA:ls_item  TYPE mereq_item,
           lt_items TYPE mmpur_requisition_items.

      TYPES:BEGIN OF lty_app,
              banpr TYPE eban-banpr,
              badat TYPE eban-badat,
              frgdt TYPE eban-frgdt,
              ernam TYPE eban-ernam,
              banfn TYPE eban-banfn,
              mtart TYPE mara-mtart,
              matnr TYPE eban-matnr,
              maktx TYPE makt-maktx,
              wgbez TYPE t023t-wgbez,
              preis TYPE eban-preis,
              peinh TYPE eban-peinh,
              menge TYPE eban-menge,
              waers TYPE eban-waers,
              bnfpo TYPE eban-bnfpo,
            END OF lty_app.
      DATA:lt_app   TYPE TABLE OF lty_app,
           ls_app   TYPE lty_app,
           ls_appxx TYPE lty_app.

      DATA:lv_total TYPE i,
           lv_tot   TYPE i.

      DATA:lv_mail_flag TYPE xfeld.

      DATA:lv_content     TYPE string,
           lv_subject     TYPE so_obj_des,
           lt_recipients  TYPE bcsy_smtpa,
           lt_pdf_name    TYPE zabap_001_tt_fpname,
           lv_tabname     TYPE tabname,
           lv_return      TYPE string,
           lv_return_type TYPE char1.

      CALL METHOD im_header->get_items
        RECEIVING
          re_items = lt_items.                " Items

      LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<lfs_items>).
        <lfs_items>-item->get_data(
          RECEIVING
            re_data = ls_item                 " Item Data
        ).
      ENDLOOP.

      IF ls_item IS NOT INITIAL.

        SELECT
        eban~bnfpo,
        eban~banpr,
        eban~badat,
        eban~frgdt,
        eban~ernam,
        eban~banfn,
        mara~mtart,
        eban~matnr,
        makt~maktx,
        t023t~wgbez,
        eban~preis,
        eban~peinh,
        eban~menge,
        eban~waers
        FROM eban
        INNER JOIN mara  ON eban~matnr = mara~matnr
        LEFT JOIN makt  ON mara~matnr = makt~maktx
        LEFT JOIN t023t ON eban~matkl = t023t~matkl
        WHERE eban~banfn = @im_banfn
        AND   makt~spras = 'T'
        AND   t023t~spras = 'T'
        INTO TABLE @DATA(lt_data).

        SELECT SINGLE
        eban~bnfpo,
        eban~banpr,
        eban~badat,
        eban~frgdt,
        eban~ernam,
        eban~banfn,
        mara~mtart,
        eban~matnr,
        makt~maktx,
        t023t~wgbez,
        eban~preis,
        eban~peinh,
        eban~menge,
        eban~waers
        FROM eban
        INNER JOIN mara  ON eban~matnr = mara~matnr
        LEFT JOIN makt  ON mara~matnr = makt~maktx
        LEFT JOIN t023t ON eban~matkl = t023t~matkl
        WHERE eban~banfn = @im_banfn
        AND   eban~bnfpo = @ls_item-bnfpo
        AND   makt~spras = 'T'
        AND   t023t~spras = 'T'
        INTO  @DATA(ls_dataxx).


        lt_app = CORRESPONDING #( lt_data ).

        LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lfs_data>).
          IF ( <lfs_data>-banpr NE ls_item-banpr ) AND ( ls_item-banpr EQ '05' ).
            lv_mail_flag = abap_true.
            ls_appxx = CORRESPONDING #( ls_dataxx ).
            lv_tot = ( ls_appxx-preis / ls_appxx-peinh ) * ls_appxx-menge.

            LOOP AT lt_app INTO ls_app WHERE bnfpo = <lfs_data>-bnfpo.
              lv_total += ( ls_app-preis / ls_app-peinh ) * ls_app-menge.
            ENDLOOP.
          ELSE.
            lv_mail_flag = abap_false.
          ENDIF.
        ENDLOOP.

        IF lv_mail_flag EQ abap_true.

          DATA:lt_smtp   TYPE TABLE OF bapiadsmtp,
               ls_smtp   TYPE bapiadsmtp,
               lt_return TYPE TABLE OF bapiret2,
               ls_return TYPE bapiret2.

          CALL FUNCTION 'BAPI_USER_GET_DETAIL'
            EXPORTING
              username = ls_app-ernam
            TABLES
              return   = lt_return
              addsmtp  = lt_smtp.

          LOOP AT lt_smtp INTO ls_smtp.
            DATA(lv_recipient) = ls_smtp-e_mail.
          ENDLOOP.

          lt_recipients = VALUE #( ( lv_recipient ) ).


          lv_subject = |SAT Onay Hk.|.

          lv_content =   '<!DOCTYPE html> '
               && '<html>  '
               && '<head> '
               && '<meta charset="utf-8"  '
               && '</head>  '
               && '<body style="background-color:#FFFFFF;"> '
               && '<p>Sayın İlgili,</p> '                                 "<br> boşluk <b> arasında bold</b>
               && '<p>'
               && ls_app-banfn
               && ' numaralı satınalma talebi onaylanmıştır. </p> '
               && '<p>Belge detaylarını kontrol edebilirsiniz.</p> '
               && '<table>'
               && '<br><tr>'
               && '<td><b> Talep Tarihi</b></td>'
               && '<td>'
               && ls_app-badat
               && '</td></tr>'
               && '<tr>'
               && '<td><b> Talep Onay Tarihi</b></td>'
               && '<td>'
               && ls_app-frgdt
               && '</td></tr>'
               && '<tr>'
               && '<td><b> Talep Eden</b></td>'
               && '<td>'
               && ls_app-ernam
               && '</td></tr>'
               && '<tr>'
               && '<td><b> Satınalma Talep Numarası</b></td>'
               && '<td>'
               && ls_app-banfn
               && '</td></tr>'
               && '</table> <br> <br>'
               && '<p>SAT Detayları</p>'
               && '<table border=1> '
               && '<tr><th bgcolor = "#E6E6FA" align = "LEFT" > Malzeme Türü </th><th  bgcolor = "#E6E6FA" align = "LEFT" > Malzeme No </th>'
               && '<th bgcolor = "#E6E6FA" align = "LEFT" > Malzeme Adı </th><th  bgcolor = "#E6E6FA" align = "LEFT" > Mal Grubu </th>'
               && '<th bgcolor = "#E6E6FA" align = "LEFT" > Birim Fiyat </th><th  bgcolor = "#E6E6FA" align = "LEFT" > Adet </th>'
               && '<th bgcolor = "#E6E6FA" align = "LEFT" > Para Birimi </th>'
               && '<th bgcolor = "#E6E6FA" align = "LEFT" > Toplam Tutar </th>'
               && '</tr>'
               && '<tr><td>'
               && ls_app-mtart
               && '</td>'
               && '<td>'
               && ls_app-matnr
               && '</td>'
               && '<td>'
               && ls_app-maktx
               && '</td>'
               && '<td>'
               && ls_app-wgbez
               && '</td>'
               && '<td>'
               && ls_app-preis
               && '</td>'
               && '<td>'
               && ls_app-menge
               && '</td>'
               && '<td>'
               && ls_app-waers
               && '</td>'
               && '<td>'
               && lv_tot
               && '</td></tr>'
               && '</table> '
               && '<table border=1> '
               && '<br><tr>'
               && '<td><b> Genel Toplam</b></td>'
               && '<td><b>'
               && lv_total
               && '</b></td></tr>'
               && '</table> '
               && '</body>  '
               && '</html> '.

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

        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  method IF_EX_ME_PROCESS_REQ_CUST~PROCESS_ACCOUNT.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~PROCESS_HEADER.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~PROCESS_ITEM.
  endmethod.
ENDCLASS.
