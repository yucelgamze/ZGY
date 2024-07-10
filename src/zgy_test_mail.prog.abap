*&---------------------------------------------------------------------*
*& Report ZGY_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_test_mail.

DATA:lv_content     TYPE string,
     lv_subject     TYPE so_obj_des,
     lt_recipients  TYPE bcsy_smtpa,
     lt_pdf_name    TYPE zabap_001_tt_fpname,
     lv_tabname     TYPE tabname,
     lv_return      TYPE string,
     lv_return_type TYPE char1.

DATA(lv_banfn) = 10000099.
DATA(lv_data1) = 10.
DATA(lv_data2) = 20.
DATA(lv_data3) = 30.
DATA(lv_data4) = 40.
DATA(lv_data5) = 50.
DATA(lv_data6) = 60.
DATA(lv_data7) = 70000.
DATA(lv_data8) = 80.
DATA(lv_data9) = 90.
DATA(lv_data10) = 99999.

TYPES:BEGIN OF gty_data,
        name  TYPE sy-uname,
        time  TYPE sy-uzeit,
        date  TYPE sy-datum,
        unvan TYPE char30,
      END OF gty_data.
DATA:ls_data TYPE gty_data.

ls_data = VALUE #( name  = sy-uname
                   date  = sy-datum
                   time  = sy-uzeit
                   unvan = |ABAP DEVELOPER| ).


lv_subject = |HTML Table Test Hk.|.

*lv_content = |Sayın İlgili, { cl_abap_char_utilities=>newline } { lv_ebeln } nolu satınalma siparişinin { lv_ebelp } nolu kaleminde |
*             & |teslimat toleransı değiştirilmiştir. |
*             & |Belgeyi lütfen kontrol ediniz. |.


lv_content =   '<!DOCTYPE html> '
     && '<html>  '
     && '<head> '
     && '<meta charset="utf-8"  '
     && '</head>  '
     && '<body style="background-color:#FFFFFF;"> '
     && '<p>Sayın İlgili,</p> '                                 "<br> boşluk <b> arasında bold</b>
     && '<p>'
     && lv_banfn
     && ' numaralı satınalma talebi onaylanmıştır. </p> '
     && '<p>Belge detaylarını kontrol edebilirsiniz.</p> '
     && '<table>'
     && '<br><tr>'
     && '<td><b> Talep Tarihi</b></td>'
     && '<td>'
     && ls_data-date
     && '</td></tr>'
     && '<tr>'
     && '<td><b> Talep Onay Tarihi</b></td>'
     && '<td>'
     && ls_data-date
     && '</td></tr>'
     && '<tr>'
     && '<td><b> Talep Eden</b></td>'
     && '<td>'
     && ls_data-name
     && '</td></tr>'
     && '<tr>'
     && '<td><b> Talep Eden Unvan</b></td>'
     && '<td>'
     && ls_data-unvan
     && '</td></tr>'
     && '<tr>'
     && '<td><b> Satınalma Talep Numarası</b></td>'
     && '<td>'
     && lv_banfn
     && '</td></tr>'
     && '</table> <br> <br>'
     && '<p>SAT Detayları</p>'
     && '<table border=1> '
     && '<tr><th bgcolor = "#E6E6FA" align = "LEFT" > Malzeme Türü </th><th  bgcolor = "#E6E6FA" align = "LEFT" > Malzeme No </th>'
     && '<th bgcolor = "#E6E6FA" align = "LEFT" > Malzeme Adı </th><th  bgcolor = "#E6E6FA" align = "LEFT" > Mal Grubu </th>'
     && '<th bgcolor = "#E6E6FA" align = "LEFT" > Birim Fiyat </th><th  bgcolor = "#E6E6FA" align = "LEFT" > Adet </th>'
     && '<th bgcolor = "#E6E6FA" align = "LEFT" > PB </th><th  bgcolor = "#E6E6FA" align = "LEFT" > Kur </th>'
     && '<th bgcolor = "#E6E6FA" align = "LEFT" > Toplam Tutar </th>'
     && '</tr>'
     && '<tr><td>'
     && lv_data1
     && '</td>'
     && '<td>'
     && lv_data2
     && '</td>'
     && '<td>'
     && lv_data3
     && '</td>'
     && '<td>'
     && lv_data4
     && '</td>'
     && '<td>'
     && lv_data5
     && '</td>'
     && '<td>'
     && lv_data6
     && '</td>'
     && '<td>'
     && lv_data7
     && '</td>'
     && '<td>'
     && lv_data8
     && '</td>'
      && '<td>'
     && lv_data9
     && '</td></tr>'
     && '</table> '
     && '<table border=1> '
     && '<br><tr>'
     && '<td><b> Genel Toplam</b></td>'
     && '<td><b>'
     && lv_data10
     && '</b></td></tr>'
     && '</table> '
     && '</body>  '
     && '</html> '.

lt_recipients = VALUE #( ( |test@test.com| ) ).

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


DATA:lt_smtp   TYPE TABLE OF bapiadsmtp,
     ls_smtp   TYPE bapiadsmtp,
     lt_return TYPE TABLE OF bapiret2,
     ls_return TYPE bapiret2.

CALL FUNCTION 'BAPI_USER_GET_DETAIL'
  EXPORTING
*   username = sy-uname
    username = ls_data-name
  TABLES
    return   = lt_return
    addsmtp  = lt_smtp.

DATA(lv_msg) = | { lv_return_type } -> { lv_return } |.
MESSAGE lv_msg TYPE 'I' DISPLAY LIKE lv_return_type.
