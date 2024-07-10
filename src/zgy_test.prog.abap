*&---------------------------------------------------------------------*
*& Report ZGY_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_test.

DATA:lv_content     TYPE string,
     lv_subject     TYPE so_obj_des,
     lt_recipients  TYPE bcsy_smtpa,
     lt_pdf_name    TYPE zabap_001_tt_fpname,
     lv_tabname     TYPE tabname,
     lv_return      TYPE string,
     lv_return_type TYPE char1.

DATA: lv_data1 TYPE char20 VALUE '10000099'.
DATA: lv_data2 TYPE char10 VALUE  '20'.

lv_subject = |HTML Table Test Hk.|.

*lv_content = |Sayın İlgili, { cl_abap_char_utilities=>newline } { lv_ebeln } nolu satınalma siparişinin { lv_ebelp } nolu kaleminde |
*             & |teslimat toleransı değiştirilmiştir. |
*             & |Belgeyi lütfen kontrol ediniz. |.


*                lv_content =   '<!DOCTYPE html>                                            '
*                      && '<html>                                                     '
*                      && '  <head>                                                   '
*                      && '     <meta charset="utf-8"                                 '
*                      && '  </head>                                                  '
*                      && '  <body>                                                   '
*                      && '    <p><b>Sayın İlgili,</b></p><br>                        '
*                      && '    <p><b>                                                 '
*                      && lv_ebeln
*                      && ' nolu satınalma siparişinin :'
*                      && lv_ebelp
*                      && ' nolu kaleminde teslimat toleransı değiştirilmiştir.'
*                      && '    </b></p><br>                                           '
*                      && '    <p><b>Belgeyi lütfen kontrol ediniz.</b></p>                                '
*                      && '  </body>                                                  '
*                      && '</html>                                                    '.

*lv_content =   '<!DOCTYPE html>                                     '
*     && '<html lang="en">                                                    '
*     && '<head>                                                   '
*     && '<meta charset="utf-8"                                 '
*     && '</head>                                                  '
*     && '<body style="background-color:#FFFFFF;"> '
*     && '<p>Sayın İlgili,</p>                       '  "<br> boşluk <b> arasında bold</b>
*     && '<p> bilgiler bilgiler </p>  '
*     && '<p>Belgeyi lütfen kontrol ediniz.</p>           '
*     && '<table border=1> '
*     && '<tr><th bgcolor = "#E6E6FA" align = "LEFT" >Data 1</th><th  bgcolor = "#E6E6FA" align = "LEFT" >Data 2</th></tr>'
*     && '<tr><td>'
*     && lv_data1
*     && '</td>'
*     && '<td>'
*     && lv_data2
*     && '</td></tr>'
*     && '</table> '
*     && '</body>                                                  '
*     && '</html>
*                                               '.
DATA:lines TYPE TABLE OF  tline.

CALL FUNCTION 'READ_TEXT'
  EXPORTING
    client   = sy-mandt
    id       = 'ST'
    language = sy-langu
    name     = 'ZGY_TEST_MAIL_TEXT'
    object   = 'TEXT'
  TABLES
    lines    = lines.

LOOP AT lines ASSIGNING FIELD-SYMBOL(<lfs_lines>).
  lv_content &&= <lfs_lines>-tdline.
ENDLOOP.

REPLACE ALL OCCURRENCES OF '<DATA1>' IN lv_content WITH lv_data1.
REPLACE ALL OCCURRENCES OF '<DATA2>' IN lv_content WITH lv_data2.

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
    username = sy-uname
  TABLES
    return   = lt_return
    addsmtp  = lt_smtp.

DATA(lv_msg) = | { lv_return_type } -> { lv_return } |.
MESSAGE lv_msg TYPE 'I' DISPLAY LIKE lv_return_type.
