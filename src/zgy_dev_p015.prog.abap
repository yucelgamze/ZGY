*&---------------------------------------------------------------------*
*& Report ZGY_DEV_P015
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_dev_p015.

TYPES:BEGIN OF lty_app,
        banfn    TYPE eban-banfn,
        bnfpo    TYPE eban-bnfpo,
        banpr    TYPE eban-banpr,
        badat    TYPE eban-badat,
        frgdt    TYPE eban-frgdt,
        ernam    TYPE eban-ernam,
        mtart    TYPE mara-mtart,
        matnr    TYPE eban-matnr,
        maktx    TYPE makt-maktx,
        matkl    TYPE eban-matkl,
        wgbez    TYPE t023t-wgbez,
        preis    TYPE eban-preis,
        peinh    TYPE eban-peinh,
        menge    TYPE eban-menge,
        meins    TYPE eban-meins,
        waers    TYPE eban-waers,
        lv_totc  TYPE char20,
        lv_menge TYPE char20,
        lv_preis TYPE char20,
      END OF lty_app.
DATA:lt_app   TYPE TABLE OF lty_app,
     ls_app   TYPE lty_app,
     ls_appxx TYPE lty_app.

DATA:lv_total TYPE i,
     lv_tot   TYPE i.

DATA:lv_mail_flag TYPE xfeld.

*        RANGES:lr_bnfpo FOR eban-bnfpo.
DATA:lr_bnfpo TYPE RANGE OF eban-bnfpo.

DATA:lv_content     TYPE string,
     lv_subject     TYPE so_obj_des,
     lt_recipients  TYPE bcsy_smtpa,
     lt_pdf_name    TYPE zabap_001_tt_fpname,
     lv_tabname     TYPE tabname,
     lv_return      TYPE string,
     lv_return_type TYPE char1.


ls_app = VALUE #( BASE ls_app
                       banfn = '1234567890'
                       bnfpo = '0010'
                       banpr = '05'
                       badat = '19990314'
                       frgdt = '20240723'
                       matnr = 'test'
                       maktx = 'TEST Maktx'
                       mtart = 'TEST Mtart'
                       meins = 'ST'
                       menge = '1234.12'
                       peinh = 1
                       preis = 1
                       waers = 'TRY'
                       ernam = 'XTALHAC').


lv_tot = ( ls_app-preis / ls_app-peinh ) * ls_app-menge.
lv_total += ( ls_app-preis / ls_app-peinh ) * ls_app-menge.
ls_app-lv_totc = lv_tot.
APPEND ls_app TO lt_app.

ls_app = VALUE #( BASE ls_app
                       banfn = '9934567890'
                       bnfpo = '0020'
                       banpr = '05'
                       badat = '19990314'
                       frgdt = '20240723'
                       matnr = '2 test'
                       maktx = '2 Maktx'
                       mtart = '2 Mtart'
                       meins = 'ST'
                       menge = '5534.12'
                       peinh = 1
                       preis = 10
                       waers = 'TRY'
                       ernam = 'XTALHAC').
CLEAR:lv_tot.
lv_tot = ( ls_app-preis / ls_app-peinh ) * ls_app-menge.
lv_total += ( ls_app-preis / ls_app-peinh ) * ls_app-menge.
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
    input          = ls_app-meins
    language       = sy-langu
  IMPORTING
    output         = ls_app-meins
  EXCEPTIONS
    unit_not_found = 1
    OTHERS         = 2.

DATA:lv_menge TYPE char20,
     lv_a     TYPE char20,
     lv_b     TYPE char20,
     lv_c     TYPE char20.

lv_menge = CONV #( ls_app-menge ).

CALL FUNCTION 'HRCM_AMOUNT_TO_STRING_CONVERT'
  EXPORTING
    betrg  = ls_app-menge
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

lv_menge = |{ lv_menge } { ls_app-meins }|.

DATA:lv_preis TYPE char20,
     lv_x     TYPE char20,
     lv_y     TYPE char20,
     lv_z     TYPE char20.
CALL FUNCTION 'HRCM_AMOUNT_TO_STRING_CONVERT'
  EXPORTING
    betrg  = ls_app-preis
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

ls_app = VALUE #( BASE ls_app
                       lv_totc  = lv_totc
                       lv_menge = lv_menge
                       lv_preis = lv_preis ).
APPEND ls_app TO lt_app.


DATA(lv_badat) = |{ ls_app-badat+6(2) }.{ ls_app-badat+4(2) }.{ ls_app-badat+0(4) }|.

DATA(lv_frgdt) = |{ ls_app-frgdt+6(2) }.{ ls_app-frgdt+4(2) }.{ ls_app-frgdt+0(4) }|.

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

lv_subject = TEXT-000.

DATA:lines TYPE TABLE OF  tline.

CALL FUNCTION 'READ_TEXT'
  EXPORTING
    client   = sy-mandt
    id       = 'ST'
    language = sy-langu
    name     = 'ZMM_000_STR_ONAY'
    object   = 'TEXT'
  TABLES
    lines    = lines.

LOOP AT lines ASSIGNING FIELD-SYMBOL(<lfs_lines>).
  lv_content &&= <lfs_lines>-tdline.
ENDLOOP.

REPLACE ALL OCCURRENCES OF '<BANFN>' IN lv_content WITH ls_app-banfn.
REPLACE ALL OCCURRENCES OF '<BADAT>' IN lv_content WITH lv_badat.
REPLACE ALL OCCURRENCES OF '<FRGDT>' IN lv_content WITH lv_frgdt.
REPLACE ALL OCCURRENCES OF '<ERNAM>' IN lv_content WITH ls_app-ernam.
REPLACE ALL OCCURRENCES OF '<BANFN>' IN lv_content WITH ls_app-banfn.
REPLACE ALL OCCURRENCES OF '<TOTAL>' IN lv_content WITH CONV string( lv_total ).

DATA:lv_splitxx TYPE string,
     lv_splityy TYPE string,
     lv_splitzz TYPE string,
     lv_split   TYPE string.

SPLIT lv_content AT 'Toplam Tutar </th></tr>' INTO lv_split lv_splityy.
lv_split &&= |Toplam Tutar </th></tr>|.

LOOP AT lt_app INTO ls_app.

  SPLIT lv_content AT 'Toplam Tutar </th></tr>' INTO lv_splitxx lv_splityy.
  SPLIT lv_splityy AT '</table>' INTO lv_splityy lv_splitzz.

  REPLACE ALL OCCURRENCES OF '<MTART>' IN lv_splityy WITH ls_app-mtart.
  REPLACE ALL OCCURRENCES OF '<MATNR>' IN lv_splityy WITH ls_app-matnr.
  REPLACE ALL OCCURRENCES OF '<MAKTX>' IN lv_splityy WITH ls_app-maktx.
  REPLACE ALL OCCURRENCES OF '<WGBEZ>' IN lv_splityy WITH ls_app-wgbez.
  REPLACE ALL OCCURRENCES OF '<PREIS>' IN lv_splityy WITH CONV string( ls_app-lv_preis )."CONV string( ls_app-preis ).
  REPLACE ALL OCCURRENCES OF '<MENGE>' IN lv_splityy WITH CONV string( ls_app-lv_menge ). "CONV string( ls_app-menge ).
  REPLACE ALL OCCURRENCES OF '<WAERS>' IN lv_splityy WITH ls_app-waers.
  REPLACE ALL OCCURRENCES OF '<TOT>'   IN lv_splityy WITH CONV string( ls_app-lv_totc ).

  lv_split &&= |{ lv_splityy }|.
ENDLOOP.

lv_split &&= |</table>{ lv_splitzz }|.

lv_content = lv_split.

IF lt_recipients IS NOT INITIAL.

  CALL FUNCTION 'ZABAP_001_FM_SENDMAIL' "IN BACKGROUND TASK
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
