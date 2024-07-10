*&---------------------------------------------------------------------*
*& Report ZGY_DEV_P008
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_dev_p008.

PARAMETERS:p_banfn TYPE eban-banfn,
           p_bnfpo TYPE eban-bnfpo.

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

SELECT
eban~bnfpo,
eban~banpr,
eban~badat,
eban~frgdt,
eban~ernam,
eban~banfn,
*mara~mtart,
eban~matnr,
*makt~maktx,
*t023t~wgbez,
eban~preis,
eban~peinh,
eban~menge,
eban~waers
FROM eban
*LEFT JOIN mara  ON eban~matnr = mara~matnr
*LEFT JOIN makt  ON mara~matnr = makt~maktx
*LEFT JOIN t023t ON eban~matkl = t023t~matkl
WHERE eban~banfn = @p_banfn
*AND   eban~bnfpo = @p_bnfpo
*AND   makt~spras = 'T'
*AND   t023t~spras = 'T'
INTO TABLE @DATA(lt_data).

SELECT SINGLE
eban~bnfpo,
eban~banpr,
eban~badat,
eban~frgdt,
eban~ernam,
eban~banfn,
*mara~mtart,
eban~matnr,
*makt~maktx,
*t023t~wgbez,
eban~preis,
eban~peinh,
eban~menge,
eban~waers
FROM eban
*LEFT JOIN mara  ON eban~matnr = mara~matnr
*LEFT JOIN makt  ON mara~matnr = makt~maktx
*LEFT JOIN t023t ON eban~matkl = t023t~matkl
WHERE eban~banfn = @p_banfn
AND   eban~bnfpo = @p_bnfpo
*AND   makt~spras = 'T'
*AND   t023t~spras = 'T'
INTO  @DATA(ls_dataxx).

lt_app = CORRESPONDING #( lt_data ).

LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lfs_data>).

  ls_appxx = CORRESPONDING #( ls_dataxx ).
  lv_tot = ( ls_appxx-preis / ls_appxx-peinh ) * ls_appxx-menge.

  LOOP AT lt_app INTO ls_app WHERE bnfpo = <lfs_data>-bnfpo.
    lv_total += ( ls_app-preis / ls_app-peinh ) * ls_app-menge.
  ENDLOOP.

ENDLOOP.

*lt_app = CORRESPONDING #( lt_data ).
*
*LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lfs_data>).
*  IF ( <lfs_data>-banpr NE ls_item-banpr ) AND ( ls_item-banpr EQ '05' ).
*
*    lv_mail_flag = abap_true.
*
*    ls_appxx = CORRESPONDING #( ls_dataxx ).
*    lv_tot = ( ls_appxx-preis / ls_appxx-peinh ) * ls_appxx-menge.
*
*    LOOP AT lt_app INTO ls_app WHERE bnfpo = <lfs_data>-bnfpo.
*      lv_total += ( ls_app-preis / ls_app-peinh ) * ls_app-menge.
*    ENDLOOP.
*
*  ELSE.
*    lv_mail_flag = abap_false.
*  ENDIF.
*ENDLOOP.
