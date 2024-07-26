*&---------------------------------------------------------------------*
*& Report ZGY_DEV_P014
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_dev_p014.

RANGES:gr_mblnr FOR mseg-mblnr,
       gr_matnr FOR mseg-matnr,
       gr_charg FOR mseg-charg,
       gr_xblnr FOR mkpf-xblnr.

PARAMETERS:p_mblnr TYPE mseg-mblnr MODIF ID x,
           p_lgpla TYPE lagp-lgpla MODIF ID y,
           p_matnr TYPE matnr MODIF ID z,
           p_charg TYPE mseg-charg MODIF ID w,
           p_xblnr TYPE mkpf-xblnr MODIF ID i.


IF p_mblnr IS NOT INITIAL.
  gr_mblnr = VALUE #(
                      sign   = 'I'
                      option = 'CP'
*                      option = 'EQ'
                      low    = |{ p_mblnr }| ).
  APPEND gr_mblnr TO gr_mblnr[].
ENDIF.

IF p_matnr IS NOT INITIAL.
  gr_matnr = VALUE #(
                      sign   = 'I'
                      option = 'CP'
                      low    = |{ p_matnr }| ).
  APPEND gr_matnr TO gr_matnr[].
ENDIF.

IF p_charg IS NOT INITIAL.
  gr_charg = VALUE #(
                      sign   = 'I'
                      option = 'CP'
                      low    = |{ p_charg }| ).
  APPEND gr_charg TO gr_charg[].
ENDIF.

IF p_xblnr IS NOT INITIAL.
  gr_xblnr = VALUE #(
                     sign   = 'I'
                     option = 'CP'
                     low    = |{ p_xblnr }| ).
ENDIF.


*DATA:lt_data TYPE TABLE OF zmm_012_s_ware_i.

DATA:lv_concat   TYPE char28,
     lv_datatype TYPE dd01v-datatype,
     lv_matnr    TYPE char50.

DATA:lv_objectkey       TYPE bapi1003_key-object,
     lt_allocvaluesnum  TYPE TABLE OF  bapi1003_alloc_values_num,
     lt_allocvalueschar TYPE TABLE OF  bapi1003_alloc_values_char,
     lt_allocvaluescurr TYPE TABLE OF bapi1003_alloc_values_curr,
     lt_return          TYPE TABLE OF bapiret2,
     ls_return          TYPE bapiret2.

SELECT
mseg~zeile AS kap,
mseg~charg AS zparti,
mseg~mblnr,
mseg~zeile,
mseg~charg,
lfa1~name1,
mara~matnr,
makt~maktx,
mkpf~xblnr,
mseg~erfmg,
mseg~erfme
FROM mseg
INNER JOIN mkpf ON mseg~mblnr = mkpf~mblnr
INNER JOIN mara ON mseg~matnr = mara~matnr
LEFT  JOIN lfa1 ON mseg~lifnr = lfa1~lifnr
LEFT  JOIN makt ON mara~matnr = makt~matnr
WHERE makt~spras = 'T'
AND   mseg~mblnr IN @gr_mblnr[]
AND   mseg~matnr IN @gr_matnr[]
AND   mseg~charg IN @gr_charg[]
AND   mkpf~xblnr IN @gr_xblnr[]
INTO TABLE @DATA(lt_data).
*INTO CORRESPONDING FIELDS OF TABLE @lt_data.

DELETE ADJACENT DUPLICATES FROM lt_data COMPARING mblnr zeile.


*SELECT
*ddl~matnr,
*ddl~charg,
*ddl~class
*FROM zmm_000_ddl_batch_char AS ddl
*INNER JOIN @lt_data AS lt ON ddl~matnr = lt~matnr
*INTO TABLE @DATA(lt_ddl).

*DELETE ADJACENT DUPLICATES FROM lt_ddl COMPARING matnr.

LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lfs_x>).
  CLEAR:lv_concat,
        lv_matnr,
        lv_objectkey,
        lv_datatype.

  IF <lfs_x>-matnr IS NOT INITIAL AND <lfs_x>-charg IS NOT INITIAL.

    <lfs_x>-charg = |{ <lfs_x>-charg ALPHA = IN }|.
    DATA(lv_charg_len) = strlen( <lfs_x>-charg ).

    IF lv_charg_len NE 10.
      DATA(lv_lead) = 10 - lv_charg_len.
      WRITE <lfs_x>-charg TO <lfs_x>-charg RIGHT-JUSTIFIED.
      TRANSLATE <lfs_x>-charg USING ' 0'.
    ENDIF.

    lv_concat = <lfs_x>-charg.
    DATA(concat) = |{ <lfs_x>-matnr }{ lv_concat }|. "yani toplamda 28 karakter olacak
    DATA(lv_len) = strlen( concat ).

    CALL FUNCTION 'NUMERIC_CHECK'
      EXPORTING
        string_in = <lfs_x>-matnr
      IMPORTING
        htype     = lv_datatype.

    IF lv_datatype EQ 'NUMC'.

      lv_matnr = |{ <lfs_x>-matnr }{ lv_concat }|.

    ELSEIF <lfs_x>-matnr CA sy-abcde OR lv_datatype NE 'NUMC'.
      IF lv_len NE 28.
        DATA(lv_space) = 28 - lv_len.
        SHIFT lv_concat RIGHT BY lv_space PLACES.
      ENDIF.

      lv_matnr = |{ <lfs_x>-matnr }{ lv_concat }|.
    ENDIF.

    lv_objectkey = lv_matnr.

*    LOOP AT lt_ddl ASSIGNING FIELD-SYMBOL(<lfs_ddl>) WHERE matnr = <lfs_x>-matnr
*                                                     AND   charg = <lfs_x>-charg.

    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey       = lv_objectkey
        objecttable     = 'MCH1'
        classnum        = 'HAMKUMAS_PARTI' "<lfs_ddl>-class
        classtype       = '023'
        keydate         = sy-datum
        language        = sy-langu
      TABLES
        allocvaluesnum  = lt_allocvaluesnum
        allocvalueschar = lt_allocvalueschar
        allocvaluescurr = lt_allocvaluescurr
        return          = lt_return.

    LOOP AT lt_allocvaluesnum ASSIGNING FIELD-SYMBOL(<lfs_allocvaluesnum>).
      IF <lfs_allocvaluesnum>-charact = 'KAP_SAYISI'.
        DATA(lv_kap) = <lfs_allocvaluesnum>-value_from.
*            ELSEIF <lfs_allocvaluesnum>-charact = ''. "zparti karakteristik !!!
      ENDIF.
    ENDLOOP.

    LOOP AT lt_allocvalueschar ASSIGNING FIELD-SYMBOL(<lfs_allocvalueschar>).
      IF <lfs_allocvalueschar>-charact = 'ZPARTI'.
        DATA(lv_zparti) = <lfs_allocvalueschar>-value_char.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lfs_data>) WHERE charg = <lfs_x>-charg.
      <lfs_data>-kap = lv_kap.
      <lfs_data>-zparti = lv_zparti.
    ENDLOOP.

*    ENDLOOP.

  ENDIF.

ENDLOOP.
