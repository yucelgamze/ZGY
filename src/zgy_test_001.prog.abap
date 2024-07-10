*&---------------------------------------------------------------------*
*& Report ZGY_TEST_000
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_test_001.

DATA:gv_matnr    TYPE matnr,
     gv_charg    TYPE mseg-charg,
     gv_concat   TYPE char28,
     lv_datatype TYPE dd01v-datatype.

gv_charg = |abc|.
gv_charg = |{ gv_charg ALPHA = IN }|.  "10 bu yemedi alpha in
DATA(lv_charg_len) = strlen( gv_charg ).

IF lv_charg_len NE 10.
  DATA(lv_lead) = 10 - lv_charg_len.
*  gv_charg = |{ gv_charg WIDTH = 10 PAD = '0' }|. "sonuna sıfır ekledi.
  WRITE gv_charg TO gv_charg RIGHT-JUSTIFIED.
  TRANSLATE gv_charg USING ' 0'.
ENDIF.

gv_concat = gv_charg.
gv_matnr = |ABC123456789|.  "bunun 18 olmasını istiyoruz

*gv_matnr = |{ gv_matnr WIDTH = 18 PAD = '0' }|. "sonuna sıfır ekledi.
*TRANSLATE gv_matnr USING '0 '.

DATA(concat) = |{ gv_matnr }{ gv_concat }|. "yani toplamda 28 karakter olacak
DATA(lv_len) = strlen( concat ).

CALL FUNCTION 'NUMERIC_CHECK'
  EXPORTING
    string_in = gv_matnr
  IMPORTING
    htype     = lv_datatype.

IF lv_datatype EQ 'NUMC'.

  gv_matnr = |{ gv_matnr }{ gv_concat }|.

ELSEIF gv_matnr CA sy-abcde OR lv_datatype NE 'NUMC'.
  IF lv_len NE 28.
    DATA(lv_space) = 28 - lv_len.
    SHIFT gv_concat RIGHT BY lv_space PLACES.
  ENDIF.

  gv_matnr = |{ gv_matnr }{ gv_concat }|.
ENDIF.

DATA(lv_len_x) = strlen( gv_matnr ).
WRITE: lv_len_x.
WRITE: / ,gv_matnr.
