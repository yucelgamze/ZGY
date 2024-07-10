*&---------------------------------------------------------------------*
*& Report ZGY_TEST_000
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_test_000.

DATA:gv_matnr  TYPE matnr,
     gv_charg  TYPE mseg-charg,
     gv_concat TYPE char28.

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

DATA(concat) = |{ gv_matnr }{ gv_concat }|. "yani toplamda 28 karakter olacak

DATA(lv_len) = strlen( concat ).

IF lv_len NE 28.

  DATA(lv_space) = 28 - lv_len.

*  SHIFT gv_matnr RIGHT BY lv_space PLACES. "başına ekliyor

*  SHIFT gv_matnr LEFT BY lv_space PLACES. "baştan lv_space kadarını siliyor

*  DO lv_space TIMES.
*    gv_matnr = |{ gv_matnr  WIDTH = 18 PAD = space }|.
*    CONCATENATE gv_matnr ` ` INTO gv_matnr RESPECTING BLANKS.
*  ENDDO.

  SHIFT gv_concat RIGHT BY lv_space PLACES.

ENDIF.

gv_matnr = |{ gv_matnr }{ gv_concat }|.

DATA(lv_len_x) = strlen( gv_matnr ).
WRITE: lv_len_x.
WRITE: / ,gv_matnr.
