FUNCTION zgy_fm_0001.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_MATNR) TYPE  ZGY_TT_001
*"  EXPORTING
*"     VALUE(ET_KONTROL) TYPE  ZGY_TT_002
*"----------------------------------------------------------------------

  DATA:es_kontrol TYPE zgy_s_002.

  IF it_matnr IS NOT INITIAL.

    SELECT
    matnr
    FROM mara
    FOR ALL ENTRIES IN @it_matnr
    WHERE matnr = @it_matnr-matnr
    INTO TABLE @DATA(lt_mara).


    LOOP AT it_matnr ASSIGNING FIELD-SYMBOL(<lfs_matnr>).

      LOOP AT lt_mara ASSIGNING FIELD-SYMBOL(<lfs_mara>) WHERE matnr = <lfs_matnr>-matnr.
        EXIT.
      ENDLOOP.

      IF sy-subrc IS INITIAL .
        es_kontrol = CORRESPONDING #( <lfs_matnr> ).
        es_kontrol = VALUE #( BASE es_kontrol
                              kontrol = 'VAR' ).
      ELSE.
        es_kontrol = CORRESPONDING #( <lfs_matnr> ).
        es_kontrol = VALUE #( BASE es_kontrol
                              kontrol = 'YOK' ).
      ENDIF.

      APPEND es_kontrol TO et_kontrol.
    ENDLOOP.


*    LOOP AT it_matnr ASSIGNING FIELD-SYMBOL(<lfs_matnr>).
*
*      READ TABLE lt_mara ASSIGNING FIELD-SYMBOL(<lfs_mara>) WITH KEY matnr = <lfs_matnr>-matnr.
*      IF sy-subrc IS INITIAL.
*        es_kontrol = CORRESPONDING #( <lfs_matnr> ).
*        es_kontrol = VALUE #( BASE es_kontrol
*                              kontrol = 'VAR' ).
*      ELSE.
*        es_kontrol = CORRESPONDING #( <lfs_matnr> ).
*        es_kontrol = VALUE #( BASE es_kontrol
*                              kontrol = 'YOK' ).
*      ENDIF.
*
*      APPEND es_kontrol TO et_kontrol.
*    ENDLOOP.

  ENDIF.

ENDFUNCTION.
