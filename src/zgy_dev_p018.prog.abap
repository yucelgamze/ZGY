*&---------------------------------------------------------------------*
*& Report ZGY_DEV_P018
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_dev_p018.

RANGES:gr_mblnr FOR mseg-mblnr,
       gr_matnr FOR mseg-matnr,
       gr_charg FOR mseg-charg,
       gr_xblnr FOR mkpf-xblnr.

TYPES:BEGIN OF gty_help,
        mblnr TYPE mseg-mblnr,
        charg TYPE mseg-charg,
        matnr TYPE mara-matnr,
        maktx TYPE makt-maktx,
        meins TYPE mara-meins,
      END OF gty_help.

DATA:gt_help TYPE TABLE OF gty_help.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-000.
  PARAMETERS:rb_ham   RADIOBUTTON GROUP a DEFAULT 'X' USER-COMMAND cmd,
             rb_balya RADIOBUTTON GROUP a,
             rb_kimya RADIOBUTTON GROUP a,
             rb_spare RADIOBUTTON GROUP a,
             rb_cloth RADIOBUTTON GROUP a,
             rb_stock RADIOBUTTON GROUP a,
             rb_knit  RADIOBUTTON GROUP a,
             rb_ware  RADIOBUTTON GROUP a.
SELECTION-SCREEN END OF BLOCK a.

SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE TEXT-001.
  PARAMETERS:p_mblnr TYPE mseg-mblnr MODIF ID x , "MATCHCODE OBJECT zmm_012_sh_material_document,
             p_lgpla TYPE lagp-lgpla MODIF ID y,
             p_matnr TYPE matnr MODIF ID z,
             p_charg TYPE mseg-charg MODIF ID w,
             p_xblnr TYPE mkpf-xblnr MODIF ID i.

*  SELECT-OPTIONS:so_mblnr FOR mseg-mblnr MODIF ID w NO INTERVALS,
*       so_matnr FOR mseg-matnr MODIF ID w NO INTERVALS,
*       so_charg FOR mseg-charg MODIF ID w NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_mblnr.

  REFRESH:gt_help.
  CLEAR:gt_help.

  CASE abap_true.
    WHEN rb_ham.

      "metotdan go_local->f4_sh( ) ile çağırabiliriz.
      "gv_mtart her rb için değeri değişir

      "parametreli DDL ile mtart içeriye gönderilir :

*      SELECT
*      ddl~mblnr,
*      ddl~charg,
*      ddl~matnr,
*      ddl~maktx,
*      ddl~meins
*      FROM zmm_012_ddl_md( p_mtart = 'Z210' ) AS ddl
*      INTO TABLE @gt_help
*      WHERE ddl~charg <> @space.
    WHEN rb_balya.
    WHEN rb_kimya.
    WHEN rb_spare.
    WHEN rb_cloth.
    WHEN rb_stock.
    WHEN rb_knit.
    WHEN rb_ware.
  ENDCASE.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'MBLNR'
      dynprofield = 'P_MBLNR'
      dynpprog    = sy-cprog
      dynpnr      = sy-dynnr
      value_org   = 'S'
    TABLES
      value_tab   = gt_help.


AT SELECTION-SCREEN OUTPUT.

  CASE abap_true.
    WHEN rb_ham.
      LOOP AT SCREEN.
        IF screen-group1 EQ 'X'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'Y'.
          screen-active = 0.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'Z'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'W'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'I'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    WHEN rb_balya.
      LOOP AT SCREEN.
        IF screen-group1 EQ 'X'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'Y'.
          screen-active = 0.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'Z'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'W'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'I'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    WHEN rb_kimya.
      LOOP AT SCREEN.
        IF screen-group1 EQ 'X'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'Y'.
          screen-active = 0.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'Z'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'W'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'I'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    WHEN rb_spare.
      LOOP AT SCREEN.
        IF screen-group1 EQ 'X'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'Y'.
          screen-active = 0.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'Z'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'W'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'I'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    WHEN rb_cloth.
      LOOP AT SCREEN.
        IF screen-group1 EQ 'X'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'Y'.
          screen-active = 0.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'Z'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'W'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'I'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    WHEN rb_stock.
      LOOP AT SCREEN.
        IF screen-group1 EQ 'X'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'Y'.
          screen-active = 0.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'Z'.
          screen-active = 0.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'W'.
          screen-active = 0.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'I'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    WHEN rb_knit.
      LOOP AT SCREEN.
        IF screen-group1 EQ 'X'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'Y'.
          screen-active = 0.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'Z'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'W'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'I'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN rb_ware.
      LOOP AT SCREEN.
        IF screen-group1 EQ 'X'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'Y'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'Z'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'W'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'I'.
          screen-active = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

  ENDCASE.

START-OF-SELECTION.

  CASE abap_true.
    WHEN rb_cloth.
      IF ( p_mblnr IS INITIAL ) AND ( p_matnr IS INITIAL ) AND  ( p_charg IS INITIAL ).
        MESSAGE s000(zmm_012) DISPLAY LIKE 'W'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    WHEN rb_ham.
      IF ( p_mblnr IS INITIAL ) AND ( p_matnr IS INITIAL ) AND  ( p_charg IS INITIAL ).
        MESSAGE s000(zmm_012) DISPLAY LIKE 'W'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    WHEN rb_kimya.
      IF ( p_mblnr IS INITIAL ) AND ( p_matnr IS INITIAL ) AND ( p_charg IS INITIAL ).
        MESSAGE s000(zmm_012) DISPLAY LIKE 'W'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    WHEN rb_spare.
      IF ( p_mblnr IS INITIAL ) AND ( p_matnr IS INITIAL ) AND ( p_charg IS INITIAL ).
        MESSAGE s000(zmm_012) DISPLAY LIKE 'W'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    WHEN rb_balya.
      IF ( p_mblnr IS INITIAL ) AND ( p_matnr IS INITIAL ) AND ( p_charg IS INITIAL ).
        MESSAGE s000(zmm_012) DISPLAY LIKE 'W'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    WHEN rb_knit.
      IF ( p_mblnr IS INITIAL ) AND ( p_matnr IS INITIAL ) AND ( p_charg IS INITIAL ).
        MESSAGE s000(zmm_012) DISPLAY LIKE 'W'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    WHEN rb_ware.
      IF ( p_xblnr IS INITIAL ) AND ( p_matnr IS INITIAL ) AND ( p_charg IS INITIAL ) AND ( p_lgpla IS INITIAL ).
        MESSAGE s000(zmm_012) DISPLAY LIKE 'W'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    WHEN OTHERS.
      IF p_mblnr IS INITIAL.
        MESSAGE s000(zmm_012) DISPLAY LIKE 'W'.
        LEAVE LIST-PROCESSING.
      ENDIF.

  ENDCASE.

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
