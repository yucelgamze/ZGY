*&---------------------------------------------------------------------*
*& Report ZGY_DEV_020
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_dev_020.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-000.
  PARAMETERS:rb_x RADIOBUTTON GROUP a DEFAULT 'X' USER-COMMAND cmd,
             rb_y RADIOBUTTON GROUP a.
SELECTION-SCREEN END OF BLOCK a.

PARAMETERS:p_a TYPE i MODIF ID x,
           p_b TYPE i MODIF ID x,
           p_c TYPE i MODIF ID x,
           p_d TYPE i MODIF ID x,
           p_e TYPE i MODIF ID y.

INITIALIZATION.

AT SELECTION-SCREEN.  "enter a basıldığında diğer parametreyi doldursun
  IF p_a IS NOT INITIAL.
    IF sy-ucomm NE 'ONLI'.

      SELECT SINGLE likp~inco1,
                    likp~inco2,
                    likp~vsart
        FROM vbss
        INNER JOIN likp ON vbss~vbeln = likp~vbeln
        INNER JOIN lips ON likp~vbeln = lips~vbeln
        INNER JOIN vbkd ON lips~vgbel = vbkd~vbeln
                       AND vbkd~posnr = ' '

        WHERE vbss~sammg = @p_a
        INTO @DATA(ls_dev).
      p_b = ls_dev-inco1.
      p_c = ls_dev-inco2.
      p_d = ls_dev-vsart.
    ENDIF.
  ENDIF.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_a.

AT SELECTION-SCREEN OUTPUT.   "parametreler halihazırda dolu gelsin
  CASE abap_true.
    WHEN rb_x.
*      p_a = 1.
*      p_b = 10.
      LOOP AT SCREEN.
        IF screen-group1 EQ 'X'.
          screen-active = 1.
          screen-display_3d = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'Y'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    WHEN rb_y.
*      p_c = 20.
      LOOP AT SCREEN.
        IF screen-group1 EQ 'Y'.
          screen-active = 1.
          screen-display_3d = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'X'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

  ENDCASE.



START-OF-SELECTION.
