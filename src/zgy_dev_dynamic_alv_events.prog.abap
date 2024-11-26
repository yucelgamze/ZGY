*&---------------------------------------------------------------------*
*& Include          ZGY_DEV_P016_EVENTS
*&---------------------------------------------------------------------*

INITIALIZATION.

  go_local = NEW lcl_class( ).

*AT SELECTION-SCREEN.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_bukrs.

AT SELECTION-SCREEN OUTPUT.
  CASE abap_true.
    WHEN rb_sat.

      LOOP AT SCREEN.
        IF screen-group1 EQ 'X'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'Y'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    WHEN rb_mus.
      LOOP AT SCREEN.
        IF screen-group1 EQ 'Y'.
          screen-active = 1.
          MODIFY SCREEN.
        ELSEIF screen-group1 EQ 'X'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
  ENDCASE.

START-OF-SELECTION.

  go_local->get_data( ).
  go_local->set_fcat( ).
  go_local->set_layout( ).
  go_local->call_screen( ).
