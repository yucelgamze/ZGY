*&---------------------------------------------------------------------*
*& Include          ZGY_DEV_P016_EVENTS
*&---------------------------------------------------------------------*

INITIALIZATION.

  go_local = NEW lcl_class( ).

*AT SELECTION-SCREEN.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_bukrs.

*AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.

  go_local->get_data( ).
  go_local->set_fcat( ).
  go_local->set_layout( ).
  go_local->call_screen( ).
