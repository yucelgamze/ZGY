*&---------------------------------------------------------------------*
*& Include          ZSD_002_FLOW_REPORTXXX_EVENT
*&---------------------------------------------------------------------*

INITIALIZATION.

  go_local = NEW lcl_class( ).

START-OF-SELECTION.

  go_local->get_data( ).
  go_local->modify_data( ).
  go_local->set_fcat( ).
  go_local->set_layout( ).
  IF gt_alv IS NOT INITIAL.
    go_local->call_screen( ).
  ELSE.
    MESSAGE TEXT-000 TYPE 'S' DISPLAY LIKE 'W'.
    LEAVE LIST-PROCESSING.
    LEAVE TO TRANSACTION 'ZGYRFLOW'.
  ENDIF.
