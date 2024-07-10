*&---------------------------------------------------------------------*
*& Report ZGY_DEV_P001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_dev_p010.

INCLUDE:zgy_dev_p010_top,
        zgy_dev_p010_lcl,
        zgy_dev_p010_mdl.

INITIALIZATION.
  go_local = NEW lcl_class( ).

  go_local->set_fcat( ).
  go_local->set_fcat2( ).
  go_local->set_layout( ).
  go_local->display_alv( ).

START-OF-SELECTION.
  go_local->get_data( ).


*  go_local->call_screen( ).
