*&---------------------------------------------------------------------*
*& Report ZGY_DEV_P013
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_dev_p013.

INCLUDE:zgy_dev_p013_top,
        zgy_dev_p013_lcl,
        zgy_dev_p013_mdl.

START-OF-SELECTION.

go_local = NEW lcl_class( ).

go_local->get_data( ).
go_local->set_fcat( ).
go_local->set_layout( ).
go_local->call_screen( ).
