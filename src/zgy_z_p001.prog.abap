*&---------------------------------------------------------------------*
*& Report ZGY_Z_P001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_z_p001.

INCLUDE:zgy_z_p001_top,
        zgy_z_p001_lcl,
        zgy_z_p001_frm,
        zgy_z_p001_mdl.

INITIALIZATION.

  go_local = NEW lcl_class( ).

START-OF-SELECTION.

*  go_local->get_data( ).
*  go_local->set_fcat( ).
*  go_local->set_layout( ).
*  go_local->display_alv( ).

  PERFORM get_data.
  PERFORM set_fcat.
  PERFORM set_layout.
  PERFORM display_alv.
