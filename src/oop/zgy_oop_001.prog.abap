*&---------------------------------------------------------------------*
*& Report ZGY_OOP_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_oop_001.

INCLUDE:zgy_oop_001_top,
        zgy_oop_001_lcl,
        zgy_oop_001_mdl.

INITIALIZATION.
  go_local = NEW lcl_class( ).

START-OF-SELECTION.

  go_local->sum( ).
  go_local->sub( ).
