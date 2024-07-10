*&---------------------------------------------------------------------*
*& Report ZGY_OOP_009
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_oop_009.

INCLUDE:zgy_oop_009_top,
        zgy_oop_009_lcl,
        zgy_oop_009_mdl.


START-OF-SELECTION.

  go_dog   = NEW lcl_dog( ).
  go_cat   = NEW lcl_cat( ).
  go_local = NEW lcl_local( ).

  go_local->play( io_animal = go_dog ).
  go_local->play( io_animal = go_cat ).
