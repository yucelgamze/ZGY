*&---------------------------------------------------------------------*
*& Report ZGY_OOP_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_oop_003.

INCLUDE:zgy_oop_003_top,
        zgy_oop_003_lcl,
        zgy_oop_003_mdl.

INITIALIZATION.
*  go_local = NEW lcl_class( ).  böyle yaptığımızda constructor a girmiyor.

START-OF-SELECTION.

  go_local = NEW lcl_class( ).

*  go_local->mv_val1 = p_val1.
*  go_local->mv_val2 = p_val2.

  go_local->sum( ).
