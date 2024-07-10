*&---------------------------------------------------------------------*
*& Report ZGY_OOP_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_oop_004.

INCLUDE:zgy_oop_004_top,
        zgy_oop_004_lcl,
        zgy_oop_004_mdl.

INITIALIZATION.
*  go_local = NEW lcl_class( ).  böyle yaptığımızda constructor a girmiyor.

START-OF-SELECTION.

  go_local1 = NEW lcl_class( ).
  go_local2 = NEW lcl_class( ).
  go_local3 = NEW lcl_class( ).

  go_local1->make_assigments(
    iv_pers_id   = '0000000001'
    iv_pers_name = 'HOMER'
    iv_pers_age  = '39'
  ).

  go_local2->make_assigments(
  iv_pers_id   = '0000000002'
  iv_pers_name = 'MARGE'
  iv_pers_age  = '32'
).

  go_local3->make_assigments(
  iv_pers_id   = '0000000003'
  iv_pers_name = 'BART'
  iv_pers_age  = '10'
).

  WRITE:go_local1->mv_pers_id, go_local1->mv_pers_name, go_local1->mv_pers_age.
  WRITE:/ go_local2->mv_pers_id, go_local2->mv_pers_name, go_local2->mv_pers_age.
  WRITE:/ go_local3->mv_pers_id, go_local3->mv_pers_name, go_local3->mv_pers_age.
