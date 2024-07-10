*&---------------------------------------------------------------------*
*& Report ZGY_OOP_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_oop_002.

INCLUDE:zgy_oop_002_top,
        zgy_oop_002_lcl,
        zgy_oop_002_mdl.

INITIALIZATION.
  go_local = NEW lcl_class( ).

START-OF-SELECTION.

  go_local->sum( ).

  go_local->sum_v2(
    EXPORTING
      iv_val1 = p_val1
      iv_val2 = p_val2
    IMPORTING
      ev_sum  = gv_sum
  ).
  go_local->sub( ).

  gv_change = p_val2.

  go_local->sub_v2(
    EXPORTING
      iv_val1 = p_val1
    CHANGING
      cv_val1 = gv_change
  ).

  go_local->mul(
    EXPORTING
      iv_val1 = p_val1
      iv_val2 = p_val2
    RECEIVING
      rv_mul  = gv_mul
  ).

  gv_mul = go_local->mul(
            iv_val1 = p_val1
            iv_val2 = p_val2
  ).
