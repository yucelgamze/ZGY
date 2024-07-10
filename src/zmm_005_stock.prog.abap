*&---------------------------------------------------------------------*
*& Report ZMM_005_STOCK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_005_stock.
INCLUDE: zmm_005_stock_top,
         zmm_005_stock_lcl,
         zmm_005_stock_mdl.

START-OF-SELECTION.
  go_local = NEW lcl_class( ).

  go_local->print_adobe( ).
