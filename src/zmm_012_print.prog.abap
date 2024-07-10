*&---------------------------------------------------------------------*
*& Report ZMM_012_PRINT
*&---------------------------------------------------------------------*
*&ABAP Consultant: Gamze Yücel
*&MM Consultant  : Talha Camcıoğlu
*&---------------------------------------------------------------------*
REPORT zmm_012_print.

INCLUDE:zmm_012_print_top,
        zmm_012_print_lcl,
        zmm_012_print_mdl.

INITIALIZATION.

  go_local = NEW lcl_class( ).

START-OF-SELECTION.

  go_local->print_adobe( ).
