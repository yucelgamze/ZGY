*&---------------------------------------------------------------------*
*& Report ZMM_006_BALYA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_006_balya.

INCLUDE:zmm_006_balya_top,
        zmm_006_balya_lcl,
        zmm_006_balya_mdl.

START-OF-SELECTION.

  go_local = NEW lcl_class( ).

*  go_local->call_screen( ).
  go_local->print_adobe( ).
