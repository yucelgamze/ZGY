*&---------------------------------------------------------------------*
*& Report ZMM_006_BALYA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_008_spare.

INCLUDE: zmm_008_spare_top,
         zmm_008_spare_lcl,
         zmm_008_spare_mdl.

START-OF-SELECTION.

  go_local = NEW lcl_class( ).

*  go_local->call_screen( ).
  go_local->print_adobe( ).
