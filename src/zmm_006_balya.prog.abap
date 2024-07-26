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

  IF p_mblnr IS NOT INITIAL.
    gr_mblnr = VALUE #(
                        sign   = 'I'
                        option = 'CP'
*                      option = 'EQ'
                        low    = |{ p_mblnr }| ).
    APPEND gr_mblnr TO gr_mblnr[].
  ENDIF.

*  go_local->call_screen( ).
  go_local->print_adobe( ).
