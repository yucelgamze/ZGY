*&---------------------------------------------------------------------*
*& Report ZSD_002_FLOW
*&---------------------------------------------------------------------*
*&ABAP Consultant: Gamze Yücel
*&SD Consultant  : Sinem Nur Ateş
*&---------------------------------------------------------------------*
REPORT zsd_002_flow.

INCLUDE:zsd_002_flow_top,
        zsd_002_flow_lcl,
        zsd_002_flow_mdl.

INITIALIZATION.

  go_local = NEW lcl_class( ).

START-OF-SELECTION.

  IF p_done = abap_true.
    go_local->done( ).
  ELSEIF ( p_done = abap_false ).
    IF so_tes IS NOT INITIAL.
      go_local->teslimat( ).
    ELSEIF so_fat IS NOT INITIAL.
      go_local->fatura( ).
    ELSE.
      go_local->get_data( ).
    ENDIF.

  ENDIF.

  go_local->set_fcat( ).
  go_local->set_layout( ).
  go_local->call_screen( ).
