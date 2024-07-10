*&---------------------------------------------------------------------*
*& Report ZMM_001_CONTRACT
*&---------------------------------------------------------------------*
*&ABAP Consultant: Gamze Yücel
*&MM Consultant  : Oktay Ceviz
*&---------------------------------------------------------------------*
REPORT zmm_001_contract.

INCLUDE:zmm_001_contract_top,
        zmm_001_contract_lcl,
        zmm_001_contract_mdl.

START-OF-SELECTION.

  go_local = NEW lcl_class( ).

  go_local->get_data( ).
  go_local->set_fcat( ).
  go_local->set_layout( ).
  IF gt_alv IS NOT INITIAL.
    go_local->send_mail( ).
  ENDIF.
  go_local->call_screen( ).
