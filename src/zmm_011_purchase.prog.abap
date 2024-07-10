*&---------------------------------------------------------------------*
*& Report ZMM_011_PURCHASE
*&---------------------------------------------------------------------*
*&ABAP Consultant: Gamze Yücel
*&MM Consultant  : Ayşegül Aydoğan
*&---------------------------------------------------------------------*
REPORT zmm_011_purchase.

INCLUDE:zmm_011_purchase_top,
        zmm_011_purchase_lcl,
        zmm_011_purchase_mdl.

INITIALIZATION.

  go_local = NEW lcl_class( ).

START-OF-SELECTION.

  IF  gv_purchase = abap_true.
    go_local->get_data( ).
    go_local->set_fcat( ).
    go_local->set_layout( ).

    IF gt_alv IS NOT INITIAL.
      go_local->update_req( ).
      go_local->send_mail( ).
    ENDIF.
    go_local->call_screen( ).

  ELSEIF   gv_list = abap_true.
    go_local->db_list( ).
    go_local->set_fcat( ).
    go_local->set_layout( ).
    go_local->call_screen_list( ).
  ENDIF.
