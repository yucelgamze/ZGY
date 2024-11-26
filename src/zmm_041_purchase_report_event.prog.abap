*&---------------------------------------------------------------------*
*& Include          ZMM_041_PURCHASE_REPORT_EVENT
*&---------------------------------------------------------------------*

INITIALIZATION.
  go_local = NEW lcl_class( ).

  gs_variant_default = VALUE #( BASE gs_variant_default
                                     report  = sy-repid ).

  CALL FUNCTION 'LVC_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save        = 'A'
    CHANGING
      cs_variant    = gs_variant_default
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.

  IF sy-subrc IS INITIAL.
    p_var = gs_variant_default-variant.
  ENDIF.


*AT SELECTION-SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var.
  CALL FUNCTION 'LVC_VARIANT_F4'
    EXPORTING
      is_variant    = gs_variant_default
      i_save        = 'A'
    IMPORTING
      es_variant    = gs_variant_default
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF sy-subrc IS INITIAL.
    p_var = gs_variant_default-variant.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_ernam-low.
  go_local->on_value_req_for_pr_ernam( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_sasby-low.
  go_local->on_value_req_for_po_ernam( ).


START-OF-SELECTION.

  go_local->get_data( ).
  go_local->modify_data( ).
  go_local->fullname( ).
  go_local->color_data( ).
  go_local->set_fcat( ).
  go_local->set_layout( ).


  IF gt_alv IS NOT INITIAL.
    go_local->call_screen( ).
  ELSE.
    MESSAGE s000(zmm_041) DISPLAY LIKE 'W'.
    LEAVE LIST-PROCESSING.
    LEAVE TO TRANSACTION 'ZMMR037'.
  ENDIF.
