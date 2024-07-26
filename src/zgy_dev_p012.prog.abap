*&---------------------------------------------------------------------*
*& Report ZGY_DEV_P012
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_dev_p012.

INCLUDE:zgy_dev_p012_top,
        zgy_dev_p012_lcl,
        zgy_dev_p012_mdl.

START-OF-SELECTION.

  go_local = NEW lcl_class( ).

  CASE abap_true.
    WHEN rb_load.
      go_local->image_upload( ).
    WHEN rb_print.
      go_local->print_adobe( ).
  ENDCASE.
