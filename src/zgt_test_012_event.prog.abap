*&---------------------------------------------------------------------*
*& Include          ZGT_TEST_012_EVENT
*&---------------------------------------------------------------------*

INITIALIZATION.
  go_local = NEW lcl_class( ).

  go_local->template_xls_button( ).

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      go_local->download_template_xls( ).
  ENDCASE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'P_FILE'
    IMPORTING
      file_name     = p_file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fitab.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'P_FITAB'
    IMPORTING
      file_name     = p_fitab.

START-OF-SELECTION.
  go_local->get_data( ).

  IF p_file IS NOT INITIAL.
    go_local->upload_xls( ).
  ENDIF.

  go_local->call_screen( ).
