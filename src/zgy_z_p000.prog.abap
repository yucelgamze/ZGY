*&---------------------------------------------------------------------*
*& Report ZGY_Z_P000
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_z_p000.

DATA:gt_itab        TYPE TABLE OF scarr,
     go_salv        TYPE REF TO cl_salv_table,
     go_display     TYPE REF TO cl_salv_display_settings,
     go_columns     TYPE REF TO cl_salv_columns,
     go_column      TYPE REF TO cl_salv_column,
     go_column2     TYPE REF TO cl_salv_column,
     go_functions   TYPE REF TO cl_salv_functions,
     go_form_layout TYPE REF TO cl_salv_form_layout_grid,
     go_label       TYPE REF TO cl_salv_form_label,
     go_flow        TYPE REF TO cl_salv_form_layout_flow.

INITIALIZATION.

START-OF-SELECTION.

  SELECT * FROM scarr INTO CORRESPONDING FIELDS OF TABLE @gt_itab.

  cl_salv_table=>factory(
    IMPORTING
      r_salv_table   =  go_salv                         " Basis Class Simple ALV Tables
    CHANGING
      t_table        = gt_itab
  ).

  go_display   = go_salv->get_display_settings( ).
  go_columns   = go_salv->get_columns( ).
  go_column    = go_columns->get_column( columnname = 'CURRCODE' ).
  go_column2   = go_columns->get_column( columnname = 'MANDT' ).
  go_functions = go_salv->get_functions( ).



  go_display->set_list_header( value = |SALV DISPLAY| ).
  go_display->set_striped_pattern( value = |X| ).

  go_columns->set_optimize( value = 'X' ).

  TRY.
      go_column->set_long_text( value = 'Ulusal Para Birimi' ).
    CATCH cx_salv_not_found.
  ENDTRY.

  TRY.
      go_column2->set_visible( value = if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
  ENDTRY.

  go_functions->set_all( value = if_salv_c_bool_sap=>true ).

  CREATE OBJECT go_form_layout.

  go_label = go_form_layout->create_label(
               row         = 1                 " Natural Number
               column      = 1                 " Natural Number
             ).

  go_label->set_text( value = 'Label Text' ).

  go_flow = go_form_layout->create_flow(
              row     =  2                " Natural Number
              column  =  1                " Natural Number
            ).

  go_flow->create_text(
    EXPORTING
      text     = 'Flow Text'
  ).

  go_salv->set_end_of_list( value = go_form_layout ).

  go_salv->set_screen_popup(
    start_column = 5
    end_column   = 50
    start_line   = 5
    end_line     = 50
  ).

  go_salv->display( ).
