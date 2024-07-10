*&---------------------------------------------------------------------*
*& Include          ZGY_DEV_P004_LCL
*&---------------------------------------------------------------------*
CLASS select_list DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING dbtab  TYPE string
                            except TYPE string,
      get_token   RETURNING VALUE(token)  TYPE string,
      get_target  RETURNING VALUE(target) TYPE REF TO data,
      rtts.
  PRIVATE SECTION.
    DATA
    components TYPE cl_abap_structdescr=>component_table.
ENDCLASS.

CLASS select_list IMPLEMENTATION.
  METHOD constructor.
    components = CAST cl_abap_structdescr(
      cl_abap_typedescr=>describe_by_name( to_upper( dbtab ) )
        )->get_components( ).

    SPLIT except AT `,` INTO TABLE DATA(columns).
    LOOP AT columns ASSIGNING FIELD-SYMBOL(<column>).
      DELETE components WHERE name = to_upper( condense( <column> ) ).
    ENDLOOP.
  ENDMETHOD.
  METHOD get_token.

    token =
      REDUCE string( INIT s = ``
                     FOR <wa> IN components
                     NEXT s &&=  COND #( WHEN s = ``  THEN <wa>-name
                                         ELSE  `, ` && <wa>-name ) ).

  ENDMETHOD.
  METHOD get_target.

    DATA:lt_tab TYPE REF TO data.

    DATA(itab_type) =
     cl_abap_tabledescr=>get(
             p_line_type  = cl_abap_structdescr=>get(
                               p_components = components )
             p_table_kind = cl_abap_tabledescr=>tablekind_std ).

    CREATE DATA lt_tab TYPE HANDLE itab_type.
    ASSIGN lt_tab->* TO <fs_table>.       "dereference pointer

  ENDMETHOD.

  METHOD rtts.

*    DATA:tablename TYPE tabname.
*    tablename = iv_tab_for_excel.

    DATA:data_ref  TYPE REF TO data.

    DATA:lo_line_type TYPE REF TO cl_abap_structdescr.

    lo_line_type ?= cl_abap_typedescr=>describe_by_name( p_tab ).     "wide cast
    DATA(lo_table_type) = cl_abap_tabledescr=>create( p_line_type = lo_line_type ).

    DATA:lt_tab TYPE REF TO data.
    CREATE DATA lt_tab TYPE HANDLE lo_table_type.

    FIELD-SYMBOLS : <fs_table> TYPE ANY TABLE.

    ASSIGN lt_tab->* TO <fs_table>.       "dereference pointer

    SELECT * FROM (p_tab) INTO TABLE <fs_table>.

    cl_demo_output=>display_data(
      value   = <fs_table> ).
  ENDMETHOD.
ENDCLASS.
