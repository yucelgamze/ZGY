*&---------------------------------------------------------------------*
*& Report ZGY_DEV_P005
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_dev_p005.

PARAMETERS:p_tab TYPE tabname.

DATA:dbtab      TYPE string,
     except     TYPE string,
     token      TYPE string,
     target     TYPE REF TO data,
     components TYPE cl_abap_structdescr=>component_table.

DATA:lt_tab TYPE REF TO data.
FIELD-SYMBOLS : <fs_table> TYPE ANY TABLE.

dbtab = CONV #( p_tab ).
except = 'mandt'.

DATA:lo_line_type TYPE REF TO cl_abap_structdescr.

lo_line_type ?= cl_abap_typedescr=>describe_by_name( p_tab ).     "wide cast

DATA(lo_table_type) = cl_abap_tabledescr=>create( p_line_type = lo_line_type ).

components = CAST cl_abap_structdescr(
  cl_abap_typedescr=>describe_by_name( to_upper( dbtab ) )
    )->get_components( ).

SPLIT except AT `,` INTO TABLE DATA(columns).
LOOP AT columns ASSIGNING FIELD-SYMBOL(<column>).
  DELETE components WHERE name = to_upper( condense( <column> ) ).
ENDLOOP.

token =
  REDUCE string( INIT s = ``
                 FOR <wa> IN components
                 NEXT s &&=  COND #( WHEN s = ``  THEN <wa>-name
                                     ELSE  `, ` && <wa>-name ) ).

DATA(itab_type) =
 cl_abap_tabledescr=>get(
         p_line_type  = cl_abap_structdescr=>get(
                           p_components = components )
         p_table_kind = cl_abap_tabledescr=>tablekind_std ).
CREATE DATA lt_tab TYPE HANDLE itab_type.  "lo_table_type yerine bu geldi

ASSIGN lt_tab->* TO <fs_table>.       "dereference pointer

SELECT (token) FROM (p_tab) INTO CORRESPONDING FIELDS OF TABLE @<fs_table>.

cl_demo_output=>display_data(
  value   = <fs_table> ).
