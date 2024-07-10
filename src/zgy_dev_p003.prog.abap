*&---------------------------------------------------------------------*
*& Report ZGY_DEV_P003
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_dev_p003.

PARAMETERS:p_tab type tabname.

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
        value   = <fs_table>
*        name    =                  " Name
*        exclude =                  " Exclude structure components
*        include =                  " Include structure components
      ).
