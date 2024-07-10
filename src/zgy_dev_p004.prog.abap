*&---------------------------------------------------------------------*
*& Report ZGY_DEV_P004
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_dev_p004.

INCLUDE:zgy_dev_p004_top,
        zgy_dev_p004_lcl,
        zgy_dev_p004_mdl.

INITIALIZATION.


START-OF-SELECTION.

  go_local = NEW select_list(
    dbtab  = CONV #(  p_tab  )
    except = 'mandt'
  ).

  DATA(token)  = go_local->get_token( ).
  DATA(target) = go_local->get_target( ).

  SELECT (token)
  FROM (p_tab)
  INTO CORRESPONDING FIELDS OF TABLE @<fs_table>.

*  cl_demo_output=>display( target->* ).
  cl_demo_output=>display_data(
    value   = <fs_table> ).
