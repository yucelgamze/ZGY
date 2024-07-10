*&---------------------------------------------------------------------*
*& Include          ZGY_DEV_P004_TOP
*&---------------------------------------------------------------------*
CLASS select_list DEFINITION DEFERRED.

DATA:go_local TYPE REF TO select_list.

PARAMETERS:p_tab TYPE tabname.
DATA:dbtab  TYPE string.

FIELD-SYMBOLS : <fs_table> TYPE ANY TABLE.
