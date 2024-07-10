*&---------------------------------------------------------------------*
*& Include          ZGY_OOP_001_TOP
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION DEFERRED.
DATA:go_local TYPE REF TO lcl_class.

PARAMETERS:p_val1 TYPE i,
           p_val2 TYPE i.

DATA:gv_sum    TYPE i,
     gv_sub    TYPE i,
     gv_mul    TYPE i,
     gv_change TYPE i.
