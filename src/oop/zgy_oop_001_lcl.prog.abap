*&---------------------------------------------------------------------*
*& Include          ZGY_OOP_001_LCL
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS:
      sum,
      sub.
    DATA:mv_sum TYPE i,
         mv_sub TYPE i.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD sum.
    mv_sum = p_val1 + p_val2.
    WRITE:/,|Toplam:|,mv_sum.
  ENDMETHOD.

  METHOD sub.
    mv_sub = p_val1 - p_val2.
    WRITE:/,|Fark:|,mv_sub.
  ENDMETHOD.

ENDCLASS.
