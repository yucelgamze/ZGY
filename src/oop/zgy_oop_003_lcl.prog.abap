*&---------------------------------------------------------------------*
*& Include          ZGY_OOP_001_LCL
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor,
      sum.
    DATA:mv_sum  TYPE i,
         mv_val1 TYPE i,
         mv_val2 TYPE i.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.

  METHOD constructor.
    mv_val1 = p_val1.
    mv_val2 = p_val2.
  ENDMETHOD.

  METHOD sum.
    mv_sum = mv_val1 + mv_val2.
    WRITE:/,|Toplam:|,mv_sum.
  ENDMETHOD.

ENDCLASS.
