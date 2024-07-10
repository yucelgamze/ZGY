*&---------------------------------------------------------------------*
*& Include          ZGY_OOP_001_LCL
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS:
      sum,
      sum_v2 IMPORTING iv_val1 TYPE i
                       iv_val2 TYPE i
             EXPORTING ev_sum  TYPE i,
      sub,
      sub_v2 IMPORTING iv_val1 TYPE i
             CHANGING  cv_val1 TYPE i,

      mul IMPORTING iv_val1       TYPE i
                    iv_val2       TYPE i
          RETURNING VALUE(rv_mul) TYPE i.

    DATA:mv_sum TYPE i,
         mv_sub TYPE i.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD sum.
    mv_sum = p_val1 + p_val2.
    WRITE:/,|Toplam:|,mv_sum.
  ENDMETHOD.

  METHOD sum_v2.
    ev_sum = iv_val1 + iv_val2.
    WRITE:/,|Toplam v2:|,ev_sum.
  ENDMETHOD.

  METHOD sub.
    mv_sub = p_val1 - p_val2.
    WRITE:/,|Fark:|,mv_sub.
  ENDMETHOD.

  METHOD sub_v2.
    cv_val1 = p_val1 - cv_val1.
    WRITE:/,|Fark v2:|,cv_val1.
  ENDMETHOD.

  METHOD mul.
    rv_mul = iv_val1 * iv_val2.
    WRITE:/,|Çarpım:|,rv_mul.
  ENDMETHOD.

ENDCLASS.
