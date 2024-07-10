*&---------------------------------------------------------------------*
*& Include          ZGY_OOP_001_LCL
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor,
      make_assigments IMPORTING iv_pers_id   TYPE char10
                                iv_pers_name TYPE char20
                                iv_pers_age  TYPE numc2.
    DATA:mv_pers_id   TYPE char10,
         mv_pers_name TYPE char20.

    CLASS-METHODS:class_constructor.

    CLASS-DATA:mv_pers_age  TYPE numc2.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.

  METHOD class_constructor.
  ENDMETHOD.

  METHOD make_assigments.
    mv_pers_id    =  iv_pers_id   .
    mv_pers_name  =  iv_pers_name .
    mv_pers_age   =  iv_pers_age  .
  ENDMETHOD.

ENDCLASS.
