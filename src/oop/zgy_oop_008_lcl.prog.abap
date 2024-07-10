*&---------------------------------------------------------------------*
*& Include          ZGY_OOP_001_LCL
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      get_legs ABSTRACT RETURNING VALUE(rv_legs) TYPE i, "interface
      get_arms RETURNING VALUE(rv_arms) TYPE i.          "inheritance

    DATA:mv_legs TYPE i,
         mv_arms TYPE i.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD get_arms.
    rv_arms = mv_arms.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_cat DEFINITION INHERITING FROM lcl_class.
  PUBLIC SECTION.
    METHODS:
      constructor,
      get_legs REDEFINITION.
ENDCLASS.

CLASS lcl_cat IMPLEMENTATION.
  METHOD constructor.

    super->constructor( ).

    mv_arms = 0.
    mv_legs = 4.
  ENDMETHOD.

  METHOD get_legs.
    rv_legs = mv_legs.
  ENDMETHOD.

ENDCLASS.
