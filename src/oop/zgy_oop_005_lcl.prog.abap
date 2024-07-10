*&---------------------------------------------------------------------*
*& Include          ZGY_OOP_001_LCL
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS:
      get_legs RETURNING VALUE(rv_legs) TYPE i,
      get_arms RETURNING VALUE(rv_arms) TYPE i.

    DATA:mv_legs TYPE i,
         mv_arms TYPE i.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD get_arms.
    rv_arms = mv_arms.
  ENDMETHOD.

  METHOD get_legs.
    rv_legs = mv_legs.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_cat DEFINITION INHERITING FROM lcl_class.
  PUBLIC SECTION.
    METHODS:
      constructor.
ENDCLASS.

CLASS lcl_cat IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_arms = 0.
    mv_legs = 4.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_bird DEFINITION INHERITING FROM lcl_class.
  PUBLIC SECTION.
    METHODS:
      constructor.
ENDCLASS.

CLASS lcl_bird IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_arms = 2.
    mv_legs = 2.
  ENDMETHOD.

ENDCLASS.
