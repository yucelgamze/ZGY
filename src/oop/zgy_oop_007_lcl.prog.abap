*&---------------------------------------------------------------------*
*& Include          ZGY_OOP_007_LCL
*&---------------------------------------------------------------------*

INTERFACE lif_animal.
  METHODS:
    get_arms RETURNING VALUE(rv_arms) TYPE i,
    get_legs RETURNING VALUE(rv_legs) TYPE i.

  DATA:mv_arms TYPE i,
       mv_legs TYPE i.

ENDINTERFACE.

CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor.
    INTERFACES lif_animal.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD constructor.
    lif_animal~mv_arms = 0.
    lif_animal~mv_legs = 4.
  ENDMETHOD.

  METHOD lif_animal~get_arms.
    rv_arms = lif_animal~mv_arms.
  ENDMETHOD.

  METHOD lif_animal~get_legs.
    rv_legs = lif_animal~mv_legs.
  ENDMETHOD.
ENDCLASS.
