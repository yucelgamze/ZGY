*&---------------------------------------------------------------------*
*& Report ZGY_OOP_007
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_oop_007.

INCLUDE:zgy_oop_007_top,
        zgy_oop_007_lcl,
        zgy_oop_007_mdl.


INITIALIZATION.

AT SELECTION-SCREEN.

AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.

  go_local = NEW lcl_class( ).

  WRITE:|Kedinin kol sayısı:|, go_local->lif_animal~get_arms( ), |Kedinin bacak sayısı:|, go_local->lif_animal~get_legs( ).
