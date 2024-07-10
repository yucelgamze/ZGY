*&---------------------------------------------------------------------*
*& Report ZGY_OOP_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_oop_005.

INCLUDE:zgy_oop_005_top,
        zgy_oop_005_lcl,
        zgy_oop_005_mdl.


INITIALIZATION.


START-OF-SELECTION.

  go_local = NEW lcl_class( ).
  go_cat   = NEW lcl_cat( ).
  go_bird  = NEW lcl_bird( ).


  WRITE:|Kedinin kol sayısı: |,go_cat->get_arms( ), |Kedinin bacak saysı: |, go_cat->get_legs( ).
  WRITE:/ |Kuşun kanat sayısı: |,go_bird->get_arms( ), |Kuşun bacak saysı: |, go_bird->get_legs( ).
