*&---------------------------------------------------------------------*
*& Report ZGY_OOP_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_oop_008.

INCLUDE:zgy_oop_008_top,
        zgy_oop_008_lcl,
        zgy_oop_008_mdl.

INITIALIZATION.


START-OF-SELECTION.

  go_cat   = NEW lcl_cat( ).


  WRITE:|Kedinin kol sayısı: |,go_cat->get_arms( ), |Kedinin bacak saysı: |, go_cat->get_legs( ).
