*&---------------------------------------------------------------------*
*& Report ZGY_OOP_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_oop_006.

INCLUDE:zgy_oop_006_top,
        zgy_oop_006_lcl,
        zgy_oop_006_mdl.

INITIALIZATION.


START-OF-SELECTION.

  go_local = NEW lcl_class( ).
  go_cat   = NEW lcl_cat( ).


  WRITE:go_local->get_arms( ).  "protected olsaydı burada çağıramazdık hem de aşağıda da çağıramazdık
                                  "private olsaydı aşağıdaki miras sınıfta da çağıramazdık
  WRITE:/ go_local->get_legs( ).

  WRITE:/ go_cat->get_arms( ).
  WRITE:/ go_cat->get_legs( ).
