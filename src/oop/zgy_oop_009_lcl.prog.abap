*&---------------------------------------------------------------------*
*& Include          ZGY_OOP_009_LCL
*&---------------------------------------------------------------------*

CLASS lcl_animal DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      get_type ABSTRACT,
      speak ABSTRACT.
ENDCLASS.

CLASS lcl_dog DEFINITION INHERITING FROM lcl_animal.
  PUBLIC SECTION.
    METHODS:
      get_type REDEFINITION,
      speak REDEFINITION.
ENDCLASS.

CLASS lcl_dog IMPLEMENTATION.
  METHOD get_type.
    WRITE:|Köpek|.
  ENDMETHOD.

  METHOD speak.
    WRITE:|Hav hav hav|.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_cat DEFINITION INHERITING FROM lcl_animal.
  PUBLIC SECTION.
    METHODS:
      get_type REDEFINITION,
      speak REDEFINITION.
ENDCLASS.

CLASS lcl_cat IMPLEMENTATION.
  METHOD get_type.
    WRITE:|Kedi|.
  ENDMETHOD.

  METHOD speak.
    WRITE:|Miyaaaaavvv|.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_local DEFINITION.
  PUBLIC SECTION.
    METHODS:
      play IMPORTING io_animal TYPE REF TO lcl_animal.
ENDCLASS.

CLASS lcl_local IMPLEMENTATION.
  METHOD play.
    WRITE:|Bu:|.
    io_animal->get_type( ).
    WRITE:|şu sesi çıkarır:|.
    io_animal->speak( ).
    NEW-LINE.
  ENDMETHOD.
ENDCLASS.
