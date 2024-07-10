*&---------------------------------------------------------------------*
*& Include          ZGY_OOP_009_TOP
*&---------------------------------------------------------------------*

CLASS lcl_dog   DEFINITION DEFERRED.
CLASS lcl_cat   DEFINITION DEFERRED.
CLASS lcl_local DEFINITION DEFERRED.

DATA:go_dog   TYPE REF TO lcl_dog,
     go_cat   TYPE REF TO lcl_cat,
     go_local TYPE REF TO lcl_local.
