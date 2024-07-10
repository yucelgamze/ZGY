*&---------------------------------------------------------------------*
*& Report ZPP_013_QTRANS
*&---------------------------------------------------------------------*
*&ABAP Consultant: Gamze Yücel
*&PP Consultant  : Kemal Yılmaz
*&---------------------------------------------------------------------*
REPORT zpp_013_qtrans MESSAGE-ID zpp_013.

INCLUDE: zpp_013_qtrans_top,
         zpp_013_qtrans_lcl,
         zpp_013_qtrans_mdl.

INITIALIZATION.

  go_local = NEW lcl_class( ).

START-OF-SELECTION.

  go_local->call_screen( ).
