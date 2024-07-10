*&---------------------------------------------------------------------*
*& Report ZPP_002_ISEMRI
*&---------------------------------------------------------------------*
*&ABAP Consultant: Gamze Yücel
*&PP Consultant  : Mustafa Kayacı
*&---------------------------------------------------------------------*
REPORT zpp_002_isemri.
INCLUDE:zpp_002_isemri_top,
        zpp_002_isemri_cls,
        zpp_002_isemri_mdl.

START-OF-SELECTION.

  go_local = NEW lcl_class( ).

*  IF rb_gos = abap_true.
*
**--------------------------------------------------------------------* TEST GOS
*    DATA:manager TYPE REF TO cl_gos_manager,
*         obj     TYPE borident.
*
*    obj-objtype = sy-repid.
**  obj-objkey  = |CONVERSE0001|.       "caufv den plnbez = mara-matnr !!!!  ---> bir de plnbez ile marka da mapli olmalı!!!
*    obj-objkey  = CONV #( p_plnbez ).       "caufv den plnbez = mara-matnr !!!!  ---> bir de plnbez ile marka da mapli olmalı!!!
*
*    CREATE OBJECT manager
*      EXPORTING
*        is_object    = obj    " BOR Object
*        ip_no_commit = abap_false.    " See Fixed Values of SGS_CMODE
*
*    WRITE:| { p_plnbez } için GOS OBJECT MANAGER|.
**--------------------------------------------------------------------* TEST GOS
*  ENDIF.

  IF p_file IS NOT INITIAL AND gv_image = abap_true.
    go_local->image_upload( ).
  ENDIF.

  IF rb_domv IS NOT INITIAL AND gv_info = abap_true.
    go_local->get_brand_name( ).
  ENDIF.

  IF gv_pdf = abap_true.
    go_local->print_adobe( ).
  ENDIF.
*  go_local->call_screen( ).
