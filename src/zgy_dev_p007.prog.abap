*&---------------------------------------------------------------------*
*& Report ZGY_DEV_P007
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_dev_p007.

".. CONV dtype|#( ... ) ...

DATA text TYPE c LENGTH 255.

text = 'gamzecik'.

DATA(xstr) = cl_abap_codepage=>convert_to( source = CONV #( text ) ).

WRITE:xstr.

*--------------------------------------------------------------------*

*.. CAST dtype|class|interface|#( ... ) ...

*You use CAST for a down cast where you needed helper variables before in order to cast with ?= to a requested reference type.
*You use CAST for an up cast, e,g, with an inline declaration, in order to construct a more general type.
*You can write a compnent selector -> directly behind CAST type( ... ).

*Before release 7.40

DATA structdescr TYPE REF TO cl_abap_structdescr.
structdescr ?= cl_abap_typedescr=>describe_by_name( 'MAKT' ).

DATA components  TYPE abap_compdescr_tab.

components = structdescr->components.

*With release 7.40

DATA(componentsx) = CAST cl_abap_structdescr(

  cl_abap_typedescr=>describe_by_name( 'MAKT' ) )->components.

LOOP AT componentsx ASSIGNING FIELD-SYMBOL(<lfs_comp>).
  WRITE:/,<lfs_comp>-name.
ENDLOOP.

*--------------------------------------------------------------------*
TYPES: BEGIN OF t_struc,
         col1 TYPE i,
         col2 TYPE i,
       END OF t_struc.

DATA dref  TYPE REF TO data.
DATA struc TYPE t_struc.

dref = NEW t_struc( ).

CAST t_struc( dref )->col1 = struc-col1.
