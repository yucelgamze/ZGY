*&---------------------------------------------------------------------*
*& Include          ZSD_002_FLOW_LCL
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor,
      get_data,
      call_screen,
      pbo_0100,
      pai_0100 IMPORTING iv_ucomm TYPE sy-ucomm,
      set_fcat,
      set_layout,
      display_alv.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.

  METHOD get_data.
  ENDMETHOD.

  METHOD call_screen.
    CALL SCREEN 0100.
  ENDMETHOD.

  METHOD pbo_0100.
    SET PF-STATUS '0100'.
    SET TITLEBAR '0100'.
  ENDMETHOD.

  METHOD pai_0100.
    CASE iv_ucomm.
      WHEN '&BACK'.
        SET SCREEN 0.
    ENDCASE.
  ENDMETHOD.

  METHOD set_fcat.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZSD_002_S_ALV'
      CHANGING
        ct_fieldcat      = gt_fcat.

  ENDMETHOD.

  METHOD set_layout.
    gs_layout = VALUE #( zebra      = abap_true
                         col_opt    = abap_true
                         cwidth_opt = abap_true
                         sel_mode   = |A| ).
  ENDMETHOD.

  METHOD display_alv.
    IF  go_alv_grid IS INITIAL.
      CREATE OBJECT go_container
        EXPORTING
          container_name = 'CC_ALV'.

      CREATE OBJECT go_alv_grid
        EXPORTING
*         i_parent = go_container.
          i_parent = cl_gui_container=>screen0.

      CALL METHOD go_alv_grid->set_table_for_first_display
        EXPORTING
          is_layout       = gs_layout                 " Layout
        CHANGING
          it_outtab       = gt_alv                 " Output Table
          it_fieldcatalog = gt_fcat.                " Field Catalog

    ELSE.
      CALL METHOD go_alv_grid->refresh_table_display( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
