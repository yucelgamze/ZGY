*&---------------------------------------------------------------------*
*& Include          ZMM_012_PRINT_LCL
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS:
      get_data,
      call_screen,
      print_adobe,
      pbo_0100,
      pai_0100 IMPORTING iv_ucomm TYPE sy-ucomm,
      set_fcat,
      set_layout,
      display_alv.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD get_data.
  ENDMETHOD.

  METHOD print_adobe.

    IF sy-subrc IS INITIAL.

      fp_outputparams-device   = 'PRINTER'.
      fp_outputparams-nodialog = abap_true.
      fp_outputparams-preview  = abap_true.
      fp_outputparams-dest     = 'LP01'.

      CALL FUNCTION 'FP_JOB_OPEN'
        CHANGING
          ie_outputparams = fp_outputparams
        EXCEPTIONS
          cancel          = 1
          usage_error     = 2
          system_error    = 3
          internal_error  = 4
          OTHERS          = 5.

      CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
        EXPORTING
          i_name     = gv_fm_name                                 "
        IMPORTING
          e_funcname = fm_name.

      CALL FUNCTION fm_name
        EXPORTING
          /1bcdwb/docparams = fp_docparams
          iv_mblnr          = p_mblnr
* IMPORTING
*         /1BCDWB/FORMOUTPUT       =
        EXCEPTIONS
          usage_error       = 1
          system_error      = 2
          internal_error    = 3
          OTHERS            = 4.

      CALL FUNCTION 'FP_JOB_CLOSE'
*       IMPORTING
*         E_RESULT             =
        EXCEPTIONS
          usage_error    = 1
          system_error   = 2
          internal_error = 3
          OTHERS         = 4.

    ENDIF.

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
        i_structure_name = ''
      CHANGING
        ct_fieldcat      = gt_fcat.
  ENDMETHOD.

  METHOD set_layout.
    gs_layout = VALUE #( zebra      = abap_true
                         cwidth_opt = abap_true
                         col_opt    = abap_true ).
  ENDMETHOD.

  METHOD display_alv.
*    IF go_alv_grid IS INITIAL.
*      CREATE OBJECT go_container
*        EXPORTING
*          container_name = 'CC_ALV'.
*      CREATE OBJECT go_alv_grid
*        EXPORTING
**         i_parent = go_container.
*          i_parent = cl_gui_custom_container=>screen0.
*      CALL METHOD go_alv_grid->set_table_for_first_display
*        EXPORTING
*          is_layout       = gs_layout   " Layout
*        CHANGING
*          it_outtab       = gt_alv   " Output Table
*          it_fieldcatalog = gt_fcat.   " Field Catalog
*    ELSE.
*      CALL METHOD go_alv_grid->refresh_table_display.
*    ENDIF.
  ENDMETHOD.
ENDCLASS.
