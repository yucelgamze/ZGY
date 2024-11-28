*&---------------------------------------------------------------------*
*& Include          ZGY_TEST_003_CLS
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS:
      get_data,
      upload_xls,
      download_xls,
      download_template_xls,
      template_xls_button,
      call_screen,
      pbo_0100,
      pai_0100,
      set_fcat,
      set_layout,
      display_alv.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD get_data.
    SELECT DISTINCT ebeln,
                    ebelp
    FROM ekpo
    UP TO 20 ROWS
    INTO TABLE @gt_data.
  ENDMETHOD.

  METHOD upload_xls.

    DATA:lt_raw TYPE truxs_t_text_data.
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        i_line_header        = abap_true
        i_tab_raw_data       = lt_raw
        i_filename           = p_file
      TABLES
        i_tab_converted_data = gt_xls
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.

  ENDMETHOD.

  METHOD download_xls.

    CLEAR:gs_header.
    gs_header-header = 'Satınalma Belgesi'.
    APPEND gs_header TO gt_header.

    CLEAR:gs_header.
    gs_header-header = 'Kalem'.
    APPEND gs_header TO gt_header.


    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = CONV string( p_fitab )
        filetype                = 'ASC'
        write_field_separator   = abap_true
      TABLES
        data_tab                = gt_data
        fieldnames              = gt_header
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.


  ENDMETHOD.

  METHOD download_template_xls.

**    DATA:lt_excel_format TYPE TABLE OF gty_excel WITH HEADER LINE,
**         ls_excel_format TYPE gty_excel.

    DATA: excel    TYPE ole2_object,
          workbook TYPE ole2_object,
          sheet    TYPE ole2_object,
          cell     TYPE ole2_object,
          row      TYPE ole2_object.

    CREATE OBJECT excel 'EXCEL.APPLICATION'.

    CALL METHOD OF excel 'WORKBOOKS' = workbook.

    SET PROPERTY OF excel 'VISIBLE' = 0 .

    SET PROPERTY OF excel 'VISIBLE' = 1 .

    CALL METHOD OF workbook 'add'.

    CALL METHOD OF excel 'Worksheets' = sheet EXPORTING #1 = 1.

    CALL METHOD OF sheet 'Activate'.

    " Excel Raw Header Line
    CALL METHOD OF excel 'RANGE' = cell EXPORTING #1 = 'A1'.
    SET PROPERTY OF cell 'VALUE' = 'Satınalma Belgesi'.
    CALL METHOD OF excel 'RANGE' = cell EXPORTING #1 = 'B1'.
    SET PROPERTY OF cell 'VALUE' = 'Kalem'.


*  " Excel Sample Row 1
**  CLEAR ls_excel_format.
**  ls_excel_format = VALUE #( BASE ls_excel_format
**                                  EBELN = '4100000124' ).
**  APPEND ls_excel_format TO lt_excel_format.
**
**  " Append Excel Sample Data to Internal Table.
**  LOOP AT lt_excel_format.
**    CALL METHOD OF excel 'ROWS' = row EXPORTING #1 = '2'.
**    CALL METHOD OF row 'INSERT' NO FLUSH.
**    CALL METHOD OF excel 'RANGE' = cell EXPORTING #1 = 'A2'.
**    SET PROPERTY OF cell 'VALUE' = lt_excel_format-ebelm NO FLUSH.
**  ENDLOOP.

    CALL METHOD OF excel 'SAVE'.
    CALL METHOD OF excel 'QUIT'.

    FREE OBJECT cell.
    FREE OBJECT workbook.
    FREE OBJECT excel.
    excel-handle = -1.
    FREE OBJECT row.

  ENDMETHOD.

  METHOD template_xls_button.
    gs_sel_button = VALUE #( BASE gs_sel_button
                              icon_id   = icon_xls
                              icon_text = 'Template Excel'
                              quickinfo = 'Download Template Excel' ).

    sscrfields-functxt_01 = gs_sel_button.
  ENDMETHOD.

  METHOD call_screen.
    CALL SCREEN 0100.
  ENDMETHOD.

  METHOD pbo_0100.
    SET PF-STATUS '0100'.
    SET TITLEBAR '0100'.
  ENDMETHOD.

  METHOD pai_0100.
    CASE sy-ucomm.
      WHEN '&BACK'.
        SET SCREEN 0.
      WHEN 'XLS'.
        me->download_xls( ).
    ENDCASE.
  ENDMETHOD.

  METHOD set_fcat.
    CLEAR:gs_fcat.
    gs_fcat = VALUE #( BASE gs_fcat
                            fieldname = 'EBELN'
                            tabname   = 'EKPO'
                            scrtext_l = 'Satınalma Belgesi'
                            scrtext_m = 'Satınalma Belgesi'
                            scrtext_s = 'Satınalma Belgesi'
                            ).
    APPEND gs_fcaT TO gt_fcat.

    CLEAR:gs_fcat.
    gs_fcat = VALUE #( BASE gs_fcat
                            fieldname = 'EBELP'
                            tabname   = 'EKPO'
                            scrtext_l = 'Kalem'
                            scrtext_m = 'Kalem'
                            scrtext_s = 'Kalem'
                            ).
    APPEND gs_fcaT TO gt_fcat.
  ENDMETHOD.

  METHOD set_layout.
    gs_layout =  VALUE #( col_opt    = abap_true
                          cwidth_opt = abap_true
                          zebra      = abap_true ).
  ENDMETHOD.

  METHOD display_alv.

    IF go_alv_grid IS INITIAL.
      IF p_file IS NOT INITIAL.
        CREATE OBJECT go_alv_grid
          EXPORTING
            i_parent = cl_gui_container=>screen0.               " Parent Container

        CALL METHOD go_alv_grid->set_table_for_first_display
          EXPORTING
            is_layout       = gs_layout             " Layout
          CHANGING
            it_outtab       = gt_xls                " Output Table
            it_fieldcatalog = gt_fcat.                " Field Catalog
      ELSE.
        CREATE OBJECT go_alv_grid
          EXPORTING
            i_parent = cl_gui_container=>screen0.               " Parent Container

        CALL METHOD go_alv_grid->set_table_for_first_display
          EXPORTING
            is_layout       = gs_layout             " Layout
          CHANGING
            it_outtab       = gt_data                " Output Table
            it_fieldcatalog = gt_fcat.                " Field Catalog

      ENDIF.
    ELSE.
      CALL METHOD go_alv_grid->refresh_table_display( ).
    ENDIF.


  ENDMETHOD.
ENDCLASS.
