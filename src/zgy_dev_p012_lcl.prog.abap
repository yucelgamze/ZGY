*&---------------------------------------------------------------------*
*& Include          ZGY_DEV_P012_LCL
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS:
      image_upload,
      print_adobe.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD image_upload.

    DATA:ls_000 TYPE zgy_t_000.

    DATA:lv_length   TYPE i,
         lt_data_tab TYPE TABLE OF x255,
         lv_buffer   TYPE xstring.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = CONV #( p_file ) " Name of file
        filetype                = 'BIN'            " File Type (ASCII, Binary)
      IMPORTING
        filelength              = lv_length        " File Length
      CHANGING
        data_tab                = lt_data_tab      " Transfer table for file contents
      EXCEPTIONS
        file_open_error         = 1                " File does not exist and cannot be opened
        file_read_error         = 2                " Error when reading file
        no_batch                = 3                " Front-End Function Cannot Be Executed in Backgrnd
        gui_refuse_filetransfer = 4                " Incorrect front end or error on front end
        invalid_type            = 5                " Incorrect parameter FILETYPE
        no_authority            = 6                " No Upload Authorization
        unknown_error           = 7                " Unknown error
        bad_data_format         = 8                " Cannot Interpret Data in File
        header_not_allowed      = 9                " Invalid header
        separator_not_allowed   = 10               " Invalid separator
        header_too_long         = 11               " Header information currently restricted to 1023 bytes
        unknown_dp_error        = 12               " Error when calling data provider
        access_denied           = 13               " Access to File Denied
        dp_out_of_memory        = 14               " Not Enough Memory in DataProvider
        disk_full               = 15               " Storage Medium full
        dp_timeout              = 16               " Timeout of DataProvider
        not_supported_by_gui    = 17               " GUI does not support this
        error_no_gui            = 18               " GUI not available
        OTHERS                  = 19
    ).

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_length
      IMPORTING
        buffer       = lv_buffer
      TABLES
        binary_tab   = lt_data_tab
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

    CLEAR:ls_000.
    ls_000 = VALUE #( BASE ls_000
                                 id     = p_id
                                 zimage = lv_buffer ).
    MODIFY zgy_t_000 FROM ls_000.
    IF sy-subrc IS INITIAL.
      MESSAGE i004(zgy_000) DISPLAY LIKE 'S'.
    ENDIF.

  ENDMETHOD.

  METHOD print_adobe.

    SELECT SINGLE
    db~zimage
    FROM zgy_t_000 AS db
    WHERE id = @p_id
    INTO @DATA(lv_zimage).

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
          i_name     = 'ZGY_AF_000'
        IMPORTING
          e_funcname = fm_name.

      CALL FUNCTION fm_name
        EXPORTING
          /1bcdwb/docparams = fp_docparams
          iv_image          = lv_zimage
*       IMPORTING
*         /1BCDWB/FORMOUTPUT       =
        EXCEPTIONS
          usage_error       = 1
          system_error      = 2
          internal_error    = 3
          OTHERS            = 4.

      CALL FUNCTION 'FP_JOB_CLOSE'
        EXCEPTIONS
          usage_error    = 1
          system_error   = 2
          internal_error = 3
          OTHERS         = 4.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
