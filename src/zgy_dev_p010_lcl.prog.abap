*&---------------------------------------------------------------------*
*& Include          ZGY_DEV_P001_LCL
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor,
      get_data,

      handle_double_click
        FOR EVENT double_click OF cl_gui_alv_grid   "DOUBLE_CLICK
        IMPORTING
          e_row
          e_column
          es_row_no,

      handle_top_of_page                            "TOP_OF_PAGE
        FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING
          e_dyndoc_id
          table_index,

      call_screen,
      pbo_0100,
      pai_0100 IMPORTING iv_ucomm TYPE sy-ucomm,
      set_fcat,
      set_fcat2,
      set_layout,
      display_alv.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.

  METHOD get_data.
    SELECT
    bkpf~bukrs,
    bkpf~belnr,
    bkpf~gjahr,
    bkpf~blart,
    t003t~ltext,
    bkpf~budat,
    bkpf~bktxt
    FROM bkpf
    LEFT JOIN t003t ON t003t~blart = bkpf~blart
    WHERE t003t~spras = 'T'
    AND   bkpf~bukrs  IN @so_bukrs
    AND   bkpf~belnr  IN @so_belnr
    AND   bkpf~gjahr  IN @so_gjahr
    INTO CORRESPONDING FIELDS OF TABLE @gt_header.

    EXPORT data = gt_header TO MEMORY ID sy-cprog.

  ENDMETHOD.

  METHOD handle_double_click.
    IF gt_header IS NOT INITIAL.
      READ TABLE gt_header INTO gs_header INDEX e_row-index.
      IF sy-subrc IS INITIAL.
*        CASE e_column-fieldname.
*          WHEN 'BELNR'.

        SELECT
        bseg~bukrs,
        bseg~belnr,
        bseg~gjahr,
        bseg~buzei,
        bseg~bschl,
        tbslt~ltext,
        bseg~dmbtr,
        bseg~wrbtr
        FROM bseg
        LEFT JOIN tbslt ON tbslt~bschl = bseg~bschl
        WHERE tbslt~spras = 'T'
        AND bseg~bukrs = @gs_header-bukrs
        AND bseg~belnr = @gs_header-belnr
        AND bseg~gjahr = @gs_header-gjahr
        INTO CORRESPONDING FIELDS OF TABLE @gt_item.
*        ENDCASE.

        EXPORT data2 = gt_item TO MEMORY ID sy-cprog.

        CALL METHOD go_alv_grid2->refresh_table_display( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD handle_top_of_page.

    DATA:lv_text TYPE sdydo_text_element.
    lv_text = |Top of Page Split ALV|.

    CALL METHOD go_document->add_text
      EXPORTING
        text      = lv_text
        sap_style = cl_dd_document=>heading.

    CALL METHOD go_document->new_line.

    CLEAR:lv_text.
    lv_text = | { sy-uname } - { sy-datum } |.
    CALL METHOD go_document->add_text
      EXPORTING
        text         = lv_text
        sap_color    = cl_dd_document=>list_positive
        sap_fontsize = cl_dd_document=>medium.

    CALL METHOD go_document->display_document
      EXPORTING
        parent = go_topofpage.

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
        i_structure_name = 'ZGY_S_ACCOUNT_H'
      CHANGING
        ct_fieldcat      = gt_fcat.
  ENDMETHOD.

  METHOD set_fcat2.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZGY_S_ACCOUNT_I'
      CHANGING
        ct_fieldcat      = gt_fcat2.
  ENDMETHOD.

  METHOD set_layout.
    gs_layout = VALUE #(
                 zebra      = abap_true
                 col_opt    = abap_true
                 cwidth_opt = abap_true
                 sel_mode   = 'A'
                 ).
  ENDMETHOD.

  METHOD display_alv.

    IF go_alv_grid IS INITIAL AND go_alv_grid2 IS INITIAL.

    IMPORT data = gt_header FROM MEMORY ID sy-cprog.
    FREE MEMORY ID sy-cprog.
    IMPORT data2 = gt_item  FROM MEMORY ID sy-cprog.
    FREE MEMORY ID sy-cprog.

    CHECK gt_header IS NOT INITIAL.

    CREATE OBJECT go_docking
      EXPORTING
        repid = sy-cprog
        dynnr = sy-dynnr
        ratio = 80
        side  = cl_gui_docking_container=>dock_at_bottom
        name  = 'DOCK_CONT'.

      TRY.
          go_container ?= go_docking.

*          CREATE OBJECT go_container
*            EXPORTING
*              container_name = 'CC_ALV'.

          CREATE OBJECT go_splitter
            EXPORTING
              parent  = go_container
              rows    = 3
              columns = 1.

          CALL METHOD go_splitter->get_container
            EXPORTING
              row       = 1
              column    = 1
            RECEIVING
              container = go_topofpage.

          CALL METHOD go_splitter->set_row_height
            EXPORTING
              id     = 1                 " Row ID
              height = 10.                 " Height

          CALL METHOD go_splitter->get_container
            EXPORTING
              row       = 2
              column    = 1
            RECEIVING
              container = go_split1.

          CALL METHOD go_splitter->get_container
            EXPORTING
              row       = 3
              column    = 1
            RECEIVING
              container = go_split2.

          CREATE OBJECT go_document
            EXPORTING
              style = 'ALV_GRID'.


          CREATE OBJECT go_alv_grid
            EXPORTING
              i_parent = go_split1.

          SET HANDLER me->handle_double_click FOR go_alv_grid.
          SET HANDLER me->handle_top_of_page  FOR go_alv_grid.

          CALL METHOD go_alv_grid->register_edit_event
            EXPORTING
              i_event_id = cl_gui_alv_grid=>mc_evt_modified.

          CALL METHOD go_alv_grid->list_processing_events
            EXPORTING
              i_event_name = 'TOP_OF_PAGE'
              i_dyndoc_id  = go_document.

          CALL METHOD go_alv_grid->set_table_for_first_display
            EXPORTING
              is_layout       = gs_layout                " Layout
            CHANGING
              it_outtab       = gt_header                " Output Table
              it_fieldcatalog = gt_fcat.              " Field Catalog


          CREATE OBJECT go_alv_grid2
            EXPORTING
              i_parent = go_split2.

          CALL METHOD go_alv_grid2->set_table_for_first_display
            EXPORTING
              is_layout       = gs_layout
            CHANGING
              it_outtab       = gt_item
              it_fieldcatalog = gt_fcat2.

        CATCH cx_salv_msg .
      ENDTRY.
    ELSE.
      CALL METHOD go_alv_grid->refresh_table_display( ).
      CALL METHOD go_alv_grid2->refresh_table_display( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
