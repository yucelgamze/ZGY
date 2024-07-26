*&---------------------------------------------------------------------*
*& Include          ZGY_DEV_P013_LCL
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS:
      get_data,
      call_screen,
      pbo_0100,
      pai_0100,
      set_fcat,
      set_layout,
      display_alv,
      delete,

      register_f4,

      handle_onf4
        FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING
          e_fieldname
          e_fieldvalue
          es_row_no
          er_event_data
          et_bad_cells
          e_display,

      handle_top_of_page
        FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING
          e_dyndoc_id
          table_index,

      handle_hotspot_click
        FOR EVENT hotspot_click  OF cl_gui_alv_grid
        IMPORTING
          e_row_id
          e_column_id,

      handle_double_click
        FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING
          e_row
          e_column
          es_row_no,

      handle_data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING
          er_data_changed
          e_onf4
          e_onf4_before
          e_onf4_after
          e_ucomm,

      handle_button_click
        FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING
          es_col_id
          es_row_no,
      handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object
          e_interactive,
      handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm.

ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD get_data.
    SELECT *
      FROM scarr
      INTO CORRESPONDING FIELDS OF TABLE gt_alv.

    LOOP AT gt_alv ASSIGNING <gfs_alv>.
      <gfs_alv>-button = '@11@'.
    ENDLOOP.

  ENDMETHOD.

  METHOD call_screen.

    CALL SCREEN 0100.
  ENDMETHOD.

  METHOD pbo_0100.

    SET PF-STATUS '0100'.
* SET TITLEBAR 'xxx'.

  ENDMETHOD.

  METHOD pai_0100.

    CASE sy-ucomm.
      WHEN '&BACK'.
        SET SCREEN 0.
    ENDCASE.

  ENDMETHOD.

  METHOD set_fcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZGY_S_000'
      CHANGING
        ct_fieldcat      = gt_fcat.

    LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<lfs_fcat>).
      CASE <lfs_fcat>-fieldname.
        WHEN 'CARRNAME'.
          <lfs_fcat>-f4availabl = abap_true.
        WHEN 'BUTTON'.
          <lfs_fcat>-style = cl_gui_alv_grid=>mc_style_button.
          <lfs_fcat>-icon = abap_true.
        WHEN OTHERS.
      ENDCASE.
*          <lfs_fcat>-style = cl_gui_alv_grid=>mc_style_f4.
    ENDLOOP.


  ENDMETHOD.

  METHOD set_layout.

    gs_layout-zebra = abap_true.
    gs_layout-col_opt = abap_true.
    gs_layout-cwidth_opt = abap_true.
    gs_layout-sel_mode = 'A'. "A = çoklu seçim, B = tekli seçim.

  ENDMETHOD.

  METHOD display_alv.
    IF go_alv_grid IS  INITIAL.
*
*      CREATE OBJECT go_container
*        EXPORTING
*          container_name = 'CC_ALV'.
      CREATE OBJECT go_alv_grid
        EXPORTING
          i_parent = cl_gui_container=>screen0.

      go_local->register_f4( ).
      SET HANDLER go_local->handle_onf4          FOR go_alv_grid.
      SET HANDLER go_local->handle_top_of_page   FOR go_alv_grid.
      SET HANDLER go_local->handle_hotspot_click FOR go_alv_grid.
      SET HANDLER go_local->handle_double_click  FOR go_alv_grid.
      SET HANDLER go_local->handle_data_changed  FOR go_alv_grid.
      SET HANDLER go_local->handle_button_click  FOR go_alv_grid.
      SET HANDLER go_local->handle_toolbar       FOR go_alv_grid.
      SET HANDLER go_local->handle_user_command  FOR go_alv_grid.

      CALL METHOD go_alv_grid->set_table_for_first_display
        EXPORTING
          is_layout       = gs_layout
        CHANGING
          it_outtab       = gt_alv
          it_fieldcatalog = gt_fcat.

    ELSE.
      CALL METHOD go_alv_grid->refresh_table_display.
    ENDIF.
  ENDMETHOD.

  METHOD register_f4.

    DATA: lt_f4 TYPE  lvc_t_f4,
          ls_f4 TYPE lvc_s_f4.
    CLEAR: ls_f4.
    ls_f4-fieldname = 'CARRNAME'.
    ls_f4-register = abap_true.
    APPEND ls_f4 TO lt_f4.
    CALL METHOD go_alv_grid->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4.
  ENDMETHOD.

  METHOD handle_onf4.

    DATA: lt_return_tab TYPE TABLE OF ddshretval,
          ls_return_tab TYPE ddshretval.

    TYPES: BEGIN OF lty_value_tab,
             carrname TYPE s_carrname,
             pb_tanim TYPE char20,
           END OF lty_value_tab.

    DATA: lt_value_tab TYPE TABLE OF lty_value_tab,
          ls_value_tab TYPE lty_value_tab.
    CLEAR: lS_value_tab.
    ls_value_tab-carrname = 'TRY'.
    ls_value_tab-pb_tanim = 'Türk Lirası'.
    APPEND ls_value_tab TO lt_value_tab.

    CLEAR: lS_value_tab.
    ls_value_tab-carrname = 'EUR'.
    ls_value_tab-pb_tanim = 'Avrupa Eurosu'.
    APPEND ls_value_tab TO lt_value_tab.
    CLEAR: lS_value_tab.
    ls_value_tab-carrname = 'USD'.
    ls_value_tab-pb_tanim = 'Amerikan Doları'.
    APPEND ls_value_tab TO lt_value_tab.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield     = 'CARRNAME'
        window_title = 'Para Birimleri'
        value_org    = 'S'
      TABLES
        value_tab    = lt_value_tab
        return_tab   = lt_return_tab.

    READ TABLE lt_return_tab INTO ls_return_tab WITH KEY fieldname = 'F0001'.
    IF sy-subrc EQ 0.
      READ TABLE gt_alv ASSIGNING <gfs_alv> INDEX es_row_no-row_id.
      IF sy-subrc EQ 0.
        IF <gfs_alv> IS ASSIGNED.
          <gfs_alv>-carrname = ls_return_tab-fieldval.
          CALL METHOD go_alv_grid->refresh_table_display( ).
        ENDIF.
      ENDIF.
    ENDIF.
    er_event_data->m_event_handled = 'X'.
  ENDMETHOD.

  METHOD handle_top_of_page.

  ENDMETHOD.

  METHOD handle_hotspot_click.

  ENDMETHOD.

  METHOD handle_double_click.

  ENDMETHOD.

  METHOD handle_data_changed.

  ENDMETHOD.

  METHOD handle_button_click.

    READ TABLE gt_alv INTO gs_alv INDEX es_row_no-row_id.
    IF sy-subrc EQ 0.
      DATA(lv_id) = gs_alv-carrid.
      CASE es_col_id-fieldname.
        WHEN 'BUTTON'.
          DELETE gt_alv WHERE carrid EQ lv_id.
          IF sy-subrc EQ 0.
            COMMIT WORK.
            go_alv_grid->refresh_table_display( ).
            MESSAGE: i005(zgy_000) DISPLAY LIKE 'S'.
          ELSE.
            ROLLBACK WORK.
            MESSAGE i006(zgy_000) DISPLAY LIKE 'W'.
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD handle_toolbar.

    DATA: ls_toolbar TYPE stb_button.

    CLEAR: ls_toolbar.
    ls_toolbar-function = '&DEL'.
    ls_toolbar-text = 'Silme Butonu'.
    ls_toolbar-icon = '@11@'.
    ls_toolbar-quickinfo = 'Toolbar Silme Butonu'.
    ls_toolbar-disabled = abap_false.
    APPEND ls_toolbar TO e_object->mt_toolbar.
  ENDMETHOD.

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN '&DEL'.
        go_local->delete( ).
        CALL METHOD go_alv_grid->refresh_table_display( )..
    ENDCASE.
  ENDMETHOD.

  METHOD delete.

    DATA: lt_index_rows TYPE lvc_t_row.

    CALL METHOD go_alv_grid->get_selected_rows
      IMPORTING
        et_index_rows = lt_index_rows.

    IF lt_index_rows IS INITIAL.
      MESSAGE i000(zgy_000) DISPLAY LIKE 'E'.

    ELSE.
      LOOP AT lt_index_rows ASSIGNING FIELD-SYMBOL(<lfs_del>).
        READ TABLE gt_alv INTO gs_alv INDEX <lfs_del>-index.
        IF sy-subrc EQ 0.
          DATA(lv_id) = gs_alv-carrid.
        ENDIF.

        DELETE gt_alv WHERE carrid EQ lv_id.
      ENDLOOP.

      IF sy-subrc EQ 0.
        COMMIT WORK.
        MESSAGE: i005(zgy_000) DISPLAY LIKE 'S'.
      ELSE.
        ROLLBACK WORK.
        MESSAGE i006(zgy_000) DISPLAY LIKE 'W'.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
