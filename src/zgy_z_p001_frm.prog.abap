*&---------------------------------------------------------------------*
*& Include          ZGY_Z_P001_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form data_changed
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ER_DATA_CHANGED
*&---------------------------------------------------------------------*

FORM data_changed  USING er_data_changed TYPE REF TO cl_alv_changed_data_protocol.


  DATA: ls_modi       TYPE lvc_s_modi,
        lt_good_index TYPE lvc_t_row.

  DATA: ref_grid TYPE REF TO cl_gui_alv_grid,
        l_valid  TYPE c.

  LOOP AT er_data_changed->mt_good_cells INTO ls_modi.
    READ TABLE gt_alv ASSIGNING <gfs_alv> INDEX ls_modi-row_id.
    IF sy-subrc IS INITIAL AND <gfs_alv> IS ASSIGNED.
      CASE ls_modi-fieldname.
        WHEN 'EDIT'.

          <gfs_alv>-edit = ls_modi-value.
          <gfs_alv>-field = ls_modi-value.

          MESSAGE | 'Row:', { ls_modi-row_id }, 'Field:', { ls_modi-fieldname }, 'Value:', { ls_modi-value }.| TYPE 'I' .

*          ASSIGN COMPONENT ls_modi-fieldname OF STRUCTURE <gfs_alv> TO FIELD-SYMBOL(<fs_value>).
*          IF sy-subrc = 0.
*            <fs_value> = ls_modi-value.
*            <gfs_alv>-field = <fs_value>.
*          ENDIF.

      ENDCASE.
    ENDIF.
  ENDLOOP.

*  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*    IMPORTING
*      e_grid = ref_grid.
*
*  IF ref_grid IS NOT INITIAL.
*    CALL METHOD ref_grid->check_changed_data( ).
*  ENDIF.

  IF er_data_changed IS BOUND.
    er_data_changed->refresh_protocol( ).
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form top_of_page
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*

FORM top_of_page .
*ALV Header declarations
  DATA: lt_header     TYPE slis_t_listheader,
        ls_header     TYPE slis_listheader,
        t_line        LIKE ls_header-info,
        ld_lines      TYPE i,
        ld_linesc(10) TYPE c.

* Title
  CLEAR ls_header.
  ls_header-typ  = 'H'.
  ls_header-info = 'TEST REPORT'.
  APPEND ls_header TO lt_header.


* Date
  CLEAR: ls_header.
  ls_header-typ  = 'S'.
  ls_header-key = 'Date: '.
  ls_header-info = |{ sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum+0(4) }|.
  APPEND ls_header TO lt_header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_header.
  "top-of-page
ENDFORM.


*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .

  SELECT
  ekko~ebeln,
  ekpo~ebelp,
  ekko~bstyp,
  ekko~bsart,
  ekpo~matnr,
  ekpo~menge,
  ekpo~meins
  FROM ekko
  INNER JOIN ekpo ON ekko~ebeln EQ ekpo~ebeln
  UP TO 10 ROWS
  INTO CORRESPONDING FIELDS OF TABLE @gt_alv.

  SORT gt_alv BY ebeln ebelp.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_fcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_fcat .

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name   = sy-repid
*     i_internal_tabname = 'GT_ALV'
      i_structure_name = 'ZGY_S_PO'
      i_inclname       = sy-repid
    CHANGING
      ct_fieldcat      = gt_fcat.

  LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<lfs_fcat>).
    CASE <lfs_fcat>-fieldname.
      WHEN 'EDIT'.
        <lfs_fcat> = VALUE #( BASE <lfs_fcat>
                                   edit = abap_true ).
    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_layout
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_layout .

  gs_layout  = VALUE #( BASE gs_layout
                             window_titlebar   = |Reuse ALV|
                             zebra             = abap_true
                             colwidth_optimize = abap_true
  ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form display_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_alv .

  CLEAR:gs_event.
  gs_event-name = slis_ev_top_of_page.
  gs_event-form = 'TOP_OF_PAGE'.
  APPEND gs_event TO gt_events.

  CLEAR:gs_event.
  gs_event-name = slis_ev_data_changed.
  gs_event-form = 'DATA_CHANGED'.
  APPEND gs_event TO gt_events.

*  DATA:ls_grid TYPE lvc_s_glay.
*  ls_grid-edt_cll_cb = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER = ' '
*     I_BUFFER_ACTIVE    = ' '
      i_callback_program = sy-repid
*     I_CALLBACK_PF_STATUS_SET    = ' '
*     I_CALLBACK_USER_COMMAND = ' '
*     i_callback_top_of_page  = 'TOP_OF_PAGE'
*     I_CALLBACK_HTML_TOP_OF_PAGE = ' '
*     I_CALLBACK_HTML_END_OF_LIST = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
*     I_GRID_TITLE       =
*     i_grid_settings    = ls_grid
      is_layout          = gs_layout
      it_fieldcat        = gt_fcat
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS  =
*     IT_SORT            =
*     IT_FILTER          =
*     IS_SEL_HIDE        =
*     I_DEFAULT          = 'X'
*     I_SAVE             = ' '
*     IS_VARIANT         =
      it_events          = gt_events
*     IT_EVENT_EXIT      =
*     IS_PRINT           =
*     IS_REPREP_ID       =
*     I_SCREEN_START_COLUMN   = 0
*     I_SCREEN_START_LINE     = 0
*     I_SCREEN_END_COLUMN     = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  = 0
*     I_HTML_HEIGHT_END  = 0
*     IT_ALV_GRAPHICS    =
*     IT_HYPERLINK       =
*     IT_ADD_FIELDCAT    =
*     IT_EXCEPT_QINFO    =
*     IR_SALV_FULLSCREEN_ADAPTER  =
*     O_PREVIOUS_SRAL_HANDLER =
*     IMPORTING
*     E_EXIT_CAUSED_BY_CALLER =
*     ES_EXIT_CAUSED_BY_USER  =
    TABLES
      t_outtab           = gt_alv.
ENDFORM.
