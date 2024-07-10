*&---------------------------------------------------------------------*
*& Include          ZMM_005_STOCK_LCL
*&---------------------------------------------------------------------*
  CLASS lcl_class DEFINITION.
    PUBLIC SECTION.
      METHODS:
        get_data,
        print_adobe,
        call_screen,
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

      SELECT
      mkpf~cpudt,
      lfa1~lifnr,
      lfa1~name1,
      t001~butxt,
      mseg~lgort,
      t001l~lgobe,
      mseg~ebeln,
      mseg~mblnr,
      mkpf~xblnr
      FROM mseg
      INNER JOIN mkpf ON mseg~mblnr = mkpf~mblnr
      LEFT JOIN lfa1 ON mseg~lifnr = lfa1~lifnr
      LEFT JOIN t001 ON mseg~bukrs = t001~bukrs
      LEFT JOIN t001l ON mseg~lgort = t001l~lgort
      WHERE mseg~mblnr = @p_mblnr
      INTO TABLE @DATA(lt_data).

      DELETE ADJACENT DUPLICATES FROM lt_data COMPARING mblnr.

      BREAK gamzey.

      LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lfs_data>).
        gs_data = CORRESPONDING #( <lfs_data> ).
        gs_data = VALUE #( BASE gs_data
                               datum     = sy-datum
                               uname     = sy-uname
                               tedarikci = | { <lfs_data>-lifnr }{ <lfs_data>-name1 } |
                               depo      = | { <lfs_data>-lgort }{ <lfs_data>-lgobe } |
                              ).
      ENDLOOP.

      IF sy-subrc IS INITIAL AND lt_data IS NOT INITIAL.
        SELECT
        mseg~matnr,
        makt~maktx,
        mseg~meins,
        mseg~bpmng,
        mseg~erfmg,
        mseg~dmbtr,
        ekko~ebeln,
        ekpo~peinh,
        makt~spras
        FROM mseg
        LEFT JOIN makt ON mseg~matnr = makt~matnr
        INNER JOIN ekko ON mseg~ebeln = ekko~ebeln
        INNER JOIN ekpo ON ekko~ebeln = ekpo~ebeln
        FOR ALL ENTRIES IN @lt_data
        WHERE mseg~mblnr = @lt_data-mblnr
          AND   makt~spras = 'T'
        INTO TABLE @DATA(lt_tab).

        LOOP AT lt_tab ASSIGNING FIELD-SYMBOL(<lfs_tab>).
          gs_tab = CORRESPONDING #( <lfs_tab> ).
          gs_tab-br_fiyat = | { <lfs_tab>-peinh } |.
          APPEND gs_tab TO gt_tab.
        ENDLOOP.

      ENDIF.

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
            i_name     = 'ZMM_005_AF_STOCK'                                 "
          IMPORTING
            e_funcname = fm_name.

        CALL FUNCTION fm_name
          EXPORTING
            /1bcdwb/docparams = fp_docparams
            iv_mblnr          = p_mblnr
* IMPORTING
*           /1BCDWB/FORMOUTPUT       =
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
*      IF go_alv_grid IS INITIAL.
*        CREATE OBJECT go_container
*          EXPORTING
*            container_name = 'CC_ALV'.
*        CREATE OBJECT go_alv_grid
*          EXPORTING
**           i_parent = go_container.
*            i_parent = cl_gui_custom_container=>screen0.
*        CALL METHOD go_alv_grid->set_table_for_first_display
*          EXPORTING
*            is_layout       = gs_layout   " Layout
*          CHANGING
*            it_outtab       = gt_alv   " Output Table
*            it_fieldcatalog = gt_fcat.   " Field Catalog
*      ELSE.
*        CALL METHOD go_alv_grid->refresh_table_display.
*      ENDIF.
    ENDMETHOD.
  ENDCLASS.
