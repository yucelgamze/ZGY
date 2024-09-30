*&---------------------------------------------------------------------*
*& Include          ZPP_013_QTRANS_LCL
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor,

      handle_data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid   "DATA_CHANGED
        IMPORTING
          er_data_changed
          e_onf4
          e_onf4_before
          e_onf4_after
          e_ucomm,

      register_f4 ,

      handle_onf4_depo_yeri
        FOR EVENT onf4 OF cl_gui_alv_grid            "ONF4
        IMPORTING
          e_fieldname
          e_fieldvalue
          es_row_no
          er_event_data
          et_bad_cells
          e_display,

      handle_onf4_parti_no
        FOR EVENT onf4 OF cl_gui_alv_grid            "ONF4
        IMPORTING
          e_fieldname
          e_fieldvalue
          es_row_no
          er_event_data
          et_bad_cells
          e_display,

      handle_onf4_kalite_malzeme
        FOR EVENT onf4 OF cl_gui_alv_grid            "ONF4
        IMPORTING
          e_fieldname
          e_fieldvalue
          es_row_no
          er_event_data
          et_bad_cells
          e_display,

      handle_onf4_hedef_depo
        FOR EVENT onf4 OF cl_gui_alv_grid            "ONF4
        IMPORTING
          e_fieldname
          e_fieldvalue
          es_row_no
          er_event_data
          et_bad_cells
          e_display,

      handle_button_click
        FOR EVENT button_click OF cl_gui_alv_grid   "BUTTON_CLICK
        IMPORTING
          es_col_id
          es_row_no,

      handle_user_command                                 "USER_COMMAND
        FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm,

      call_func,
      bapi_commit,
      after_commit,
      make_non_editable,
      add_row,
      delete_row,
      call_screen,
      call_popup_screen,
      pbo_0100,
      pbo_0200,
      pai_0100 IMPORTING iv_ucomm TYPE sy-ucomm,
      pai_0200 IMPORTING iv_ucomm TYPE sy-ucomm,
      set_fcat,
      set_fcat2,
      set_layout,
      display_alv,
      display_alv2.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.

  METHOD handle_data_changed.

    DATA: ls_modi TYPE lvc_s_modi.

    LOOP AT er_data_changed->mt_good_cells INTO ls_modi.
      READ TABLE gt_table ASSIGNING <gfs_table> INDEX ls_modi-row_id.

      IF sy-subrc IS INITIAL.
        CASE ls_modi-fieldname.
          WHEN 'MATNR'.
            <gfs_table>-matnr = ls_modi-value.
            gv_matnr  = ls_modi-value.
          WHEN 'LGORT'.
            <gfs_table>-lgort = ls_modi-value.
            gv_lgort = ls_modi-value.
          WHEN 'TRANSFER_MIKTAR'.
            <gfs_table>-transfer_miktar = ls_modi-value.
            gv_transfer_miktar = ls_modi-value.
          WHEN  'KALITE_MATNR'.
            <gfs_table>-kalite_matnr = ls_modi-value.
            gv_kalite_matnr = ls_modi-value.
          WHEN 'HEDEF_DEPO'.
            <gfs_table>-hedef_depo = ls_modi-value.
            gv_hedef_depo  = ls_modi-value.
          WHEN 'CHARG'.
            <gfs_table>-charg = ls_modi-value.                 "bapi de batch e eşlenecek !!
            gv_charg = ls_modi-value.
            gv_hedef_parti = ls_modi-value.
        ENDCASE.
      ENDIF.
    ENDLOOP.

    SELECT SINGLE
    mara~matnr,
    makt~maktx
    FROM mara
    LEFT JOIN makt ON mara~matnr = makt~matnr
    WHERE mara~matnr = @gv_matnr
    AND   makt~spras = 'T'
    INTO @DATA(ls_maktx).

*    SELECT SINGLE
*    mard~lgort,
*    mard~matnr,
*    mard~labst
*    FROM mard
*    INNER JOIN t001l ON mard~lgort = t001l~lgort
*    WHERE mard~matnr = @gv_matnr
*    AND   mard~lgort = @gv_lgort
*    AND   mard~werks IN @so_werks
*    AND   mard~lfgja = @so_datum-low+0(4)
*    AND   mard~lfmon = @so_datum-low+4(2)
*    AND   mard~lvorm = @space
*    INTO @DATA(ls_labst).

    SELECT SINGLE                     " stok miktarlarını mard dan değil, parti no üzerinden mchb den çekilecek revize
    mchb~lgort,
    mchb~matnr,
    mchb~clabs AS labst,
    mchb~charg
    FROM mchb
    INNER JOIN t001l ON mchb~lgort = t001l~lgort
    WHERE mchb~matnr = @gv_matnr
    AND   mchb~lgort = @gv_lgort
    AND   mchb~charg = @gv_charg
    AND   mchb~werks IN @so_werks
    AND   mchb~lfgja = @so_datum-low+0(4)
    AND   mchb~lfmon = @so_datum-low+4(2)
    AND   mchb~lvorm = @space
    INTO @DATA(ls_labst).

    SELECT SINGLE
    mara~matnr,
    mara~meins
    FROM mara
    WHERE mara~matnr = @gv_matnr
    INTO @DATA(ls_meins).

    SELECT SINGLE
    marc~matnr,
    makt~maktx
    FROM marc
    LEFT JOIN makt ON marc~matnr = makt~matnr
    WHERE marc~matnr = @gv_kalite_matnr
    AND   makt~spras = 'T'
    AND   marc~werks IN @so_werks
    INTO @DATA(ls_kalite_matnr).

*    SELECT SINGLE
*    mard~lgort,
*    mard~matnr,
*    mard~labst
*    FROM mard
*    INNER JOIN t001l ON mard~lgort = t001l~lgort
*    WHERE mard~matnr = @gv_kalite_matnr
*    AND   mard~lgort = @gv_hedef_depo
*    AND   mard~werks IN @so_werks
*    AND   mard~lfgja = @so_datum-low+0(4)
*    AND   mard~lfmon = @so_datum-low+4(2)
*    AND   mard~lvorm = @space
*    INTO @DATA(ls_hedef_depo).

    SELECT SINGLE                     " stok miktarlarını mard dan  değil, parti no üzerinden mchb den çekilecek revize
    mchb~lgort,
    mchb~matnr,
    mchb~clabs AS labst,
    mchb~charg
    FROM mchb
    INNER JOIN t001l ON mchb~lgort = t001l~lgort
    WHERE mchb~matnr = @gv_kalite_matnr
    AND   mchb~lgort = @gv_hedef_depo
    AND   mchb~charg = @gv_charg
    AND   mchb~werks IN @so_werks
    AND   mchb~lfgja = @so_datum-low+0(4)
    AND   mchb~lfmon = @so_datum-low+4(2)
    AND   mchb~lvorm = @space
    INTO @DATA(ls_hedef_depo).

*    DESCRIBE TABLE gt_table LINES gv_lines.
*
*    IF ( gv_lines = 0 ) OR ( gv_lines = 1 ).

    IF <gfs_table> IS ASSIGNED.
      <gfs_table> = VALUE #( BASE <gfs_table>
                                  maktx        = ls_maktx-maktx
                                  labst        = ls_labst-labst
                                  meins        = ls_meins-meins
                                  kalite_maktx = ls_kalite_matnr-maktx
                                  hedef_stok   = ls_hedef_depo-labst
***                                  hedef_parti  = ls_hedef_depo-charg
                                  ).

      CALL METHOD go_alv_grid->refresh_table_display( ).
    ENDIF.

    LOOP AT gt_table ASSIGNING <gfs_table>.
      IF <gfs_table> IS ASSIGNED.

        CLEAR:gs_cell_color.
        gs_cell_color = VALUE #( BASE gs_cell_color
         fname = 'MATNR'
         color-col = '6'
         color-int = '1'
         color-inv = '0' ).
        APPEND gs_cell_color TO <gfs_table>-cell_color.

        CLEAR:gs_cell_color.
        gs_cell_color = VALUE #( BASE gs_cell_color
         fname =  'KALITE_MATNR'
         color-col = '5'
         color-int = '1'
         color-inv = '0' ).
        APPEND gs_cell_color TO <gfs_table>-cell_color.
      ENDIF.
    ENDLOOP.

**    ELSE.
**
**      LOOP AT gt_table ASSIGNING FIELD-SYMBOL(<lfs_table_update>) WHERE matnr = ls_maktx-matnr
***                                                                  AND   lgort = ls_labst-lgort
***                                                                  AND   kalite_matnr = ls_kalite_matnr-matnr
***                                                                  AND   hedef_depo = ls_hedef_depo-lgort
**                                                                                                              .
**        <lfs_table_update> = VALUE #( BASE <lfs_table_update>
**                                    maktx        = ls_maktx-maktx
**                                    labst        = ls_labst-labst
**                                    meins        = ls_meins-meins
**                                    kalite_maktx = ls_kalite_matnr-maktx
**                                    hedef_stok   = ls_hedef_depo-labst ).
**      ENDLOOP.
**    ENDIF.
  ENDMETHOD.

  METHOD call_func.

    DATA:ls_header           TYPE bapi2017_gm_head_01,
         ls_code             TYPE bapi2017_gm_code,
         ls_head_ret         TYPE bapi2017_gm_head_ret,
         lv_materialdocument TYPE mblnr,
         lv_matdocumentyear  TYPE mjahr,
         lt_item             TYPE TABLE OF bapi2017_gm_item_create,
         ls_item             TYPE bapi2017_gm_item_create,
         lt_return           TYPE TABLE OF bapiret2,
         ls_return           TYPE bapiret2.

    DATA:lt_index_rows TYPE lvc_t_row.

    CALL METHOD go_alv_grid->get_selected_rows
      IMPORTING
        et_index_rows = lt_index_rows.

*    IF lt_index_rows IS INITIAL.
*      MESSAGE i003 DISPLAY LIKE 'E'.
*    ELSE.
*      LOOP AT lt_index_rows ASSIGNING FIELD-SYMBOL(<lfs_index_rows>).
*        READ TABLE gt_table ASSIGNING FIELD-SYMBOL(<lfs_table>) INDEX <lfs_index_rows>-index.
*        IF sy-subrc IS INITIAL.

    LOOP AT gt_table ASSIGNING FIELD-SYMBOL(<lfs_table>).
      CLEAR:ls_header,
            ls_code,
            ls_item,
            ls_head_ret,
            lv_materialdocument,
            lv_matdocumentyear.


      ls_header-pstng_date = so_datum-low.

      ls_code-gm_code = |06|.

      ls_item = CORRESPONDING #( <lfs_table> ).
      ls_item = VALUE #( BASE ls_item
                              material      = <lfs_table>-matnr
                              material_long = <lfs_table>-matnr
                              plant         = so_werks-low
                              stge_loc      = <lfs_table>-lgort
                              move_type     = |309|
                              entry_qnt     = <lfs_table>-transfer_miktar
                              entry_uom     = <lfs_table>-meins
                              move_mat      = <lfs_table>-kalite_matnr
                              move_mat_long = <lfs_table>-kalite_matnr
                              move_plant    = so_werks-low
                              move_stloc    = <lfs_table>-hedef_depo
                              batch         = <lfs_table>-charg
                              move_batch    = <lfs_table>-charg  """<lfs_table>-hedef_parti
                              ).
      APPEND ls_item TO lt_item.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header  = ls_header
          goodsmvt_code    = ls_code
          testrun          = 'X'
        IMPORTING
          goodsmvt_headret = ls_head_ret
          materialdocument = lv_materialdocument
          matdocumentyear  = lv_matdocumentyear
        TABLES
          goodsmvt_item    = lt_item
          return           = lt_return.

*      READ TABLE lt_return INTO ls_return WITH KEY TYPE = 'S'.
      LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'AEX'.
        EXIT.
      ENDLOOP.

*      IF sy-subrc NE 0.
      IF sy-subrc NE 0 AND ( <lfs_table>-kalite_matnr IS NOT INITIAL ) AND ( <lfs_table>-hedef_depo IS NOT INITIAL ) AND ( <lfs_table>-transfer_durumu NE '@2L@' ).

        <lfs_table>-transfer_durumu = '@01@'.

        gv_save = abap_true.

        CLEAR: <lfs_table>-type, <lfs_table>-message.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.


      ELSE.

        IF <lfs_table>-transfer_durumu NE '@2L@'.

          <lfs_table>-transfer_durumu = '@02@'.
          gv_save = abap_false.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

*        gt_return = CORRESPONDING #( lt_return ).
*        go_local->call_popup_screen( ).

          IF <lfs_table>-transfer_miktar = 0 .
            <lfs_table>-type = 'E'.
            <lfs_table>-message = |Transfer miktarı 0 olamaz !|.
          ELSEIF <lfs_table>-kalite_matnr IS INITIAL.
            <lfs_table>-type = 'E'.
            <lfs_table>-message = |2/3. Kalite Malzeme girin !|.
          ELSEIF <lfs_table>-hedef_depo IS INITIAL.
            <lfs_table>-type = 'E'.
            <lfs_table>-message = |Hedep Depo girin !|.
          ELSE.
            LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<lfs_return>).
              CLEAR:<lfs_table>-type,<lfs_table>-message.
              <lfs_table>-type = <lfs_return>-type.
              <lfs_table>-message = <lfs_return>-message.
            ENDLOOP.
          ENDIF.

        ENDIF.

*        ENDIF.

      ENDIF.

    ENDLOOP.

*    ENDIF.
  ENDMETHOD.

  METHOD bapi_commit.

    DATA:ls_header           TYPE bapi2017_gm_head_01,
         ls_code             TYPE bapi2017_gm_code,
         ls_head_ret         TYPE bapi2017_gm_head_ret,
         lv_materialdocument TYPE mblnr,
         lv_matdocumentyear  TYPE mjahr,
         lt_item             TYPE TABLE OF bapi2017_gm_item_create,
         ls_item             TYPE bapi2017_gm_item_create,
         lt_return           TYPE TABLE OF bapiret2,
         ls_return           TYPE bapiret2.

    DATA:lt_header_tab TYPE TABLE OF gty_table.

    lt_header_tab = gt_table.



    LOOP AT lt_header_tab INTO DATA(ls_header_tab) WHERE transfer_durumu = '@01@'.
      CLEAR:ls_header,
            ls_code,
            ls_item,
            ls_head_ret,
            lv_materialdocument,
            lv_matdocumentyear.
*      REFRESH:lt_item.

      ls_header-pstng_date = so_datum-low.

      ls_code-gm_code = |06|.

      LOOP AT gt_table ASSIGNING FIELD-SYMBOL(<lfs_table>) WHERE matnr = ls_header_tab-matnr
                                                           AND   lgort = ls_header_tab-lgort
                                                           AND   kalite_matnr = ls_header_tab-kalite_matnr
                                                           AND   hedef_depo = ls_header_tab-hedef_depo
                                                           AND   transfer_durumu = ls_header_tab-transfer_durumu.

        CLEAR:ls_item.
        ls_item = CORRESPONDING #( <lfs_table> ).
        ls_item = VALUE #( BASE ls_item
                                material      = <lfs_table>-matnr
                                material_long = <lfs_table>-matnr
                                plant         = so_werks-low
                                stge_loc      = <lfs_table>-lgort
                                move_type     = |309|
                                entry_qnt     = <lfs_table>-transfer_miktar
                                entry_uom     = <lfs_table>-meins
                                move_mat      = <lfs_table>-kalite_matnr
                                move_mat_long = <lfs_table>-kalite_matnr
                                move_plant    = so_werks-low
                                move_stloc    = <lfs_table>-hedef_depo
                                batch         = <lfs_table>-charg
                                move_batch    = <lfs_table>-charg """<lfs_table>-hedef_parti
                                ).
        APPEND ls_item TO lt_item.
      ENDLOOP.
    ENDLOOP.


    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header  = ls_header
        goodsmvt_code    = ls_code
      IMPORTING
        goodsmvt_headret = ls_head_ret
        materialdocument = lv_materialdocument
        matdocumentyear  = lv_matdocumentyear
      TABLES
        goodsmvt_item    = lt_item
        return           = lt_return.

    LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'AEX'.
      EXIT.
    ENDLOOP.

    IF sy-subrc NE 0.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      LOOP AT gt_table ASSIGNING <gfs_table> WHERE transfer_durumu NE '@2L@'.

        IF <gfs_table> IS ASSIGNED.
          CLEAR:<gfs_table>-type,<gfs_table>-message.
          <gfs_table>-transfer_durumu = '@2L@'.
          COMMIT WORK AND WAIT.
        ENDIF.

      ENDLOOP.

      gv_commit = abap_true.

*        <lfs_table>-transfer_miktar = 0.

      CALL METHOD go_alv_grid->refresh_table_display( ).

*--------------------------------------------------------------------*
      me->make_non_editable( ).    "commit işlemi başarılı olduysa disable edit!!
*--------------------------------------------------------------------*

    ELSE.
      LOOP AT gt_table ASSIGNING <gfs_table> WHERE transfer_durumu NE '@2L@'.
        IF <gfs_table> IS ASSIGNED.
          <gfs_table>-transfer_durumu = '@02@'.
        ENDIF.
      ENDLOOP.

      gv_save = abap_false.
      gv_commit = abap_false.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

*        gt_return = CORRESPONDING #( lt_return ).
*        go_local->call_popup_screen( ).

      LOOP AT gt_table ASSIGNING <gfs_table> WHERE transfer_durumu NE '@2L@'.
        IF <gfs_table> IS ASSIGNED.
          LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<lfs_return>).
            CLEAR:<gfs_table>-type,<gfs_table>-message.
            <gfs_table>-type = <lfs_return>-type.
            <gfs_table>-message = <lfs_return>-message.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.


    gt_trans = CORRESPONDING #( gt_table ).
    MODIFY zpp_013_t_trans FROM TABLE gt_trans.


  ENDMETHOD.

  METHOD after_commit.

    IF gv_commit = abap_true.
      MESSAGE i001 DISPLAY LIKE 'S'.

      SELECT
      transfer_durumu,
      matnr,
      maktx,
      lgort,
      charg,
      labst,
      meins,
      transfer_miktar,
      kalite_matnr,
      kalite_maktx,
      hedef_depo,
      hedef_parti,
      hedef_stok,
      type,
      message
      FROM zpp_013_t_trans
      INTO TABLE @DATA(lt_db)
      WHERE transfer_durumu = '@2L@'.

      IF lt_db IS NOT INITIAL.                                     " STOK MİKTARI -> MCHB DEN PARTİ DEN GELECEK DEĞİŞECEK ( mard iptal parti no esas alınacak )

*        SELECT
*        mard~lgort ,
*        mard~matnr ,
*        mard~labst
*        FROM mard
*        INNER JOIN t001l ON mard~lgort = t001l~lgort
*        INNER JOIN @lt_db AS db ON mard~matnr = db~matnr
*                               AND mard~lgort = db~lgort
*        WHERE
*         mard~werks IN @so_werks
*        AND   mard~lfgja = @so_datum-low+0(4)
*        AND   mard~lfmon = @so_datum-low+4(2)
*        AND   mard~lvorm = @space
*        INTO TABLE @DATA(lt_labst_inner).
*
*
*        SELECT
*        mard~lgort,
*        mard~matnr,
*        mard~labst
*        FROM mard
*        INNER JOIN t001l ON mard~lgort = t001l~lgort
*        INNER JOIN @lt_db AS db ON mard~matnr = db~kalite_matnr
*                               AND mard~lgort = db~hedef_depo
*        WHERE
*              mard~werks IN @so_werks
*        AND   mard~lfgja = @so_datum-low+0(4)
*        AND   mard~lfmon = @so_datum-low+4(2)
*        AND   mard~lvorm = @space
*        INTO TABLE @DATA(lt_hedef_inner).


        SELECT
        mchb~lgort,
        mchb~matnr,
        mchb~clabs AS labst,
        mchb~charg
        FROM mchb
        INNER JOIN t001l ON mchb~lgort = t001l~lgort
        INNER JOIN @lt_db AS db ON mchb~matnr = db~matnr
                               AND mchb~lgort = db~lgort
                               AND mchb~charg = db~charg
        WHERE
         mchb~werks IN @so_werks
        AND   mchb~lfgja = @so_datum-low+0(4)
        AND   mchb~lfmon = @so_datum-low+4(2)
        AND   mchb~lvorm = @space
        INTO TABLE @DATA(lt_labst_inner).


        SELECT
        mchb~lgort,
        mchb~matnr,
        mchb~clabs AS labst,
        mchb~charg
        FROM mchb
        INNER JOIN t001l ON mchb~lgort = t001l~lgort
        INNER JOIN @lt_db AS db ON mchb~matnr = db~kalite_matnr
                               AND mchb~lgort = db~hedef_depo
                               AND mchb~charg = db~hedef_parti
        WHERE
              mchb~werks IN @so_werks
        AND   mchb~lfgja = @so_datum-low+0(4)
        AND   mchb~lfmon = @so_datum-low+4(2)
        AND   mchb~lvorm = @space
        INTO TABLE @DATA(lt_hedef_inner).

      ENDIF.

      "kaynak stok için update
      LOOP AT lt_labst_inner INTO DATA(ls_labst_inner).
        LOOP AT gt_table ASSIGNING FIELD-SYMBOL(<lfs_tab_commit_update>) WHERE matnr = ls_labst_inner-matnr
                                                                   AND   lgort = ls_labst_inner-lgort
                                                                   AND   charg = ls_labst_inner-charg.
          <lfs_tab_commit_update> = VALUE #( BASE <lfs_tab_commit_update>
                                labst        = ls_labst_inner-labst
*                                  transfer_miktar = 0
                                ).
*            CLEAR:<lfs_tab_commit_update>-transfer_durumu.
          CALL METHOD go_alv_grid->refresh_table_display( ).

        ENDLOOP.
      ENDLOOP.

      "hedef stok için update
      LOOP AT lt_hedef_inner INTO DATA(ls_hedef_inner).
        LOOP AT gt_table ASSIGNING FIELD-SYMBOL(<lfs_tab_commit_updatey>) WHERE
                                                                         hedef_parti = ls_hedef_inner-charg
                                                                   AND   kalite_matnr = ls_hedef_inner-matnr
                                                                   AND   hedef_depo = ls_hedef_inner-lgort.
          <lfs_tab_commit_updatey> = VALUE #( BASE <lfs_tab_commit_updatey>
                                hedef_stok   = ls_hedef_inner-labst
*                                  transfer_miktar = 0
                                ).
*            CLEAR:<lfs_tab_commit_update>-transfer_durumu.
          CALL METHOD go_alv_grid->refresh_table_display( ).
        ENDLOOP.
      ENDLOOP.


      IF sy-subrc IS INITIAL.
        DELETE FROM zpp_013_t_trans.
      ENDIF.


    ENDIF.
  ENDMETHOD.

  METHOD make_non_editable.

    DATA:lt_style TYPE lvc_t_styl,
         ls_style TYPE lvc_s_styl.

    LOOP AT gt_table ASSIGNING FIELD-SYMBOL(<lfs>).

      CLEAR:ls_style.
      ls_style = VALUE #( BASE ls_style
       style     = cl_gui_alv_grid=>mc_style_disabled + cl_gui_alv_grid=>mc_style_f4_no
       fieldname = 'CHARG'
       ).
      INSERT ls_style INTO TABLE lt_style.

      CLEAR:ls_style.
      ls_style = VALUE #( BASE ls_style
       style     = cl_gui_alv_grid=>mc_style_disabled + cl_gui_alv_grid=>mc_style_f4_no
       fieldname = 'HEDEF_DEPO'
       ).
      INSERT ls_style INTO TABLE lt_style.

      CLEAR:ls_style.
      ls_style = VALUE #( BASE ls_style
       style     = cl_gui_alv_grid=>mc_style_disabled + cl_gui_alv_grid=>mc_style_f4_no
       fieldname = 'KALITE_MATNR'
       ).
      INSERT ls_style INTO TABLE lt_style.

      CLEAR:ls_style.
      ls_style = VALUE #( BASE ls_style
       style     = cl_gui_alv_grid=>mc_style_disabled + cl_gui_alv_grid=>mc_style_f4_no
       fieldname = 'LGORT'
       ).
      INSERT ls_style INTO TABLE lt_style.

      CLEAR:ls_style.
      ls_style = VALUE #( BASE ls_style
       style     = cl_gui_alv_grid=>mc_style_disabled + cl_gui_alv_grid=>mc_style_f4_no
       fieldname = 'MATNR'
       ).
      INSERT ls_style INTO TABLE lt_style.

      CLEAR:ls_style.
      ls_style = VALUE #( BASE ls_style
       style     = cl_gui_alv_grid=>mc_style_disabled
       fieldname = 'TRANSFER_MIKTAR'
       ).
      INSERT ls_style INTO TABLE lt_style.

      CLEAR:<lfs>-cellstyle.
      INSERT LINES OF lt_style INTO TABLE <lfs>-cellstyle.

    ENDLOOP.

  ENDMETHOD.

  METHOD add_row.

    DATA:ls_table TYPE gty_table.
    CLEAR:ls_table.
    APPEND ls_table TO gt_table.
    CALL METHOD go_alv_grid->refresh_table_display( ).

  ENDMETHOD.

  METHOD delete_row.

    DATA:lt_index_rows TYPE lvc_t_row.
    CALL METHOD go_alv_grid->get_selected_rows
      IMPORTING
        et_index_rows = lt_index_rows.

    IF lt_index_rows IS INITIAL.
      MESSAGE i000 DISPLAY LIKE 'E'.
    ELSE.
      LOOP AT lt_index_rows ASSIGNING FIELD-SYMBOL(<lfs_index_rows>).
        READ TABLE gt_table ASSIGNING FIELD-SYMBOL(<lfs_tab>) INDEX <lfs_index_rows>-index.
        IF sy-subrc IS INITIAL.
          <lfs_tab>-delete = 'X'.
        ENDIF.
      ENDLOOP.

      DELETE gt_table WHERE delete = 'X'.

      CALL METHOD go_alv_grid->refresh_table_display( ).
    ENDIF.

  ENDMETHOD.

  METHOD register_f4.

    DATA: lt_f4 TYPE lvc_t_f4,
          ls_f4 TYPE lvc_s_f4.

    CLEAR:ls_f4.
    ls_f4 = VALUE #( BASE ls_f4
            fieldname = 'CHARG'
            register  = abap_true
*            getbefore = abap_true
*            chngeafter = abap_true
  ).
    APPEND ls_f4 TO lt_f4.

    CLEAR:ls_f4.
    ls_f4 = VALUE #( BASE ls_f4
            fieldname = 'HEDEF_DEPO'
            register  = abap_true
*            getbefore = abap_true
*            chngeafter = abap_true
  ).
    APPEND ls_f4 TO lt_f4.

    CLEAR:ls_f4.
    ls_f4 = VALUE #( BASE ls_f4
            fieldname = 'KALITE_MATNR'
            register  = abap_true
*            getbefore = abap_true
*            chngeafter = abap_true
             ).
    APPEND ls_f4 TO lt_f4.

    CLEAR:ls_f4.
    ls_f4 = VALUE #( BASE ls_f4
            fieldname = 'LGORT'
            register  = abap_true
*            getbefore = abap_true
*            chngeafter = abap_true
             ).
    APPEND ls_f4 TO lt_f4.

    CALL METHOD go_alv_grid->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4.

  ENDMETHOD.

  METHOD handle_onf4_depo_yeri.

    DATA:lt_return_tab TYPE TABLE OF ddshretval,
         ls_return_tab TYPE ddshretval.

    TYPES:BEGIN OF lty_value_tab,
            matnr TYPE 	matnr,
            werks	TYPE werks_d,
            lgort	TYPE lgort_d,
            lgobe	TYPE lgobe,
            labst	TYPE labst,
            meins	TYPE meins,
          END OF lty_value_tab.

    DATA:lt_value_tab TYPE TABLE OF lty_value_tab.

*    gv_matnrxxx = gv_matnr.

    SELECT
    matnr,
    werks,
    lgort,
    lgobe,
    labst,
    meins
**    FROM zpp_013_ddl_depo_yeri
      FROM zpp_013_v_depo
    INTO CORRESPONDING FIELDS OF TABLE @lt_value_tab
    WHERE matnr = @gv_matnr
    AND werks IN @so_werks.

    SORT lt_value_tab BY matnr.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield     = 'LGORT'
        window_title = 'Depo Yeri Search Help'
        value_org    = 'S'
      TABLES
        value_tab    = lt_value_tab
        return_tab   = lt_return_tab.

    READ TABLE lt_return_tab INTO ls_return_tab WITH KEY fieldname = 'F0003' BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      READ TABLE gt_table ASSIGNING <gfs_table> INDEX es_row_no-row_id.
      IF sy-subrc IS INITIAL.
        <gfs_table>-lgort = ls_return_tab-fieldval.

        gv_lgort = ls_return_tab-fieldval.

        CALL METHOD go_alv_grid->refresh_table_display( ).
      ENDIF.
    ENDIF.

    er_event_data->m_event_handled = 'X'.

*    SELECT SINGLE
*    mard~lgort,
*    mard~matnr,
*    mard~labst
*    FROM mard
*    INNER JOIN t001l ON mard~lgort = t001l~lgort
**    INNER JOIN @lt_value_tab AS lt ON mard~matnr = lt~matnr
*    WHERE mard~matnr = @gv_matnr
*    AND   mard~lgort = @gv_lgort
*    AND   mard~werks IN @so_werks
*    AND   mard~lfgja = @so_datum-low+0(4)
*    AND   mard~lfmon = @so_datum-low+4(2)
*    AND   mard~lvorm = @space
*    INTO @DATA(ls_labst).
*
*    LOOP AT gt_table ASSIGNING FIELD-SYMBOL(<lfs_table_f4>) WHERE matnr = ls_labst-matnr.
*      <lfs_table_f4> = VALUE #( BASE <lfs_table_f4>
*                                         labst = ls_labst-labst ).
*    ENDLOOP.

    CALL METHOD go_alv_grid->refresh_table_display( ).

*    ENDIF.

  ENDMETHOD.

  METHOD handle_onf4_parti_no.

    DATA:lt_return_tab_p TYPE TABLE OF ddshretval,
         ls_return_tab_p TYPE ddshretval.

    TYPES:BEGIN OF lty_value_tab_p,
            matnr TYPE mchb-matnr,
            werks TYPE mchb-werks,
            lgort TYPE mchb-lgort,
            lgobe TYPE t001l-lgobe,
            charg TYPE mchb-charg,
            clabs TYPE mchb-clabs,
            meins TYPE mara-meins,
          END OF lty_value_tab_p.

    DATA:lt_value_tab_p TYPE TABLE OF lty_value_tab_p.

    SELECT
    matnr,
    werks,
    lgort,
    lgobe,
    charg,
    clabs,
    meins
**    FROM zpp_013_ddl_parti_no
      FROM zpp_013_v_parti
    INTO CORRESPONDING FIELDS OF TABLE @lt_value_tab_p
    WHERE matnr = @gv_matnr
    AND werks IN @so_werks.

    SORT lt_value_tab_p BY matnr charg.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield     = 'CHARG'
        window_title = 'Parti No Search Help'
        value_org    = 'S'
      TABLES
        value_tab    = lt_value_tab_p
        return_tab   = lt_return_tab_p.

    READ TABLE lt_return_tab_p INTO ls_return_tab_p WITH KEY fieldname = 'F0005' BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      READ TABLE gt_table ASSIGNING <gfs_table> INDEX es_row_no-row_id.
      IF sy-subrc IS INITIAL.

        <gfs_table>-charg = ls_return_tab_p-fieldval.
        <gfs_table>-hedef_parti = ls_return_tab_p-fieldval.

        gv_charg = ls_return_tab_p-fieldval.
        gv_hedef_parti = ls_return_tab_p-fieldval.

        CALL METHOD go_alv_grid->refresh_table_display( ).
      ENDIF.
    ENDIF.

    er_event_data->m_event_handled = 'X'.


    SELECT SINGLE
    mchb~lgort,
    mchb~matnr,
    mchb~clabs AS labst,
    mchb~charg
    FROM mchb
    INNER JOIN t001l ON mchb~lgort = t001l~lgort
    WHERE mchb~matnr = @gv_matnr
    AND   mchb~lgort = @gv_lgort
    AND   mchb~charg = @gv_charg
    AND   mchb~werks IN @so_werks
    AND   mchb~lfgja = @so_datum-low+0(4)
    AND   mchb~lfmon = @so_datum-low+4(2)
    AND   mchb~lvorm = @space
    INTO @DATA(ls_labst).

    LOOP AT gt_table ASSIGNING FIELD-SYMBOL(<lfs_table_f4>) WHERE matnr = ls_labst-matnr AND charg = ls_labst-charg.
      <lfs_table_f4> = VALUE #( BASE <lfs_table_f4>
                                         labst = ls_labst-labst ).
    ENDLOOP.

    CALL METHOD go_alv_grid->refresh_table_display( ).

  ENDMETHOD.

  METHOD handle_onf4_kalite_malzeme.

    DATA:lt_return_tab_ TYPE TABLE OF ddshretval,
         ls_return_tab_ TYPE ddshretval.

    TYPES:BEGIN OF lty_value_tab_,
            kalite_matnr TYPE   matnr,
            kalite_maktx TYPE  maktx,
          END OF lty_value_tab_.

    DATA:lt_value_tab_ TYPE TABLE OF lty_value_tab_.

    gr_matnr = VALUE #( sign = 'I'
                        option = 'CP'
                        low = |*{ gv_matnr }*| ).
    APPEND gr_matnr TO gr_matnr.

    SELECT
    matnr AS kalite_matnr,
    maktx AS kalite_maktx
**    FROM zpp_013_ddl_kalite_malzeme
      FROM zpp_013_v_kalite
    INTO CORRESPONDING FIELDS OF TABLE @lt_value_tab_
    WHERE matnr IN @gr_matnr[].

    SORT lt_value_tab_ BY kalite_matnr.

*--------------------------------------------------------------------* kalite matnr de 1.kalite malzeme olmasın
    DELETE lt_value_tab_ WHERE kalite_matnr = gv_matnr.
*--------------------------------------------------------------------*
    IF lt_value_tab_ IS INITIAL.   "02.08.2024 edit
      MESSAGE i004.
    ENDIF.
*--------------------------------------------------------------------*
    CLEAR:gr_matnr.
    REFRESH:gr_matnr[].

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield     = 'KALITE_MATNR'
        window_title = 'Hedef Malzeme Search Help'
        value_org    = 'S'
      TABLES
        value_tab    = lt_value_tab_
        return_tab   = lt_return_tab_.

    READ TABLE lt_return_tab_ INTO ls_return_tab_ WITH KEY fieldname = 'F0001' BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      READ TABLE gt_table ASSIGNING <gfs_table> INDEX es_row_no-row_id.
      IF sy-subrc IS INITIAL.
        <gfs_table>-kalite_matnr = ls_return_tab_-fieldval.

        gv_kalite_matnr = ls_return_tab_-fieldval.

        CALL METHOD go_alv_grid->refresh_table_display( ).
      ENDIF.
    ENDIF.

    er_event_data->m_event_handled = 'X'.

    SELECT SINGLE
    marc~matnr,
    makt~maktx
    FROM marc
    LEFT JOIN makt ON marc~matnr = makt~matnr
    WHERE marc~matnr = @gv_kalite_matnr
    AND   makt~spras = 'T'
    AND   marc~werks IN @so_werks
    INTO @DATA(ls_kalite_matnr).

    LOOP AT gt_table ASSIGNING FIELD-SYMBOL(<lfs_table_f4_>) WHERE kalite_matnr = ls_kalite_matnr-matnr.
      <lfs_table_f4_> = VALUE #( BASE <lfs_table_f4_>
                                         kalite_maktx = ls_kalite_matnr-maktx ).
    ENDLOOP.

    CALL METHOD go_alv_grid->refresh_table_display( ).
  ENDMETHOD.

  METHOD handle_onf4_hedef_depo.

    DATA:lt_return_tab_x TYPE TABLE OF ddshretval,
         ls_return_tab_x TYPE ddshretval.

    TYPES:BEGIN OF lty_value_tab_x,
            matnr      TYPE matnr,
            werks	     TYPE werks_d,
            hedef_depo TYPE lgort_d,
            lgobe	     TYPE lgobe,
            hedef_stok TYPE labst,
            meins	     TYPE meins,
          END OF lty_value_tab_x.

    DATA:lt_value_tab_x TYPE TABLE OF lty_value_tab_x.

    SELECT
    matnr,
    werks,
    lgort AS hedef_depo,
    lgobe,
    labst AS hedef_stok,
    meins
**    FROM zpp_013_ddl_hedef_depo
      FROM zpp_013_v_target
    INTO CORRESPONDING FIELDS OF TABLE @lt_value_tab_x
    WHERE matnr = @gv_kalite_matnr
    AND werks IN @so_werks.

    SORT lt_value_tab_x BY matnr.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield     = 'HEDEF_DEPO'
        window_title = 'Hedef Depo Yeri Search Help'
        value_org    = 'S'
      TABLES
        value_tab    = lt_value_tab_x
        return_tab   = lt_return_tab_x.

    READ TABLE lt_return_tab_x INTO ls_return_tab_x WITH KEY fieldname = 'F0003' BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      READ TABLE gt_table ASSIGNING <gfs_table> INDEX es_row_no-row_id.
      IF sy-subrc IS INITIAL.
        <gfs_table>-hedef_depo = ls_return_tab_x-fieldval.

        gv_hedef_depo = ls_return_tab_x-fieldval.

        CALL METHOD go_alv_grid->refresh_table_display( ).
      ENDIF.
    ENDIF.

    er_event_data->m_event_handled = 'X'.

*    SELECT SINGLE
*    mard~lgort,
*    mard~matnr,
*    mard~labst
*    FROM mard
*    INNER JOIN t001l ON mard~lgort = t001l~lgort
*    WHERE mard~matnr = @gv_kalite_matnr
*    AND   mard~lgort = @gv_hedef_depo
*    AND   mard~werks IN @so_werks
*    AND   mard~lfgja = @so_datum-low+0(4)
*    AND   mard~lfmon = @so_datum-low+4(2)
*    AND   mard~lvorm = @space
*    INTO @DATA(ls_hedef_depo).
*
*    LOOP AT gt_table ASSIGNING FIELD-SYMBOL(<lfs_table_f4_x>) WHERE kalite_matnr = ls_hedef_depo-matnr
*                                                              AND   hedef_depo   = ls_hedef_depo-lgort.
*      <lfs_table_f4_x> = VALUE #( BASE <lfs_table_f4_x>
*                                         hedef_stok   = ls_hedef_depo-labst ).
*    ENDLOOP.
*
*    CALL METHOD go_alv_grid->refresh_table_display( ).

    SELECT SINGLE
    mchb~lgort,
    mchb~matnr,
    mchb~clabs AS labst,
    mchb~charg
    FROM mchb
    INNER JOIN t001l ON mchb~lgort = t001l~lgort
    WHERE mchb~matnr = @gv_kalite_matnr
    AND   mchb~lgort = @gv_hedef_depo
    AND   mchb~charg = @gv_charg
    AND   mchb~werks IN @so_werks
    AND   mchb~lfgja = @so_datum-low+0(4)
    AND   mchb~lfmon = @so_datum-low+4(2)
    AND   mchb~lvorm = @space
    INTO @DATA(ls_labst).

    LOOP AT gt_table ASSIGNING FIELD-SYMBOL(<lfs_table_f4_x>) WHERE kalite_matnr = ls_labst-matnr AND hedef_parti = ls_labst-charg.
      <lfs_table_f4_x> = VALUE #( BASE <lfs_table_f4_x>
                                        hedef_stok = ls_labst-labst ).
    ENDLOOP.

    CALL METHOD go_alv_grid->refresh_table_display( ).

  ENDMETHOD.

  METHOD handle_button_click.
  ENDMETHOD.

  METHOD handle_user_command.
  ENDMETHOD.

  METHOD call_screen.
    CALL SCREEN 0100.
  ENDMETHOD.

  METHOD call_popup_screen.
    CALL SCREEN 0200 STARTING AT    80 80
                     ENDING AT     190 100.
  ENDMETHOD.

  METHOD pbo_0100.
    SET PF-STATUS '0100'.
    SET TITLEBAR '0100'.
  ENDMETHOD.

  METHOD pbo_0200.
    SET PF-STATUS '0200'.
    SET TITLEBAR '0200'.
  ENDMETHOD.

  METHOD pai_0100.
    CASE iv_ucomm.
      WHEN '&BACK'.
        SET SCREEN 0.
      WHEN '&ADD_ROW'.
        go_local->add_row( ).
      WHEN '&DELETE_R'.
        go_local->delete_row( ).
      WHEN '&CHECK_TRA'.
        go_local->call_func( ).
      WHEN '&SAVE'.
        IF gv_save = abap_true.
          go_local->bapi_commit( ).
          IF gv_commit = abap_true.
            go_local->after_commit( ).
          ELSE.
            EXIT.
          ENDIF.
        ELSE.
          EXIT.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD pai_0200.
    CASE iv_ucomm.
      WHEN '&OK'.
        SET SCREEN 0.
      WHEN '&CANCEL'.
        SET SCREEN 0.
    ENDCASE.
  ENDMETHOD.

  METHOD set_fcat.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZPP_013_S_TRANSFER'
      CHANGING
        ct_fieldcat      = gt_fcat.

    LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<lfs_fcat>).
      CASE <lfs_fcat>-fieldname.
        WHEN 'TRANSFER_DURUMU'.
          <lfs_fcat> = VALUE #( BASE <lfs_fcat>
                        icon      = abap_true
*                        edit      = abap_true
                         ).
        WHEN 'MATNR'.
          <lfs_fcat>-edit = abap_true .
        WHEN 'LGORT'.
          <lfs_fcat> = VALUE #( BASE <lfs_fcat>
                        edit  = abap_true
                        f4availabl = abap_true ).
*                        style = cl_gui_alv_grid=>mc_style_f4 ).
        WHEN 'TRANSFER_MIKTAR'.
          <lfs_fcat>-edit = abap_true .
        WHEN  'KALITE_MATNR'.
          <lfs_fcat> = VALUE #( BASE <lfs_fcat>
                        edit  = abap_true
                        f4availabl = abap_true ).
        WHEN 'HEDEF_DEPO'.
          <lfs_fcat> = VALUE #( BASE <lfs_fcat>
                         edit  = abap_true
                         f4availabl = abap_true ).
        WHEN 'CHARG'.
          <lfs_fcat> = VALUE #( BASE <lfs_fcat>
                         edit  = abap_true
                         f4availabl = abap_true ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD set_fcat2.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'BAPIRET2'
      CHANGING
        ct_fieldcat      = gt_fcat2.

  ENDMETHOD.

  METHOD set_layout.
    gs_layout = VALUE #( zebra      = abap_true
                         cwidth_opt = abap_true
                         col_opt    = abap_true
                         sel_mode   = 'A'
                         no_toolbar = abap_true
                         ctab_fname = 'CELL_COLOR'
                         stylefname = 'CELLSTYLE' ).
  ENDMETHOD.

  METHOD display_alv.
    IF go_alv_grid IS INITIAL.

      CREATE OBJECT go_container
        EXPORTING
          container_name = 'CC_ALV'.
      CREATE OBJECT go_alv_grid
        EXPORTING
*         i_parent = go_container.
          i_parent = cl_gui_container=>screen0.

      go_local->register_f4( ).

      SET HANDLER go_local->handle_data_changed        FOR go_alv_grid.
      SET HANDLER go_local->handle_onf4_depo_yeri      FOR go_alv_grid.
      SET HANDLER go_local->handle_onf4_parti_no       FOR go_alv_grid.
      SET HANDLER go_local->handle_onf4_kalite_malzeme FOR go_alv_grid.
      SET HANDLER go_local->handle_onf4_hedef_depo     FOR go_alv_grid.

      SET HANDLER go_local->handle_button_click        FOR go_alv_grid.
      SET HANDLER go_local->handle_user_command        FOR go_alv_grid.

      CALL METHOD go_alv_grid->register_edit_event
        EXPORTING
          i_event_id = go_alv_grid->mc_evt_enter. " Event ID

      CALL METHOD go_alv_grid->set_table_for_first_display
        EXPORTING
          is_layout       = gs_layout   " Layout
        CHANGING
          it_outtab       = gt_table   " Output Table
          it_fieldcatalog = gt_fcat.   " Field Catalog
    ELSE.
      CALL METHOD go_alv_grid->refresh_table_display.
    ENDIF.
  ENDMETHOD.

  METHOD display_alv2.
    IF go_alv_grid2 IS INITIAL.
      CREATE OBJECT go_container
        EXPORTING
          container_name = 'CC_ALV'.
      CREATE OBJECT go_alv_grid2
        EXPORTING
          i_parent = go_container.

      CALL METHOD go_alv_grid2->set_table_for_first_display
        EXPORTING
          is_layout       = gs_layout
        CHANGING
          it_outtab       = gt_return
          it_fieldcatalog = gt_fcat2.
    ELSE.
      CALL METHOD go_alv_grid2->refresh_table_display.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
