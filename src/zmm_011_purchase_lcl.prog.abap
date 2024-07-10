*&---------------------------------------------------------------------*
*& Include          ZMM_011_PURCHASE_LCL
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor,
      db_list,
      get_data,
      update_req,
      send_mail,
      call_screen,
      call_screen_list,
      pbo_0100,
      pai_0100 IMPORTING iv_ucomm TYPE sy-ucomm,
      pbo_0200,
      pai_0200 IMPORTING iv_ucomm TYPE sy-ucomm,
      set_fcat,
      set_layout,
      display_alv,
      display_list.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.

  METHOD db_list.

    gv_date = sy-datum - 42.

    SELECT
    pr~purchaserequisition,
    pr~purchaserequisitionitem,
    pr~requestedquantity,
    pr~creationdate,
    pr~createdbyuser,
    pr~purchasingdocument,
    pr~orderedquantity
    FROM i_purchaserequisition_api01 AS pr
    WHERE pr~purchaserequisition     IN @so_pr
    AND   pr~purchaserequisitionitem IN @so_pri
    AND   pr~creationdate            IN @so_cd
    INTO TABLE @DATA(lt_list).

    SORT lt_list BY purchaserequisition.


    LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<lfs_list>) GROUP BY ( purchaserequisition = <lfs_list>-purchaserequisition
                                                                creationdate        = <lfs_list>-creationdate
                                                                requestedquantity   = <lfs_list>-requestedquantity
                                                                orderedquantity     = <lfs_list>-orderedquantity
                                                                purchasingdocument  = <lfs_list>-purchasingdocument ) ASCENDING ASSIGNING FIELD-SYMBOL(<lfs_tablist>).
      IF <lfs_tablist>-purchasingdocument IS INITIAL.

        IF <lfs_tablist>-creationdate <= gv_date.                  "6 ve 6 haftadan evveli

          LOOP AT GROUP <lfs_tablist> ASSIGNING FIELD-SYMBOL(<lfs_s_tabxx>) WHERE purchaserequisition = <lfs_tablist>-purchaserequisition.

            gs_list = CORRESPONDING #( <lfs_s_tabxx> ).
            gs_list-convertion_status = '@11@'.
            APPEND gs_list TO gt_list.
          ENDLOOP.
        ENDIF.


      ELSEIF <lfs_tablist>-purchasingdocument IS NOT INITIAL.


        IF <lfs_tablist>-requestedquantity EQ <lfs_tablist>-orderedquantity.

          CONTINUE.

        ELSEIF <lfs_tablist>-requestedquantity NE <lfs_tablist>-orderedquantity.

          IF <lfs_tablist>-creationdate <= gv_date.                  "6 ve 6 haftadan evveli

            LOOP AT GROUP <lfs_tablist> ASSIGNING FIELD-SYMBOL(<lfs_s_tabyy>) WHERE purchaserequisition = <lfs_tablist>-purchaserequisition.
              gs_list = CORRESPONDING #( <lfs_s_tabyy> ).
              gs_list-convertion_status = '@11@'.
              APPEND gs_list TO gt_list.
            ENDLOOP.
          ENDIF.

        ENDIF.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_data.

    gv_date = sy-datum - 42.

    SELECT
    pr~purchaserequisition,
    pr~purchaserequisitionitem,
    pr~requestedquantity,
    pr~creationdate,
    pr~createdbyuser,
    pr~purchasingdocument,
    pr~orderedquantity
    FROM i_purchaserequisition_api01 AS pr
    WHERE pr~purchaserequisition     IN @so_pr
    AND   pr~purchaserequisitionitem IN @so_pri
    AND   pr~creationdate            IN @so_cd
    AND   pr~isdeleted                = @space
    INTO TABLE @DATA(lt_alv).

    SORT lt_alv BY purchaserequisition.


    LOOP AT lt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv>) GROUP BY ( purchaserequisition = <lfs_alv>-purchaserequisition
                                                                creationdate        = <lfs_alv>-creationdate
                                                                requestedquantity   = <lfs_alv>-requestedquantity
                                                                orderedquantity     = <lfs_alv>-orderedquantity
                                                                purchasingdocument  = <lfs_alv>-purchasingdocument ) ASCENDING ASSIGNING FIELD-SYMBOL(<lfs_tab>).
      IF <lfs_tab>-purchasingdocument IS INITIAL.

        IF <lfs_tab>-creationdate <= gv_date.                  "6 ve 6 haftadan evveli

          LOOP AT GROUP <lfs_tab> ASSIGNING FIELD-SYMBOL(<lfs_s_tabxx>) WHERE purchaserequisition = <lfs_tab>-purchaserequisition.

            gs_alv = CORRESPONDING #( <lfs_s_tabxx> ).
            gs_alv-convertion_status = '@11@'.
            APPEND gs_alv TO gt_alv.
          ENDLOOP.
        ENDIF.


      ELSEIF <lfs_tab>-purchasingdocument IS NOT INITIAL.


        IF <lfs_tab>-requestedquantity EQ <lfs_tab>-orderedquantity.

          CONTINUE.

        ELSEIF <lfs_tab>-requestedquantity NE <lfs_tab>-orderedquantity.

          IF <lfs_tab>-creationdate <= gv_date.                  "6 ve 6 haftadan evveli

            LOOP AT GROUP <lfs_tab> ASSIGNING FIELD-SYMBOL(<lfs_s_tabyy>) WHERE purchaserequisition = <lfs_tab>-purchaserequisition.
              gs_alv = CORRESPONDING #( <lfs_s_tabyy> ).
              gs_alv-convertion_status = '@11@'.
              APPEND gs_alv TO gt_alv.
            ENDLOOP.
          ENDIF.

        ENDIF.

      ENDIF.
    ENDLOOP.

*    IF gt_alv IS NOT INITIAL.
*      me->update_req( ).
*    ENDIF.

    DATA:lt_log TYPE TABLE OF zmm_011_t_alv.
    lt_log = CORRESPONDING #( gt_alv ).
    MODIFY zmm_011_t_alv FROM TABLE lt_log.

  ENDMETHOD.

  METHOD update_req.
    DATA:
      lv_number                      TYPE  bapieban-preq_no,
      lt_requisition_items_to_delete TYPE TABLE OF  bapieband,
      ls_requisition_items_to_delete TYPE bapieband,
      lt_return                      TYPE TABLE OF  bapireturn.

    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_del>) GROUP BY ( purchaserequisition = <lfs_del>-purchaserequisition )
                                                     ASCENDING ASSIGNING FIELD-SYMBOL(<lfs_delx>).

      lv_number = <lfs_delx>-purchaserequisition.

      LOOP AT GROUP <lfs_delx> ASSIGNING FIELD-SYMBOL(<lfs_delxx>).


        ls_requisition_items_to_delete = VALUE #( BASE ls_requisition_items_to_delete
                                      preq_item = <lfs_delxx>-purchaserequisitionitem
                                      delete_ind = 'X' ).
        APPEND ls_requisition_items_to_delete TO lt_requisition_items_to_delete.
      ENDLOOP.

      CALL FUNCTION 'BAPI_REQUISITION_DELETE'
        EXPORTING
          number                      = lv_number
        TABLES
          requisition_items_to_delete = lt_requisition_items_to_delete
          return                      = lt_return.

      LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'AEX'.
        EXIT.
      ENDLOOP.

      IF sy-subrc NE 0.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD send_mail.

    DATA:lv_content     TYPE string,
         lv_subject     TYPE so_obj_des,
         lt_recipients  TYPE bcsy_smtpa,
         lt_pdf_name    TYPE zabap_001_tt_fpname,
         lv_tabname     TYPE tabname,
         lv_return      TYPE string,
         lv_return_type TYPE char1.


    lv_subject = |SAT-SAS Geçişi Hk.|.

    lv_content =   '<!DOCTYPE html>  '
            && '<html>     '
            && '  <head>       '
            && '     <meta charset="utf-8" '
            && '  </head>'
            && '  <body> '
            && '    <p><b>Sayın Kullanıcı Merhaba,</b></p><br>'
            && '    <p><b> '
            && '  Ekteki exceldeki SAT verileri 6 haftayı geçmiş ve SAS a dönmemiştir '
            && '    </b></p><br>     '
            && '    <p><b>Kontrol ediniz.</b></p> '
            && '  </body>   '
            && '</html>   '.


    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<lfs_user>).
      gr_createdbyuser = VALUE #( sign   = 'I'
                                  option = 'CP'
                                  low    = |{ <lfs_user>-createdbyuser }| ).
      APPEND gr_createdbyuser TO gr_createdbyuser.
    ENDLOOP.

    SELECT
    usrid,
    pernr
    FROM pa0105
    WHERE subty = '0001'
    AND   usrid IN @gr_createdbyuser[]     "SAT ları oluşturanlar
    INTO TABLE @DATA(lt_recid).

    IF lt_recid IS NOT INITIAL.
      SELECT
      usrid_long,
      pernr
      FROM pa0105
      FOR ALL ENTRIES IN @lt_recid
      WHERE subty = '0010'
      AND   pernr = @lt_recid-pernr
      INTO TABLE @DATA(lt_rec).
    ENDIF.

    LOOP AT lt_rec ASSIGNING FIELD-SYMBOL(<lfs_rec>).
      lt_recipients = VALUE #( BASE lt_recipients ( <lfs_rec>-usrid_long ) ).
    ENDLOOP.

*    lt_recipients = VALUE #( ( |test@test.com| ) ).

    lv_tabname = |ZMM_011_T_ALV|.

    CALL FUNCTION 'ZABAP_001_FM_SENDMAIL'
      EXPORTING
        iv_content       = lv_content
        iv_subject       = lv_subject
        it_recipients    = lt_recipients
        it_pdf_name      = lt_pdf_name
        iv_tab_for_excel = lv_tabname
      IMPORTING
        ev_return        = lv_return
        ev_return_type   = lv_return_type.

    DATA(lv_msg) = | { lv_return_type } -> { lv_return } |.
    MESSAGE lv_msg TYPE 'I' DISPLAY LIKE lv_return_type.
    IF lv_return_type EQ 'S'.
      DELETE FROM zmm_011_t_alv .
    ENDIF.

  ENDMETHOD.

  METHOD call_screen.
    CALL SCREEN 0100.
  ENDMETHOD.

  METHOD call_screen_list.
    CALL SCREEN 0200.
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

  METHOD pbo_0200.
    SET PF-STATUS '0100'.
    SET TITLEBAR '0200'.
  ENDMETHOD.

  METHOD pai_0200.
    CASE iv_ucomm.
      WHEN '&BACK'.
        SET SCREEN 0.
    ENDCASE.
  ENDMETHOD.

  METHOD set_fcat.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZMM_011_S_ALV'
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

  METHOD display_list.
    IF  go_alv_grid_list IS INITIAL.
      CREATE OBJECT go_container
        EXPORTING
          container_name = 'CC_ALV'.

      CREATE OBJECT go_alv_grid_list
        EXPORTING
*         i_parent = go_container.
          i_parent = cl_gui_container=>screen0.

      CALL METHOD go_alv_grid_list->set_table_for_first_display
        EXPORTING
          is_layout       = gs_layout                 " Layout
        CHANGING
          it_outtab       = gt_list                 " Output Table
          it_fieldcatalog = gt_fcat.                " Field Catalog

    ELSE.
      CALL METHOD go_alv_grid_list->refresh_table_display( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
