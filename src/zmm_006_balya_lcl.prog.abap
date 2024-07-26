*&---------------------------------------------------------------------*
*& Include          ZMM_006_BALYA_LCL
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include          ZMM_006_BALYA_LCL
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

    DATA:lv_concat   TYPE char28,
         lv_datatype TYPE dd01v-datatype,
         lv_matnr    TYPE char50.

    DATA:lv_objectkey       TYPE bapi1003_key-object,
         lt_allocvaluesnum  TYPE TABLE OF  bapi1003_alloc_values_num,
         lt_allocvalueschar TYPE TABLE OF  bapi1003_alloc_values_char,
         lt_allocvaluescurr TYPE TABLE OF bapi1003_alloc_values_curr,
         lt_return          TYPE TABLE OF bapiret2,
         ls_return          TYPE bapiret2.

    SELECT
    mseg~mblnr,
    mseg~zeile,
    lfa1~lifnr,
    lfa1~name1,
    makt~maktx,
    makt~spras,
    mkpf~xblnr,
    mkpf~cpudt,
    mseg~erfmg,
    mseg~erfme,
    mseg~charg,
    mara~ntgew,
    mara~brgew,
    mara~gewei,
    mseg~bwart,
    mseg~matnr
    FROM mseg
    INNER JOIN mkpf ON mseg~mblnr = mkpf~mblnr
    LEFT  JOIN lfa1 ON mseg~lifnr = lfa1~lifnr
    INNER JOIN mara ON mseg~matnr = mara~matnr
    LEFT  JOIN makt ON mara~matnr = makt~matnr
    WHERE makt~spras = 'T'
    AND  mseg~mblnr IN @gr_mblnr[]
*    AND  mseg~matnr IN @gr_matnr[]
*    AND  mseg~charg IN @gr_charg[]
    AND   mseg~bwart = '101'
    INTO TABLE @DATA(lt_data).

    DELETE ADJACENT DUPLICATES FROM lt_data COMPARING mblnr zeile.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lfs_x>).
      CLEAR:lv_concat,
            lv_matnr,
            lv_objectkey,
            lv_datatype.

      IF <lfs_x>-matnr IS NOT INITIAL AND <lfs_x>-charg IS NOT INITIAL.

        <lfs_x>-charg = |{ <lfs_x>-charg ALPHA = IN }|.
        DATA(lv_charg_len) = strlen( <lfs_x>-charg ).

        IF lv_charg_len NE 10.
          DATA(lv_lead) = 10 - lv_charg_len.
          WRITE <lfs_x>-charg TO <lfs_x>-charg RIGHT-JUSTIFIED.
          TRANSLATE <lfs_x>-charg USING ' 0'.
        ENDIF.

        lv_concat = <lfs_x>-charg.
        DATA(concat) = |{ <lfs_x>-matnr }{ lv_concat }|. "yani toplamda 28 karakter olacak
        DATA(lv_len) = strlen( concat ).

        CALL FUNCTION 'NUMERIC_CHECK'
          EXPORTING
            string_in = <lfs_x>-matnr
          IMPORTING
            htype     = lv_datatype.

        IF lv_datatype EQ 'NUMC'.

          lv_matnr = |{ <lfs_x>-matnr }{ lv_concat }|.

        ELSEIF <lfs_x>-matnr CA sy-abcde OR lv_datatype NE 'NUMC'.
          IF lv_len NE 28.
            DATA(lv_space) = 28 - lv_len.
            SHIFT lv_concat RIGHT BY lv_space PLACES.
          ENDIF.

          lv_matnr = |{ <lfs_x>-matnr }{ lv_concat }|.
        ENDIF.

        lv_objectkey = lv_matnr.

        CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
          EXPORTING
            objectkey       = lv_objectkey
            objecttable     = 'MCH1'
            classnum        = 'HAMIPLIK_PARTI'
            classtype       = '023'
            keydate         = sy-datum
            language        = sy-langu
          TABLES
            allocvaluesnum  = lt_allocvaluesnum
            allocvalueschar = lt_allocvalueschar
            allocvaluescurr = lt_allocvaluescurr
            return          = lt_return.

        LOOP AT lt_allocvaluesnum ASSIGNING FIELD-SYMBOL(<lfs_allocvaluesnum>).
          IF <lfs_allocvaluesnum>-charact = 'BRUT_KG'.
            DATA(lv_brgew) = <lfs_allocvaluesnum>-value_from.
          ELSEIF <lfs_allocvaluesnum>-charact = 'NET_KG'.
            DATA(lv_ntgew) = <lfs_allocvaluesnum>-value_from.
          ENDIF.
        ENDLOOP.

        LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lfs_data>) WHERE charg = <lfs_x>-charg.
          <lfs_data>-brgew = lv_brgew.
*      <lfs_data>-ntgew = lv_ntgew.
          <lfs_data>-ntgew = <lfs_data>-erfmg.
        ENDLOOP.

      ENDIF.

    ENDLOOP.

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
          i_name     = 'ZMM_006_AF_BALYA'                                 "
        IMPORTING
          e_funcname = fm_name.

*      CALL FUNCTION '/1BCDWB/SM00000058'
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
