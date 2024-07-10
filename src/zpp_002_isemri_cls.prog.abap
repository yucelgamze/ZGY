*&---------------------------------------------------------------------*
*& Include          ZPP_002_ISEMRI_CLS
*&---------------------------------------------------------------------*
CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor,
      get_archive_attachment
        IMPORTING
          VALUE(im_object_id)     TYPE saeobjid
          VALUE(im_ar_object)     TYPE saeobjart
          VALUE(im_reserve)       TYPE saereserve
        RETURNING
          VALUE(re_xstring_table) TYPE xstring_table,
      get_gos_attachment
        IMPORTING
          VALUE(im_object_instid) TYPE sibfboriid
          VALUE(im_object_typeid) TYPE saeanwdid
          VALUE(im_object_catid)  TYPE sibfcatid
          VALUE(im_role)          TYPE oblroltype
        RETURNING
          VALUE(re_xstring_table) TYPE xstring_table,
      get_data,
      image_upload,
      get_brand_name,
      print_adobe,
      call_screen,
      pbo_0100,
      pai_0100 IMPORTING iv_ucomm TYPE sy-ucomm,
      set_fcat,
      set_layout,
      display_alv.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ATTACHMENTS->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ATTACHMENTS->GET_ARCHIVE_ATTACHMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_OBJECT_ID                   TYPE        SAEOBJID
* | [--->] IM_AR_OBJECT                   TYPE        SAEOBJART
* | [--->] IM_RESERVE                     TYPE        SAERESERVE
* | [<-()] RE_XSTRING_TABLE               TYPE        XSTRING_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_archive_attachment.
    DATA: lt_connect_info    TYPE STANDARD TABLE OF toav0,
          lt_archivobject    TYPE STANDARD TABLE OF docs,
          lt_binarchivobject TYPE STANDARD TABLE OF tbl1024,
          lv_length          TYPE num12,
          lv_xstring         TYPE xstring.

    " Checking wheter the material image is archived on the link tables (TOA01, TOA02, TOA03)
    " Get material object id
    SELECT SINGLE *
      INTO @DATA(ls_arcdocid)
      FROM toa01
     WHERE object_id = @im_object_id
       AND ar_object = @im_ar_object
       AND reserve   = @im_reserve.

    IF sy-subrc NE 0.
      SELECT SINGLE *
        INTO ls_arcdocid
        FROM toa02
       WHERE object_id = im_object_id
         AND ar_object = im_ar_object
         AND reserve   = im_reserve.
    ENDIF.

    IF sy-subrc NE 0.
      SELECT SINGLE *
        INTO ls_arcdocid
        FROM toa03
       WHERE object_id = im_object_id
         AND ar_object = im_ar_object
         AND reserve   = im_reserve.
    ENDIF.

    IF sy-subrc = 0.
      " Recover image from archiveLink
      CALL FUNCTION 'ARCHIV_CONNECTINFO_GET_META'
        EXPORTING
          ar_object             = ls_arcdocid-ar_object
          object_id             = ls_arcdocid-object_id
          sap_object            = ls_arcdocid-sap_object
        TABLES
          connect_info          = lt_connect_info
        EXCEPTIONS
          error_connectiontable = 1
          OTHERS                = 2.

      CHECK sy-subrc = 0.

      " Link table
      LOOP AT lt_connect_info INTO DATA(ls_connect_info).
        " Bynary format Object
        CALL FUNCTION 'ARCHIVOBJECT_GET_BYTES'
          EXPORTING
            archiv_id                = ls_connect_info-archiv_id
            archiv_doc_id            = ls_connect_info-arc_doc_id
            document_type            = CONV saedoktyp( im_reserve )
            length                   = 0
            offset                   = 0
          IMPORTING
            length                   = lv_length
          TABLES
            archivobject             = lt_archivobject
            binarchivobject          = lt_binarchivobject
          EXCEPTIONS
            error_archiv             = 1
            error_communicationtable = 2
            error_kernel             = 3
            OTHERS                   = 4.


        IF sy-subrc = 0.
          " Convert Binary table to Xstring
          CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
            EXPORTING
              input_length = CONV i( lv_length )
            IMPORTING
              buffer       = lv_xstring
            TABLES
              binary_tab   = lt_binarchivobject
            EXCEPTIONS
              failed       = 1
              OTHERS       = 2.

          IF sy-subrc = 0.
            APPEND lv_xstring TO re_xstring_table.
          ENDIF.
        ENDIF.

      ENDLOOP.
    ENDIF.
  ENDMETHOD.
* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ATTACHMENTS->GET_GOS_ATTACHMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_OBJECT_INSTID               TYPE        SIBFBORIID
* | [--->] IM_OBJECT_TYPEID               TYPE        SAEANWDID
* | [--->] IM_OBJECT_CATID                TYPE        SIBFCATID
* | [--->] IM_ROLE                        TYPE        OBLROLTYPE
* | [<-()] RE_XSTRING_TABLE               TYPE        XSTRING_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_gos_attachment.
    DATA: ls_object        TYPE sibflporb,
          lt_links         TYPE obl_t_link,
          lt_contents_hex  TYPE TABLE OF solix,
          lv_document_id   TYPE so_entryid,
          ls_document_data TYPE sofolenti1,
          lv_xstring       TYPE xstring.

    " get the attachment through the business object using the Generic object services
    ls_object-typeid = im_object_typeid.
    ls_object-catid  = im_object_catid.
    ls_object-instid = im_object_instid.

    CALL METHOD cl_binary_relation=>read_links_of_binrel
      EXPORTING
        is_object   = ls_object
        ip_relation = cl_gos_api=>c_atta
        ip_role     = im_role
      IMPORTING
        et_links    = lt_links.

    LOOP AT lt_links INTO DATA(ls_links).
      lv_document_id = ls_links-instid_b.

      CALL FUNCTION 'SO_DOCUMENT_READ_API1'
        EXPORTING
          document_id                = CONV so_entryid( ls_links-instid_b )
        IMPORTING
          document_data              = ls_document_data
        TABLES
          contents_hex               = lt_contents_hex
        EXCEPTIONS
          document_id_not_exist      = 1
          operation_no_authorization = 2
          x_error                    = 3
          OTHERS                     = 4.

      IF sy-subrc = 0.
        " Convert Binary table to Xstring
        CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
          EXPORTING
            input_length = CONV i( ls_document_data-doc_size )
          IMPORTING
            buffer       = lv_xstring
          TABLES
            binary_tab   = lt_contents_hex
          EXCEPTIONS
            failed       = 1
            OTHERS       = 2.
        IF sy-subrc = 0.
          APPEND lv_xstring TO re_xstring_table.
        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD get_data.
  ENDMETHOD.

  METHOD image_upload.

    DATA:lt_tab    TYPE TABLE OF x255,
         lv_buffer TYPE  xstring.

    cl_gui_frontend_services=>gui_upload(
  EXPORTING
    filename                = CONV #( p_file )   " Name of file
    filetype                = 'BIN'    " File Type (ASCII, Binary)
  IMPORTING
    filelength              = gv_length   " File Length
  CHANGING
    data_tab                = lt_tab    " Transfer table for file contents
  EXCEPTIONS
    file_open_error         = 1
    file_read_error         = 2
    no_batch                = 3
    gui_refuse_filetransfer = 4
    invalid_type            = 5
    no_authority            = 6
    unknown_error           = 7
    bad_data_format         = 8
    header_not_allowed      = 9
    separator_not_allowed   = 10
    header_too_long         = 11
    unknown_dp_error        = 12
    access_denied           = 13
    dp_out_of_memory        = 14
    disk_full               = 15
    dp_timeout              = 16
    not_supported_by_gui    = 17
    error_no_gui            = 18
    OTHERS                  = 19
    ).

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = gv_length
      IMPORTING
        buffer       = lv_buffer
      TABLES
        binary_tab   = lt_tab
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

    gs_brand = VALUE #( BASE gs_brand
                                      brand_id    = p_brand
                                      brand_image = lv_buffer ).

    MODIFY zpp_002_t_isemri FROM gs_brand.
    IF sy-subrc IS INITIAL.
      MESSAGE |Amblem database'e yüklendi!| TYPE 'I' DISPLAY LIKE 'S'.
    ENDIF.

  ENDMETHOD.
*--------------------------------------------------------------------*

  METHOD get_brand_name.

    SELECT
    brand_id,
    brand_image
    FROM zpp_002_t_isemri
    INTO TABLE @DATA(lt_brand).

    DATA(lt_domain) = VALUE dd07v_tab( ).

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'ZPP_002_DO_ISEMRI'
        text           = 'X'
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_domain
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    LOOP AT lt_brand ASSIGNING FIELD-SYMBOL(<lfs_brand>) WHERE brand_id = p_brand.

      READ TABLE lt_domain INTO DATA(ls_domain) WITH KEY domvalue_l = <lfs_brand>-brand_id BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        DATA(brand_name) = CONV string( ls_domain-ddtext ).
        MESSAGE | { brand_name } | TYPE 'I'.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD print_adobe.

    DATA:lv_brand_image TYPE xstring.

    CLEAR:gs_data,fm_name,lv_brand_image.

    SELECT SINGLE
    brand_image
    FROM zpp_002_t_isemri
    INTO @lv_brand_image
    WHERE brand_id = @p_brand.

    SELECT
    caufv~aufnr,
    caufv~plnbez,
    mara~satnr,
    makt~maktx,
    caufv~erdat,
    afpo~lgort,
    t001l~lgobe,
    mara~zz1_model_kod_prd,
    mara~zz1_sub_brand_name_prd,
    afko~gltrp,
    mara~zz1_renk_kod_prd,
    wrf_charvalt~atwtb,      "40 yazan yer
    caufv~gamng              "67 Yazan yer
    FROM caufv
    LEFT JOIN mara ON caufv~plnbez EQ mara~matnr
    LEFT JOIN wrf_charvalt ON mara~size1 EQ wrf_charvalt~atwrt AND mara~size1_atinn EQ wrf_charvalt~atinn
    LEFT JOIN makt ON mara~matnr   EQ makt~matnr
    LEFT JOIN afko ON caufv~aufnr EQ afko~aufnr
    LEFT JOIN afpo ON afko~aufnr EQ afpo~aufnr
    LEFT JOIN t001l ON afpo~lgort EQ t001l~lgort AND t001l~werks = caufv~werks
    WHERE caufv~aufnr = @p_aufnr
    AND   makt~spras  = 'T'
    AND   wrf_charvalt~spras = 'T'
    INTO TABLE @DATA(lt_data).


    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lfs_data>).

      <lfs_data>-satnr = | { <lfs_data>-satnr ALPHA = OUT } |.

      gs_data = CORRESPONDING #( <lfs_data> ).
      gs_data-maktx  = | { <lfs_data>-satnr }-{ <lfs_data>-maktx } |.
      gs_data-lgort  = | { <lfs_data>-lgort }-{ <lfs_data>-lgobe } |.
      gs_data-model  = <lfs_data>-zz1_model_kod_prd.
      gs_data-brand  = <lfs_data>-zz1_sub_brand_name_prd.
      gs_data-termin = <lfs_data>-gltrp.
      gs_data-renk   = <lfs_data>-zz1_renk_kod_prd.
      APPEND gs_data TO gt_data.
    ENDLOOP.

*    SELECT SINGLE
*    brand_image
*    FROM zpp_002_t_isemri
*    INTO @lv_brand_image
*    WHERE brand_id = @gs_data-brand.

    IF sy-subrc IS INITIAL AND lt_data IS NOT INITIAL.

      SELECT
      caufv~aufnr,
      afvc~ltxa1,
      afvc~vornr,
      afvc~aufpl,
      caufv~plnbez
*      makt~maktx,
*      mara~zz1_renk_kod_prd
      FROM caufv
      INNER JOIN afvc ON caufv~werks  EQ afvc~werks  AND caufv~aufpl EQ afvc~aufpl
*      INNER JOIN mara ON caufv~plnbez EQ mara~matnr
*      LEFT JOIN  makt ON mara~matnr   EQ makt~matnr
      INTO TABLE @DATA(lt_afvc)
      FOR ALL ENTRIES IN @lt_data
      WHERE caufv~aufnr = @lt_data-aufnr
*      AND   makt~spras  = 'T'
        .
      SORT lt_afvc BY vornr.

      IF lt_afvc IS NOT INITIAL.

        SELECT
        caufv~aufnr,
        afvc~ltxa1,
        resb~vornr,
        resb~matnr,
        makt~maktx,
        resb~bdmng,
        resb~meins,
        mara~zz1_renk_kod_prd,
        resb~sortf
        FROM caufv
        INNER JOIN resb ON caufv~werks EQ resb~werks AND resb~xloek = @space
        INNER JOIN mara ON resb~matnr  EQ mara~matnr
        LEFT JOIN  makt ON mara~matnr  EQ makt~matnr
        INNER JOIN afvc ON caufv~werks EQ afvc~werks  AND caufv~aufpl EQ afvc~aufpl
        INTO TABLE @DATA(lt_itab)
        FOR ALL ENTRIES IN @lt_afvc
        WHERE caufv~aufnr = @p_aufnr
        AND   makt~spras  = 'T'
        AND   resb~aufpl EQ @lt_afvc-aufpl
        AND   resb~vornr = @lt_afvc-vornr
        AND   afvc~ltxa1 = @lt_afvc-ltxa1.

        SORT lt_itab BY vornr.
*--------------------------------------------------------------------*
        TYPES:BEGIN OF lty_itab,
                aufnr            TYPE caufv-aufnr,
                ltxa1            TYPE afvc-ltxa1,
                vornr            TYPE resb-vornr,
                matnr            TYPE resb-matnr,
                maktx            TYPE makt-maktx,
                bdmng            TYPE resb-bdmng,
                meins            TYPE resb-meins,
                zz1_renk_kod_prd TYPE mara-zz1_renk_kod_prd,
                sortf            TYPE resb-sortf,
              END OF lty_itab.

        DATA:ls_blank TYPE lty_itab.
*--------------------------------------------------------------------*

*        LOOP AT lt_itab ASSIGNING FIELD-SYMBOL(<lfs_f>) GROUP BY ( aufnr = <lfs_f>-aufnr
*                                                                   ltxa1 = <lfs_f>-ltxa1 ) ASSIGNING FIELD-SYMBOL(<lfs_itabcpy>) .
*
*          LOOP AT lt_afvc ASSIGNING FIELD-SYMBOL(<lfs_afvc>) WHERE ltxa1 <> <lfs_itabcpy>-ltxa1
*                                                             AND   aufnr = <lfs_itabcpy>-aufnr.
*            ls_blank = VALUE #( aufnr = p_aufnr
*                                ltxa1 = <lfs_afvc>-ltxa1
*                                vornr = <lfs_afvc>-vornr
*                                bdmng = CONV #( ' ' ) ).
*
*          ENDLOOP.
*
*        ENDLOOP.
*        APPEND ls_blank TO lt_itab.

        LOOP AT lt_afvc ASSIGNING FIELD-SYMBOL(<lfs_afvc>).
          LOOP AT lt_itab ASSIGNING FIELD-SYMBOL(<lfs_f>) WHERE vornr EQ <lfs_afvc>-vornr.
            EXIT.
          ENDLOOP.
          IF sy-subrc <> 0.
            ls_blank = VALUE #( aufnr = p_aufnr
                                ltxa1 = <lfs_afvc>-ltxa1
                                vornr = <lfs_afvc>-vornr
                                bdmng = CONV #( ' ' ) ).
            APPEND ls_blank TO lt_itab.
          ENDIF.
        ENDLOOP.

        SORT lt_itab BY vornr.
*--------------------------------------------------------------------*
*        DATA(lt_itabcpy) = lt_itab.
*        DELETE ADJACENT DUPLICATES FROM lt_itabcpy COMPARING ltxa1.
*
*        LOOP AT lt_itabcpy ASSIGNING FIELD-SYMBOL(<lfs_itabcpyx>).
*          READ TABLE lt_afvc ASSIGNING FIELD-SYMBOL(<lfs_afvcx>) WITH KEY ltxa1 = <lfs_itabcpyx>-ltxa1 BINARY SEARCH.
*          IF sy-subrc <> 0  AND <lfs_afvcx> IS ASSIGNED.
*            ls_blank = VALUE #( aufnr = p_aufnr
*                                ltxa1 = <lfs_afvcx>-ltxa1
*                                vornr = <lfs_afvcx>-vornr
*                                bdmng = CONV #( ' ' ) ).
*
*            APPEND ls_blank TO lt_itab.
*          ENDIF.
*        ENDLOOP.




*        SELECT
*        caufv~aufnr,
*        afvc~ltxa1,
*        resb~vornr,
*        resb~matnr,
*        makt~maktx,
*        resb~bdmng,
*        resb~meins,
*        mara~zz1_renk_kod_prd,
*        resb~sortf
*        FROM caufv
*        INNER JOIN afvc ON caufv~werks  EQ afvc~werks  AND caufv~aufpl EQ afvc~aufpl
*        INNER JOIN resb ON afvc~werks   EQ resb~werks  AND resb~aufpl EQ afvc~aufpl AND resb~xloek = @space AND resb~vornr = afvc~vornr
*        INNER JOIN mara ON resb~matnr   EQ mara~matnr
*        LEFT JOIN  makt ON mara~matnr   EQ makt~matnr
*        WHERE caufv~aufnr = @p_aufnr
*        AND   makt~spras  = 'T'
*        GROUP BY resb~vornr,caufv~aufnr,afvc~ltxa1,resb~matnr,makt~maktx,resb~bdmng,resb~meins, mara~zz1_renk_kod_prd,resb~sortf
*        INTO TABLE @DATA(lt_table).

*        SORT lt_table BY vornr.

        SORT lt_itab BY vornr.

        LOOP AT lt_itab ASSIGNING FIELD-SYMBOL(<lfs_table>) GROUP BY ( aufnr = <lfs_table>-aufnr
                                                                       ltxa1 = <lfs_table>-ltxa1 ) "ASCENDING
          ASSIGNING FIELD-SYMBOL(<lfs_head>).

          gs_table = CORRESPONDING #( <lfs_head> ).
          LOOP AT GROUP <lfs_head> ASSIGNING FIELD-SYMBOL(<lfs_body>) WHERE ltxa1 = <lfs_head>-ltxa1.

            gs_table-table = VALUE #( BASE gs_table-table
                                                          ( maktx      = <lfs_body>-maktx
                                                           description = <lfs_body>-sortf
                                                           color       = <lfs_body>-zz1_renk_kod_prd
                                                           bdmng       = <lfs_body>-bdmng
                                                           meins       = <lfs_body>-meins ) ).
          ENDLOOP.
          APPEND gs_table TO gt_table.
        ENDLOOP.

      ENDIF.
    ENDIF.

*    " Search for the image on the ArchiveLink
*  DATA(lt_xstring_table) = go_local->get_archive_attachment(
*                                                              im_object_id = GOSKEY_PLNBEZ "CONV #( lv_matnr ) " Material Number
*                                                              im_ar_object = <AR_OBJECT>        " Document type. Maintained on view TOAVE using SM30
*                                                              im_reserve   = 'JPG'              " Image extension (BMP,JPG)
*                                                            ).
*


**-----------------GOS İPTAL YERİNE DMS DEN VERİ OKUYACAĞIZ----------------------------*
**  IF lt_xstring_table IS INITIAL.
*    " OtherWise serach for the attachments linked to the Business Object
*    DATA(lt_xstring_table) = go_local->get_gos_attachment(
**                                                      im_object_instid = |CONVERSE0001| "CONV #( lv_plnbez ) " Material Number
*                                                      im_object_instid = CONV #( p_plnbez )         "CONV #( lv_plnbez ) " Material Number
*                                                      im_object_typeid = 'ZPP_002_IS'       " Business object - Standard Material
*                                                      im_object_catid  = 'BO'               " Business Object
*                                                      im_role          = 'GOSAPPLOBJ'       " Generic object services application object
*                                                    ).
**  ENDIF.
*
**-----------------GOS İPTAL YERİNE DMS DEN VERİ OKUYACAĞIZ----------------------------*

    "DMS de veri okunurken her görsel bi malzeme ile linkli durumda  ( cv01n ) dms doknr sini malzeme ( plnbez ) ile çekmeyi dene.
    DATA:lv_xstring_dms TYPE xstring,
         lv_plnbez      TYPE char90.

    IF lt_data IS NOT INITIAL.

      LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lfs_data_plnbez>).
        lv_plnbez = <lfs_data_plnbez>-plnbez.
      ENDLOOP.

      SELECT
      doknr
      FROM drad
      WHERE drad~objky = @lv_plnbez
*      INNER JOIN lt_data ON drad~objky = @lt_data-plnbez
      INTO TABLE @DATA(lt_doknr).

      LOOP AT lt_doknr ASSIGNING FIELD-SYMBOL(<lfs_doknr>).
        DATA(lv_doknr) = <lfs_doknr>-doknr.
      ENDLOOP.

      CALL FUNCTION 'ZABAP_001_FM_DMS_TO_BINARY'
        EXPORTING
          ip_doknr   = lv_doknr
        IMPORTING
*         EP_MSG     =
*         ES_ACCESS_INFO       =
          ep_xstring = lv_xstring_dms
*         EP_FILE_TYPE         =
*         EP_FILE_NAME         =
        .

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
          i_name     = 'ZPP_002_AF_ISEMRI2'
*         i_name     = 'ZPP_002_AF_ISEMRI2_COPY'             "
*         i_name     = 'ZPP_002_AF_ISEMRI2_VOL3'                                    "
        IMPORTING
          e_funcname = fm_name.

*      CALL FUNCTION '/1BCDWB/SM00000007'
      CALL FUNCTION fm_name
        EXPORTING
          /1bcdwb/docparams = fp_docparams
          is_data           = gs_data
          it_table          = gt_table
          iv_brand          = lv_brand_image
*         iv_image          = lt_xstring_table[ 1 ]
          iv_image          = lv_xstring_dms
* IMPORTING
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
*--------------------------------------------------------------------*
  METHOD call_screen.
    CALL SCREEN 0100.
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD pbo_0100.
    SET PF-STATUS '0100'.
    SET TITLEBAR '0100'.
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD pai_0100.
    CASE iv_ucomm.
      WHEN '&BACK'.
        SET SCREEN 0.
    ENDCASE.
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD set_fcat.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZPP_002_TT_ISEMRI'
      CHANGING
        ct_fieldcat      = gt_fcat.
  ENDMETHOD.

  METHOD set_layout.
    gs_layout = VALUE #( zebra      = abap_true
                         cwidth_opt = abap_true
                         col_opt    = abap_true
                         sel_mode   = 'A').
  ENDMETHOD.
*--------------------------------------------------------------------*
  METHOD display_alv.
    IF go_alv_grid IS INITIAL.

      CREATE OBJECT go_container
        EXPORTING
          container_name = 'CC_ALV'.
      CREATE OBJECT go_alv_grid
        EXPORTING
          i_parent = go_container.  " Parent Container
*          i_parent = cl_gui_container=>screen0.  " Parent Container
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
ENDCLASS.
*--------------------------------------------------------------------*
