FUNCTION ZABAP_001_FM_DMS_TO_BINARY.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IP_DOKNR) TYPE  DRAW-DOKNR OPTIONAL
*"  EXPORTING
*"     VALUE(EP_MSG) TYPE  STRING
*"     VALUE(ES_ACCESS_INFO) TYPE  SCMS_ACINF
*"     VALUE(EP_XSTRING) TYPE  XSTRING
*"     VALUE(EP_FILE_TYPE) TYPE  DAPPL
*"     VALUE(EP_FILE_NAME) TYPE  CHAR200
*"--------------------------------------------------------------------
*ABAP Developer : Gamze Yücel
*"--------------------------------------------------------------------
  "Bu FM; DMS ek çıktısını binary formatta dönecektir.

  DATA: gs_drad   TYPE drad,
        gs_draw   TYPE draw,
        gt_files  TYPE STANDARD TABLE OF dms_doc_files,
        gs_files  TYPE dms_doc_files,
        gt_doc2l  TYPE STANDARD TABLE OF dms_doc2loio,
        gs_doc2l  TYPE dms_doc2loio,
        gt_ph     TYPE STANDARD TABLE OF dms_ph_cd1,
        gs_ph     TYPE dms_ph_cd1,
        gs_tdwaat TYPE tdwat.

  DATA: access_info  LIKE STANDARD TABLE OF scms_acinf WITH HEADER LINE,
        content_txt  LIKE STANDARD TABLE OF sdokcntasc WITH HEADER LINE,
        content_bin  LIKE STANDARD TABLE OF sdokcntbin WITH HEADER LINE,
        content_bin1 LIKE STANDARD TABLE OF sdokcntbin WITH HEADER LINE,
        gt_bin       TYPE STANDARD TABLE OF raw255,
        gs_bin       TYPE raw255.

  IF ip_doknr IS NOT INITIAL.

    ip_doknr = |{ ip_doknr ALPHA = IN }|.

    SELECT SINGLE *
    FROM draw
    INTO CORRESPONDING FIELDS OF gs_draw
    WHERE doknr = ip_doknr.

    SELECT *
    FROM dms_doc_files
    INTO CORRESPONDING FIELDS OF TABLE gt_files
    WHERE doknr = gs_draw-doknr.

    SELECT *
    FROM dms_doc2loio
    INTO CORRESPONDING FIELDS OF TABLE gt_doc2l
    WHERE doknr = gs_draw-doknr.

    IF gt_doc2l[] IS NOT INITIAL.
      SELECT *
      FROM dms_ph_cd1
      INTO CORRESPONDING FIELDS OF TABLE @gt_ph
      FOR ALL ENTRIES IN @gt_doc2l
      WHERE loio_id = @gt_doc2l-lo_objid.
    ENDIF.


    LOOP AT gt_files INTO gs_files WHERE doknr = gs_draw-doknr.
      CLEAR: gs_doc2l.
      READ TABLE gt_doc2l INTO gs_doc2l WITH KEY doknr = gs_draw-doknr.
      CLEAR: gs_ph.
      IF gs_doc2l-lo_objid IS NOT INITIAL.
        READ TABLE gt_ph INTO gs_ph WITH KEY loio_id = gs_doc2l-lo_objid.

        CALL FUNCTION 'SCMS_DOC_READ'
          EXPORTING
*           MANDT                 = SY-MANDT
            stor_cat              = gs_ph-stor_cat
*           CREP_ID               = ' '
            doc_id                = gs_ph-phio_id
*           PHIO_ID               =
            signature             = 'X'
*           SECURITY              = ' '
*           NO_CACHE              = ' '
*           RAW_MODE              = ' '
*         IMPORTING
*           FROM_CACHE            =
*           CREA_TIME             =
*           CREA_DATE             =
*           CHNG_TIME             =
*           CHNG_DATE             =
*           STATUS                =
*           DOC_PROT              =
          TABLES
            access_info           = access_info
            content_txt           = content_txt
            content_bin           = content_bin
          EXCEPTIONS
            bad_storage_type      = 1
            bad_request           = 2
            unauthorized          = 3
            comp_not_found        = 4
            not_found             = 5
            forbidden             = 6
            conflict              = 7
            internal_server_error = 8
            error_http            = 9
            error_signature       = 10
            error_config          = 11
            error_format          = 12
            error_parameter       = 13
            error                 = 14
            OTHERS                = 15.

        IF content_bin[] IS NOT INITIAL.

          READ TABLE access_info INDEX 1.
          es_access_info = access_info.

          CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
            EXPORTING
              input_length = access_info-comp_size
*             FIRST_LINE   = 0
*             LAST_LINE    = 0
            IMPORTING
              buffer       = ep_xstring
            TABLES
              binary_tab   = content_bin
            EXCEPTIONS
              failed       = 1
              OTHERS       = 2.

        ENDIF.

        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = ep_xstring
*           APPEND_TO_TABLE       = ' '
          IMPORTING
            output_length = access_info-comp_size
          TABLES
            binary_tab    = gt_bin.

        ep_file_type = gs_files-dappl.
        ep_file_name = gs_files-filename.

      ENDIF.

      CLEAR: gs_files.
      EXIT.
    ENDLOOP.

  ELSE.
    ep_msg = |Gerekli parametreleri doldurunuz!|.
  ENDIF.

ENDFUNCTION.
