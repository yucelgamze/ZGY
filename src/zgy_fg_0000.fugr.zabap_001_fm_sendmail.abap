FUNCTION zabap_001_fm_sendmail.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_CONTENT) TYPE  STRING OPTIONAL
*"     VALUE(IV_SUBJECT) TYPE  SO_OBJ_DES OPTIONAL
*"     VALUE(IT_RECIPIENTS) TYPE  BCSY_SMTPA OPTIONAL
*"     VALUE(IT_PDF_NAME) TYPE  ZABAP_001_TT_FPNAME OPTIONAL
*"     VALUE(IV_TAB_FOR_EXCEL) TYPE  TABNAME OPTIONAL
*"  EXPORTING
*"     VALUE(EV_RETURN) TYPE  STRING
*"     VALUE(EV_RETURN_TYPE) TYPE  CHAR1
*"----------------------------------------------------------------------
*ABAP Developer : Gamze Yücel
*"----------------------------------------------------------------------

  DATA:l_bcs_exception  TYPE REF TO    cx_document_bcs,
       " BCS Exception
       l_addr_exception TYPE REF TO    cx_address_bcs,
       " Address Exception
       l_send_exception TYPE REF TO    cx_send_req_bcs.
  " E-Mail sending Exception
  TRY.
      DATA:lo_gbt       TYPE REF TO cl_gbt_multirelated_service,
           lo_bcs       TYPE REF TO cl_bcs,
           lo_doc_bcs   TYPE REF TO cl_document_bcs,
           lo_recipient TYPE REF TO if_recipient_bcs,
           lt_soli      TYPE TABLE OF soli,
           ls_soli      TYPE TABLE OF soli,
           lv_status    TYPE bcs_rqst,
           lv_content   TYPE string,
           lv_remail    TYPE ad_smtpadr.

      DATA:lv_i_attachment_size	TYPE sood-objlen,
           lt_i_att_content_hex TYPE solix_tab,
           lv_att_content       TYPE string,
           lv_att_line          TYPE string.

      lo_bcs = cl_bcs=>create_persistent( ).

      CREATE OBJECT lo_gbt.

      lt_soli = cl_document_bcs=>string_to_soli( iv_content ).

      CALL METHOD lo_gbt->set_main_html
        EXPORTING
          content = lt_soli.

      lo_doc_bcs = cl_document_bcs=>create_from_multirelated(
                     i_subject          = iv_subject

                     i_multirel_service = lo_gbt ).

      "pdf attachment
******************************************************************************************
      IF it_pdf_name IS NOT INITIAL.

        DATA:ls_outputparams  TYPE  sfpoutputparams,
             lv_name          TYPE  fpname,
             lv_funcname      TYPE  funcname,
             ls_docparamtype  TYPE  sfpdocparams,
             ls_formoutput    TYPE  fpformoutput,
             lv_output_length TYPE  int4.

        DATA:lv_index TYPE i VALUE 0.

        ls_outputparams-preview   = ''.
        ls_outputparams-nodialog  = 'X'.
        ls_outputparams-dest      = 'LP01'.
        ls_outputparams-getpdf    = 'X'.


        LOOP AT it_pdf_name INTO lv_name.

          REFRESH:lt_i_att_content_hex.
          CLEAR:lv_i_attachment_size,
                lv_att_content,
                lv_att_line.

          lv_index = lv_index + 1.

          IF sy-cprog  <> 'RS_TESTFRAME_CALL'.

            CALL FUNCTION 'FP_JOB_OPEN'
              CHANGING
                ie_outputparams = ls_outputparams
              EXCEPTIONS
                cancel          = 1
                usage_error     = 2
                system_error    = 3
                internal_error  = 4
                OTHERS          = 5.
          ENDIF.
          "lv_name = iv_pdf_name.

          CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
            EXPORTING
              i_name     = lv_name
            IMPORTING
              e_funcname = lv_funcname.

          CALL FUNCTION lv_funcname
            EXPORTING
              /1bcdwb/docparams  = ls_docparamtype
            IMPORTING
              /1bcdwb/formoutput = ls_formoutput
            EXCEPTIONS
              usage_error        = 1
              system_error       = 2
              internal_error     = 3
              OTHERS             = 4.

          IF sy-cprog  <> 'RS_TESTFRAME_CALL'.
            CALL FUNCTION 'FP_JOB_CLOSE'
              EXCEPTIONS
                usage_error    = 1
                system_error   = 2
                internal_error = 3
                OTHERS         = 4.
          ENDIF.

          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              buffer        = ls_formoutput-pdf
            IMPORTING
              output_length = lv_output_length
            TABLES
              binary_tab    = lt_i_att_content_hex.

          "char12 vs integer ataması
          lv_i_attachment_size =  lv_output_length.

          lo_doc_bcs->add_attachment(
            EXPORTING
              i_attachment_type     =  'PDF'                    " Document Class for Attachment
              i_attachment_subject  =  |attachment_PDF_{ lv_index }|        " Attachment Title
              i_attachment_size     =  lv_i_attachment_size     " Size of Document Content
              i_att_content_hex     =  lt_i_att_content_hex ).  " Content (Binary)

        ENDLOOP.
      ENDIF.
****************************************************************************************************************
      "XLS ATTACHMENT SECTION
********************************************************************************************************************
      IF iv_tab_for_excel IS NOT INITIAL.

        DATA:tablename TYPE tabname.
        tablename = iv_tab_for_excel.

        DATA:data_ref  TYPE REF TO data.
        DATA:lt_tab TYPE REF TO data.
        FIELD-SYMBOLS : <fs_table> TYPE ANY TABLE.

        DATA:lo_line_type TYPE REF TO cl_abap_structdescr.

        DATA:dbtab      TYPE string,
             except     TYPE string,
             token      TYPE string,
             target     TYPE REF TO data,
             components TYPE cl_abap_structdescr=>component_table.

        dbtab = CONV #( tablename ).
        except = 'mandt'.

        lo_line_type ?= cl_abap_typedescr=>describe_by_name( tablename ).     "wide cast
        DATA(lo_table_type) = cl_abap_tabledescr=>create( p_line_type = lo_line_type ).

        components = CAST cl_abap_structdescr(
          cl_abap_typedescr=>describe_by_name( to_upper( dbtab ) )
            )->get_components( ).

        SPLIT except AT `,` INTO TABLE DATA(columns).
        LOOP AT columns ASSIGNING FIELD-SYMBOL(<column>).
          DELETE components WHERE name = to_upper( condense( <column> ) ).
        ENDLOOP.

        token =
          REDUCE string( INIT s = ``
                         FOR <wa> IN components
                         NEXT s &&=  COND #( WHEN s = ``  THEN <wa>-name
                                             ELSE  `, ` && <wa>-name ) ).

        DATA(itab_type) =
         cl_abap_tabledescr=>get(
                 p_line_type  = cl_abap_structdescr=>get(
                                   p_components = components )
                 p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA lt_tab TYPE HANDLE itab_type.  "lo_table_type yerine bu geldi

        ASSIGN lt_tab->* TO <fs_table>.       "dereference pointer

        SELECT (token) FROM (tablename) INTO CORRESPONDING FIELDS OF TABLE @<fs_table>.

        GET REFERENCE OF <fs_table> INTO data_ref.
        DATA(lv_xstring) = NEW zcl_abap_001_general( )->itab_to_xstring( data_ref ).


        lo_doc_bcs->add_attachment(
            i_attachment_type    = 'xls'
            i_attachment_size    = CONV #( xstrlen( lv_xstring ) )
            i_attachment_subject = |attachment_EXCEL|
            i_attachment_header  = VALUE #( ( line = | { tablename }.xlsx| ) )
            i_att_content_hex    = cl_bcs_convert=>xstring_to_solix( lv_xstring )
         ).

      ENDIF.
****************************************************************************************************************

      LOOP AT it_recipients INTO lv_remail.

        lo_recipient = cl_cam_address_bcs=>create_internet_address(
                          i_address_string = CONV #( lv_remail ) ).

        lo_bcs->add_recipient( i_recipient = lo_recipient ).
      ENDLOOP.

      lo_bcs->set_document( i_document = lo_doc_bcs ).

      lv_status = 'N'.


      CALL METHOD lo_bcs->set_status_attributes
        EXPORTING
          i_requested_status = lv_status.

      lo_bcs->send( ).
      COMMIT WORK.

    CATCH cx_document_bcs INTO l_bcs_exception.

    CATCH cx_send_req_bcs INTO l_send_exception.

    CATCH cx_address_bcs  INTO l_addr_exception.

  ENDTRY.

  IF sy-subrc EQ 0.
    ev_return_type = 'S'.
    ev_return = |Mail gönderme işlemi başarılı!|.
  ELSE.
    ev_return_type = 'E'.
    ev_return = |Mail gönderme işlemi sırasında hata!|.
  ENDIF.

ENDFUNCTION.
