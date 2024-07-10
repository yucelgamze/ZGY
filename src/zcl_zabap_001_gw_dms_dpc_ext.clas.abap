class ZCL_ZABAP_001_GW_DMS_DPC_EXT definition
  public
  inheriting from ZCL_ZABAP_001_GW_DMS_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZABAP_001_GW_DMS_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_stream.

    DATA: gs_key_tab   LIKE LINE OF it_key_tab,
          lv_doknr     TYPE doknr,
          lv_xstring   TYPE xstring,
          lv_file_type TYPE dappl,
          gs_stream    TYPE ty_s_media_resource.

    IF it_key_tab[] IS NOT INITIAL.
      READ TABLE it_key_tab INTO gs_key_tab INDEX 1.
      IF sy-subrc IS INITIAL.
        lv_doknr = gs_key_tab-value.
      ENDIF.
    ENDIF.

    lv_doknr = |{ lv_doknr ALPHA = IN }|.

    CALL FUNCTION 'ZABAP_001_FM_DMS_TO_BINARY'
      EXPORTING
        ip_doknr     = lv_doknr
      IMPORTING
*       EP_MSG       =
*       ES_ACCESS_INFO       =
        ep_xstring   = lv_xstring
        ep_file_type = lv_file_type
*       EP_FILE_NAME =
      .
    gs_stream = VALUE #( BASE gs_stream
                                       value = lv_xstring
                                   mime_type = |application/{ lv_file_type }| ).

    CALL METHOD me->copy_data_to_ref
      EXPORTING
        is_data = gs_stream
      CHANGING
        cr_data = er_stream.
  ENDMETHOD.
ENDCLASS.
