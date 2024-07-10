class ZCL_ZGY_TABLET_V2_DPC_EXT definition
  public
  inheriting from ZCL_ZGY_TABLET_V2_DPC
  create public .

public section.
protected section.

  methods DATASET_CREATE_ENTITY
    redefinition .
  methods DATASET_GET_ENTITYSET
    redefinition .
  methods TABLETSET_CREATE_ENTITY
    redefinition .
  methods TABLETSET_DELETE_ENTITY
    redefinition .
  methods TABLETSET_GET_ENTITY
    redefinition .
  methods TABLETSET_GET_ENTITYSET
    redefinition .
  methods TABLETSET_UPDATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZGY_TABLET_V2_DPC_EXT IMPLEMENTATION.


  METHOD dataset_create_entity.

    TRY.

        TYPES: BEGIN OF gty_json,
                 departman(100)       TYPE c,
                 is_emri_no(10)       TYPE c,
                 operasyon_adi(100)   TYPE c,
                 siparis_tipi(50)     TYPE c,
                 model_adi(100)       TYPE c,
                 renk_adi(50)         TYPE c,
                 beden(10)            TYPE c,
                 baslama_saati        TYPE tims,
                 standart_sure        TYPE tims,
                 toplam_standart_sure TYPE tims,
                 miktar(10)           TYPE c,
                 demet_no(10)         TYPE c,
               END OF gty_json.

        DATA: json_tab TYPE TABLE OF gty_json.

        DATA: ls_entity TYPE zcl_zgy_tablet_v2_mpc=>ts_data,
              itab      TYPE TABLE OF zgy_t_0003,
              gs_tablet TYPE zgy_t_0003.

        io_data_provider->read_entry_data(
          IMPORTING
            es_data = ls_entity
        ).

        DATA(lo_http) = NEW cl_http_utility( ).
        DATA(lv_json) = lo_http->decode_base64( encoded = ls_entity-value ).

        /ui2/cl_json=>deserialize(
          EXPORTING
            json             = lv_json
            pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
          CHANGING
            data             = json_tab
        ).

        LOOP AT json_tab ASSIGNING FIELD-SYMBOL(<lfs_json_line>).
          MOVE-CORRESPONDING <lfs_json_line> TO gs_tablet.
          APPEND gs_tablet TO itab.
        ENDLOOP.

        INSERT zgy_t_0003 FROM TABLE itab.

      CATCH /iwbep/cx_mgw_busi_exception .
      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.
  ENDMETHOD.


  METHOD dataset_get_entityset.

    DATA : ls_entity TYPE zcl_zgy_tablet_v2_mpc=>ts_data.

    SELECT * FROM zgy_t_0003
      INTO TABLE @DATA(lt_0003).

    IF sy-subrc EQ 0.
      LOOP AT lt_0003 ASSIGNING FIELD-SYMBOL(<lfs_0003>).
        MOVE-CORRESPONDING <lfs_0003> TO ls_entity.
        APPEND ls_entity TO et_entityset.
      ENDLOOP.
    ENDIF.


  ENDMETHOD.


METHOD tabletset_create_entity.
  TRY.
      DATA :ls_data TYPE zcl_zgy_tablet_v2_mpc=>ts_tablet,
            gs_0003 TYPE zgy_t_0003.

      io_data_provider->read_entry_data(
        IMPORTING
          es_data  = ls_data ).
      IF ls_data IS NOT INITIAL.
        MOVE-CORRESPONDING ls_data TO gs_0003.

        INSERT zgy_t_0003 FROM gs_0003.

        IF sy-subrc EQ 0.
          er_entity = ls_data.
        ENDIF.
      ENDIF.
    CATCH /iwbep/cx_mgw_tech_exception.
  ENDTRY.
ENDMETHOD.


  METHOD tabletset_delete_entity.
    DATA: ls_key_tab TYPE /iwbep/s_mgw_name_value_pair.

    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'Departman'.
    IF sy-subrc EQ 0.
      DATA(lv_departman) = ls_key_tab-value.

      DELETE FROM zgy_t_0003 WHERE departman EQ lv_departman.
    ENDIF.
  ENDMETHOD.


  METHOD tabletset_get_entity.

    DATA: ls_key_tab TYPE /iwbep/s_mgw_name_value_pair.

    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'Departman'.
    IF sy-subrc EQ 0.
      DATA(lv_departman) = ls_key_tab-value.
    ENDIF.

    SELECT SINGLE *
      FROM zgy_t_0003
      INTO @DATA(ls_0003)
      WHERE departman EQ @lv_departman.

    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING ls_0003 TO er_entity.
    ENDIF.

  ENDMETHOD.


  method TABLETSET_GET_ENTITYSET.

    DATA : ls_entityset TYPE zcl_zgy_tablet_v2_mpc=>ts_tablet.

    SELECT * FROM zgy_t_0003
      INTO TABLE @DATA(lt_0003).

    IF sy-subrc EQ 0.
      LOOP AT lt_0003 ASSIGNING FIELD-SYMBOL(<lfs_0003>).
        MOVE-CORRESPONDING <lfs_0003> TO ls_entityset.
        APPEND ls_entityset TO et_entityset.
      ENDLOOP.
    ENDIF.

  endmethod.


  METHOD tabletset_update_entity.
    DATA: ls_key_tab TYPE /iwbep/s_mgw_name_value_pair.
    DATA: ls_data TYPE zcl_zgy_tablet_v2_mpc=>ts_tablet.

    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'Departman'.
    IF sy-subrc EQ 0.
      DATA(lv_departman) = ls_key_tab-value.
    ENDIF.

    io_data_provider->read_entry_data(
      IMPORTING
        es_data = ls_data
    ).

    IF lv_departman IS NOT INITIAL AND ls_data IS NOT INITIAL.
      UPDATE zgy_t_0003 SET baslama_saati = ls_data-baslama_saati
                            beden         = ls_data-beden
                            demet_no      = ls_data-demet_no
                            is_emri_no    = ls_data-is_emri_no
                            miktar        = ls_data-miktar
                            model_adi     = ls_data-model_adi
                            operasyon_adi = ls_data-operasyon_adi
                            renk_adi      = ls_data-renk_adi
                            siparis_tipi  = ls_data-siparis_tipi
                            standart_sure = ls_data-standart_sure
                            toplam_standart_sure = ls_data-toplam_standart_sure WHERE departman = lv_departman.
    ENDIF.

    IF sy-subrc EQ 0.
      er_entity = ls_data.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
