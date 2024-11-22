*&---------------------------------------------------------------------*
*& Report ZGY_TEST_016
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_test_016.

DATA:gt_return   TYPE TABLE OF bapiret2,
     lt_messages TYPE esp1_message_tab_type.

APPEND VALUE #( type       = 'E'
                id         = 'ZGY'
                number     = '000'
                message_v1 = 'test' ) TO gt_return.


LOOP AT gt_return ASSIGNING FIELD-SYMBOL(<lfs_msg>).
  APPEND VALUE #( msgid = <lfs_msg>-id
                  msgty = <lfs_msg>-type
                  msgno = <lfs_msg>-number
                  msgv1 = <lfs_msg>-message_v1
                  msgv2 = <lfs_msg>-message_v2
                  msgv3 = <lfs_msg>-message_v3
                  msgv4 = <lfs_msg>-message_v4 ) TO lt_messages.
ENDLOOP.

CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
  TABLES
    i_message_tab = lt_messages.


**CALL FUNCTION 'WS_DELIVERY_UPDATE_2'
**  EXPORTING
**    vbkok_wa      = ls_vbkok
**    commit        = abap_true
**    delivery      = lv_delivery
**  IMPORTING
**    ef_error_any  = lv_error
**  TABLES
**    vbpok_tab     = lt_vbpok
**    prot          = lt_prot
**  EXCEPTIONS
**    error_message = 99.
**IF sy-subrc <> 0.
**  APPEND VALUE #( type = 'E'
**                  id   = sy-msgid
**                  number = sy-msgno
**                  message_v1 = sy-msgv1
**                  message_v2 = sy-msgv2
**                  message_v3 = sy-msgv3
**                  message_v4 = sy-msgv4 ) TO gt_return.
**ELSE.
**  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
**    EXPORTING
**      wait = 'X'.
**ENDIF.
