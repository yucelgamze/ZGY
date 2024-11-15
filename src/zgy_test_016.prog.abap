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
