class ZCL_IM_MM_000_PR_MAIL definition
  public
  final
  create public .

public section.

  interfaces IF_EX_ME_PROCESS_REQ_CUST .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_MM_000_PR_MAIL IMPLEMENTATION.


  method IF_EX_ME_PROCESS_REQ_CUST~CHECK.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~CLOSE.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~FIELDSELECTION_HEADER.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~FIELDSELECTION_HEADER_REFKEYS.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~FIELDSELECTION_ITEM.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~FIELDSELECTION_ITEM_REFKEYS.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~INITIALIZE.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~OPEN.
  endmethod.


  METHOD if_ex_me_process_req_cust~post.

    "ME53N
    IF sy-uname EQ 'GAMZEY'.

      DATA:ls_item  TYPE mereq_item,
           lt_items TYPE mmpur_requisition_items.

      CALL METHOD im_header->get_items
        RECEIVING
          re_items = lt_items.                " Items

      LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<lfs_items>).
        <lfs_items>-item->get_data(
          RECEIVING
            re_data = ls_item                 " Item Data
        ).
      ENDLOOP.

      IF ls_item IS NOT INITIAL.

      ENDIF.
    ENDIF.

  ENDMETHOD.


  method IF_EX_ME_PROCESS_REQ_CUST~PROCESS_ACCOUNT.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~PROCESS_HEADER.
  endmethod.


  method IF_EX_ME_PROCESS_REQ_CUST~PROCESS_ITEM.
  endmethod.
ENDCLASS.
