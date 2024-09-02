*&---------------------------------------------------------------------*
*& Report ZGY_DEV_P019
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_dev_p019.

"Parallel Cursor is the technique to use loop inside loop.

"SAP has given SNWD* tables
"SNWD_SO -> Sales Orders demo table
"SNWD_SO_I -> Sales Orders Items table

"Tcode to generate test data : SEPM_DG

GET TIME STAMP FIELD DATA(lv_start_time).

DATA:lv_sum   TYPE snwd_so_i-gross_amount,
     lv_total TYPE int8.

SELECT
node_key,
so_id
FROM snwd_so
INTO TABLE @DATA(lt_header)
UP TO 5000 ROWS.

CHECK lt_header IS NOT INITIAL.

SELECT
node_key,
parent_key,
gross_amount
FROM snwd_so_i
INTO TABLE @DATA(lt_items)
FOR ALL ENTRIES IN @lt_header
WHERE parent_key = @lt_header-node_key.

"sort the internal table based on field on which you are comparing.
SORT lt_header BY node_key.
SORT lt_items BY parent_key.

LOOP AT lt_header INTO DATA(ls_header).

  READ TABLE lt_items INTO DATA(ls_item) WITH KEY parent_key = ls_header-node_key BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    DATA(lv_index) = sy-tabix.

    LOOP AT lt_items INTO ls_item FROM lv_index.
      IF ls_item-parent_key NE ls_header-node_key.
        EXIT.
      ENDIF.
      lv_sum += ls_item-gross_amount.
    ENDLOOP.

    WRITE: / ls_header-so_id, lv_sum.
    CLEAR:lv_sum.
  ENDIF.

ENDLOOP.

GET TIME STAMP FIELD DATA(lv_end_time).

lv_total = lv_end_time - lv_start_time.

WRITE:'Total time:',lv_total.
