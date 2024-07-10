*&---------------------------------------------------------------------*
*& Report ZGY_DEV_P006
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_dev_p006.

TYPES:
  BEGIN OF line,
    col1 TYPE i,
    col2 TYPE i,
    col3 TYPE i,
  END OF line,
  itab0 TYPE STANDARD TABLE OF line WITH EMPTY KEY.

DATA(itab0) = VALUE itab0(
     FOR j = 11 THEN j + 10 UNTIL j > 40
     ( col1 = j col2 = j + 1 col3 = j + 2  ) ).

*--------------------------------------------------------------------*

DATA itab TYPE STANDARD TABLE OF i WITH EMPTY KEY.
itab = VALUE #( FOR j = 1 WHILE j <= 10 ( j ) ).


DATA(sum) = REDUCE i( INIT x = 0 FOR wa IN itab NEXT x = x + wa ).

*--------------------------------------------------------------------*

DATA(result) =

  REDUCE string( INIT text = `Count up:`

                 FOR n = 1 UNTIL n > 10

                 NEXT text = text && | { n }| ).

*--------------------------------------------------------------------*
TYPES:BEGIN OF gty_test,
        name TYPE char10,
      END OF gty_test.

DATA:gt_test TYPE TABLE OF gty_test.

*gt_test = VALUE #( ( name = 'row1')
*                   ( name = 'row2')
*                   ( name = 'row3') ).
gt_test = VALUE #( FOR i = 1 WHILE i <= 5 ( name = |row{ i } | ) ).
DATA(test) =
  REDUCE string( INIT s = ``
                 FOR <wa> IN gt_test
                 NEXT s &&=  COND #( WHEN s = ``  THEN <wa>-name
                                     ELSE  `, ` && <wa>-name ) ).

"row1, row2, row3, row4, row5

WRITE:sum,/,result,/,test.

*--------------------------------------------------------------------*

TYPES outref TYPE REF TO if_demo_output.

DATA(output) =
  REDUCE outref( INIT out  = cl_demo_output=>new( )
                      text = `Count up:`
                 FOR n = 1 UNTIL n > 11
                 NEXT out = out->write( text )
                      text = |{ n }| ).

output->display( ).
