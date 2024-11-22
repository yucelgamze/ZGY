*&---------------------------------------------------------------------*
*& Report ZGY_TEST_002
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_test_002.

PARAMETERS:p_tab TYPE tabname.

DATA:gr_tab TYPE REF TO data,
     gr_wa  TYPE REF TO data,
     gr_var TYPE REF TO data.

CREATE DATA:gr_var TYPE i,
            gr_wa TYPE (p_tab),
            gr_tab TYPE TABLE OF (p_tab).

ASSIGN gr_var->* TO FIELD-SYMBOL(<lfs_var>).
<lfs_var> = 10.

ASSIGN gr_wa->* TO FIELD-SYMBOL(<lfs_wa>).

SELECT SINGLE *
FROM (p_tab)
INTO @<lfs_wa>.

ASSIGN gr_tab->* TO FIELD-SYMBOL(<lfs_tab>).

SELECT DISTINCT *
FROM (p_tab)
INTO TABLE @<lfs_tab>
UP TO 20 ROWS.

BREAK gamzey.
