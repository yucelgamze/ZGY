*&---------------------------------------------------------------------*
*& Include          ZPP_013_QTRANS_TOP
*&---------------------------------------------------------------------*
TABLES:mara,mard,marc,mchb.
TYPE-POOLS:icon.
CLASS lcl_class DEFINITION DEFERRED.
DATA:go_local     TYPE REF TO lcl_class,
     go_alv_grid  TYPE REF TO cl_gui_alv_grid,
     go_alv_grid2 TYPE REF TO cl_gui_alv_grid,
     go_container TYPE REF TO cl_gui_custom_container,
     gt_fcat      TYPE lvc_t_fcat,
     gt_fcat2     TYPE lvc_t_fcat,
     gs_fcat      TYPE lvc_s_fcat,
     gs_layout    TYPE lvc_s_layo.

TYPES: BEGIN OF gty_table,
         transfer_durumu TYPE icon_d,
         matnr           TYPE mara-matnr,
         maktx           TYPE makt-maktx,
         lgort           TYPE mard-lgort,
         charg           TYPE mchb-charg,
         labst           TYPE mard-labst,
         meins           TYPE mara-meins,
         transfer_miktar TYPE labst,
         kalite_matnr    TYPE marc-matnr,
         kalite_maktx    TYPE makt-maktx,
         hedef_depo      TYPE lgort_d,
         hedef_parti     TYPE mchb-charg,
         hedef_stok      TYPE labst,
         delete          TYPE char1,
         type            TYPE bapi_mtype,
         message         TYPE bapi_msg,
         cell_color      TYPE lvc_t_scol,
         cellstyle       TYPE lvc_t_styl, "field used for editing the particular cell
       END OF gty_table.

DATA: gs_cell_color TYPE lvc_s_scol.

DATA: gt_table TYPE TABLE OF gty_table,
      gs_table TYPE gty_table.

FIELD-SYMBOLS: <gfs_table> TYPE gty_table.

DATA: gv_save   TYPE xfeld,
      gv_commit TYPE xfeld.

DATA: gt_return TYPE TABLE OF bapiret2.

DATA: gv_matnr           TYPE zpp_013_de_malzeme,
      gv_lgort           TYPE zpp_013_de_depo_yeri,
      gv_charg           TYPE zpp_013_de_parti,
      gv_transfer_miktar TYPE zpp_013_de_transfer_miktar,
      gv_kalite_matnr    TYPE zpp_013_de_kalite_malzeme,
      gv_hedef_depo      TYPE zpp_013_de_hedef_depo,
      gv_hedef_parti     TYPE zpp_013_de_hedef_parti.

RANGES: gr_matnr FOR mara-matnr.

DATA: gv_lines TYPE i.

DATA: gt_trans TYPE TABLE OF zpp_013_t_trans.

SELECTION-SCREEN   BEGIN OF BLOCK a WITH FRAME TITLE TEXT-000.
  SELECT-OPTIONS:so_werks FOR mard-werks NO INTERVALS DEFAULT 8108 OBLIGATORY,
                 so_datum FOR sy-datum NO INTERVALS DEFAULT sy-datum OBLIGATORY.
*  PARAMETERS: p_datum TYPE sy-datum DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK a.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_datum.

AT SELECTION-SCREEN.
  IF ( sy-datum+6(2) NE 01 ) AND ( sy-datum+6(2) NE 02 ).
    IF  sy-datum+4(2) - so_datum-low+4(2) GE 1.
      MESSAGE e002 DISPLAY LIKE 'I'.
*      LEAVE LIST-PROCESSING.
*      LEAVE TO TRANSACTION 'ZPPR013'.
    ENDIF.
  ENDIF.

*INITIALIZATION.
*
*  CASE so_datum+6(2).
*    WHEN '01'.
*
*    WHEN '02'.
*
*    WHEN OTHERS.
*  ENDCASE.
