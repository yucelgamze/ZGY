*&---------------------------------------------------------------------*
*& Include          ZMM_011_PURCHASE_TOP
*&---------------------------------------------------------------------*
TABLES:ipritemapi01.
TYPE-POOLS:icon.
CLASS lcl_class DEFINITION DEFERRED.
DATA:go_local     TYPE REF TO lcl_class,
     go_alv_grid  TYPE REF TO cl_gui_alv_grid,
     go_container TYPE REF TO cl_gui_custom_container,
     gt_fcat      TYPE lvc_t_fcat,
     gs_fcat      TYPE lvc_s_fcat,
     gs_layout    TYPE lvc_s_layo.

DATA:go_alv_grid_list TYPE REF TO cl_gui_alv_grid.

* i_purchaserequisition_api01

TYPES:BEGIN OF gty_alv,
        convertion_status       TYPE icon_d,
        purchaserequisition     TYPE ipritemapi01-purchaserequisition,
        purchaserequisitionitem TYPE ipritemapi01-purchaserequisitionitem,
        requestedquantity       TYPE ipritemapi01-requestedquantity,
        creationdate            TYPE ipritemapi01-creationdate,
        createdbyuser           TYPE ipritemapi01-createdbyuser,
      END OF gty_alv.

DATA:gt_alv TYPE TABLE OF gty_alv,
     gs_alv TYPE gty_alv.

DATA:gt_list TYPE TABLE OF gty_alv,
     gs_list TYPE gty_alv.

DATA:gv_date TYPE datum.

DATA:gv_purchase TYPE xfeld,
     gv_list     TYPE xfeld.

RANGES:gr_createdbyuser FOR ipritemapi01-createdbyuser.  "mail adresleri için user range

SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE TEXT-000.
  PARAMETERS:rb_alv  RADIOBUTTON GROUP a DEFAULT 'X' USER-COMMAND cmd,
             rb_list RADIOBUTTON GROUP a.
SELECTION-SCREEN END OF BLOCK b.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:so_pr  FOR ipritemapi01-purchaserequisition,
                 so_pri FOR ipritemapi01-purchaserequisitionitem,
                 so_cd  FOR ipritemapi01-creationdate.
SELECTION-SCREEN END OF BLOCK a.

AT SELECTION-SCREEN OUTPUT.
  CASE abap_true.

    WHEN rb_alv.
      gv_purchase = abap_true.
      gv_list = abap_false.
    WHEN rb_list.
      gv_purchase = abap_false.
      gv_list = abap_true.

*      LOOP AT SCREEN.
*        IF screen-group1 EQ 'X'.
*          screen-active = 1.
*          MODIFY SCREEN.
*        ELSEIF screen-group1 EQ 'Y'.
*          screen-active = 0.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDLOOP.
  ENDCASE.
