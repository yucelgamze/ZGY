*&---------------------------------------------------------------------*
*& Include          ZGY_Z_P001_TOP
*&---------------------------------------------------------------------*
TYPE-POOLS:slis.
TABLES:ekko,ekpo.
CLASS lcl_class DEFINITION DEFERRED.
DATA:go_local  TYPE REF TO lcl_class,
     gt_fcat   TYPE slis_t_fieldcat_alv,
     gs_fcat   TYPE slis_fieldcat_alv,
     gs_layout TYPE slis_layout_alv.

DATA:gt_events TYPE slis_t_event,
     gs_event  TYPE slis_alv_event.

TYPES:BEGIN OF gty_alv,
        ebeln TYPE ekko-ebeln,
        ebelp TYPE ekpo-ebelp,
        bstyp TYPE ekko-bstyp,
        bsart TYPE ekko-bsart,
        matnr TYPE ekpo-matnr,
        menge TYPE ekpo-menge,
        meins TYPE ekpo-meins,
        edit  TYPE zgy_de_edit,
        field TYPE char20,
      END OF gty_alv.

DATA:
  gt_alv TYPE TABLE OF gty_alv,  "header line olmadan internal table
  gs_alv TYPE gty_alv.

FIELD-SYMBOLS:<gfs_alv> TYPE gty_alv.

*DATA:BEGIN OF gt_alv OCCURS 0,      " header line lı internal table tanımı
*       ebeln LIKE ekko-ebeln,
*       ebelp LIKE ekpo-ebelp,
*       bstyp LIKE ekko-bstyp,
*       bsart LIKE ekko-bsart,
*       matnr LIKE ekpo-matnr,
*       menge LIKE ekpo-menge,
*       meins LIKE ekpo-meins,
*     END OF gt_alv.

*DATA:gt_alv TYPE gty_alv OCCURS 0 WITH HEADER LINE.  "types kullanarak header line tanımı daha pratik yöntemi
