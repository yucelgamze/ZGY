*&---------------------------------------------------------------------*
*& Report ZGY_DEV_P009
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgy_dev_p009.

TYPES:BEGIN OF lty_user,
        createdbyuser TYPE ipritemapi01-createdbyuser,
      END OF lty_user.

DATA:ls_user TYPE lty_user,
     lt_user TYPE TABLE OF lty_user.

CLEAR ls_user.
ls_user-createdbyuser = 'S4H_HCM_EE'.
APPEND ls_user TO lt_user.

CLEAR ls_user.
ls_user-createdbyuser = 'SERV_EMPL'.
APPEND ls_user TO lt_user.

RANGES:lr_createdbyuser FOR ipritemapi01-createdbyuser.

LOOP AT lt_user ASSIGNING FIELD-SYMBOL(<lfs_user>).
  lr_createdbyuser = VALUE #( sign   = 'I'
                              option = 'CP'
                              low    = |{ <lfs_user>-createdbyuser }| ).
  APPEND lr_createdbyuser TO lr_createdbyuser.
ENDLOOP.


SELECT
usrid,
pernr
FROM pa0105
WHERE subty = '0001'
AND   usrid IN @lr_createdbyuser[]     "SAT ı oluşturan olacak mesela
INTO TABLE @DATA(lt_recid).

IF lt_recid IS NOT INITIAL.
  SELECT
  usrid_long,
  pernr
  FROM pa0105
  FOR ALL ENTRIES IN @lt_recid
  WHERE subty = '0010'
  AND   pernr = @lt_recid-pernr
  INTO TABLE @DATA(lt_rec).
ENDIF.

WRITE: |hi|.
