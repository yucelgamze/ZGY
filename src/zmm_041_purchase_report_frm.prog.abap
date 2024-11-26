*&---------------------------------------------------------------------*
*& Include          ZMM_041_PURCHASE_REPORT_FRM
*&---------------------------------------------------------------------*
FORM callback_f4 TABLES record_tab STRUCTURE seahlpres
            CHANGING shlp TYPE shlp_descr
                     callcontrol LIKE ddshf4ctrl.
  DATA:ls_intf LIKE LINE OF shlp-interface,
       ls_prop LIKE LINE OF shlp-fieldprop.

  CLEAR: ls_prop-shlpselpos,
         ls_prop-shlplispos.

  REFRESH: shlp-interface.
  ls_intf = VALUE #( shlpfield = 'F0001'
                     valfield  = 'SO_ERNAM-LOW'
                     f4field   = 'X').
  APPEND ls_intf TO shlp-interface.

  ls_intf = VALUE #( shlpfield = 'F0002'
                     valfield  = 'SO_BANFN-LOW'
                     f4field   = 'X').
  APPEND ls_intf TO shlp-interface.
ENDFORM.

FORM callback_f4_sas TABLES record_tab STRUCTURE seahlpres
            CHANGING shlp TYPE shlp_descr
                     callcontrol LIKE ddshf4ctrl.
  DATA:ls_intf LIKE LINE OF shlp-interface,
       ls_prop LIKE LINE OF shlp-fieldprop.

  CLEAR: ls_prop-shlpselpos,
         ls_prop-shlplispos.

  REFRESH: shlp-interface.
  ls_intf = VALUE #( shlpfield = 'F0001'
                     valfield  = 'SO_SASBY-LOW'
                     f4field   = 'X').
  APPEND ls_intf TO shlp-interface.

  ls_intf = VALUE #( shlpfield = 'F0002'
                     valfield  = 'SO_EBELN-LOW'
                     f4field   = 'X').
  APPEND ls_intf TO shlp-interface.
ENDFORM.
