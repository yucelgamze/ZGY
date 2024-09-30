class ZGY_CL_ME_CHANGE_OUTTAB_CUS definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ME_CHANGE_OUTTAB_CUS .
protected section.
private section.
ENDCLASS.



CLASS ZGY_CL_ME_CHANGE_OUTTAB_CUS IMPLEMENTATION.


  METHOD if_ex_me_change_outtab_cus~fill_outtab.

    DATA:lt_outtab TYPE TABLE OF merep_outtab_purchdoc.
    FIELD-SYMBOLS:<fs_data> TYPE any.

    IF im_struct_name = 'MEREP_OUTTAB_PURCHDOC'.

      APPEND LINES OF ch_outtab TO lt_outtab.

      SELECT itab~ebeln,
             itab~ebelp,
             eket~eindt AS zzeindt
      FROM @lt_outtab AS itab
      INNER JOIN eket ON eket~ebeln = itab~ebeln
                     AND eket~ebelp = itab~ebelp
      ORDER BY itab~ebeln,itab~ebelp
      INTO TABLE @DATA(lt_tab).

      SELECT zzdata,
             uname
      FROM zgy_t_001
      INTO TABLE @DATA(lt_001).

      LOOP AT lt_outtab ASSIGNING FIELD-SYMBOL(<lfs_outtab>).
        READ TABLE lt_tab INTO DATA(ls_tab) WITH KEY ebeln = <lfs_outtab>-ebeln
                                                     ebelp = <lfs_outtab>-ebelp BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          <lfs_outtab>-zzeindt = ls_tab-zzeindt.
        ENDIF.

        READ TABLE lt_001 INTO DATA(ls_001) WITH KEY uname = 'GAMZEY' BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          ASSIGN COMPONENT 'ZZDATA' OF STRUCTURE <lfs_outtab> TO <fs_data>.
          IF sy-subrc IS INITIAL.
            <fs_data> = ls_001-zzdata.
          ENDIF.
        ENDIF.

      ENDLOOP.

      ch_outtab[] = lt_outtab[].
    ENDIF.
  ENDMETHOD.
ENDCLASS.
