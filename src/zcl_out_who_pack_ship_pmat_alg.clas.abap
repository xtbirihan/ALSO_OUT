CLASS zcl_out_who_pack_ship_pmat_alg DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES /scwm/if_ex_who_packing .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA mo_who_pack TYPE REF TO zcl_out_who_pack_wt .

    CLASS-METHODS pack_in_ship_pmat
      IMPORTING
        !is_wcr    TYPE /scwm/twcr
        !it_to     TYPE /scwm/tt_ordim_o_int
        !it_pmat   TYPE /scwm/tt_who_pmat
      CHANGING
        !ct_to     TYPE /scwm/tt_ordim_o_int
        !ct_packed TYPE /scwm/tt_ordim_o_int
        !cv_pmatid TYPE /scwm/de_pmatid
        !cv_split  TYPE syst_tabix .
    CLASS-METHODS pack_in_totes
      IMPORTING
        !it_to     TYPE /scwm/tt_ordim_o_int
        !it_pmat   TYPE /scwm/tt_who_pmat
      CHANGING
        !ct_to     TYPE /scwm/tt_ordim_o_int
        !ct_packed TYPE /scwm/tt_ordim_o_int
        !cv_pmatid TYPE /scwm/de_pmatid
        !cv_split  TYPE syst_tabix .
ENDCLASS.



CLASS ZCL_OUT_WHO_PACK_SHIP_PMAT_ALG IMPLEMENTATION.


  METHOD /scwm/if_ex_who_packing~packing.
********************************************************************
*& Key          : <BSUGAREV>-15.06.2023 16:02:22
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
*&                GAP-54 – Process-Oriented Storage Control - custom settings
********************************************************************
*& Description
*&
********************************************************************
    DATA: lv_pmatid TYPE /scwm/de_pmatid.

    " field level switch on/off
    DATA(lt_switch_fields) = VALUE ztt_switch_fields( ( field       = zif_switch_const=>c_packprofile
                                                        field_value = is_tpack-packprofile ) ).

    IF zcl_switch=>get_switch_state( iv_lgnum  = is_tpack-lgnum
                                     iv_devid  = zif_switch_const=>c_zout_005
                                     it_fields = lt_switch_fields ) EQ abap_false.
      RETURN.
    ENDIF.

    mo_who_pack = NEW #( is_tpack-lgnum ).

    " update switch fields: Use warehouse order creation rule
    lt_switch_fields = VALUE #( ( field       = zif_switch_const=>c_wocr
                                  field_value = is_wcr-wcr ) ).

    IF zcl_switch=>get_switch_state( iv_lgnum  = is_tpack-lgnum
                                     iv_devid  = zif_switch_const=>c_zout_015
                                     it_fields = lt_switch_fields ) EQ abap_true.
      pack_in_totes(
        EXPORTING
          it_to     = it_to
          it_pmat   = it_pmat
        CHANGING
          ct_to     = ct_to
          ct_packed = ct_packed
          cv_pmatid = lv_pmatid
          cv_split  = ev_split ).
    ELSE.

      pack_in_ship_pmat(
        EXPORTING
          is_wcr    = is_wcr
          it_to     = it_to
          it_pmat   = it_pmat
        CHANGING
          ct_to     = ct_to
          ct_packed = ct_packed
          cv_pmatid = lv_pmatid
          cv_split  = ev_split ).
    ENDIF.

    IF lines( ct_packed ) = 0.
      RETURN.
    ENDIF.

    " now create the pick HU, update tasks and WHOHU
    DATA(ls_huhdr_out) = mo_who_pack->create_ship_hu(
        is_param = VALUE #( lgnum      = is_tpack-lgnum
                            no_huident = abap_true )
        is_huhdr_crea = VALUE #( pmat_guid  = lv_pmatid
                                 max_weight = REDUCE #( INIT x = 0 FOR <l> IN ct_packed NEXT x += <l>-weight )
                                 unit_gw    = ct_packed[ 1 ]-unit_w
                                 max_volume = REDUCE #( INIT x = 0 FOR <l> IN ct_packed NEXT x += <l>-volum )
                                 unit_gv    = ct_packed[ 1 ]-unit_v ) ).

    LOOP AT ct_packed ASSIGNING FIELD-SYMBOL(<ls_packed>).
      <ls_packed>-huid = ls_huhdr_out-guid_hu.
    ENDLOOP.

    DATA(ls_whohu) = VALUE /scwm/s_whohu( lgnum     = is_wcr-lgnum
                                          hukng     = 1
                                          pmat_guid = ls_huhdr_out-pmat_guid
                                          huid      = ls_huhdr_out-guid_hu
                                          prces     = is_wcr-prces
                                          wcr       = is_wcr-wcr
                                          updkz     = zif_wme_c=>gs_whohu_upd-create ).

    APPEND ls_whohu TO ct_whohu.
  ENDMETHOD.


  METHOD pack_in_ship_pmat.
********************************************************************
*& Key          : BSUGAREV-Jan 5, 2024
*& Request No.  : GAP-54 – Process-Oriented Storage Control - custom settings
********************************************************************
*& Description  :
*&
*&
********************************************************************
    " find pack materials for tasks
    NEW zcl_ship_pmat_real_algorithm( )->determine_pmat_for_tasks(
      EXPORTING
        it_to        = it_to
        it_packmat   = it_pmat
      IMPORTING
        es_pack_output = DATA(ls_pack_output) ).

    " filter pack result by WOCR
    ls_pack_output = mo_who_pack->filter_pack_result_by_wocr(
        iv_wocr     = is_wcr-wcr
        is_pack     = ls_pack_output ).

    mo_who_pack->pack_tasks_in_carton(
      EXPORTING
        it_tasks       = it_to
        is_carton      = VALUE #( ls_pack_output-carton[ 1 ] OPTIONAL )
      IMPORTING
        et_packed      = ct_packed
        et_to_next_who = ct_to
        ev_pmatid      = cv_pmatid
        ev_split       = cv_split ).

    IF lines( ct_packed ) = 0.
      mo_who_pack->pack_tasks_in_shippmat(
        EXPORTING
          it_tasks       = it_to
          it_shipping    = ls_pack_output-shipping
        IMPORTING
          et_packed      = ct_packed
          et_to_next_who = ct_to
          ev_pmatid      = cv_pmatid
          ev_split       = cv_split  ).
    ENDIF.

  ENDMETHOD.


  METHOD pack_in_totes.
********************************************************************
*& Key          : BSUGAREV-Jan 5, 2024
*& Request No.  : GAP-54 – Process-Oriented Storage Control - custom settings
********************************************************************
*& Description  :
*&
*&
********************************************************************
    " find pack materials for tasks
    NEW zcl_ship_pmat_real_algorithm( )->determine_pmat_for_tasks(
      EXPORTING
        it_to        = it_to
        it_packmat   = it_pmat
        iv_full_pack = abap_false
        iv_use_only_supplied_pmats = abap_true
      IMPORTING
        es_pack_output = DATA(ls_pack_output) ).

    mo_who_pack->pack_tasks_in_shippmat(
      EXPORTING
        it_tasks       = it_to
        it_shipping    = ls_pack_output-shipping
      IMPORTING
        et_packed      = ct_packed
        et_to_next_who = ct_to
        ev_pmatid      = cv_pmatid
        ev_split       = cv_split  ).

  ENDMETHOD.
ENDCLASS.
