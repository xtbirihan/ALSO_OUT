class ZCL_SHIP_PMAT_ROUGH_ALGORITHM definition
  public
  inheriting from ZCL_SHIP_PMAT_ALGORITHM_BASE
  final
  create public .

public section.

  types:
    BEGIN OF ty_docno,
        docno TYPE /scwm/dlv_docno_str-docno,
      END OF ty_docno .
  types:
    BEGIN OF ty_rough_mat_packed,
        matnr TYPE /scwm/s_material_base_global-matnr,
        quan  TYPE /scwm/s_quan-quan,
        unit  TYPE /scwm/s_quan-unit,
      END OF ty_rough_mat_packed .
  types:
    tt_rough_mat_packed TYPE STANDARD TABLE OF ty_rough_mat_packed WITH DEFAULT KEY .
  types:
    BEGIN OF ty_pmat_cart_nested,
        pmatnr    TYPE /scwm/de_pmat,
        dist_dim  TYPE ty_distance_dim,
        volum     TYPE /scwm/s_quan,
        weight    TYPE /scwm/s_quan,
        materials TYPE tt_rough_mat_packed,
      END OF ty_pmat_cart_nested .
  types:
    tt_pmat_cart_nested TYPE STANDARD TABLE OF ty_pmat_cart_nested WITH DEFAULT KEY .
  types:
    BEGIN OF ty_pmat_outdata,
        pmatid   TYPE /scwm/de_pmatid,
        pmatnr   TYPE /scwm/de_pmat,
        dist_dim TYPE ty_distance_dim,
        volum    TYPE /scwm/s_quan,
        weight   TYPE /scwm/s_quan,
        nested   TYPE tt_pmat_cart_nested,
*        nested   TYPE tt_rough_mat_packed,
      END OF ty_pmat_outdata .
  types:
    tt_pmat_outdata TYPE STANDARD TABLE OF ty_pmat_outdata WITH DEFAULT KEY .
  types:
    BEGIN OF ty_count_with_pmat_data,
        total        TYPE i,
        pmat_details TYPE tt_pmat_outdata,
      END OF ty_count_with_pmat_data .
  types:
*    TYPES:
*      BEGIN OF ty_pallet_output,
*        pmatid     TYPE /scwm/de_pmatid,
*        volume     TYPE /scwm/s_quan,
*        mat_packed TYPE tt_pmat_qty,
*        cartons    TYPE tt_pmat_qty,
*      END OF ty_pallet_output.
*    TYPES tt_pallet_output TYPE STANDARD TABLE OF ty_pallet_output WITH DEFAULT KEY.
    BEGIN OF ty_rough_pack_output,
        docno  TYPE /scwm/dlv_docno_str-docno,
        pallet TYPE ty_count_with_pmat_data,
        carton TYPE ty_count_with_pmat_data,
      END OF ty_rough_pack_output .
  types:
    tt_docno             TYPE STANDARD TABLE OF ty_docno WITH EMPTY KEY .
  types:
    tt_rough_pack_output TYPE STANDARD TABLE OF ty_rough_pack_output WITH DEFAULT KEY .

  methods DETERMINE_PMAT_FOR_DELIVERY
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_DOCCAT type /SCDL/DL_DOCCAT
      !IT_DOCNO type TT_DOCNO
      !IT_PACKMAT type /SCWM/TT_WHO_PMAT optional
    exporting
      !ET_PACK_OUTPUT type TT_ROUGH_PACK_OUTPUT
      !ET_BAPIRET type BAPIRETTAB .
  methods PACK_DELIV_BY_DEFAULT_PMATS
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_DOCCAT type /SCDL/DL_DOCCAT
      !IT_DOCNO type TT_DOCNO
      !IT_PACKMAT type /SCWM/TT_WHO_PMAT optional
    exporting
      !ET_PACK_OUTPUT type TT_ROUGH_PACK_OUTPUT
      !ET_BAPIRET type BAPIRETTAB .
  PROTECTED SECTION.

private section.

  methods CALCULATE_PALLETS
    importing
      !IS_DOC_MAT type TY_DOCNO_MATS_MAP
    exporting
      !ET_PALLETS type TT_PALLET_DATA   "" tt_pmat_qty
      !ES_DOC_MAT type TY_DOCNO_MATS_MAP .
  methods CALCULATE_PALLET_VOLUME
    importing
      !IS_PALLET_DATA type TY_PALLET_DATA
    returning
      value(RS_RESULT) type /SCWM/S_QUAN .
  methods FILL_NESTED_MATS_OUTPUT
    importing
      !IV_PALLET type BOOLE_D optional
      !IS_PALLET_CONTENT type TY_PALLET_DATA optional
      !IS_PACKED_MATERIALS type TY_PMAT_QTY optional
      !IT_MAT_DATA type TT_MAT_DETAILS optional
    returning
      value(RT_RESULT) type TT_PMAT_CART_NESTED .
  methods PACK_CARTONS_IN_PALLET
    importing
      !IS_BP type ZSTR_SHIP_BP
      !IT_CARTONS type TT_PMAT_QTY
    returning
      value(ET_PALLETS) type TT_PALLET_DATA . ""tt_pmat_qty .
ENDCLASS.



CLASS ZCL_SHIP_PMAT_ROUGH_ALGORITHM IMPLEMENTATION.


  METHOD calculate_pallets.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    CLEAR: et_pallets.

    es_doc_mat = is_doc_mat.

    DATA(lo_conv) = NEW /scwm/cl_dlv_md_access( ).

    DATA(lo_prod) = CAST /scwm/if_af_product(
        /scdl/cl_af_management=>get_instance( )->get_service( /scwm/if_af_product=>sc_me_as_service ) ).

    TRY.
        lo_prod->read_material_multiple(
          EXPORTING
            iv_lgnum      = mv_lgnum
            it_matid      = VALUE #( FOR <mat> IN is_doc_mat-mats ( <mat>-matid ) )
            iv_notext     = abap_true
          IMPORTING
            et_mat_global = DATA(lt_mat_global)
            et_mat_uom    = DATA(lt_mat_uom) ).
        ##NO_HANDLER
      CATCH /scwm/cx_md_api_faulty_call.
        ##NO_HANDLER
      CATCH /scwm/cx_md_exception.
    ENDTRY.

    LOOP AT es_doc_mat-mats ASSIGNING FIELD-SYMBOL(<ls_mat>).

      DATA(ls_mat_global) = VALUE #( lt_mat_global[ matid = <ls_mat>-matid ] OPTIONAL ).
      DATA(ls_mat_pal) = VALUE #( lt_mat_uom[ matid = <ls_mat>-matid meinh = zif_wme_c=>gs_uom-pal ] OPTIONAL ).

      CHECK ls_mat_pal IS NOT INITIAL.

      IF ls_mat_global-meins <> <ls_mat>-qty-unit.
        TRY.
            <ls_mat>-qty-quan = lo_conv->/scdl/if_af_quantity_conversio~convert_unit_quantity_single(
                iv_productid = <ls_mat>-matid
                iv_uom_from  = <ls_mat>-qty-unit
                iv_uom_to    = ls_mat_global-meins
                iv_qty_from  = <ls_mat>-qty-quan ).

            <ls_mat>-qty-unit = ls_mat_global-meins.
            ##NO_HANDLER
          CATCH /scdl/cx_af_quantity_conversio.
        ENDTRY.
      ENDIF.

      WHILE <ls_mat>-qty-quan >= ls_mat_pal-umrez.
        <ls_mat>-qty-quan -= ls_mat_pal-umrez.

        et_pallets = VALUE #( BASE et_pallets ( volume = VALUE #( quan = ls_mat_pal-volum
                                                                  unit = ls_mat_pal-voleh )
                                                mat_packed = VALUE #( ( matid = <ls_mat>-matid
                                                                        quan  = ls_mat_pal-umrez
                                                                        unit  = ls_mat_global-meins
                                                                    ) ) ) ).
      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.


  METHOD calculate_pallet_volume.
********************************************************************
*& Key          : <BSUGAREV>-07.07.2023 14:56:51
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  : Each pallet will have only materials packed inside
*&     or only cartons.
*&     1. When there are only materials packed, volume
*&     is calculated from the pack spec, this is already done during
*&     the packing of the materials in the pallet.
*&     2. When cartons are packed in the pallet volume is calculate
*&     from the volume sum of all cartons + pallet volume(volume of the pallet base)
********************************************************************
    DATA(lo_conv_new) = /scmb/cl_md_access_mdl=>get_md_access( ).

    rs_result-unit = zif_wme_c=>gs_uom-cdm.

    IF lines( is_pallet_data-mat_packed ) > 0.
      rs_result = is_pallet_data-volume.
      RETURN.
    ENDIF.

    DATA(lo_prod) = CAST /scwm/if_af_product(
        /scdl/cl_af_management=>get_instance( )->get_service( /scwm/if_af_product=>sc_me_as_service ) ).

    DATA(lt_matid_for_select) = VALUE /scwm/tt_matid( ( is_pallet_data-pmatid ) ).
    TRY.
        lo_prod->read_material_multiple(
          EXPORTING
            iv_lgnum      = mv_lgnum
            it_matid      = lt_matid_for_select
            iv_notext     = abap_true
          IMPORTING
            et_mat_global = DATA(lt_mat_global)
            et_mat_uom    = DATA(lt_mat_uom) ).
        ##NO_HANDLER
      CATCH /scwm/cx_md_api_faulty_call.
        ##NO_HANDLER
      CATCH /scwm/cx_md_exception.
    ENDTRY.

    DATA(lt_pallet_uom) = VALUE /scwm/tt_material_uom(
            FOR <mat_gl>  IN lt_mat_global WHERE ( matid = is_pallet_data-pmatid )
            FOR <mat_uom> IN lt_mat_uom    WHERE ( matid = <mat_gl>-matid AND meinh = <mat_gl>-meins ) ( <mat_uom> ) ).

    DATA(ls_pallet_vol) = VALUE /scwm/s_quan(
        quan = VALUE #( lt_pallet_uom[ 1 ]-volum OPTIONAL )
        unit = VALUE #( lt_pallet_uom[ 1 ]-voleh OPTIONAL ) ).

    IF ls_pallet_vol-unit <> rs_result-unit.
      TRY.
          ls_pallet_vol-quan = lo_conv_new->prod_quan_conversion(
            iv_prodid   = is_pallet_data-pmatid
            iv_uom_from = ls_pallet_vol-unit
            iv_uom_to   = rs_result-unit
            iv_quan     = ls_pallet_vol-quan ).
          ##NO_HANDLER
        CATCH /scmb/cx_md_access.
      ENDTRY.
    ENDIF.

    rs_result-quan += ls_pallet_vol-quan.

    LOOP AT is_pallet_data-cartons ASSIGNING FIELD-SYMBOL(<ls_carts>).
      DATA(lv_cart_vol) = <ls_carts>-volume-quan.

      IF rs_result-unit <> <ls_carts>-volume-unit.
        TRY.
            lv_cart_vol = lo_conv_new->prod_quan_conversion(
              iv_prodid   = <ls_carts>-pmatid
              iv_uom_from = <ls_carts>-volume-unit
              iv_uom_to   = rs_result-unit
              iv_quan     = lv_cart_vol ).
            ##NO_HANDLER
          CATCH /scmb/cx_md_access.
        ENDTRY.
      ENDIF.

      rs_result-quan += lv_cart_vol.

    ENDLOOP.
  ENDMETHOD.


  METHOD determine_pmat_for_delivery.
********************************************************************
*& Key          : <BSUGAREV>-05.05.2023 16:06:40
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description
*&
********************************************************************
    CONSTANTS lv_pmatid_init TYPE /scwm/de_pmatid VALUE ''.
    DATA: lt_pack_out TYPE tt_pack_output. ""tt_rough_pack_output. "".

    BREAK-POINT ID zcg_ship_pmat_rough_algorithm.

    CLEAR: et_pack_output, et_bapiret.

    DATA(lt_dlv_sel) = VALUE /scwm/dlv_selection_tab( FOR <l> IN it_docno
        ( fieldname = /scdl/if_dl_logfname_c=>sc_docno_h
          sign      = wmegc_sign_inclusive
          option    = wmegc_option_eq
          low       = |{ <l>-docno ALPHA = IN }| ) ).

    TRY.
        /scwm/cl_dlv_management_prd=>get_instance( )->query(
           EXPORTING
             it_selection    = lt_dlv_sel
             is_read_options = VALUE #( data_retrival_only = abap_true
                                        mix_in_object_instances = abap_true
                                        item_part_select = abap_true )
             is_include_data = VALUE #( head_partyloc = abap_true
                                        item_partyloc = abap_true )
           IMPORTING
             et_headers      = DATA(lt_headers)
             et_items        = DATA(lt_items) ).
        ##NO_HANDLER
      CATCH /scdl/cx_delivery.
    ENDTRY.

    IF lines( lt_items ) = 0.
      RETURN.
    ENDIF.

    mv_lgnum = iv_lgnum.
    mv_entitled = lt_items[ 1 ]-sapext-entitled.
    mo_packmat = NEW zcl_packmmat_algo( mv_lgnum ).

    " get materials UoM and dispatchable indicator
    DATA(lt_mat_data) = get_mat_details_for_packing( it_matid = VALUE #( FOR <mat> IN lt_items ( <mat>-product-productid ) ) ).

    DATA(lo_conv) = NEW /scwm/cl_dlv_md_access( ).

    LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_dummy>) GROUP BY ( docid = <ls_dummy>-docid )
                     ASSIGNING FIELD-SYMBOL(<ls_doc_gr>).

      DATA(ls_bp) = get_header_bp( lt_headers[ docid = <ls_doc_gr>-docid ] ).

      DATA(ls_doc_mat) = VALUE ty_docno_mats_map( ).
      LOOP AT GROUP <ls_doc_gr> ASSIGNING FIELD-SYMBOL(<ls_item>).
        DATA(ls_mat_data) = REF #( lt_mat_data[ matid = <ls_item>-product-productid ] OPTIONAL ).

        DELETE ls_mat_data->auom_tab WHERE unit = zif_wme_c=>gs_uom-pal.
        SORT ls_mat_data->auom_tab BY quan ASCENDING.
        DELETE ls_mat_data->auom_tab FROM 2.

        " check material is a dispatchable
        CHECK ls_mat_data IS NOT INITIAL.

        ls_doc_mat-docid = <ls_item>-docid.
        ls_doc_mat-docno = <ls_item>-docno.

        IF ls_bp-sold_to IS INITIAL.
          ls_bp-sold_to = VALUE #( <ls_item>-partyloc[ party_role = /scdl/if_dl_partyloc_c=>sc_party_role_sotprt ]-partyno OPTIONAL ).
        ENDIF.

        DATA(ls_mat) = REF #( ls_doc_mat-mats[ matid = ls_mat_data->matid ] OPTIONAL ).

        IF ls_mat IS INITIAL.
          ls_doc_mat-mats = VALUE #( BASE ls_doc_mat-mats ( matid = ls_mat_data->matid
                                                            meins = ls_mat_data->meins
                                                            disp  = ls_mat_data->disp
                                                            auom_tab = ls_mat_data->auom_tab
                                                            qty-quan = <ls_item>-qty-qty
                                                            qty-unit = <ls_item>-qty-uom ) ).
        ELSE.
          DATA(lv_item_qty) = <ls_item>-qty-qty.

          IF ls_mat->qty-unit <> <ls_item>-qty-uom.
            TRY.
                lv_item_qty = lo_conv->/scdl/if_af_quantity_conversio~convert_unit_quantity_single(
                    iv_productid = ls_mat->matid
                    iv_uom_from  = <ls_item>-qty-uom
                    iv_uom_to    = ls_mat->qty-unit
                    iv_qty_from  = <ls_item>-qty-qty ).
                ##NO_HANDLER
              CATCH /scdl/cx_af_quantity_conversio.
            ENDTRY.
          ENDIF.

          ls_mat->qty-quan += lv_item_qty.
        ENDIF.
      ENDLOOP.

      "===========================================================
      "================ Pallets result
      "===========================================================
      DATA(ls_pack_out) = VALUE ty_pack_output( docid = ls_doc_mat-docid    ""
                                                docno = ls_doc_mat-docno ).

      " calculate pallets
      calculate_pallets(
        EXPORTING
          is_doc_mat = ls_doc_mat
        IMPORTING
          et_pallets = DATA(lt_pallets_with_materials)
          es_doc_mat = DATA(ls_leftover_for_cartons) ).

      " calculate master and ship cartons considering item qty and UoM MC
      DATA(ls_cartons_for_pallet) = determine_ship_pmats( is_bp      = ls_bp
                                                   is_doc_mat = ls_leftover_for_cartons
                                                   it_packmat = it_packmat ).

      DATA(lt_cartons) = VALUE tt_pmat_qty( ).

      " update the master carton flag, it is needed to calculate packing parameters
      LOOP AT ls_cartons_for_pallet-carton ASSIGNING FIELD-SYMBOL(<ls_palcart>).
        <ls_palcart>-mc_flag = abap_true.
        APPEND <ls_palcart> TO lt_cartons.
      ENDLOOP.

      APPEND LINES OF ls_cartons_for_pallet-shipping TO lt_cartons.

      DATA(lt_pallets_with_cartons) = pack_cartons_in_pallet( is_bp      = ls_bp
                                                              it_cartons = lt_cartons ).

      APPEND LINES OF lt_pallets_with_materials TO ls_pack_out-pallet.
      APPEND LINES OF lt_pallets_with_cartons   TO ls_pack_out-pallet.

      "===========================================================
      "================ Cartons result
      "===========================================================
      " calculate master and ship cartons variant for the hole document
      DATA(ls_cartons_pack_output) = determine_ship_pmats( is_bp      = ls_bp
                                                           is_doc_mat = ls_doc_mat
                                                           it_packmat = it_packmat ).

      " update the master carton flag, it is needed to calculate packing parameters
      LOOP AT ls_cartons_pack_output-carton ASSIGNING FIELD-SYMBOL(<ls_cart>).
        <ls_cart>-mc_flag = abap_true.
        APPEND <ls_cart> TO ls_pack_out-carton.
      ENDLOOP.

      APPEND LINES OF ls_cartons_pack_output-shipping TO ls_pack_out-carton.

      lt_pack_out = VALUE #( BASE lt_pack_out ( ls_pack_out ) ).
    ENDLOOP.

    et_bapiret = mt_bapiret.

    " calculate total number of pack materials
    LOOP AT lt_pack_out ASSIGNING FIELD-SYMBOL(<ls_out>).
      DATA(lv_carton) = lines( <ls_out>-carton ).

      DATA(lt_pallets_pack_details) = VALUE tt_pmat_outdata( FOR <pal> IN <ls_out>-pallet
          ( pmatid = <pal>-pmatid
            pmatnr = get_packmat_nr( <pal>-pmatid )
            dist_dim = get_pmat_distance_dimensions(
                          iv_matid = SWITCH #( <pal>-pmatid WHEN lv_pmatid_init THEN VALUE #( <pal>-mat_packed[ 1 ]-matid OPTIONAL )
                                                            ELSE <pal>-pmatid )
                          iv_is_pmat_mc = SWITCH #( <pal>-pmatid WHEN lv_pmatid_init THEN abap_true
                                                                 ELSE abap_false )
                          iv_from_auom = zif_wme_c=>gs_uom-pal )
            " pallet volume must be calculated from packed materials or shipping/master cartons
            volum    = calculate_pallet_volume( is_pallet_data = <pal> )
            weight   = calculate_mats_weight( iv_complete_pallet = abap_true
                                              it_materials = <pal>-mat_packed
                                              it_cartons   = <pal>-cartons )
            nested   = fill_nested_mats_output( iv_pallet = abap_true
                                                is_pallet_content = <pal>
                                                it_mat_data = lt_mat_data )
            ) ).


      DATA(lt_cartons_pack_details) = VALUE tt_pmat_outdata( FOR <cart> IN <ls_out>-carton
          ( pmatid = <cart>-pmatid
            pmatnr = get_packmat_nr( <cart>-pmatid )
            " carton: distance dimensions must be from AUoM of packed material
            dist_dim = get_pmat_distance_dimensions(
                          iv_matid = SWITCH #( <cart>-mc_flag WHEN space THEN <cart>-pmatid
                                                              ELSE VALUE #( <cart>-mat_packed[ 1 ]-matid OPTIONAL ) )
                          iv_is_pmat_mc = <cart>-mc_flag
                          iv_from_auom = VALUE #( lt_mat_data[ matid = <cart>-mat_packed[ 1 ]-matid ]-auom_tab[ 1 ]-unit OPTIONAL ) )
            " carton: volume must be from AUoM of packed material
            volum    = <cart>-volume
            weight   = calculate_mats_weight( it_cartons = VALUE #( ( <cart> ) ) )
            nested   = fill_nested_mats_output( is_packed_materials = <cart> )
            ) ).

      et_pack_output = VALUE #( BASE et_pack_output (
          docno = <ls_out>-docno
          pallet-total = lines( <ls_out>-pallet )
          pallet-pmat_details = lt_pallets_pack_details
          carton-total = lv_carton
          carton-pmat_details = lt_cartons_pack_details ) ).

    ENDLOOP.
  ENDMETHOD.


  METHOD fill_nested_mats_output.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA(lo_prod) = CAST /scwm/if_af_product( /scdl/cl_af_management=>get_instance( )->get_service( /scwm/if_af_product=>sc_me_as_service ) ).


    DATA(lt_matids) = VALUE /scwm/tt_matid( ( is_packed_materials-pmatid ) ).
    lt_matids = VALUE #( BASE lt_matids FOR <l> IN is_packed_materials-mat_packed ( <l>-matid ) ).

    lt_matids = VALUE #( BASE lt_matids ( is_pallet_content-pmatid ) ).
    lt_matids = VALUE #( BASE lt_matids FOR <l> IN is_pallet_content-mat_packed ( <l>-matid ) ).
    lt_matids = VALUE #( BASE lt_matids FOR <lc> IN is_pallet_content-cartons ( <lc>-pmatid ) ).
    lt_matids = VALUE #( BASE lt_matids FOR <lc> IN is_pallet_content-cartons
                                        FOR <cc> IN <lc>-mat_packed ( <cc>-matid ) ).


    TRY.
        lo_prod->convert_matid_to_matnr_multi(
          EXPORTING
            it_matid     = lt_matids
          IMPORTING
            et_matnr     = DATA(lt_matnr) ).
        ##NO_HANDLER
      CATCH /scwm/cx_md_api_faulty_call.
        ##NO_HANDLER
      CATCH /scwm/cx_md_exception.
    ENDTRY.

    IF iv_pallet = abap_true.

      IF lines( is_pallet_content-mat_packed ) > 0.
        APPEND INITIAL LINE TO rt_result ASSIGNING FIELD-SYMBOL(<ls_direct_mats>).
        <ls_direct_mats>-materials = VALUE #( FOR <l> IN is_pallet_content-mat_packed
                                 ( matnr = VALUE #( lt_matnr[ matid = <l>-matid ] OPTIONAL )
                                   quan  = <l>-quan
                                   unit  = <l>-unit ) ).
      ENDIF.

      IF lines( is_pallet_content-cartons ) > 0.
        rt_result = VALUE #( BASE rt_result FOR <cart> IN is_pallet_content-cartons
            ( pmatnr   = VALUE #( lt_matnr[ matid = <cart>-pmatid ] OPTIONAL )
              dist_dim = get_pmat_distance_dimensions(
                            iv_matid = SWITCH #( <cart>-mc_flag WHEN space THEN <cart>-pmatid
                                                                ELSE VALUE #( <cart>-mat_packed[ 1 ]-matid OPTIONAL ) )
                            iv_is_pmat_mc = <cart>-mc_flag
                            iv_from_auom = VALUE #( it_mat_data[ matid = <cart>-mat_packed[ 1 ]-matid ]-auom_tab[ 1 ]-unit OPTIONAL ) )
              volum    = <cart>-volume
              weight   = calculate_mats_weight( it_cartons = VALUE #( ( <cart> ) ) )
              materials = VALUE #( FOR <l> IN <cart>-mat_packed
                                 ( matnr = VALUE #( lt_matnr[ matid = <l>-matid ] OPTIONAL )
                                   quan  = <l>-quan
                                   unit  = <l>-unit ) ) ) ).
      ENDIF.

    ELSE.
      rt_result = VALUE #( " FOR <l> IN is_packed_materials-mat_packed
          ( materials = VALUE #( FOR <l> IN is_packed_materials-mat_packed
                               ( matnr = VALUE #( lt_matnr[ matid = <l>-matid ] OPTIONAL )
                                 quan  = <l>-quan
                                 unit  = <l>-unit ) )
            ) ).
    ENDIF.
  ENDMETHOD.


  METHOD pack_cartons_in_pallet.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    CLEAR: et_pallets.

    " first need to determine list of pack materials from packaging specification
    DATA(lt_pmats) = mo_packmat->get_pmat_pallet_sped( ).
    DATA(lt_filtered_pmats) = mo_packmat->filter_pmats_by_bp( iv_sped  = abap_true
                                                              is_bp    = is_bp
                                                              it_pmats = lt_pmats ).

    DATA(lt_packmats) = VALUE /scwm/tt_who_pmat( FOR <inp> IN lt_filtered_pmats ( pmat_guid = <inp>-pmatid ) ).

    mo_packmat->update_pmat_dimensions( CHANGING ct_packmat = lt_packmats ).

    DATA(lt_cartons_for_pallet_pack) = VALUE /scwm/tt_ordim_o_int( FOR <l> IN it_cartons
                                          ( huid = <l>-pmatid
                                            volum = <l>-volume-quan
                                            unit_v = <l>-volume-unit ) ).

    DATA(lt_pallet_packed) = NEW zcl_out_who_pack_wt( mv_lgnum )->pack_mc_in_pallet(
        is_hucrea_param = VALUE #( lgnum      = mv_lgnum
                                   no_huident = abap_true )
        it_pmats        = lt_packmats
        it_cartons      = lt_cartons_for_pallet_pack ).

    DATA(lt_cartons_input) = it_cartons.
    /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = DATA(lo_pack) ).

    LOOP AT lt_pallet_packed ASSIGNING FIELD-SYMBOL(<ls_dummy>) GROUP BY ( shiphuid = <ls_dummy>-shiphuid )
                             ASSIGNING FIELD-SYMBOL(<ls_cartons_by_pallet>).

      lo_pack->get_hu(
        EXPORTING
          iv_guid_hu = <ls_cartons_by_pallet>-shiphuid
        IMPORTING
          es_huhdr   = DATA(ls_huhdr_buffer)
        EXCEPTIONS
          not_found  = 1
          OTHERS     = 2 ).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO et_pallets ASSIGNING FIELD-SYMBOL(<ls_pallet_result>).
      <ls_pallet_result>-pmatid = ls_huhdr_buffer-pmat_guid.

      LOOP AT GROUP <ls_cartons_by_pallet> ASSIGNING FIELD-SYMBOL(<ls_carton>).

        DATA(ls_carton_single) = VALUE #( lt_cartons_input[ pmatid = <ls_carton>-huid ] OPTIONAL ).
        " remove read line not to be duplicated for the next iteration. Master cartons are all the same but
        " ship cartons can have the same pack material and different content
        DELETE TABLE lt_cartons_input FROM ls_carton_single.

        <ls_pallet_result>-volume-quan += ls_carton_single-volume-quan.
        <ls_pallet_result>-volume-unit = ls_carton_single-volume-unit.

        APPEND INITIAL LINE TO <ls_pallet_result>-cartons ASSIGNING FIELD-SYMBOL(<ls_carton_in_pal>).
        <ls_carton_in_pal>-pmatid = ls_carton_single-pmatid.
        <ls_carton_in_pal>-volume = ls_carton_single-volume.
        <ls_carton_in_pal>-mc_flag = VALUE #( it_cartons[ pmatid = ls_carton_single-pmatid ]-mc_flag OPTIONAL ).

        APPEND LINES OF ls_carton_single-mat_packed TO <ls_carton_in_pal>-mat_packed.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD pack_deliv_by_default_pmats.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA(lt_pmat_ids) = NEW zcl_packmmat_algo( iv_lgnum )->get_pmat_planned_shipping( ).

    DATA(lt_packmat) = it_packmat.

    " L/W/H, Volume are determine in DETERMINE_PMAT_FOR_TASKS
    lt_packmat = VALUE #( BASE lt_packmat FOR <l> IN lt_pmat_ids ( pmat_guid = <l>-matid ) ).

    determine_pmat_for_delivery(
      EXPORTING
        iv_lgnum       = iv_lgnum
        iv_doccat      = iv_doccat
        it_docno       = it_docno
        it_packmat     = lt_packmat
      IMPORTING
        et_pack_output = et_pack_output
        et_bapiret     = et_bapiret ).
  ENDMETHOD.
ENDCLASS.
