CLASS zcl_ship_pmat_algorithm_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_distance_dim,
        length   TYPE /scwm/length,
        width    TYPE /scwm/width,
        height   TYPE /scwm/height,
        unit_lwh TYPE /scwm/dimeh,
      END OF ty_distance_dim.

    TYPES:
      BEGIN OF ty_mat_details,
        matid      TYPE /scwm/s_material_lgnum-matid,
        meins      TYPE /scwm/s_material_lgnum-meins,
        disp       TYPE /scwm/s_material_lgnum-zz1_disp_whd,
        no_mc_pack TYPE boole_d,
        qty        TYPE /scwm/s_quan,
        auom_tab   TYPE /scwm/tt_quan,
      END OF ty_mat_details .
    TYPES:
      BEGIN OF ty_docno_mats_map,
        docid TYPE /scwm/dlv_docid_item_str-docid,
        docno TYPE /scwm/dlv_docno_str-docno,
        mats  TYPE STANDARD TABLE OF ty_mat_details WITH KEY matid,
      END OF ty_docno_mats_map .
    TYPES:
      BEGIN OF ty_bp,
        carrier TYPE ztout_ship_pmat-carrier,
        ship_to TYPE ztout_ship_pmat-ship_to,
        sold_to TYPE ztout_ship_pmat-sold_to,
      END OF ty_bp .
    TYPES:
      BEGIN OF ty_mat_packed,
        matid TYPE /scwm/s_material_lgnum-matid,
        quan  TYPE /scwm/s_quan-quan,
        unit  TYPE /scwm/s_quan-unit,
        auom  TYPE /scwm/s_quan-unit,
      END OF ty_mat_packed,
      tt_mat_packed TYPE STANDARD TABLE OF ty_mat_packed WITH EMPTY KEY.

*    TYPES:
*      BEGIN OF ty_ship_mat_packed,
*        matid  TYPE /scwm/s_material_lgnum-matid,
*        quan   TYPE /scwm/s_quan-quan,
*        unit   TYPE /scwm/s_quan-unit,
*        volum  TYPE volum,
*        unit_v TYPE /scwm/de_vol_uom,
*      END OF ty_ship_mat_packed.

    TYPES:
      BEGIN OF ty_pmat_qty,
        pmatid     TYPE /scwm/de_pmatid,
        mc_flag    TYPE boole_d,
        volume     TYPE /scwm/s_quan,
        mat_packed TYPE tt_mat_packed,
      END OF ty_pmat_qty .
    TYPES tt_pmat_qty TYPE STANDARD TABLE OF ty_pmat_qty WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_pallet_data,
        pmatid     TYPE /scwm/de_pmatid,
        volume     TYPE /scwm/s_quan,
        mat_packed TYPE tt_mat_packed,
        cartons    TYPE tt_pmat_qty,
      END OF ty_pallet_data.
    TYPES tt_pallet_data TYPE STANDARD TABLE OF ty_pallet_data WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_pack_output,
        docid    TYPE /scwm/dlv_docid_item_str-docid,
        docno    TYPE /scwm/dlv_docno_str-docno,
        pallet   TYPE tt_pallet_data, ""tt_pmat_qty,
        carton   TYPE tt_pmat_qty,
        shipping TYPE tt_pmat_qty,
      END OF ty_pack_output .
    TYPES:
      tt_mat_details    TYPE STANDARD TABLE OF ty_mat_details WITH KEY matid .
    TYPES:
      tt_docno_mats_map TYPE STANDARD TABLE OF ty_docno_mats_map WITH KEY docid .
    TYPES:
      tt_pack_output    TYPE STANDARD TABLE OF ty_pack_output WITH EMPTY KEY .
protected section.

  data MO_PACKMAT type ref to ZCL_PACKMMAT_ALGO .
  data MO_PACK_WHO type ref to ZCL_OUT_WHO_PACK_WT .
  data MT_BAPIRET type BAPIRETTAB .
  data MV_ENTITLED type /SCWM/DE_ENTITLED .
  data MV_FULL_PACK type BOOLE_D value ABAP_TRUE ##NO_TEXT.
  data MV_LGNUM type /SCWM/LGNUM .
  data MV_MSG type STRING .
  data MV_USE_ONLY_SUPPLIED_PMATS type BOOLE_D .

  methods ADD_MESSAGE .
  methods CALCULATE_MATS_WEIGHT
    importing
      !IV_PALLET_PMAT type /SCWM/DE_PMATID optional
      !IV_COMPLETE_PALLET type BOOLE_D optional
      !IT_MATERIALS type TT_MAT_PACKED optional
      !IT_CARTONS type TT_PMAT_QTY optional
      !IT_NESTED_MATS type TT_MAT_PACKED optional
    returning
      value(RS_RESULT) type /SCWM/S_QUAN .
  methods CALC_PMAT_VOLUM_BY_INNER_PMATS
    importing
      !IT_SUB_PMATS type TT_PMAT_QTY
    returning
      value(RS_RESULT) type /SCWM/S_QUAN .
  methods DETERMINE_SHIP_PMATS
    importing
      !IS_BP type ZSTR_SHIP_BP
      !IS_DOC_MAT type TY_DOCNO_MATS_MAP
      !IT_PACKMAT type /SCWM/TT_WHO_PMAT
    returning
      value(RS_PACK_OUTPUT) type TY_PACK_OUTPUT .
  methods GET_HEADER_BP
    importing
      !IS_HEADER type /SCWM/DLV_HEADER_OUT_PRD_STR
    returning
      value(RS_BP) type ZSTR_SHIP_BP .
  methods GET_MAT_DETAILS_FOR_PACKING
    importing
      !IT_MATID type /SCWM/TT_MATID
    returning
      value(RT_RESULT) type TT_MAT_DETAILS .
  methods GET_PACKMAT_NR
    importing
      !IV_PMATID type /SCWM/DE_PMATID
    returning
      value(RV_RESULT) type /SCWM/DE_PMAT .
  methods GET_PACKMAT_VOLUME
    importing
      !IV_MATID type /SCWM/DE_PMATID
      !IV_FROM_AUOM type /SCWM/DE_UNIT optional
    returning
      value(RS_RESULT) type /SCWM/S_QUAN .
  methods GET_PMAT_DISTANCE_DIMENSIONS
    importing
      !IV_MATID type /SCWM/DE_PMATID
      !IV_FROM_AUOM type /SCWM/DE_UNIT optional
      !IV_IS_PMAT_MC type BOOLE_D optional
    returning
      value(RS_RESULT) type TY_DISTANCE_DIM .
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_SHIP_PMAT_ALGORITHM_BASE IMPLEMENTATION.


  METHOD add_message.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    mt_bapiret = VALUE #( BASE mt_bapiret ( id         = sy-msgid
                                            number     = sy-msgno
                                            type       = sy-msgty
                                            message_v1 = sy-msgv1
                                            message_v2 = sy-msgv2
                                            message_v3 = sy-msgv3
                                            message_v4 = sy-msgv4 ) ).
  ENDMETHOD.


  METHOD calculate_mats_weight.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    IF lines( it_materials ) = 0 AND lines( it_cartons ) = 0.
      RETURN.
    ENDIF.

    DATA(lt_materials) = it_materials.

    lt_materials = VALUE #( BASE lt_materials FOR <cart> IN it_cartons
                                              FOR <mat> IN <cart>-mat_packed ( <mat> ) ).

    DATA(lo_conv_new) = /scmb/cl_md_access_mdl=>get_md_access( ).

    DATA(lo_prod) = CAST /scwm/if_af_product(
        /scdl/cl_af_management=>get_instance( )->get_service( /scwm/if_af_product=>sc_me_as_service ) ).

    DATA(lt_matid_for_select) = VALUE /scwm/tt_matid( FOR <l> IN lt_materials ( <l>-matid ) ).
    lt_matid_for_select = VALUE #( BASE lt_matid_for_select ( iv_pallet_pmat ) ).
    lt_matid_for_select = VALUE #( BASE lt_matid_for_select FOR <car_pmid> IN it_cartons ( <car_pmid>-pmatid ) ).
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

    rs_result-unit = zif_wme_c=>gs_uom-kg.

    " special case, when materials are provided but iv_pallet_pmat is initial, this means that we should get
    "  weight of the pallet from the AUOM /PAL/ from the packed material
    IF lines( it_materials ) > 0 AND iv_complete_pallet = abap_true.
      " it should be only one material in a pallet
      DATA(ls_uom_pal) = VALUE #( lt_mat_uom[ matid = it_materials[ 1 ]-matid meinh = zif_wme_c=>gs_uom-pal ] OPTIONAL ).

      IF ls_uom_pal IS NOT INITIAL AND ls_uom_pal-gewei <> rs_result-unit.
        TRY.
            DATA(lv_weight_pal) = lo_conv_new->prod_quan_conversion(
              iv_prodid   = iv_pallet_pmat
              iv_uom_from = ls_uom_pal-gewei
              iv_uom_to   = rs_result-unit
              iv_quan     = CONV #( ls_uom_pal-brgew ) ).

            ls_uom_pal-brgew = lv_weight_pal.
            ##NO_HANDLER
          CATCH /scmb/cx_md_access.
        ENDTRY.
      ENDIF.

      rs_result-quan += ls_uom_pal-brgew.

      RETURN.
    ENDIF.

    " start with the net weight of the pallet itself
    IF iv_pallet_pmat IS NOT INITIAL.
      DATA(lt_pallet_uom) = VALUE /scwm/tt_material_uom(
              FOR <mat_gl>  IN lt_mat_global WHERE ( matid = iv_pallet_pmat )
              FOR <mat_uom> IN lt_mat_uom    WHERE ( matid = <mat_gl>-matid AND meinh = <mat_gl>-meins ) ( <mat_uom> ) ).

      rs_result = VALUE #( quan = VALUE #( lt_pallet_uom[ 1 ]-brgew OPTIONAL )
                           unit = VALUE #( lt_pallet_uom[ 1 ]-gewei OPTIONAL ) ).
      IF rs_result-unit <> zif_wme_c=>gs_uom-kg.
        TRY.
            rs_result-quan = lo_conv_new->prod_quan_conversion(
              iv_prodid   = iv_pallet_pmat
              iv_uom_from = rs_result-unit
              iv_uom_to   = zif_wme_c=>gs_uom-kg
              iv_quan     = rs_result-quan ).
            ##NO_HANDLER
          CATCH /scmb/cx_md_access.
        ENDTRY.
      ENDIF.
    ENDIF.

    " add net weight of the master carton
    IF lines( it_cartons ) > 0.
      DATA(lt_carton_uom) = VALUE /scwm/tt_material_uom(
              FOR <ipc>     IN it_cartons
              FOR <mat_gl>  IN lt_mat_global WHERE ( matid = <ipc>-pmatid )
              FOR <mat_uom> IN lt_mat_uom    WHERE ( matid = <mat_gl>-matid AND meinh = <mat_gl>-meins ) ( <mat_uom> ) ).

      LOOP AT lt_carton_uom ASSIGNING FIELD-SYMBOL(<ls_cart_uom>).

        DATA(lv_carton_weight) = <ls_cart_uom>-brgew.

        IF <ls_cart_uom>-gewei <> rs_result-unit.
          TRY.
              lv_carton_weight = lo_conv_new->prod_quan_conversion(
                  iv_prodid   = <ls_cart_uom>-matid
                  iv_uom_from = <ls_cart_uom>-gewei
                  iv_uom_to   = rs_result-unit
                  iv_quan     = CONV #( lv_carton_weight ) ).
              ##NO_HANDLER
            CATCH /scmb/cx_md_access.
          ENDTRY.
        ENDIF.

        rs_result-quan += lv_carton_weight.
      ENDLOOP.
    ENDIF.

    " add weight of the packed materials
    LOOP AT lt_materials ASSIGNING FIELD-SYMBOL(<ls_mat>).

      DATA(ls_global) = REF #( lt_mat_global[ matid = <ls_mat>-matid ] OPTIONAL ).
      CHECK ls_global IS NOT INITIAL.

      DATA(ls_uom) = REF #( lt_mat_uom[ matid = <ls_mat>-matid meinh = ls_global->meins ] OPTIONAL ).
      CHECK ls_uom IS NOT INITIAL.

      DATA(lv_mat_qty) = <ls_mat>-quan.

      IF <ls_mat>-unit <> ls_global->meins.
        TRY.
            lv_mat_qty = lo_conv_new->prod_quan_conversion(
              iv_prodid   = <ls_mat>-matid
              iv_uom_from = <ls_mat>-unit
              iv_uom_to   = ls_global->meins
              iv_quan     = <ls_mat>-quan ).
            ##NO_HANDLER
          CATCH /scmb/cx_md_access.
        ENDTRY.
      ENDIF.

      DATA(lv_material_weight) = ls_uom->brgew.
      IF ls_uom->gewei <> rs_result-unit.
        TRY.
            lv_material_weight = lo_conv_new->prod_quan_conversion(
                iv_prodid   = <ls_mat>-matid
                iv_uom_from = ls_uom->gewei
                iv_uom_to   = rs_result-unit
                iv_quan     = CONV #( lv_material_weight ) ).
            ##NO_HANDLER
          CATCH /scmb/cx_md_access.
        ENDTRY.
      ENDIF.

      rs_result-quan += ( lv_mat_qty * lv_material_weight ).

    ENDLOOP.

  ENDMETHOD.


  METHOD calc_pmat_volum_by_inner_pmats.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    IF lines( it_sub_pmats ) = 0.
      RETURN.
    ENDIF.

    rs_result-quan = REDUCE #( INIT x = 0 FOR <l> IN it_sub_pmats NEXT x += <l>-volume-quan ).
    rs_result-unit = it_sub_pmats[ 1 ]-volume-unit.
  ENDMETHOD.


  METHOD determine_ship_pmats.
********************************************************************
*& Key          : <BSUGAREV>-15.06.2023 16:02:22
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description
*&
********************************************************************
    DATA: lt_mats_cuboid       TYPE STANDARD TABLE OF ty_mat_details WITH KEY matid,
          lt_cartons           TYPE STANDARD TABLE OF ty_pmat_qty WITH DEFAULT KEY.

    NEW /scwm/cl_ui_stock_fields( )->prefetch_matkey_by_id(
      EXPORTING
        it_matid = VALUE #( FOR <mi> IN is_doc_mat-mats ( matid = <mi>-matid ) )
      IMPORTING
        et_matid_extkey = DATA(lt_matid_matnr) ).

    " get instance of producet class
    DATA(lo_prod) = CAST /scwm/if_af_product( /scdl/cl_af_management=>get_instance(
        )->get_service( /scwm/if_af_product=>sc_me_as_service ) ).

    TRY.
        lo_prod->read_material_multiple(
          EXPORTING
            iv_lgnum      = mv_lgnum
            iv_entitled   = mv_entitled
            it_matid      = VALUE #( FOR <m> IN is_doc_mat-mats ( <m>-matid ) )
            iv_notext     = abap_true
          IMPORTING
            et_mat_lgnum  = DATA(lt_mat_lgnum) ).
        ##NO_HANDLER
      CATCH /scwm/cx_md_api_faulty_call.
        ##NO_HANDLER
      CATCH /scwm/cx_md_exception.
    ENDTRY.

    DATA(lt_pmat_carton) = mo_packmat->get_pmat_carton( ).

    LOOP AT is_doc_mat-mats ASSIGNING FIELD-SYMBOL(<ls_mat>).

      DATA(ls_matnr) = VALUE #( lt_matid_matnr[ matid = <ls_mat>-matid ] OPTIONAL ).

      " Total packing quantity for material &1: &2[&3].
      MESSAGE i007(zmc_out) WITH ls_matnr-matnr <ls_mat>-qty-quan <ls_mat>-qty-unit INTO mv_msg.
      add_message( ).

      " find master carton with lowest value
      DATA(ls_master_carton_capacity) = VALUE #( <ls_mat>-auom_tab[ 1 ] OPTIONAL ).

      " current material doesn't have master carton UoM. Pack material will be determined by Cuboid alg.
      IF ls_master_carton_capacity   IS INITIAL OR
         mv_use_only_supplied_pmats = abap_true OR
         <ls_mat>-no_mc_pack = abap_true        OR
         ( <ls_mat>-disp = zif_wme_c=>gs_matdisp-can_dispatch OR
           <ls_mat>-disp = zif_wme_c=>gs_matdisp-non_dispatch ).

        " Material &1 doesn't have master carton maintained as AUoM.
        MESSAGE i008(zmc_out) WITH ls_matnr-matnr INTO mv_msg.
        add_message( ).

        " Material will be packed by cuboid algorithm.
        MESSAGE i009(zmc_out) WITH '' INTO mv_msg.
        add_message( ).

        APPEND <ls_mat> TO lt_mats_cuboid.
        CONTINUE.
      ENDIF.

      " Maintained capacity for master carton is: &1[&2]. ls_master_carton_capacity-quan
      MESSAGE i010(zmc_out) WITH ls_master_carton_capacity-quan ls_master_carton_capacity-unit INTO mv_msg.
      add_message( ).

      " calculate number of cartons for current material
      DATA(lv_no_master) = CONV i( floor( <ls_mat>-qty-quan / ls_master_carton_capacity-quan ) ).
      DATA(lv_left_qty) = CONV i( <ls_mat>-qty-quan MOD ls_master_carton_capacity-quan ).

      IF lv_left_qty IS NOT INITIAL.
        lt_mats_cuboid = VALUE #( BASE lt_mats_cuboid ( matid    = <ls_mat>-matid
                                                        qty-quan = lv_left_qty
                                                        qty-unit = <ls_mat>-qty-unit
                                                        auom_tab = <ls_mat>-auom_tab ) ).
      ENDIF.

      IF lv_no_master IS INITIAL.
        " Quantity is not enough for full carton, will be packed by cuboid algorithm.
        MESSAGE i011(zmc_out) INTO mv_msg.
        add_message( ).

        CONTINUE.
      ENDIF.

      DO lv_no_master TIMES.
        DATA(ls_mlgnum) = VALUE #( lt_mat_lgnum[ matid = <ls_mat>-matid ] OPTIONAL ).

        DATA(ls_mc) = VALUE #( lt_pmat_carton[ nonconv = ls_mlgnum-zz1_nonconveyable_whd ] OPTIONAL ).

        lt_cartons = VALUE #( BASE lt_cartons ( pmatid = ls_mc-matid
                                                volume = get_packmat_volume( iv_matid = <ls_mat>-matid
                                                                             iv_from_auom = ls_master_carton_capacity-unit )
                                                mat_packed = VALUE #( ( matid = <ls_mat>-matid
                                                                        quan  = ls_master_carton_capacity-quan
                                                                        unit  = <ls_mat>-meins
                                                                        auom  = ls_master_carton_capacity-unit ) ) ) ).
      ENDDO.

      " if full pack is not requested and there is a full carton, no need to continue with other materials.
      " after packing, algorith will be executed again for the left quantities
      IF mv_full_pack = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF ( mv_full_pack = abap_true OR lines( lt_cartons ) = 0 ) AND lines( lt_mats_cuboid ) > 0.
      " pack can/non-dispatchable products and left overs from dispatchable products
      " by cuboid algorithm
      TRY.
          DATA(lo_cuboid_alg) = NEW zcl_cuboid_algorithm( mv_lgnum ).

          DATA(lt_mats_for_packing) = VALUE zcl_cuboid_algorithm=>tt_mat_cuboid_input(
                  FOR <l> IN lt_mats_cuboid ( matid = <l>-matid
                                              quan  = <l>-qty-quan
                                              meins = <l>-qty-unit
                                              bin_auom = VALUE #( <l>-auom_tab[ 1 ]-unit OPTIONAL )
                                              no_mc_pack = <l>-no_mc_pack ) ).

          IF mv_use_only_supplied_pmats = abap_true.

            DATA(lt_pack_result) = lo_cuboid_alg->pack_by_best_pmat( it_materials = lt_mats_for_packing
                                                                     it_packmat   = it_packmat ).

          ELSE.
            lo_cuboid_alg->pack_by_custom_pmat(
              EXPORTING
                iv_entitled    = mv_entitled
                is_bp          = is_bp
                it_materials   = lt_mats_for_packing
                it_packmat     = it_packmat
              IMPORTING
                et_pack_result = lt_pack_result
                et_bapiret     = DATA(lt_bapiret) ).

            APPEND LINES OF lt_bapiret TO mt_bapiret.
          ENDIF.
          ##NO_HANDLER
        CATCH zcx_core_exception.
      ENDTRY.

    ENDIF.

    lt_cartons = VALUE #( BASE lt_cartons FOR <spm> IN lt_pack_result WHERE ( mc_flag IS NOT INITIAL )
        ( pmatid     = <spm>-pmat_guid
          volume     = VALUE #( quan = <spm>-volum unit = <spm>-unit_v )
          mc_flag    = abap_true
          mat_packed = VALUE #( FOR <pm> IN <spm>-mat_details ( matid = <pm>-matid  quan = <pm>-pc_packed  unit = <pm>-unit auom = <spm>-mc_flag ) ) ) ).

    DELETE lt_pack_result WHERE mc_flag IS NOT INITIAL.

    " build the packing result for the delivery
    rs_pack_output = VALUE #( docid  = is_doc_mat-docid
                              docno  = is_doc_mat-docno
                              carton = lt_cartons
                              shipping = VALUE #( FOR <spm> IN lt_pack_result
                                         ( pmatid = <spm>-pmat_guid
                                           volume = VALUE #( quan = <spm>-volum unit = <spm>-unit_v )
                                           mat_packed = VALUE #( FOR <pm> IN <spm>-mat_details
                                                        ( matid = <pm>-matid  quan = <pm>-pc_packed  unit = <pm>-unit ) ) ) ) ).
  ENDMETHOD.


  METHOD get_header_bp.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    IF is_header IS INITIAL.
      RETURN.
    ENDIF.

    rs_bp-carrier = VALUE #( is_header-partyloc[ party_role = /scdl/if_dl_partyloc_c=>sc_party_role_carr ]-partyno OPTIONAL ).
    rs_bp-ship_to = VALUE #( is_header-partyloc[ party_role = /scdl/if_dl_partyloc_c=>sc_party_role_stprt ]-partyno OPTIONAL ).
  ENDMETHOD.


  METHOD get_mat_details_for_packing.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA(lo_prod) = CAST /scwm/if_af_product(
        /scdl/cl_af_management=>get_instance( )->get_service( /scwm/if_af_product=>sc_me_as_service ) ).

    TRY.
        lo_prod->read_material_multiple(
          EXPORTING
            iv_lgnum      = mv_lgnum
            iv_entitled   = mv_entitled
            it_matid      = it_matid
            iv_notext     = abap_true
          IMPORTING
            et_mat_lgnum  = DATA(lt_mat_lgnum)
            et_mat_uom    = DATA(lt_mat_uom) ).
        ##NO_HANDLER
      CATCH /scwm/cx_md_api_faulty_call.
        ##NO_HANDLER
      CATCH /scwm/cx_md_exception.
    ENDTRY.

    LOOP AT lt_mat_lgnum ASSIGNING FIELD-SYMBOL(<ls_mat_lgnum>).
      APPEND INITIAL LINE TO rt_result ASSIGNING FIELD-SYMBOL(<ls_result>).

      <ls_result>-matid = <ls_mat_lgnum>-matid.
      <ls_result>-meins = <ls_mat_lgnum>-meins.
      <ls_result>-disp  = <ls_mat_lgnum>-zz1_disp_whd.

      LOOP AT lt_mat_uom ASSIGNING FIELD-SYMBOL(<ls_uom>) WHERE matid = <ls_mat_lgnum>-matid
                                                            AND meinh <> <ls_mat_lgnum>-meins.
        <ls_result>-auom_tab = VALUE #( BASE <ls_result>-auom_tab ( unit = <ls_uom>-meinh quan = <ls_uom>-umrez ) ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_packmat_nr.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    IF iv_pmatid IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_prod) = CAST /scwm/if_af_product(
        /scdl/cl_af_management=>get_instance( )->get_service( /scwm/if_af_product=>sc_me_as_service ) ).

    TRY.
        lo_prod->read_material_multiple(
          EXPORTING
            iv_lgnum      = mv_lgnum
            it_matid      = VALUE #( ( iv_pmatid ) )
            iv_notext     = abap_true
          IMPORTING
            et_mat_global  = DATA(lt_mat_global) ).
        ##NO_HANDLER
      CATCH /scwm/cx_md_api_faulty_call.
        ##NO_HANDLER
      CATCH /scwm/cx_md_exception.
    ENDTRY.

    IF lines( lt_mat_global ) = 0.
      RETURN.
    ENDIF.

    rv_result = lt_mat_global[ 1 ]-matnr.
  ENDMETHOD.


  METHOD get_packmat_volume.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    IF iv_matid IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_prod) = CAST /scwm/if_af_product(
        /scdl/cl_af_management=>get_instance( )->get_service( /scwm/if_af_product=>sc_me_as_service ) ).

    TRY.
        lo_prod->read_material_multiple(
          EXPORTING
            iv_lgnum      = mv_lgnum
            it_matid      = VALUE #( ( iv_matid ) )
            iv_notext     = abap_true
          IMPORTING
            et_mat_global = DATA(lt_mat_global)
            et_mat_uom    = DATA(lt_mat_uom) ).
        ##NO_HANDLER
      CATCH /scwm/cx_md_api_faulty_call.
        ##NO_HANDLER
      CATCH /scwm/cx_md_exception.
    ENDTRY.


    DATA(ls_global) = REF #( lt_mat_global[ matid = iv_matid ] OPTIONAL ).
    IF ls_global IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_meinh) = iv_from_auom.

    IF lv_meinh IS INITIAL.
      lv_meinh = ls_global->meins.
    ENDIF.

    DATA(ls_mat_uom) = VALUE #( lt_mat_uom[ matid = iv_matid meinh = lv_meinh ] OPTIONAL ).


    rs_result-quan = ls_mat_uom-volum.
    rs_result-unit = ls_mat_uom-voleh.
  ENDMETHOD.


  METHOD get_pmat_distance_dimensions.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    IF iv_matid IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_prod) = CAST /scwm/if_af_product(
        /scdl/cl_af_management=>get_instance( )->get_service( /scwm/if_af_product=>sc_me_as_service ) ).

    TRY.
        lo_prod->read_material_multiple(
          EXPORTING
            iv_lgnum      = mv_lgnum
            it_matid      = VALUE #( ( iv_matid ) )
            iv_notext     = abap_true
          IMPORTING
            et_mat_global = DATA(lt_mat_global)
            et_mat_uom    = DATA(lt_mat_uom)
            et_mat_pack   = DATA(lt_mat_pack) ).
        ##NO_HANDLER
      CATCH /scwm/cx_md_api_faulty_call.
        ##NO_HANDLER
      CATCH /scwm/cx_md_exception.
    ENDTRY.

    DATA(ls_global) = REF #( lt_mat_global[ matid = iv_matid ] OPTIONAL ).
    IF ls_global IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_is_pmat_mc = abap_false.
      DATA(ls_mat_pack) = VALUE #( lt_mat_pack[ matid = iv_matid ] OPTIONAL ).

      rs_result-length = ls_mat_pack-maxl.
      rs_result-width  = ls_mat_pack-maxb.
      rs_result-height = ls_mat_pack-maxh.
      rs_result-unit_lwh = ls_mat_pack-maxdim_uom.
    ELSE.
      DATA(ls_mat_uom) = VALUE #( lt_mat_uom[ matid = iv_matid meinh = iv_from_auom ] OPTIONAL ).

      rs_result-length = ls_mat_uom-laeng.
      rs_result-width  = ls_mat_uom-breit.
      rs_result-height = ls_mat_uom-hoehe.
      rs_result-unit_lwh = ls_mat_uom-meabm.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
