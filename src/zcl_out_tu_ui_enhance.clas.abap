class ZCL_OUT_TU_UI_ENHANCE definition
  public
  final
  create public .

public section.

  class-methods GET_TU_CMR
    importing
      !IS_TU_DLV type /SCWM/S_BO_TU_DLV
      !IO_TUDLV_MANAGER type ref to /SCWM/CL_SR_TUDLV optional
      !IV_CHGIND type CHAR01
    changing
      !CV_KEP_SPED type ABAP_BOOL
      !CV_CMR type ZDE_CMR_SHIPMENT .
  class-methods GET_SPED_KEP_MIX
    importing
      !IV_TSP type /SCWM/DE_TSP
      !IV_LGNUM type /SCWM/LGNUM
    returning
      value(RV_SPED_KEP_MIX) type ABAP_BOOL .
protected section.
private section.
ENDCLASS.



CLASS ZCL_OUT_TU_UI_ENHANCE IMPLEMENTATION.


  METHOD get_sped_kep_mix.
**********************************************************************
*& Key           : LH-111123
*& Request No.   : GAP-12 – "VCE Carrier integration”
**********************************************************************
*& Description (short)
*& Get SPED/KEP mixing flag from the carrier table
**********************************************************************
    CHECK iv_tsp IS NOT INITIAL.

    DATA(lt_map) = zcl_crud_ztout_map_carr=>select_multi_by_bupartner(
        iv_lgnum   =  iv_lgnum                " Warehouse Number/Warehouse Complex
        iv_partner =  iv_tsp                " Business Partner Number
    ).

    IF lt_map IS NOT INITIAL.
      rv_sped_kep_mix = lt_map[ 1 ]-mix_sped_kep.
    ENDIF.
  ENDMETHOD.


  METHOD get_tu_cmr.
**********************************************************************
*& Key           : LH-111123
*& Request No.   : GAP-12 – "VCE Carrier integration”
**********************************************************************
*& Description (short)
*& Get default values CMR and KEP/SPED mixing
**********************************************************************
    DATA:
      ls_t300_md      TYPE  /scwm/s_t300_md,
      lt_loc_addr_tab TYPE  /sapapo/loc_addr_tab,
      ls_address      TYPE  bapibus1006_address.

    CONSTANTS:
      c_new    LIKE iv_chgind VALUE 'N',
      c_delete LIKE iv_chgind VALUE 'D' ##needed.

    IF io_tudlv_manager IS INITIAL.
      DATA(lo_tudlv_manager) = /scwm/cl_sr_tudlv=>get_instance( ).
    ELSE.
      lo_tudlv_manager = io_tudlv_manager.
    ENDIF.

    lo_tudlv_manager->get_bo_tu_dlv(
      EXPORTING
        is_tu_act_key = VALUE #( tu_num = is_tu_dlv-tu_num tu_sr_act_num = is_tu_dlv-tu_sr_act_num )
      IMPORTING
        et_bo_tu_dlvh      = DATA(lt_bo_tu_dlvh)                 " Assigned HUs
    ).


    IF iv_chgind EQ c_new.
      DATA(lv_first_delivery) = abap_true.
      LOOP AT lt_bo_tu_dlvh TRANSPORTING NO FIELDS
           WHERE docid NE is_tu_dlv-docid.
        lv_first_delivery = abap_false.
        EXIT.
      ENDLOOP.
    ENDIF.

    IF lt_bo_tu_dlvh IS INITIAL.
      cv_kep_sped = abap_false.
      cv_cmr = abap_false.
      RETURN.
    ELSE.
      "CMR Determination
      IF lv_first_delivery EQ abap_true OR cv_cmr IS INITIAL.
        DATA(lo_dlv) = /scwm/cl_dlv_management_prd=>get_instance( ).
        TRY.
            lo_dlv->query(
              EXPORTING
                it_docid        = VALUE #(  FOR cont IN lt_bo_tu_dlvh ( doccat = wmegc_doccat_pdo docid = cont-docid ) )
                is_read_options = VALUE #(   )
                is_include_data = VALUE #( head_partyloc = abap_true )
              IMPORTING
                et_headers        = DATA(lt_dlv_header)
            ).

            DATA(lv_lgnum) = lt_bo_tu_dlvh[ 1 ]-lgnum.

            "Get warehouse address
            CALL FUNCTION '/SCWM/T300_MD_READ_SINGLE'
              EXPORTING
                iv_lgnum   = lv_lgnum                 "  Lagernummer
              IMPORTING
                es_t300_md = ls_t300_md                " LVS Lagernummern
              EXCEPTIONS
                OTHERS     = 0.                " Satz nicht vorhanden

            IF ls_t300_md-scuguid IS NOT INITIAL.
              cl_system_uuid=>convert_uuid_x16_static(
                EXPORTING
                  uuid     =   ls_t300_md-scuguid
                IMPORTING
                  uuid_c22 = DATA(lv_locid)
              ).

              CALL FUNCTION '/SAPAPO/LOC_ADDR_GET'
                EXPORTING
                  it_locid        = VALUE /sapapo/locid_tab( ( lv_locid ) )                " Tabelle von Lokationen
                TABLES
                  et_loc_addr_tab = lt_loc_addr_tab                 " Includestruktur mit ADRC-Attributen ohne Namensfelder
                EXCEPTIONS
                  OTHERS          = 0.
            ENDIF.

            IF lt_loc_addr_tab IS NOT INITIAL.
              DATA(lv_wh_country) = lt_loc_addr_tab[ 1 ]-country.
            ENDIF.

            "Get ship to party country
            LOOP AT lt_dlv_header REFERENCE INTO DATA(lr_dlv).
              LOOP AT lr_dlv->partyloc INTO DATA(ls_party)
                   WHERE party_role EQ /scdl/if_dl_partyloc_c=>sc_party_role_stprt.
                CALL FUNCTION 'BUPA_ADDRESS_GET_DETAIL'
                  EXPORTING
                    iv_partner_guid = ls_party-partyid
                  IMPORTING
                    es_address      = ls_address.
                IF ls_address-country NE lv_wh_country.
                  cv_cmr = 'CMR Shipment'(cmr).
                  RETURN.
                ENDIF.
              ENDLOOP.
            ENDLOOP.
          CATCH /scdl/cx_delivery ##no_handler.
          CATCH cx_uuid_error ##no_handler.

        ENDTRY.
      ENDIF.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
