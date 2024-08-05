CLASS zcl_core_rms_quantity DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /scwm/if_ex_core_rms_quantity .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS change_bulk_quantity
      IMPORTING
        VALUE(iv_call)  TYPE /scwm/de_rbdcall OPTIONAL
        VALUE(iv_anfml) TYPE /scwm/de_quantity OPTIONAL
        VALUE(iv_vorga) TYPE /scwm/ltap_vorga OPTIONAL
        !is_aqua        TYPE /scwm/aqua OPTIONAL
        VALUE(iv_ruom)  TYPE /scwm/de_ruom OPTIONAL
        !iv_opunit_std  TYPE /scwm/de_opunit OPTIONAL
        !iv_hutyp_std   TYPE /scwm/de_hutyp OPTIONAL
        !is_ltap        TYPE /scwm/ltap OPTIONAL
        !is_mat_global  TYPE /scwm/s_material_global OPTIONAL
        !is_mat_lgnum   TYPE /scwm/s_material_lgnum OPTIONAL
        !is_mat_lgtyp   TYPE /scwm/s_material_lgtyp OPTIONAL
        !is_mat_hazard  TYPE /scwm/s_material_hazard OPTIONAL
        !it_mat_uom     TYPE /scwm/tt_material_uom OPTIONAL
        !is_t331        TYPE /scwm/t331 OPTIONAL
        !is_t333        TYPE /scwm/t333 OPTIONAL
        !it_qmat        TYPE /scwm/tt_aqua_int OPTIONAL
        !io_log         TYPE REF TO /scwm/cl_log OPTIONAL
        !iv_row         TYPE bapi_line OPTIONAL
      EXPORTING
        !ev_anfml       TYPE /scwm/de_quantity
        !ev_set         TYPE xfeld
      CHANGING
        !cs_ordim_cust  TYPE /scwm/incl_eew_s_ordim OPTIONAL .
    METHODS round_up_repl_quantity
      IMPORTING
        VALUE(iv_call)  TYPE /scwm/de_rbdcall OPTIONAL
        VALUE(iv_anfml) TYPE /scwm/de_quantity OPTIONAL
        VALUE(iv_vorga) TYPE /scwm/ltap_vorga OPTIONAL
        !is_aqua        TYPE /scwm/aqua OPTIONAL
        VALUE(iv_ruom)  TYPE /scwm/de_ruom OPTIONAL
        !iv_opunit_std  TYPE /scwm/de_opunit OPTIONAL
        !iv_hutyp_std   TYPE /scwm/de_hutyp OPTIONAL
        !is_ltap        TYPE /scwm/ltap OPTIONAL
        !is_mat_global  TYPE /scwm/s_material_global OPTIONAL
        !is_mat_lgnum   TYPE /scwm/s_material_lgnum OPTIONAL
        !is_mat_lgtyp   TYPE /scwm/s_material_lgtyp OPTIONAL
        !is_mat_hazard  TYPE /scwm/s_material_hazard OPTIONAL
        !it_mat_uom     TYPE /scwm/tt_material_uom OPTIONAL
        !is_t331        TYPE /scwm/t331 OPTIONAL
        !is_t333        TYPE /scwm/t333 OPTIONAL
        !it_qmat        TYPE /scwm/tt_aqua_int OPTIONAL
        !io_log         TYPE REF TO /scwm/cl_log OPTIONAL
        !iv_row         TYPE bapi_line OPTIONAL
      EXPORTING
        !ev_anfml       TYPE /scwm/de_quantity
        !ev_set         TYPE xfeld
      CHANGING
        !cs_ordim_cust  TYPE /scwm/incl_eew_s_ordim OPTIONAL .
ENDCLASS.



CLASS ZCL_CORE_RMS_QUANTITY IMPLEMENTATION.


  METHOD /scwm/if_ex_core_rms_quantity~quantity.
**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-036
*& Author        : Tugay Birihan
*& e-mail        : tugay.birihan@qinlox.com
*& Module Cons.  : Sevil Rasim
*& Date          : 09.03.2023
**********************************************************************
*& Description (short)
*& /SCWM/IF_EX_CORE_RMS_QUANTITY~QUANTITY(BAdI) method is used to change the required quantity (for under- and over-deliveries),
*& the HU type and the alternative unit of measure (AUoM) of the warehouse task.
*& The EV_SET field needs to be set to X in order to apply the changes to the internal standard table.
*& change_bulk_quantity method has been added for GAP-036 with private sign,
*& Necessary import and export parameters have been directly copied from BADI interface to newly created methods.
**********************************************************************

    BREAK-POINT ID zcg_badi.
    BREAK-POINT ID zcg_ex_core_rms_quantity.

    me->change_bulk_quantity(
      EXPORTING
        iv_anfml      = iv_anfml
        is_ltap       = is_ltap
        is_t331       = is_t331
        it_qmat       = it_qmat
      IMPORTING
        ev_anfml      = ev_anfml
        ev_set        = ev_set ).

    me->round_up_repl_quantity(
      EXPORTING
        iv_anfml      = iv_anfml
        is_aqua       = is_aqua
        is_ltap       = is_ltap
        is_mat_lgtyp  = is_mat_lgtyp
      IMPORTING
        ev_anfml      = ev_anfml
        ev_set        = ev_set ).

    zcl_core_ltap=>set_ltap( iv_reqqty = iv_anfml
                             is_ltap   = is_ltap ).

  ENDMETHOD.


  METHOD change_bulk_quantity.
**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-036
*& Author        : Tugay Birihan
*& e-mail        : tugay.birihan@qinlox.com
*& Module Cons.  : Sevil Rasim
*& Date          : 09.03.2023
**********************************************************************
*& Description (short)
*& Picking from the High Rack area
*& The stock is picked in the following order:
*& 1-Best Before Date in ascending order
*& 2-Quantity descending order
*& 3-FIFO
**********************************************************************
    DATA(lo_full_palet_picking) = zcl_out_full_palet_picking=>get_instance( ).

    IF lo_full_palet_picking->check_parameters( iv_devid         = zif_switch_const=>c_zout_003
                                                it_switch_fields = VALUE ztt_switch_fields( ( field       = zif_switch_const=>c_lgtype
                                                                                              field_value = is_t331-lgtyp ) )
                                                iv_trart         = is_ltap-trart
                                                iv_anfml         = iv_anfml  ) EQ abap_false.
      RETURN.
    ENDIF.


    DATA(lt_qmat) = it_qmat.
    SORT lt_qmat BY vfdat quan DESCENDING wdatu.
    DATA(lv_requsted_quan) = iv_anfml.

    DATA(lt_stock_mon) = lo_full_palet_picking->get_physical_stock( it_qmat = lt_qmat ).
    IF lt_stock_mon IS INITIAL .
      RETURN.
    ENDIF.

    TRY.
        LOOP AT lt_qmat  ASSIGNING FIELD-SYMBOL(<ls_qmat>).

          IF <ls_qmat>-matid IS INITIAL OR <ls_qmat>-lgpla IS INITIAL.
            CONTINUE.
          ENDIF.
          IF <ls_qmat>-quan <= 0. " do not allow negative picks
            CONTINUE.
          ENDIF.
          IF lv_requsted_quan <= 0.
            EXIT.
          ENDIF.

          LOOP AT lt_stock_mon ASSIGNING FIELD-SYMBOL(<ls_stock_mon>) WHERE lgtyp = <ls_qmat>-lgtyp
                                                                        AND lgpla = <ls_qmat>-lgpla
                                                                        AND matid = <ls_qmat>-matid.
            IF <ls_stock_mon>-quan <= 0. " do not allow negative picks
              CONTINUE.
            ENDIF.
            IF lv_requsted_quan <= 0.
              EXIT.
            ENDIF.

            IF <ls_stock_mon>-quan <= lv_requsted_quan.
              lv_requsted_quan = lv_requsted_quan - <ls_stock_mon>-quan.
              lo_full_palet_picking->delete_picked_bin( iv_bin = <ls_qmat>-lgpla ).
              DATA(lv_exit) = abap_true.
            ENDIF.
          ENDLOOP.
          IF lv_exit EQ abap_true.
            ev_anfml = iv_anfml - lv_requsted_quan.
            EXIT.
          ENDIF.
        ENDLOOP.

      CATCH /scwm/cx_core.
        RETURN.
    ENDTRY.
    ev_set = abap_true.

  ENDMETHOD.


  METHOD round_up_repl_quantity.
********************************************************************
*& Key          : <RMANOVA>-Nov 23, 2023 GAP-074
*& Request No.  : TE1K900125 Main Request
********************************************************************
*& Description  : Prevent order-based
*& replenishments to "steal" stock from other
*& waves that were released for order-based
*& replenishment before
********************************************************************

    DATA: ls_mat_lgtyp  TYPE /scwm/s_material_lgtyp.

    IF zcl_switch=>get_switch_state( iv_lgnum = is_ltap-lgnum
                                     iv_devid = zif_switch_const=>c_zint_003 ) EQ abap_false.
      RETURN.
    ENDIF.

    "Field level switch on/off
    DATA(lt_switch_fields) = VALUE ztt_switch_fields( ( field       = zif_switch_const=>c_act_type
                                                        field_value = is_ltap-act_type ) ).


    IF zcl_switch=>get_switch_state( iv_lgnum  = is_ltap-lgnum
                                     iv_devid  = zif_switch_const=>c_zint_003
                                     it_fields = lt_switch_fields ) EQ abap_false.
      RETURN.
    ENDIF.

    " AAHMEDOV-20240102 Add check WT storage location type
    IF is_ltap-sloc_type EQ wmegc_rsrc.
      RETURN.
    ENDIF.
    " AAHMEDOV-20240102 Add check WT storage location type

    IF is_ltap-trart <> wmegc_trart_int.
      RETURN.
    ENDIF.

    IF iv_anfml >= is_aqua-quan.
      RETURN.
    ENDIF.

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid     = is_ltap-matid
            iv_lgnum     = is_ltap-lgnum
            iv_lgtyp     = is_ltap-nltyp
            iv_entitled  = is_ltap-entitled
          IMPORTING
            es_mat_lgtyp = ls_mat_lgtyp.
      CATCH /scwm/cx_md.
    ENDTRY.

    " BSUGAREV-20231123 Add check for destination storage type
    DATA(ls_lgtyp_roundup) = zcl_crud_ztout_lgtyprndup=>select_single_by_key(
      iv_lgnum = is_ltap-lgnum
      iv_lgtyp = is_ltap-nltyp ).

    IF ls_lgtyp_roundup-roundup = abap_true OR ls_mat_lgtyp-zz1_dirrpl_stt = abap_true.
      ev_anfml = is_aqua-quan.
      ev_set = abap_true.
    ELSE.
      ev_anfml = ls_mat_lgtyp-maxqty.
      ev_set = abap_true.
    ENDIF.
    " BSUGAREV-20231123 Add check for destination storage type

  ENDMETHOD.
ENDCLASS.
