class ZCL_CORE_RMS_DETERMINE definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_CORE_RMS_DETERMINE .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.

  methods PICK_FROM_BULK_AREA
    importing
      !IV_CALL type /SCWM/DE_RBDCALL optional
      !IV_ANFML type /SCWM/DE_QUANTITY optional
      !IV_VORGA type /SCWM/LTAP_VORGA optional
      !IS_LTAP type /SCWM/LTAP optional
      !IS_MAT_GLOBAL type /SCWM/S_MATERIAL_GLOBAL optional
      !IS_MAT_LGNUM type /SCWM/S_MATERIAL_LGNUM optional
      !IS_MAT_HAZARD type /SCWM/S_MATERIAL_HAZARD optional
      !IT_MAT_UOM type /SCWM/TT_MATERIAL_UOM optional
      !IS_T331 type /SCWM/T331 optional
      !IS_T333 type /SCWM/T333 optional
      !IT_QMAT type /SCWM/TT_AQUA_INT optional
      !IT_QMAT_STD type /SCWM/TT_AQUA_INT optional
      !IV_REM_RULE type /SCWM/DE_REM_RULE optional
      !IO_LOG type ref to /SCWM/CL_LOG optional
      !IV_ROW type BAPI_LINE optional
    exporting
      !ET_QMAT_CUS type /SCWM/TT_AQUA_INT
      !EV_SET type XFELD
    changing
      !CS_ORDIM_CUST type /SCWM/INCL_EEW_S_ORDIM optional .
  methods PICK_FROM_HIGH_RACK_AREA
    importing
      !IV_CALL type /SCWM/DE_RBDCALL optional
      !IV_ANFML type /SCWM/DE_QUANTITY optional
      !IV_VORGA type /SCWM/LTAP_VORGA optional
      !IS_LTAP type /SCWM/LTAP optional
      !IS_MAT_GLOBAL type /SCWM/S_MATERIAL_GLOBAL optional
      !IS_MAT_LGNUM type /SCWM/S_MATERIAL_LGNUM optional
      !IS_MAT_HAZARD type /SCWM/S_MATERIAL_HAZARD optional
      !IT_MAT_UOM type /SCWM/TT_MATERIAL_UOM optional
      !IS_T331 type /SCWM/T331 optional
      !IS_T333 type /SCWM/T333 optional
      !IT_QMAT type /SCWM/TT_AQUA_INT optional
      !IT_QMAT_STD type /SCWM/TT_AQUA_INT optional
      !IV_REM_RULE type /SCWM/DE_REM_RULE optional
      !IO_LOG type ref to /SCWM/CL_LOG optional
      !IV_ROW type BAPI_LINE optional
    exporting
      !ET_QMAT_CUS type /SCWM/TT_AQUA_INT
      !EV_SET type XFELD
    changing
      !CS_ORDIM_CUST type /SCWM/INCL_EEW_S_ORDIM optional .
  methods STOCK_REMOVAL_RULE_GENERAL
    importing
      !IV_CALL type /SCWM/DE_RBDCALL
      !IV_ANFML type /SCWM/DE_QUANTITY
      !IV_VORGA type /SCWM/LTAP_VORGA
      !IS_LTAP type /SCWM/LTAP
      !IS_MAT_GLOBAL type /SCWM/S_MATERIAL_GLOBAL
      !IS_MAT_LGNUM type /SCWM/S_MATERIAL_LGNUM
      !IS_MAT_HAZARD type /SCWM/S_MATERIAL_HAZARD
      !IT_MAT_UOM type /SCWM/TT_MATERIAL_UOM
      !IS_T331 type /SCWM/T331
      !IS_T333 type /SCWM/T333
      !IT_QMAT type /SCWM/TT_AQUA_INT
      !IT_QMAT_STD type /SCWM/TT_AQUA_INT
      !IV_REM_RULE type /SCWM/DE_REM_RULE
      !IO_LOG type ref to /SCWM/CL_LOG
      !IV_ROW type BAPI_LINE
    exporting
      !ET_QMAT_CUS type /SCWM/TT_AQUA_INT
      !EV_SET type XFELD
    changing
      !CS_ORDIM_CUST type /SCWM/INCL_EEW_S_ORDIM .
ENDCLASS.



CLASS ZCL_CORE_RMS_DETERMINE IMPLEMENTATION.


METHOD /scwm/if_ex_core_rms_determine~determine.
**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-036
*& Author        : Tugay Birihan
*& e-mail        : tugay.birihan@qinlox.com
*& Module Cons.  : Sevil Rasim
*& Date          : 09.03.2023
**********************************************************************
*& Description (short)
*& /SCWM/IF_EX_CORE_RMS_DETERMINE~DETERMINE (BAdI) method to delete quants with available quantities from the internal table.
*& To do so,  the entries must be copied from table IT_QMAT to table ET_QMAT_CUS.
*& Furthermore, you have the option to sort the quants according to your own order.
*& To accomplish this, the entries must be sorted after being moved to ET_QMAT_CUS.
*& The EV_SET field needs to be set to X in order to apply the changes to the internal standard table.
*& pick_from_high_rack_area and pick_from_bulk_area methods have been added for GAP-036 with private sign,
*& Necessary import and export parameters have been directly copied from BADI interface to newly created methods.
**********************************************************************
  BREAK-POINT ID zcg_badi.
  BREAK-POINT ID zcg_ex_core_rms_determine.


  me->pick_from_high_rack_area(
    EXPORTING
      iv_anfml      = iv_anfml
      is_ltap       = is_ltap
      is_t331       = is_t331
      it_qmat       = it_qmat
    IMPORTING
      et_qmat_cus   = et_qmat_cus
      ev_set        = ev_set
  ).

  me->pick_from_bulk_area(
     EXPORTING
       iv_anfml      = iv_anfml
       is_ltap       = is_ltap
       is_t331       = is_t331
       it_qmat       = it_qmat
     IMPORTING
       et_qmat_cus   = et_qmat_cus
       ev_set        = ev_set
  ).

  me->stock_removal_rule_general(
         EXPORTING
          iv_call       = iv_call
          iv_anfml      = iv_anfml
          iv_vorga      = iv_vorga
          is_ltap       = is_ltap
          is_mat_global = is_mat_global
          is_mat_lgnum  = is_mat_lgnum
          is_mat_hazard = is_mat_hazard
          it_mat_uom    = it_mat_uom
          is_t331       = is_t331
          is_t333       = is_t333
          it_qmat       = it_qmat
          it_qmat_std   = it_qmat_std
          iv_rem_rule   = iv_rem_rule
          io_log        = io_log
          iv_row        = iv_row
        IMPORTING
          et_qmat_cus   = et_qmat_cus
          ev_set        = ev_set
        CHANGING
          cs_ordim_cust = cs_ordim_cust ).


ENDMETHOD.


  METHOD pick_from_bulk_area.
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
                                                it_switch_fields =  VALUE ztt_switch_fields( ( field       = zif_switch_const=>c_lgtype
                                                                                               field_value = is_t331-lgtyp ) )
                                                iv_trart         = is_ltap-trart
                                                iv_anfml         = iv_anfml  ) EQ abap_false.
      RETURN.
    ENDIF.

    DATA(lt_qmat) = it_qmat.
    SORT lt_qmat BY vfdat quan DESCENDING wdatu.
    DATA(lv_requsted_quan) = iv_anfml.

    DATA(lt_stock_mon) = lo_full_palet_picking->get_physical_stock( it_qmat =  lt_qmat ).
    IF lt_stock_mon IS INITIAL.
      RETURN.
    ENDIF.


    TRY.
        LOOP AT lt_qmat  ASSIGNING FIELD-SYMBOL(<ls_qmat>).

* UM Start 29.09.2023 - Skip Bin blocked for Stock Removal
          DATA: ls_lagp TYPE /scwm/lagp.
          CLEAR: ls_lagp.
          CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
            EXPORTING
              iv_lgnum      = <ls_qmat>-lgnum
              iv_lgpla      = <ls_qmat>-lgpla
            IMPORTING
              es_lagp       = ls_lagp
            EXCEPTIONS
              wrong_input   = 1
              not_found     = 2
              enqueue_error = 3
              OTHERS        = 4.

          IF ls_lagp-skzua IS NOT INITIAL.
            CONTINUE.
          ENDIF.
* UM End 29.09.2023 - Skip Bin blocked for Stock Removal

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
                                                                        AND matid = <ls_qmat>-matid
                                                                        AND quan <= iv_anfml.
            IF <ls_stock_mon>-quan <= 0. " do not allow negative picks
              CONTINUE.
            ENDIF.
            IF lv_requsted_quan <= 0.
              EXIT.
            ENDIF.

            IF <ls_stock_mon>-quan <= lv_requsted_quan.
              lv_requsted_quan = lv_requsted_quan - <ls_stock_mon>-quan.
              DATA(lv_exit) = abap_true.
            ENDIF.

          ENDLOOP.
          IF sy-subrc EQ 0 AND lv_exit EQ abap_true.
            <ls_qmat>-quan = <ls_stock_mon>-quan.
            APPEND <ls_qmat> TO et_qmat_cus.
          ENDIF.
          IF lv_exit EQ abap_true.
            ev_set = abap_true.
*              EXIT.
            CLEAR: lv_exit.
          ENDIF.

        ENDLOOP.
      CATCH /scwm/cx_core.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD pick_from_high_rack_area.
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

    IF lo_full_palet_picking->check_parameters( iv_devid         = zif_switch_const=>c_zout_001
                                                it_switch_fields = VALUE ztt_switch_fields( ( field       = zif_switch_const=>c_lgtype
                                                                                              field_value = is_t331-lgtyp ) )
                                                iv_trart         = is_ltap-trart
                                                iv_anfml         = iv_anfml  ) EQ abap_false.
      RETURN.
    ENDIF.

    DATA(lt_qmat) = it_qmat.
    SORT lt_qmat BY vfdat quan DESCENDING wdatu.
    DATA(lv_requsted_quan) = iv_anfml.

    ev_set = abap_true.
    TRY.
        LOOP AT lt_qmat ASSIGNING FIELD-SYMBOL(<ls_qmat>).

* UM Start 29.09.2023 - Skip Bin blocked for Stock Removal
          DATA: ls_lagp TYPE /scwm/lagp.
          CLEAR: ls_lagp.
          CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
            EXPORTING
              iv_lgnum      = <ls_qmat>-lgnum
              iv_lgpla      = <ls_qmat>-lgpla
            IMPORTING
              es_lagp       = ls_lagp
            EXCEPTIONS
              wrong_input   = 1
              not_found     = 2
              enqueue_error = 3
              OTHERS        = 4.

          IF ls_lagp-skzua IS NOT INITIAL.
            CONTINUE.
          ENDIF.
* UM End 29.09.2023 - Skip Bin blocked for Stock Removal

          IF <ls_qmat>-quan <= 0. " do not allow negative picks
            CONTINUE.
          ENDIF.
          IF lv_requsted_quan <= 0.
            EXIT.
          ENDIF.
          IF <ls_qmat>-quan <= lv_requsted_quan.
            APPEND <ls_qmat> TO et_qmat_cus.
            lv_requsted_quan = lv_requsted_quan - <ls_qmat>-quan.
          ENDIF.
        ENDLOOP.
      CATCH /scwm/cx_core.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD stock_removal_rule_general.
**********************************************************************
*& Key           : RM-230101
*& Request No.   : GAP 35 Picking strategies
**********************************************************************
*& Description (short)
*& General rule that the system will follow to determine the st. rem.
**********************************************************************
    DATA:
          lt_qmat TYPE /scwm/tt_aqua_int.

    IF zcl_switch=>get_switch_state( iv_lgnum = is_ltap-lgnum
                                     iv_devid = zif_switch_const=>c_zout_007 ) EQ abap_false.
      RETURN.
    ENDIF.

    DATA(lo_stock_rem_gen_rule) = zcl_out_stock_rem_gen_rule=>get_inst( ).

    " Аpply in all storage types except for the PAN1 and BLN1
    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = is_ltap-lgnum
        iv_process   = zif_param_const=>c_zout_0003
        iv_parameter = zif_param_const=>c_st_type_except
      IMPORTING
        et_list      = DATA(lt_st_types_excl) ).

    DATA(lr_sttyp_excl) = VALUE /scwm/tt_lgtyp_r( FOR ls_sttyp_excl IN lt_st_types_excl
                                                    ( sign   = wmegc_sign_inclusive
                                                      option = wmegc_option_eq
                                                      low    = ls_sttyp_excl ) ).

    IF is_t331-lgtyp IN lr_sttyp_excl.
      RETURN.
    ENDIF.

    lo_stock_rem_gen_rule->pick_to_match(
      EXPORTING
        iv_call       = iv_call        " Calling Application of Source Data Determination
        iv_anfml      = iv_anfml       " Quantity Field
        iv_vorga      = iv_vorga       " Transfer procedure
        is_ltap       = is_ltap        " Warehouse Task Internal
        is_mat_global = is_mat_global  " Material: Global Data
        is_mat_lgnum  = is_mat_lgnum   " Material: Warehouse-Number-Specific Data
        is_mat_hazard = is_mat_hazard  " Material: Hazardous Material Data
        it_mat_uom    = it_mat_uom     " Material: Table Type for Units of Measure
        is_t331       = is_t331        " Storage Type Control
        is_t333       = is_t333        " Warehouse Process Type
        it_qmat       = it_qmat        " Available Quantities: Internal Table
        it_qmat_std   = it_qmat_std    " Available Quantities: Internal Table
        iv_rem_rule   = iv_rem_rule    " Stock Removal Rule
        io_log        = io_log         " Log
        iv_row        = iv_row
      IMPORTING
        et_qmat_cus   = et_qmat_cus    " Available Quantities: Internal Table
        ev_set        = ev_set         " Checkbox
      CHANGING
        cs_ordim_cust = cs_ordim_cust  " Customer Data in Warehouse Task
    ).




  ENDMETHOD.
ENDCLASS.
