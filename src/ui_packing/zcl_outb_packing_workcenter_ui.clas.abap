class ZCL_OUTB_PACKING_WORKCENTER_UI definition
  public
  final
  create private .

public section.

  interfaces ZIF_OUTB_PACKING_WORKCENTER_UI .

  constants C_MAIN_SCREEN type SY-DYNNR value '0100' ##NO_TEXT.
  constants C_PACK_APPL_SC type ZDE_PACKTYP value 'SC' ##NO_TEXT.
  constants C_PACK_APPL_SLO type ZDE_PACKTYP value 'SLO' ##NO_TEXT.
  constants C_PACK_APPL_SPO type ZDE_PACKTYP value 'SPO' ##NO_TEXT.
  constants C_PACK_APPL_TOTE type ZDE_PACKTYP value 'TOTE' ##NO_TEXT.
  constants C_PACK_APPL_MC type ZDE_PACKTYP value 'MC' ##NO_TEXT.
  constants C_FIELD_HUIDENT type SCREEN-NAME value 'ZSTR_OUT_UI_COMMON-HUIDENT' ##NO_TEXT.
  constants C_FIELD_PROD type SCREEN-NAME value 'ZSTR_OUT_UI_COMMON-PRODUCT_EAN_MPN' ##NO_TEXT.

  class-methods AT_SELL_GET_WST
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_WRKST type /SCWM/DE_WORKSTATION
    raising
      ZCX_WORKSTATION .
  class-methods CLASS_CONSTRUCTOR .
  class-methods GET_DEF_PARAMETERS
    exporting
      !EV_LGNUM type /SCWM/S_WRK_PACK-LGNUM
      !EV_WRKST type /SCWM/S_WRK_PACK-WORKSTATION
      !EV_LGPLA type /SCWM/LAGP-LGPLA .
  class-methods IS_WORKCENTER_MASTERCARTON_REL
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_WORKCENTER type /SCWM/DE_WORKSTATION
    returning
      value(RV_MS_REL) type ABAP_BOOL .
  class-methods START_UI
    importing
      !IV_LGNUM type /SCWM/S_WRK_PACK-LGNUM
      !IV_WRKST type /SCWM/S_WRK_PACK-WORKSTATION
      !IV_LGPLA type /SCWM/LGPLA
      !IV_HUIDENT type /SCWM/DE_HUIDENT
      !IV_SCALE_WEIGHT_IN_KG type ZDE_SCALE_HU_WEIGHT optional
      !IV_PRODUCT_EAN_MPN type ZDE_INB_PRODUCT_EAN_MPN
    raising
      ZCX_WORKSTATION .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_hu_doc_ref_qty,
        docid   TYPE /scdl/dl_docid,
***        itemid  TYPE /scdl/dl_itemid,
        huident TYPE /scwm/de_huident,
        qty     TYPE /scwm/de_quantity,
***        uom     TYPE /scwm/de_base_uom,
      END OF ty_hu_doc_ref_qty .
  types:
    tty_hu_doc_ref_qty  TYPE STANDARD TABLE OF ty_hu_doc_ref_qty .
  types:
    TY_SUBSCREENS TYPE STANDARD TABLE OF REF TO ZIF_OUTB_WS_SUBSCR_UI .

  class-data SO_SP type ref to ZIF_OUTB_PACKING_WC_SP .
  class-data SV_SCALE_ON_SEL_SCR type ABAP_BOOL .
  data MO_SUBSCREEN type ref to ZIF_OUTB_WS_SUBSCR_UI .
  data MS_SCREEN_DATA type ZSTR_OUT_UI_COMMON .
  data MT_SUBSCREEN_STACK type TY_SUBSCREENS .
  data MV_CURRENT_REPID type SY-REPID .
  data MV_CURRENT_SCREEN type SY-DYNNR .
  data MV_HUIDENT type /SCWM/DE_HUIDENT .
  data MV_LGPLA type /SCWM/LGPLA .
  class-data SV_LGNUM_FOR_BUFFER type /SCWM/LGNUM .
  class-data ST_RNG_MC_WORKCENTER type RSELOPTION .

  class-methods START_UI_MC
    exporting
      !EV_NEXT_PROD type ZSTR_OUT_UI_COMMON-PRODUCT_EAN_MPN .
  class-methods START_UI_SC_TOTE
    exporting
      !EV_NEXT_HU type /SCWM/HUIDENT .
  class-methods START_UI_SLO
    exporting
      !EV_NEXT_HU type /SCWM/HUIDENT .
  class-methods START_UI_SPO
    exporting
      !EV_NEXT_HU type /SCWM/HUIDENT .
ENDCLASS.



CLASS ZCL_OUTB_PACKING_WORKCENTER_UI IMPLEMENTATION.


  METHOD at_sell_get_wst.

    so_sp->set_workstation(
      EXPORTING
        iv_lgnum = iv_lgnum
        iv_wrkst = iv_wrkst
    ).

  ENDMETHOD.


  METHOD class_constructor.
    so_sp = NEW zcl_outb_packing_wc_sp( ).
  ENDMETHOD.


  METHOD get_def_parameters.
    DATA(ls_def) = so_sp->get_terminal_defaults( ).
    ev_lgnum = ls_def-lgnum.
    ev_wrkst = ls_def-workstation.
    ev_lgpla = ls_def-work_desk_stbin.

    SELECT SINGLE FROM t000
      FIELDS CASE t000~cccategory WHEN 'C' THEN @abap_true ELSE @abap_false END
      WHERE mandt EQ @sy-mandt
      INTO @sv_scale_on_sel_scr.

  ENDMETHOD.


  METHOD is_workcenter_mastercarton_rel.
    IF sv_lgnum_for_buffer NE iv_lgnum
       OR st_rng_mc_workcenter IS INITIAL.
      zcl_param=>get_parameter(
        EXPORTING
          iv_lgnum     = iv_lgnum                  " Warehouse Number/Warehouse Complex
          iv_process   = zif_param_const=>c_zout_0009                 " Process ID (Specification, Program, BAdI etc.)
          iv_parameter = zif_param_const=>c_wc_mastercarton_pack                  " Parameter ID for process
        IMPORTING
          et_range  = st_rng_mc_workcenter                 " Parameter-Framework Low
      ).
      sv_lgnum_for_buffer = iv_lgnum.
    ENDIF.

    rv_ms_rel = xsdbool( iv_workcenter IN st_rng_mc_workcenter ).
  ENDMETHOD.


  METHOD start_ui.
    DATA: lv_answer           TYPE c LENGTH 1,
          lv_next_prod        TYPE zstr_out_ui_common-product_ean_mpn,
          lv_next_hu_returned TYPE abap_bool,
          lv_cancelled        TYPE abap_bool.

    DATA(lv_next_hu) = iv_huident.
    DATA(lv_first_step) = abap_true. "call at least once event only product is filled

    DATA(lv_hu_enter_mode) = abap_true.
    lv_next_prod = iv_product_ean_mpn.
    WHILE ( lv_next_hu IS NOT INITIAL AND lv_hu_enter_mode EQ abap_true )
          OR ( lv_next_prod IS NOT INITIAL AND lv_hu_enter_mode EQ abap_false )
           OR lv_first_step EQ abap_true.
      lv_first_step = abap_false. "
      TRY.
          so_sp->init_ws(
            EXPORTING
              iv_lgnum   = iv_lgnum
              iv_wrkst   = iv_wrkst
              iv_lgpla   = iv_lgpla
              iv_huident = lv_next_hu
              iv_scale_weight_in_kg = iv_scale_weight_in_kg
              iv_product_ean_mpn = lv_next_prod
            IMPORTING
              ev_packtyp = DATA(lv_pack_type_scenario)
              ev_processed_by_other_desk = DATA(lv_proc_by_other)
              ev_weight_diff = DATA(lv_weight_diff)
              ev_get_hu_for_prod = DATA(lv_get_hu_for_prod)
          ).
        CATCH zcx_workstation INTO DATA(lx_ws).
          IF lv_next_hu_returned EQ abap_false.
            RAISE EXCEPTION lx_ws.
          ENDIF.
          DATA(lv_msg_init) = lx_ws->get_text( ).
          CALL FUNCTION 'Z_OUT_POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'HU cannot be processed'(hcp)
              text_question         = lv_msg_init
              text_button_1         = 'Try Another HU'(tah)         " Text on the first pushbutton
              text_button_2         = 'Skip this Step'(sks)         " Text on the second pushbutton
              display_cancel_button = abap_false              " Button for displaying cancel pushbutton
            IMPORTING
              answer                = lv_answer                " Return values: '1', '2', 'A'
            EXCEPTIONS
              OTHERS                = 0.
          IF lv_answer NE '1'.
            RETURN.
          ENDIF.
          CALL FUNCTION 'Z_OUT_GET_HU_UI'
            IMPORTING
              ev_huident   = lv_next_hu                 " Handling Unit Identification
              ev_cancelled = lv_cancelled.                 " General Flag
          IF lv_cancelled EQ abap_true OR lv_next_hu IS INITIAL.
            RETURN.
          ENDIF.
          lv_next_hu_returned = abap_true.
          CONTINUE.
      ENDTRY.
      IF lv_get_hu_for_prod IS NOT INITIAL.
        CALL FUNCTION 'Z_OUT_GET_HU_FROM_QUANTITY_UI'
          EXPORTING
            io_sp        = so_sp
            iv_matid     = lv_get_hu_for_prod
          IMPORTING
            ev_huident   = lv_next_hu                 " Handling Unit Identification
            ev_cancelled = lv_cancelled.                 " General Flag
        IF lv_cancelled EQ abap_true OR lv_next_hu IS INITIAL.
          RETURN.
        ENDIF.

        so_sp->init_ws(
          EXPORTING
            iv_lgnum   = iv_lgnum
            iv_wrkst   = iv_wrkst
            iv_lgpla   = iv_lgpla
            iv_huident = lv_next_hu
            iv_scale_weight_in_kg = iv_scale_weight_in_kg
            iv_product_ean_mpn = iv_product_ean_mpn
        ).

      ELSEIF lv_proc_by_other EQ abap_true.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'HU Processed by other desk'(pbo)
            text_question         = 'Picking HU is located in another Storage Bin of the same KEP packing WC. Do you want to move it to your desk?'(pbq)                 " Question text in dialog box
            text_button_1         = 'Yes'(yes)         " Text on the first pushbutton
            text_button_2         = 'No'(no_)         " Text on the second pushbutton
            display_cancel_button = abap_false              " Button for displaying cancel pushbutton
          IMPORTING
            answer                = lv_answer                " Return values: '1', '2', 'A'
          EXCEPTIONS
            OTHERS                = 0.
        IF lv_answer NE '1'.
          RETURN.
        ENDIF.
        so_sp->init_ws(
          EXPORTING
            iv_lgnum                = iv_lgnum
            iv_wrkst                = iv_wrkst
            iv_lgpla                = iv_lgpla
            iv_huident              = iv_huident
            iv_move_from_other_desk = abap_true
            iv_scale_weight_in_kg   = iv_scale_weight_in_kg
            iv_product_ean_mpn      = iv_product_ean_mpn
          IMPORTING
            ev_packtyp = lv_pack_type_scenario
        ).
      ENDIF.

      CLEAR lv_next_hu.
      IF so_sp->is_workcenter_mastercarton_rel( ).
        lv_hu_enter_mode = abap_false.
        zcl_outb_packing_workcenter_ui=>start_ui_mc(
          IMPORTING
             ev_next_prod = lv_next_prod ).
      ELSE.
        CASE lv_pack_type_scenario.
          WHEN c_pack_appl_slo.
            zcl_outb_packing_workcenter_ui=>start_ui_slo(
              IMPORTING
                ev_next_hu = lv_next_hu                 " Handling Unit Identification
            ).
          WHEN c_pack_appl_spo.
            zcl_outb_packing_workcenter_ui=>start_ui_spo(
              IMPORTING
                ev_next_hu = lv_next_hu                 " Handling Unit Identification
            ).
          WHEN c_pack_appl_sc OR c_pack_appl_tote.
            zcl_outb_packing_workcenter_ui=>start_ui_sc_tote(
              IMPORTING
                ev_next_hu = lv_next_hu                 " Handling Unit Identification
            ).
          WHEN OTHERS.
            so_sp->unlock_hu( ).

            "Error during process determination. HU not relevant.
            RAISE EXCEPTION TYPE zcx_workstation
              MESSAGE s017(zmc_out).
        ENDCASE.
      ENDIF.
      so_sp->unlock_hu( ).

      lv_next_hu = |{ lv_next_hu ALPHA = IN }|.
    ENDWHILE.
  ENDMETHOD.


  METHOD START_UI_MC.

    CALL FUNCTION 'Z_OUT_START_PACKING_MC'
      EXPORTING
        io_main_ctrl = NEW zcl_outb_packing_workcenter_ui( )
        io_sub_ctrl  = zcl_outb_packing_wc_sc_ui=>create_instance( so_sp )
      IMPORTING
        ev_next_prod = ev_next_prod.

  ENDMETHOD.


  METHOD START_UI_SC_TOTE.

    CALL FUNCTION 'Z_OUT_START_PACKING_SC'
      EXPORTING
        io_main_ctrl = NEW zcl_outb_packing_workcenter_ui( )
        io_sub_ctrl  = zcl_outb_packing_wc_sc_ui=>create_instance( so_sp )
      IMPORTING
        ev_next_hu   = ev_next_hu.
  ENDMETHOD.


  METHOD start_ui_slo.
    CALL FUNCTION 'Z_OUT_START_PACKING_SLO'
      EXPORTING
        io_main_ctrl = NEW zcl_outb_packing_workcenter_ui( )
        io_sub_ctrl  = zcl_outb_packing_wc_slo_ui=>create_instance( so_sp )
      IMPORTING
        ev_next_hu   = ev_next_hu.

  ENDMETHOD.


  METHOD start_ui_spo.

    CALL FUNCTION 'Z_OUT_START_PACKING_SPO'
      EXPORTING
        io_main_ctrl = NEW zcl_outb_packing_workcenter_ui( )
        io_sub_ctrl  = zcl_outb_packing_wc_spo_ui=>create_instance( so_sp )
      IMPORTING
        ev_next_hu   = ev_next_hu.
  ENDMETHOD.


  METHOD zif_outb_packing_workcenter_ui~get_main_screen_no.
    rv_dynnr = c_main_screen.
  ENDMETHOD.


  METHOD zif_outb_packing_workcenter_ui~get_status.
    mo_subscreen->get_status(
      IMPORTING
        ev_status   = ev_status
        et_excludes = et_excludes                 " Table of Strings
    ).
  ENDMETHOD.


  METHOD zif_outb_packing_workcenter_ui~get_subscreen_ctrl.
  ENDMETHOD.


  METHOD zif_outb_packing_workcenter_ui~get_title.
    mo_subscreen->get_title(
      IMPORTING
        ev_title = ev_title                 " Title Line
        ev_param = ev_param                 " Parameter
    ).
  ENDMETHOD.


  METHOD zif_outb_packing_workcenter_ui~init.
    IF iv_keep_active EQ abap_true AND mo_subscreen IS BOUND.
      INSERT mo_subscreen INTO mt_subscreen_stack INDEX 1.
    ENDIF.

    mo_subscreen = io_subscreen_ui.
    mo_subscreen->init(
      EXPORTING
         io_main_ui = me
      IMPORTING
         ev_subscreen_no   = mv_current_screen                 " Dynpro Number
         ev_subscreen_prg  = mv_current_repid
         es_common_data    = ms_screen_data
    ).

  ENDMETHOD.


  METHOD zif_outb_packing_workcenter_ui~pai.
  ENDMETHOD.


  METHOD zif_outb_packing_workcenter_ui~pbo.
    mo_subscreen->get_subscreen(
      IMPORTING
        ev_repid  = mv_current_repid                  " ABAP Program: Current Master Program
        ev_screen = mv_current_screen                 " Dynpro Number
    ).
    ev_tab_subscreen = mv_current_screen.
    ev_tab_repid     = mv_current_repid.
    es_screen_data   = ms_screen_data.
    LOOP AT SCREEN.
      IF ms_screen_data-docno IS INITIAL.
        IF screen-group1 = 'SCT'.
          screen-active = '0'.
        ENDIF.
      ENDIF.
      IF so_sp->is_workcenter_mastercarton_rel( ).
        IF screen-group2 = 'WGH'.
          screen-active = '0'.
        ENDIF.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_outb_packing_workcenter_ui~process_user_command.
**********************************************************************
*& Key           : LH-090123
*& Request No.   : GAP-022 – “KEP Workcenter”
**********************************************************************
*& Description (short)
*& Process user commands on the main scree
**********************************************************************
    mo_subscreen->process_user_command(
      EXPORTING
        iv_ucomm        = iv_ucomm                " Function Code
      IMPORTING
        es_bapiret      = es_bapiret                " Return Parameter
        ev_leave_screen = ev_leave_screen
    ).
    if ev_leave_screen eq abap_true and mt_subscreen_stack is not INITIAL.
      mo_subscreen = mt_subscreen_stack[ 1 ].
      delete mt_subscreen_stack INDEX 1.
    endif.
  ENDMETHOD.
ENDCLASS.
