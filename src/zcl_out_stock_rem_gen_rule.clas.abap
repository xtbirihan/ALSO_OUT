CLASS zcl_out_stock_rem_gen_rule DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_sortfld,
        sortf TYPE /scwm/de_sortf,
        sorto TYPE /scwm/de_sorto,
      END OF ty_sortfld .
    TYPES:
      ty_sortfld_tt TYPE STANDARD TABLE OF ty_sortfld WITH DEFAULT KEY .

    CONSTANTS mc_sortf_picktomatch TYPE /scwm/de_sortf VALUE 'ZZPICKTOMATCH' ##NO_TEXT.

    CLASS-METHODS get_inst
      RETURNING
        VALUE(ro_inst) TYPE REF TO zcl_out_stock_rem_gen_rule .
    METHODS pick_to_match
      IMPORTING
        !iv_call       TYPE /scwm/de_rbdcall
        !iv_anfml      TYPE /scwm/de_quantity
        !iv_vorga      TYPE /scwm/ltap_vorga
        !is_ltap       TYPE /scwm/ltap
        !is_mat_global TYPE /scwm/s_material_global
        !is_mat_lgnum  TYPE /scwm/s_material_lgnum
        !is_mat_hazard TYPE /scwm/s_material_hazard
        !it_mat_uom    TYPE /scwm/tt_material_uom
        !is_t331       TYPE /scwm/t331
        !is_t333       TYPE /scwm/t333
        !it_qmat       TYPE /scwm/tt_aqua_int
        !it_qmat_std   TYPE /scwm/tt_aqua_int
        !iv_rem_rule   TYPE /scwm/de_rem_rule
        !io_log        TYPE REF TO /scwm/cl_log
        !iv_row        TYPE i
      EXPORTING
        !et_qmat_cus   TYPE /scwm/tt_aqua_int
        !ev_set        TYPE xfeld
      CHANGING
        !cs_ordim_cust TYPE /scwm/incl_eew_s_ordim .
  PROTECTED SECTION.
private section.

  class-data SO_INST type ref to ZCL_OUT_STOCK_REM_GEN_RULE .

  methods CHECK_CONSISTENT_FIELDS
    importing
      !IT_INRECORDS type ref to DATA
      !IT_FIELDS type TY_SORTFLD_TT
    raising
      CX_SY_STRUCT_COMP_NAME .
  methods GET_RULE_SORT_FIELDS
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_REM_RULE type /SCWM/DE_REM_RULE
    returning
      value(RT_T334RR) type /SCWM/TT_T334RR .
  methods GROUP_BY
    importing
      !IT_INRECORDS type INDEX TABLE
      !IT_GROUP_BY_FIELDS type TY_SORTFLD_TT
    exporting
      !ET_OUTRECORDS type INDEX TABLE
    raising
      CX_SY_STRUCT_COMP_NAME .
  methods RESORT_GROUP
    importing
      !IV_TARGET_QTY type /SCWM/DE_QUANTITY
      !IT_SORTF type TY_SORTFLD_TT
    changing
      !ct_qmat type /SCWM/TT_AQUA_INT .
  methods SORT_TABLE
    importing
      !IT_FIELDS type TY_SORTFLD_TT
    changing
      !CT_INRECORDS type INDEX TABLE .
  methods UPDATE_SORT_VALUES_PICKTOMATCH
    changing
      !CT_QMAT type /SCWM/TT_AQUA_INT .
ENDCLASS.



CLASS ZCL_OUT_STOCK_REM_GEN_RULE IMPLEMENTATION.


  METHOD check_consistent_fields.
**********************************************************************
*& Key           : RM-230606
*& Request No.   : GAP 35 Picking strategies
**********************************************************************
*& Description (short)
*&
**********************************************************************

    DATA: lo_tabledescr TYPE REF TO cl_abap_tabledescr,
          lo_strucdescr TYPE REF TO cl_abap_structdescr.

    lo_tabledescr ?= cl_abap_tabledescr=>describe_by_data_ref( it_inrecords ).
    lo_strucdescr ?= lo_tabledescr->get_table_line_type( ).
    LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
      ASSIGN COMPONENT 'SORTF' OF STRUCTURE <ls_field> TO FIELD-SYMBOL(<ls_field_name>).

      IF NOT line_exists( lo_strucdescr->components[ name = <ls_field_name> ] ).
        RAISE EXCEPTION TYPE cx_sy_struct_comp_name.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_inst.
**********************************************************************
*& Key           : RM-230602
*& Request No.   : GAP 35 Picking strategies
**********************************************************************
*& Description (short)
*&
**********************************************************************

    IF so_inst IS NOT BOUND.
      so_inst = NEW #( ).
    ENDIF.

    ro_inst = so_inst.

  ENDMETHOD.


  METHOD get_rule_sort_fields.
**********************************************************************
*& Key           : RM-230602
*& Request No.   : GAP 35 Picking strategies
**********************************************************************
*& Description (short)
*&
**********************************************************************

    IF iv_rem_rule IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCWM/T334RR_READ_SINGLE'
      EXPORTING
        iv_lgnum    = iv_lgnum
        iv_rem_rule = iv_rem_rule
      IMPORTING
        et_t334rr   = rt_t334rr
      EXCEPTIONS
        not_found   = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      CLEAR: rt_t334rr.
    ENDIF.

  ENDMETHOD.


  METHOD group_by.
**********************************************************************
*& Key           : RM-230602
*& Request No.   : GAP 35 Picking strategies
**********************************************************************
*& Description (short)
*&
**********************************************************************

    DATA: lt_inrecords  TYPE REF TO data,
          lt_outrecords TYPE REF TO data,
          lt_outrectemp TYPE REF TO data,
          lv_index      TYPE i.

    FIELD-SYMBOLS: <lt_inrecords>  TYPE INDEX TABLE,
                   <lt_outrecords> TYPE INDEX TABLE,
                   <lt_outrectemp> TYPE INDEX TABLE.

    CREATE DATA lt_inrecords LIKE it_inrecords.
    ASSIGN lt_inrecords->* TO <lt_inrecords>.
    <lt_inrecords> = it_inrecords.

    check_consistent_fields(
      it_inrecords = lt_inrecords
      it_fields    = it_group_by_fields ).

    sort_table(
      EXPORTING
        it_fields    = it_group_by_fields
      CHANGING
        ct_inrecords = <lt_inrecords> ).

    CREATE DATA lt_outrecords LIKE et_outrecords.
    ASSIGN lt_outrecords->* TO <lt_outrecords>.

    CREATE DATA lt_outrectemp LIKE et_outrecords.
    ASSIGN lt_outrectemp->* TO <lt_outrectemp>.

    LOOP AT <lt_inrecords> ASSIGNING FIELD-SYMBOL(<ls_inrec>).
      lv_index = 0.
      APPEND INITIAL LINE TO <lt_outrectemp> ASSIGNING FIELD-SYMBOL(<ls_temp_record>).

      DO.
        lv_index = lv_index + 1.
        ASSIGN it_group_by_fields[ lv_index ] TO FIELD-SYMBOL(<ls_sort_field>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        IF <ls_sort_field>-sortf IS INITIAL.
          EXIT.
        ENDIF.
        ASSIGN COMPONENT <ls_sort_field>-sortf OF STRUCTURE <ls_inrec> TO FIELD-SYMBOL(<lv_component>).
        ASSIGN COMPONENT <ls_sort_field>-sortf OF STRUCTURE <ls_temp_record> TO FIELD-SYMBOL(<lv_temp_comp>).
        <lv_temp_comp> = <lv_component>.
      ENDDO.

      DATA(lv_lines) = lines( <lt_outrectemp> ).
      ASSIGN <lt_outrectemp>[ 1 ] TO  FIELD-SYMBOL(<ls_outrec_temp_first>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      ASSIGN <lt_outrectemp>[ lv_lines ] TO  FIELD-SYMBOL(<ls_outrec_temp_second>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      IF <ls_outrec_temp_first> <> <ls_outrec_temp_second>.
        EXIT.
      ENDIF.

      APPEND INITIAL LINE TO <lt_outrecords> ASSIGNING FIELD-SYMBOL(<ls_outrec>).
      <ls_outrec> = <ls_inrec>.
    ENDLOOP.

    et_outrecords = <lt_outrecords>.

  ENDMETHOD.


  METHOD pick_to_match.
**********************************************************************
*& Key           : RM-230602
*& Request No.   : GAP 35 Picking strategies
**********************************************************************
*& Description (short)
*&
**********************************************************************

    DATA: lt_qmat                 TYPE /scwm/tt_aqua_int,
          lt_qmat_temp            TYPE /scwm/tt_aqua_int,
          lt_group_by_before_pick TYPE ty_sortfld_tt,
          lt_group_by_after_pick  TYPE ty_sortfld_tt,
          lv_index                TYPE i.

    CLEAR: et_qmat_cus, ev_set.

* during rough bin determination do not execute PICK TO MATCH
    IF is_ltap-trart <> wmegc_trart_pick
    OR iv_call = wmegc_call_rough_bin.
      RETURN.
    ENDIF.

    lt_qmat_temp = it_qmat.

    DATA(lt_t334rr) = get_rule_sort_fields(
      iv_lgnum    = is_ltap-lgnum
      iv_rem_rule = iv_rem_rule ).

    DATA(lt_sortf_after_picktomatch) = lt_t334rr.
    ASSIGN lt_t334rr[  sortf = mc_sortf_picktomatch ] TO FIELD-SYMBOL(<ls_t334rr>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    lv_index = sy-tabix.
    DATA(lv_sortf_no) = lines( lt_t334rr ).
* get indexes of fields before and after PICKTOMATCH sorting field
*  resorting is done by groups, therefore we need sorting fields name
    IF lv_index <= lv_sortf_no.
      DELETE lt_t334rr FROM lv_index TO lv_sortf_no.
      DATA(lv_start_del_index) = lv_index - 1.
      IF lv_start_del_index >= 1.
        DELETE lt_sortf_after_picktomatch FROM 1 TO lv_index - 1.
      ENDIF.
    ENDIF.

    lt_group_by_before_pick = VALUE #(
      FOR <ls_t334rr_temp> IN lt_t334rr
      ( sortf = <ls_t334rr_temp>-sortf
        sorto = <ls_t334rr_temp>-sorto ) ).

    lt_group_by_after_pick = VALUE #(
      FOR <ls_t334rr_temp> IN lt_sortf_after_picktomatch
      ( sortf = <ls_t334rr_temp>-sortf
        sorto = <ls_t334rr_temp>-sorto ) ).

    WHILE lines( lt_qmat_temp ) > 0.
      TRY.
          group_by(
            EXPORTING
              it_inrecords       = lt_qmat_temp
              it_group_by_fields = lt_group_by_before_pick
            IMPORTING
              et_outrecords      = lt_qmat ).
          ##NO_HANDLER
        CATCH cx_sy_struct_comp_name.
      ENDTRY.

      LOOP AT lt_qmat ASSIGNING FIELD-SYMBOL(<ls_qmat>).
        DELETE TABLE lt_qmat_temp FROM <ls_qmat>.
      ENDLOOP.

      resort_group(
        EXPORTING iv_target_qty = iv_anfml
                  it_sortf      = lt_group_by_after_pick
        CHANGING  ct_qmat       = lt_qmat ).

      APPEND LINES OF lt_qmat TO et_qmat_cus.
    ENDWHILE.

    IF lines( et_qmat_cus ) > 0.
      ev_set = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD resort_group.
**********************************************************************
*& Key           : RM-230602
*& Request No.   : GAP 35 Picking strategies
**********************************************************************
*& Description (short)
*&
**********************************************************************

    DATA: lt_qmat_diff      TYPE /scwm/tt_aqua_int.

    LOOP AT ct_qmat ASSIGNING FIELD-SYMBOL(<ls_qmat>) WHERE quan IS NOT INITIAL.
      DATA(lv_difference) = CONV /scwm/de_quantity( <ls_qmat>-quan - iv_target_qty ).

      APPEND INITIAL LINE TO lt_qmat_diff ASSIGNING FIELD-SYMBOL(<ls_qmat_diff>).
      IF sy-subrc = 0.
        MOVE-CORRESPONDING <ls_qmat> TO <ls_qmat_diff>.
        <ls_qmat_diff>-zzpicktomatch = lv_difference.
      ENDIF.

    ENDLOOP.

    update_sort_values_picktomatch( CHANGING ct_qmat = lt_qmat_diff ).

    sort_table(
      EXPORTING
        it_fields    = it_sortf
      CHANGING
        ct_inrecords = lt_qmat_diff ).

    REFRESH:  ct_qmat.

    ct_qmat = VALUE #( FOR <ls_qmat_diff_temp> IN lt_qmat_diff
                       ( CORRESPONDING #( <ls_qmat_diff_temp> ) ) ).

  ENDMETHOD.


  METHOD sort_table.
**********************************************************************
*& Key           : RM-230602
*& Request No.   : GAP 35 Picking strategies
**********************************************************************
*& Description (short)
*&
**********************************************************************

    DATA: lv_sortfa TYPE /scwm/de_sortf,
          lv_sortfd TYPE /scwm/de_sortf,
          lv_index  TYPE i.
    DATA:
      BEGIN OF ls_sort,
        a01 TYPE /scwm/de_sortf,
        a02 TYPE /scwm/de_sortf,
        a03 TYPE /scwm/de_sortf,
        a04 TYPE /scwm/de_sortf,
        a05 TYPE /scwm/de_sortf,
        a06 TYPE /scwm/de_sortf,
        a07 TYPE /scwm/de_sortf,
        a08 TYPE /scwm/de_sortf,
        a09 TYPE /scwm/de_sortf,
        a10 TYPE /scwm/de_sortf,
        a11 TYPE /scwm/de_sortf,
        a12 TYPE /scwm/de_sortf,
        a13 TYPE /scwm/de_sortf,
        a14 TYPE /scwm/de_sortf,
        a15 TYPE /scwm/de_sortf,
        a16 TYPE /scwm/de_sortf,
        a17 TYPE /scwm/de_sortf,
        a18 TYPE /scwm/de_sortf,
        a19 TYPE /scwm/de_sortf,
        a20 TYPE /scwm/de_sortf,
        a21 TYPE /scwm/de_sortf,
        a22 TYPE /scwm/de_sortf,
        a23 TYPE /scwm/de_sortf,
        a24 TYPE /scwm/de_sortf,
        a25 TYPE /scwm/de_sortf,
        a26 TYPE /scwm/de_sortf,
        a27 TYPE /scwm/de_sortf,
        a28 TYPE /scwm/de_sortf,
        a29 TYPE /scwm/de_sortf,
        a30 TYPE /scwm/de_sortf,
        a31 TYPE /scwm/de_sortf,
        d01 TYPE /scwm/de_sortf,
        d02 TYPE /scwm/de_sortf,
        d03 TYPE /scwm/de_sortf,
        d04 TYPE /scwm/de_sortf,
        d05 TYPE /scwm/de_sortf,
        d06 TYPE /scwm/de_sortf,
        d07 TYPE /scwm/de_sortf,
        d08 TYPE /scwm/de_sortf,
        d09 TYPE /scwm/de_sortf,
        d10 TYPE /scwm/de_sortf,
        d11 TYPE /scwm/de_sortf,
        d12 TYPE /scwm/de_sortf,
        d13 TYPE /scwm/de_sortf,
        d14 TYPE /scwm/de_sortf,
        d15 TYPE /scwm/de_sortf,
        d16 TYPE /scwm/de_sortf,
        d17 TYPE /scwm/de_sortf,
        d18 TYPE /scwm/de_sortf,
        d19 TYPE /scwm/de_sortf,
        d20 TYPE /scwm/de_sortf,
        d21 TYPE /scwm/de_sortf,
        d22 TYPE /scwm/de_sortf,
        d23 TYPE /scwm/de_sortf,
        d24 TYPE /scwm/de_sortf,
        d25 TYPE /scwm/de_sortf,
        d26 TYPE /scwm/de_sortf,
        d27 TYPE /scwm/de_sortf,
        d28 TYPE /scwm/de_sortf,
        d29 TYPE /scwm/de_sortf,
        d30 TYPE /scwm/de_sortf,
        d31 TYPE /scwm/de_sortf,
      END OF ls_sort.
* up to 30 fields can be supplied
    DO 30 TIMES VARYING lv_sortfa FROM ls_sort-a01 NEXT ls_sort-a02
                VARYING lv_sortfd FROM ls_sort-d01 NEXT ls_sort-d02.
      lv_index = lv_index + 1.
      ASSIGN it_fields[ lv_index ] TO FIELD-SYMBOL(<ls_field>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF <ls_field>-sortf IS INITIAL.
        CONTINUE.
      ENDIF.
*     blank = ascending,   X = descending
      IF <ls_field>-sorto = abap_true.
        lv_sortfd = <ls_field>-sortf.       " descending
      ELSE.
        lv_sortfa = <ls_field>-sortf.       " ascending
      ENDIF.
    ENDDO.

    SORT ct_inrecords
      BY (ls_sort-d01) DESCENDING (ls_sort-a01) ASCENDING
         (ls_sort-d02) DESCENDING (ls_sort-a02) ASCENDING
         (ls_sort-d03) DESCENDING (ls_sort-a03) ASCENDING
         (ls_sort-d04) DESCENDING (ls_sort-a04) ASCENDING
         (ls_sort-d05) DESCENDING (ls_sort-a05) ASCENDING
         (ls_sort-d06) DESCENDING (ls_sort-a06) ASCENDING
         (ls_sort-d07) DESCENDING (ls_sort-a07) ASCENDING
         (ls_sort-d08) DESCENDING (ls_sort-a08) ASCENDING
         (ls_sort-d09) DESCENDING (ls_sort-a09) ASCENDING
         (ls_sort-d10) DESCENDING (ls_sort-a10) ASCENDING
         (ls_sort-d11) DESCENDING (ls_sort-a11) ASCENDING
         (ls_sort-d12) DESCENDING (ls_sort-a12) ASCENDING
         (ls_sort-d13) DESCENDING (ls_sort-a13) ASCENDING
         (ls_sort-d14) DESCENDING (ls_sort-a14) ASCENDING
         (ls_sort-d15) DESCENDING (ls_sort-a15) ASCENDING
         (ls_sort-d16) DESCENDING (ls_sort-a16) ASCENDING
         (ls_sort-d17) DESCENDING (ls_sort-a17) ASCENDING
         (ls_sort-d18) DESCENDING (ls_sort-a18) ASCENDING
         (ls_sort-d19) DESCENDING (ls_sort-a19) ASCENDING
         (ls_sort-d20) DESCENDING (ls_sort-a20) ASCENDING
         (ls_sort-d21) DESCENDING (ls_sort-a21) ASCENDING
         (ls_sort-d22) DESCENDING (ls_sort-a22) ASCENDING
         (ls_sort-d23) DESCENDING (ls_sort-a23) ASCENDING
         (ls_sort-d24) DESCENDING (ls_sort-a24) ASCENDING
         (ls_sort-d25) DESCENDING (ls_sort-a25) ASCENDING
         (ls_sort-d26) DESCENDING (ls_sort-a26) ASCENDING
         (ls_sort-d27) DESCENDING (ls_sort-a27) ASCENDING
         (ls_sort-d28) DESCENDING (ls_sort-a28) ASCENDING
         (ls_sort-d29) DESCENDING (ls_sort-a29) ASCENDING
         (ls_sort-d30) DESCENDING (ls_sort-a30) ASCENDING
         (ls_sort-d31) DESCENDING (ls_sort-a31) ASCENDING.

  ENDMETHOD.


  METHOD update_sort_values_picktomatch.
**********************************************************************
*& Key           : RM-230602
*& Request No.   : GAP 35 Picking strategies
**********************************************************************
*& Description (short)
*&
**********************************************************************
    DATA: lt_exact_qty  TYPE /scwm/tt_aqua_int,
          lt_bigger_qty TYPE /scwm/tt_aqua_int,
          lt_less_qty   TYPE /scwm/tt_aqua_int.

    lt_exact_qty  = VALUE #( FOR <l> IN ct_qmat WHERE ( zzpicktomatch = 0 ) ( <l> ) ).
    lt_bigger_qty = VALUE #( FOR <l> IN ct_qmat WHERE ( zzpicktomatch > 0 ) ( <l> ) ).
    lt_less_qty   = VALUE #( FOR <l> IN ct_qmat WHERE ( zzpicktomatch < 0 ) ( <l> ) ).

    CLEAR: ct_qmat.

    SORT lt_less_qty   BY zzpicktomatch ASCENDING.
    SORT lt_bigger_qty BY zzpicktomatch ASCENDING.

    APPEND LINES OF lt_exact_qty  TO ct_qmat.
    APPEND LINES OF lt_less_qty   TO ct_qmat.
    APPEND LINES OF lt_bigger_qty TO ct_qmat.

    DATA(lv_counter) = 0.
    LOOP AT ct_qmat ASSIGNING FIELD-SYMBOL(<ls_qmat>).
      lv_counter += 1.
      <ls_qmat>-zzpicktomatch = lv_counter.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
