CLASS zcl_out_who_priority DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Path sequence score for WHO.</p>
      BEGIN OF ty_pathseq_cat,
        who        TYPE /scwm/de_who,
        pathseq_fr TYPE int8,
        pathseq_to TYPE int8,
        category   TYPE int8,
      END OF ty_pathseq_cat,
      "! <p class="shorttext synchronized" lang="en">Path sequence score for WHO.</p>
      tt_pathseq_cat TYPE STANDARD TABLE OF ty_pathseq_cat WITH EMPTY KEY.

    METHODS who_find_prio
      IMPORTING
        !it_ordim_o    TYPE /scwm/tt_ordim_o_int
      RETURNING
        VALUE(rv_prio) TYPE /scwm/de_prior .

    METHODS calculate_who_heatmap
      IMPORTING
        iv_lgnum       TYPE /scwm/lgnum
        !iv_who        TYPE /scwm/de_who
      RETURNING
        VALUE(rs_whos) TYPE ty_pathseq_cat.

    METHODS sort_who_by_heatmap
      IMPORTING
        iv_lgnum             TYPE /scwm/lgnum
        iv_who_base          TYPE /scwm/de_who
        it_who_list          TYPE /scwm/tt_wo_det_mon
      RETURNING
        VALUE(rt_who_sorted) TYPE /scwm/tt_wo_det_mon.


    METHODS get_next_who_in_pathseq
      IMPORTING
        iv_lgnum     TYPE /scwm/lgnum
        iv_start_bin TYPE /scwm/de_lgpla
        iv_queue     TYPE /scwm/de_queue
      EXPORTING
        es_who       TYPE /scwm/s_wo_det_mon
        et_ordim_o   TYPE /scwm/tt_ordim_o .


  PRIVATE SECTION.
    METHODS sort_whos_by_sap_algo
      IMPORTING
        iv_lgnum       TYPE /scwm/lgnum
        !it_who_list   TYPE /scwm/tt_wo_det_mon
      RETURNING
        VALUE(rt_whos) TYPE tt_pathseq_cat.

    METHODS calc_category
      IMPORTING
        !iv_max          TYPE int1
        !iv_a            TYPE int8
        !iv_b            TYPE int8
        !iv_offset       TYPE int1
      RETURNING
        VALUE(rv_result) TYPE int8 .
ENDCLASS.



CLASS ZCL_OUT_WHO_PRIORITY IMPLEMENTATION.


  METHOD calculate_who_heatmap.
********************************************************************
*& Key          : BSUGAREV-Jan 11, 2024
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  :
*&
********************************************************************
    CONSTANTS: lc_max_degree TYPE int1 VALUE 34.

    DATA: lt_ordim_o TYPE /scwm/tt_ordim_o.
    DATA: lt_who_data TYPE tt_pathseq_cat.

    FIELD-SYMBOLS: <ls_who_data> TYPE ty_pathseq_cat.

    CALL FUNCTION '/SCWM/TO_READ_MULT'
      EXPORTING
        iv_lgnum         = iv_lgnum
        ir_who           = VALUE rseloption( ( sign   = wmegc_sign_inclusive
                                               option = wmegc_option_eq
                                               low    = iv_who ) )
        iv_add_to_memory = 'X'
      IMPORTING
        et_ordim_o       = lt_ordim_o
      EXCEPTIONS
        wrong_input      = 1
        not_found        = 2
        OTHERS           = 3.
    IF sy-subrc <> 0 OR lines( lt_ordim_o ) = 0.
      RETURN.
    ENDIF.

    DATA(lv_pathseq_min) = REDUCE int8(  INIT lv_min = 2 ** lc_max_degree
                                           FOR ls_wt IN lt_ordim_o
                                           WHERE ( pathseq IS NOT INITIAL )
                                           NEXT lv_min = COND #( WHEN ls_wt-pathseq < lv_min
                                                                 THEN ls_wt-pathseq ELSE lv_min )  ).

    DATA(lv_pathseq_max) = REDUCE int8(  INIT lv_max = 1
                                           FOR ls_wt IN lt_ordim_o
                                           WHERE ( pathseq IS NOT INITIAL )
                                           NEXT lv_max = COND #( WHEN ls_wt-pathseq > lv_max
                                                                 THEN ls_wt-pathseq ELSE lv_max )  ).

    DATA(lv_pathseq_range) = VALUE int8( ).
    lv_pathseq_range = lv_pathseq_max - lv_pathseq_min + 1.

    DATA(lv_degree) = CONV int1(
        strlen( /ui2/cl_number=>base_converter( number = lv_pathseq_max from = 10 to = 2 ) ) ).

    IF lv_degree > lc_max_degree.
      lv_degree = lc_max_degree.
    ENDIF.

    DATA(lv_pathseq_total) = COND int8( WHEN lv_pathseq_max <= 2 ** lv_degree THEN 2 ** lv_degree
                                        ELSE 2 ** ( lv_degree + 1 ) ).

    " Calculate starting and ending pathseq for every who group
    LOOP AT lt_ordim_o ASSIGNING FIELD-SYMBOL(<ls_to>).
      IF <ls_who_data> IS NOT ASSIGNED OR <ls_who_data>-who <> <ls_to>-who.

        IF line_exists( lt_who_data[ who = <ls_to>-who ] ).
          ASSIGN lt_who_data[ who = <ls_to>-who ] TO <ls_who_data>.
        ELSE.
          APPEND VALUE #( who = <ls_to>-who pathseq_fr = <ls_to>-pathseq pathseq_to = <ls_to>-pathseq )
              TO lt_who_data ASSIGNING <ls_who_data>.
          CONTINUE.
        ENDIF.
      ENDIF.

      " Check pathseq is less than start
      IF <ls_to>-pathseq < <ls_who_data>-pathseq_fr.
        <ls_who_data>-pathseq_fr = <ls_to>-pathseq.
      ENDIF.

      " Check pathseq is higher than end
      IF <ls_to>-pathseq > <ls_who_data>-pathseq_fr.
        <ls_who_data>-pathseq_to = <ls_to>-pathseq.
      ENDIF.

    ENDLOOP.

    " Adjustment to the scale from 1 to 2^max_degree coefficient
    DATA(lv_adjustment) = VALUE f( ).

    TRY.
        lv_adjustment = lv_pathseq_total / lv_pathseq_range.
      CATCH cx_sy_zerodivide.
    ENDTRY.

    " Calculate category for every WHO
    LOOP AT lt_who_data ASSIGNING <ls_who_data>.
      " convert pathseq

      <ls_who_data>-pathseq_fr = COND #(
          WHEN  <ls_who_data>-pathseq_fr = lv_pathseq_min THEN 0
          WHEN <ls_who_data>-pathseq_fr = lv_pathseq_max  THEN 2 ** ( lv_degree - 1 )
          ELSE ( ( <ls_who_data>-pathseq_fr - lv_pathseq_min  ) * lv_adjustment  ) ).


      <ls_who_data>-pathseq_to = COND #(
          WHEN  <ls_who_data>-pathseq_to = lv_pathseq_min THEN 0
          WHEN <ls_who_data>-pathseq_to = lv_pathseq_max  THEN 2 ** ( lv_degree - 1 )
          ELSE ( ( <ls_who_data>-pathseq_to - lv_pathseq_min  ) * lv_adjustment  ) ).

      <ls_who_data>-category = calc_category( iv_max = ( lv_degree + 1 )
                                              iv_a = <ls_who_data>-pathseq_fr
                                              iv_b = <ls_who_data>-pathseq_to
                                              iv_offset = ( lv_degree + 1 ) ).
    ENDLOOP.

    IF lines( lt_who_data ) = 0.
      RETURN.
    ENDIF.

*    " sort groups
*    SORT lt_who_data BY category pathseq_fr who.

    rs_whos = lt_who_data[ 1 ].

  ENDMETHOD.


  METHOD calc_category.
********************************************************************
*& Key          : BSUGAREV-Jan 12, 2024
*& Request No.  : GAP-050 RF Picking for Shipping Cartons
*&                ​​GAP-051 RF Picking transaction for KEP Master Cartons
*&                GAP-087 RF Conveyor-belt Area
********************************************************************
*& Description  :
*&
*&
********************************************************************
    CHECK iv_offset > 0.

    IF iv_a = iv_b.
      rv_result = COND #( WHEN iv_max = iv_offset AND iv_a = 0 THEN 1
                          ELSE ( iv_a * ( 2 ** ( iv_max - iv_offset ) ) +
                               COND i( WHEN iv_max = iv_offset THEN 0 ELSE 2 ** ( iv_max - iv_offset - 1 ) ) ) ).
      RETURN.
    ELSE.
      rv_result = calc_category( iv_max    = iv_max
                                 iv_a      = iv_a DIV 2
                                 iv_b      = iv_b DIV 2
                                 iv_offset = CONV int1( iv_offset - 1 ) ).
    ENDIF.
  ENDMETHOD.


  METHOD get_next_who_in_pathseq.
********************************************************************
*& Key          : BSUGAREV-Jan 25, 2024
*& Request No.  : GAP-076 Outbound Priority settings
********************************************************************
*& Description  : Find next WHO considering path sequence in tasks
*&
********************************************************************
    TYPES:
      BEGIN OF ty_who_sort_criteria,
        pathseq  TYPE int8,
        priority TYPE int8,
        who      TYPE /scwm/de_who,
      END OF ty_who_sort_criteria.

    DATA:
      ##NEEDED
      lv_msg           TYPE string,
      ls_bin_area      TYPE /scwm/lagps,
      lt_whohu         TYPE /scwm/tt_whohu_int,
      lt_ordim_o       TYPE /scwm/tt_ordim_o,
      lt_who_sort_crit TYPE STANDARD TABLE OF ty_who_sort_criteria WITH EMPTY KEY.

    CLEAR: es_who, et_ordim_o.

    " select next WHO propolsal based on SAP algorithm
    DATA(lo_who) = NEW zcl_whse_order( iv_lgnum = iv_lgnum
                                       it_selection_parameters = VALUE #(
                                       ( field = zif_whse_order=>wo_mapping_fieldname-queue
                                         select_options =  VALUE #( ( sign   = wmegc_sign_inclusive
                                                                      option = wmegc_option_eq
                                                                      low    = iv_queue ) ) )
                                       ( field = zif_whse_order=>wo_mapping_fieldname-status
                                         select_options =  VALUE #( ( sign   = wmegc_sign_inclusive
                                                                      option = wmegc_option_eq
                                                                      low    = zif_whse_order=>wo_status-open ) ) ) ) ).

    lo_who->wo_data_select( IMPORTING et_who = DATA(lt_whos) ).

    DELETE lt_whos WHERE zz_bundled = abap_true.

    IF lines( lt_whos ) = 0.
      MESSAGE e056(/scwm/rf_en) INTO lv_msg.

      RETURN.
    ENDIF.

    DATA(lt_who_sel) = VALUE /scwm/tt_whoid( FOR <w> IN lt_whos ( who = <w>-who ) ).

    TRY.
        CALL FUNCTION '/SCWM/WHO_SELECT'
          EXPORTING
            iv_to      = abap_true
            iv_lgnum   = iv_lgnum
            it_who     = lt_who_sel
          IMPORTING
            et_whohu   = lt_whohu
            et_ordim_o = lt_ordim_o.
      CATCH /scwm/cx_core.
    ENDTRY.

    " remove WHOs which already have pickHU created
    DATA(lt_who_selopt) = VALUE rseloption(
      FOR <wh> IN lt_whohu WHERE ( huident IS NOT INITIAL )
      ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <wh>-who ) ).

    IF lines( lt_who_selopt ) > 0.
      DELETE lt_whos WHERE who IN lt_who_selopt.
      DELETE lt_ordim_o WHERE who IN lt_who_selopt.
    ENDIF.

    " check that there is WHO in the scanned bin
    DATA(ls_scanned_data) = VALUE #( lt_ordim_o[ vlpla = iv_start_bin ] OPTIONAL ).
    IF ls_scanned_data IS INITIAL.

      CALL FUNCTION '/SCWM/LAGPS_READ_SINGLE'
        EXPORTING
          iv_lgnum    = iv_lgnum
          iv_lgpla    = iv_start_bin
          iv_actty    = VALUE #( lt_ordim_o[ 1 ]-act_type )
        IMPORTING
          es_lagps    = ls_bin_area
        EXCEPTIONS
          wrong_input = 1
          not_found   = 2
          OTHERS      = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.

        RETURN.
      ENDIF.

      ls_scanned_data-pathseq = ls_bin_area-srt_nr.
    ENDIF.

    " select WHOs priority
    DATA(lt_who_prio) = zcl_crud_scwm_wo_rsrc_ty=>select_multi_by_who(
        iv_lgnum      = iv_lgnum
        it_who_selopt = VALUE #( FOR <whl> IN lt_whos
                               ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <whl>-who ) ) ).

    SORT lt_who_prio BY who.
    DELETE ADJACENT DUPLICATES FROM lt_who_prio COMPARING who.

    " build support table for sorting WOs by requested criteria
    LOOP AT lt_ordim_o ASSIGNING FIELD-SYMBOL(<ls_task>).

      CHECK NOT line_exists( lt_who_sort_crit[ who = <ls_task>-who ] ).

      lt_who_sort_crit = VALUE #( BASE lt_who_sort_crit
        ( pathseq = <ls_task>-pathseq
          priority = VALUE #( lt_who_prio[ who = <ls_task>-who ]-priority OPTIONAL )
          who = <ls_task>-who  ) ).
    ENDLOOP.

    " sort WHOs and return the first that match the criteria
    SORT lt_who_sort_crit BY pathseq ASCENDING priority ASCENDING.

    LOOP AT lt_who_sort_crit ASSIGNING FIELD-SYMBOL(<ls_who>)
                                 WHERE ( pathseq >= ls_scanned_data-pathseq ).
      es_who = lt_whos[ who = <ls_who>-who ].
      EXIT.
    ENDLOOP.

    IF es_who IS INITIAL.
      MESSAGE e056(/scwm/rf_en) INTO lv_msg.

      RETURN.
    ENDIF.

    et_ordim_o = VALUE #( FOR <t> IN lt_ordim_o WHERE ( who = es_who-who ) ( <t> ) ).
  ENDMETHOD.


  METHOD sort_whos_by_sap_algo.
********************************************************************
*& Key          : BSUGAREV-Jan 3, 2024
*& Request No.  : Sort WHOs by SAP algorithm
********************************************************************
*& Description  :
*&
********************************************************************
    CONSTANTS: lc_max_degree TYPE int1 VALUE 34.

    DATA: lt_ordim_o TYPE /scwm/tt_ordim_o.
    DATA: lt_who_data TYPE tt_pathseq_cat.

    FIELD-SYMBOLS: <ls_who_data> TYPE ty_pathseq_cat.

    CALL FUNCTION '/SCWM/TO_READ_MULT'
      EXPORTING
        iv_lgnum         = iv_lgnum
        ir_who           = VALUE rseloption( FOR <l> IN it_who_list
                                             ( sign   = wmegc_sign_inclusive
                                               option = wmegc_option_eq
                                               low    = <l>-who ) )
        iv_add_to_memory = 'X'
      IMPORTING
        et_ordim_o       = lt_ordim_o
      EXCEPTIONS
        wrong_input      = 1
        not_found        = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lv_pathseq_min) = REDUCE int8(  INIT lv_min = 2 ** lc_max_degree
                                           FOR ls_wt IN lt_ordim_o
                                           WHERE ( pathseq IS NOT INITIAL )
                                           NEXT lv_min = COND #( WHEN ls_wt-pathseq < lv_min
                                                                 THEN ls_wt-pathseq ELSE lv_min )  ).

    DATA(lv_pathseq_max) = REDUCE int8(  INIT lv_max = 1
                                           FOR ls_wt IN lt_ordim_o
                                           WHERE ( pathseq IS NOT INITIAL )
                                           NEXT lv_max = COND #( WHEN ls_wt-pathseq > lv_max
                                                                 THEN ls_wt-pathseq ELSE lv_max )  ).

    DATA(lv_pathseq_range) = VALUE int8( ).
    lv_pathseq_range = lv_pathseq_max - lv_pathseq_min + 1.

    DATA(lv_degree) = CONV int1(
        strlen( /ui2/cl_number=>base_converter( number = lv_pathseq_max from = 10 to = 2 ) ) ).

    IF lv_degree > lc_max_degree.
      lv_degree = lc_max_degree.
    ENDIF.

    DATA(lv_pathseq_total) = COND int8( WHEN lv_pathseq_max <= 2 ** lv_degree THEN 2 ** lv_degree
                                        ELSE 2 ** ( lv_degree + 1 ) ).

    " Calculate starting and ending pathseq for every WHO group
    LOOP AT lt_ordim_o ASSIGNING FIELD-SYMBOL(<ls_to>).

      IF <ls_who_data> IS NOT ASSIGNED OR <ls_who_data>-who <> <ls_to>-who.

        IF line_exists( lt_who_data[ who = <ls_to>-who ] ).
          ASSIGN lt_who_data[ who = <ls_to>-who ] TO <ls_who_data>.
        ELSE.
          APPEND VALUE #( who = <ls_to>-who pathseq_fr = <ls_to>-pathseq pathseq_to = <ls_to>-pathseq )
              TO lt_who_data ASSIGNING <ls_who_data>.
          CONTINUE.
        ENDIF.
      ENDIF.

      " Check pathseq is less than start
      IF <ls_to>-pathseq < <ls_who_data>-pathseq_fr.
        <ls_who_data>-pathseq_fr = <ls_to>-pathseq.
      ENDIF.

      " Check pathseq is higher than end
      IF <ls_to>-pathseq > <ls_who_data>-pathseq_fr.
        <ls_who_data>-pathseq_to = <ls_to>-pathseq.
      ENDIF.

    ENDLOOP.

    CHECK lines( lt_who_data ) > 1.

    " Adjustment to the scale from 1 to 2^max_degree coefficient
    DATA(lv_adjustment) = VALUE f( ).
    TRY.
        lv_adjustment = lv_pathseq_total / lv_pathseq_range.
      CATCH cx_sy_zerodivide.
    ENDTRY.

    " Calculate category for every WHO group
    LOOP AT lt_who_data ASSIGNING <ls_who_data>.
      " convert path sequence
      <ls_who_data>-pathseq_fr = COND #(
          WHEN  <ls_who_data>-pathseq_fr = lv_pathseq_min THEN 0
          WHEN <ls_who_data>-pathseq_fr = lv_pathseq_max  THEN 2 ** ( lv_degree - 1 )
          ELSE ( ( <ls_who_data>-pathseq_fr - lv_pathseq_min  ) * lv_adjustment  ) ).


      <ls_who_data>-pathseq_to = COND #(
          WHEN  <ls_who_data>-pathseq_to = lv_pathseq_min THEN 0
          WHEN <ls_who_data>-pathseq_to = lv_pathseq_max  THEN 2 ** ( lv_degree - 1 )
          ELSE ( ( <ls_who_data>-pathseq_to - lv_pathseq_min  ) * lv_adjustment  ) ).

      <ls_who_data>-category = calc_category( iv_max = ( lv_degree + 1 )
                                              iv_a = <ls_who_data>-pathseq_fr
                                              iv_b = <ls_who_data>-pathseq_to
                                              iv_offset = ( lv_degree + 1 ) ).
    ENDLOOP.

    " sort groups
    SORT lt_who_data BY category pathseq_fr who.

    rt_whos = lt_who_data.
  ENDMETHOD.


  METHOD sort_who_by_heatmap.
********************************************************************
*& Key          : BSUGAREV-Jan 12, 2024
*& Request No.  : GAP-076 Outbound Priority settings
********************************************************************
*& Description  : Sort list of provided WOs by SAP algorithm
*&     In the list of WOs should be also included the base/main WO,
*&     which is the starting point for determining the next WO based
*&     on the oil slick scenario
*&     -> WO data is enhanced with 2 fields: zz_pathseq_fr and zz_category
*&        Here we also set the update of these fields for the main WO
*&        during the first calculation of the pathseq and category
*&        from SAP algorithm. This is needed because WO could be merged
*&        and we must keep the starting point of the main WO
********************************************************************
    TYPES:
      BEGIN OF ty_who_delta,
        who          TYPE /scwm/de_who,
        prio         TYPE /scwm/de_prio_who,
        pathfr_delta TYPE int8,
        cat_delta    TYPE int8,
      END OF ty_who_delta.

    DATA: lt_who_delta TYPE STANDARD TABLE OF ty_who_delta WITH EMPTY KEY.

    " Sorting is not needed
    IF lines( it_who_list ) < 2.
      RETURN.
    ENDIF.
    " calculate heatmap of the base order
*    DATA(ls_who_base_heatmap) = calculate_who_heatmap( iv_lgnum = iv_lgnum
*                                                       iv_who   = iv_who_base ).

*    DATA(lt_who_list) = it_who_list.
*
*    DELETE lt_who_list WHERE who = iv_who_base.

    " sort WHO list by SAP algorithm
    DATA(lt_who_list_sort) = sort_whos_by_sap_algo( iv_lgnum    = iv_lgnum
                                                    it_who_list = it_who_list ).

    FINAL(ls_who_base) = VALUE #( it_who_list[ who = iv_who_base ] OPTIONAL ).

    " check if custom fields have been updated. In this case use data from DB otherwise
    "  we are using calculated values in lt_who_list_sort
    " It could happen PATHSEQ_FR = 0, that is why CATEGORY is also checked
    IF ls_who_base-zz_pathseq_fr IS INITIAL AND ls_who_base-zz_category IS INITIAL.
      DATA(ls_who_base_heatmap) = VALUE #( lt_who_list_sort[ who = iv_who_base ] OPTIONAL ).

    ELSE.
      ls_who_base_heatmap = VALUE ty_pathseq_cat( who        = ls_who_base-who
                                                  pathseq_fr = ls_who_base-zz_pathseq_fr
                                                  category   = ls_who_base-zz_category ).
    ENDIF.

    " main WHO is not needed in the list of WHOs anymore. As a result we need a sorted list of
    "  all others WHOs
    DELETE lt_who_list_sort WHERE who = ls_who_base_heatmap-who.

    " set Z fields in WHO for update. They will be updated in BADI /SCWM/EX_WHO_EEW_CHANGE
    zcl_who_eew_change=>set_pathseq_category(
        is_who_eew = VALUE #( lgnum = iv_lgnum
                              who = ls_who_base_heatmap-who
                              zz_pathseq_fr = ls_who_base_heatmap-pathseq_fr
                              zz_category   = ls_who_base_heatmap-category ) ).

    " select WHOs priority
    DATA(lt_who_prio) = zcl_crud_scwm_wo_rsrc_ty=>select_multi_by_who(
        iv_lgnum      = iv_lgnum
        it_who_selopt = VALUE #( FOR <whl> IN it_who_list
                               ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <whl>-who ) ) ).

    SORT lt_who_prio BY who.
    DELETE ADJACENT DUPLICATES FROM lt_who_prio COMPARING who.

    " combine sorting parameters for WHOs in one table
    lt_who_delta = VALUE #( FOR <l>  IN lt_who_list_sort
                            FOR <wp> IN lt_who_prio WHERE ( who = <l>-who )
        ( who  = <l>-who
          prio = <wp>-priority
          pathfr_delta = abs( <l>-pathseq_fr - ls_who_base_heatmap-pathseq_fr )
          cat_delta    = abs( <l>-category - ls_who_base_heatmap-category ) ) ).

    SORT lt_who_delta BY prio pathfr_delta cat_delta.

    rt_who_sorted = VALUE #( FOR <ws> IN lt_who_delta
                             FOR <whoa> IN it_who_list WHERE ( who = <ws>-who ) ( <whoa> ) ).
  ENDMETHOD.


  METHOD who_find_prio.
**********************************************************************
*& Key           : RM-230120
*& Request No.   : GAP-076 FD Priority settings
**********************************************************************
*& Description (short)
*& Pick the WO with highest prio and earliest LSD
**********************************************************************

    DATA:
           lo_dlv TYPE REF TO /scwm/cl_dlv_management_prd.

    lo_dlv = /scwm/cl_dlv_management_prd=>get_instance( ).

    TRY.
        lo_dlv->query(
          EXPORTING
            it_docid        = VALUE #( FOR <ls_ordim> IN it_ordim_o
                                      ( VALUE /scwm/dlv_docid_item_str( docid  = <ls_ordim>-rdocid
                                                                        itemid = <ls_ordim>-ritmid
                                                                        doccat = <ls_ordim>-rdoccat ) ) )
            is_read_options = VALUE /scwm/dlv_query_contr_str( mix_in_object_instances = abap_true data_retrival_only = abap_true )
          IMPORTING
            et_items        = DATA(lt_items) ).
      CATCH /scdl/cx_delivery INTO DATA(lx_data).
        RETURN.
    ENDTRY.

    SORT lt_items BY delterm-priority ASCENDING.

    rv_prio = VALUE #( lt_items[ 1 ]-delterm-priority OPTIONAL ).

  ENDMETHOD.
ENDCLASS.
