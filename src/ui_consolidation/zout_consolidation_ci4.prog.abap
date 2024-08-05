**********************************************************************
*& Key           : AMOGYORODI-230814
*& Request No.   :
**********************************************************************
*& Description (short)
*&
*&
**********************************************************************
*----------------------------------------------------------------------*
***INCLUDE ZOUT_CONSOLIDATION_CI4.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_event_handler
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_link_click.

    DATA:
      lv_service    TYPE /scwm/de_service,
      lv_hotspot    TYPE /scwm/de_hotspot,
      lv_structure  TYPE /scwm/de_form_st,
      lt_hu_content TYPE ztt_cons_hu_item.

    CASE column.
      WHEN TEXT-002. "DOCNO
        READ TABLE lcl_view=>get_instance( )->mt_cons_headers INTO DATA(ls_dlv) INDEX row.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        DATA(lt_whrhead) = VALUE /scwm/tt_wip_whrhead_out( (
          docno_h = ls_dlv-docno
          doctype = TEXT-007
          docid   = ls_dlv-docid ) ).

        lv_service = TEXT-003.
        lv_hotspot = TEXT-004.
        lv_structure = TEXT-005.

        CALL FUNCTION '/SCWM/JUMP2OBJUI'
          EXPORTING
            iv_lgnum     = lcl_controller=>sv_lgnum
            iv_service   = lv_service
            iv_hotspot   = lv_hotspot
            iv_structure = lv_structure
            it_data      = lt_whrhead.

      WHEN TEXT-021.
        READ TABLE lcl_view=>get_instance( )->mt_cons_hus INTO DATA(ls_hu) INDEX row.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        IF ls_hu-huident = '-'.
          RETURN.
        ENDIF.

        lv_service = TEXT-022.
        lv_hotspot = TEXT-021.
        lv_structure = TEXT-023.

        DATA(lt_huhdr) = VALUE /scwm/tt_huhdr_bin_mon( ( huident = ls_hu-huident ) ).

        CALL FUNCTION '/SCWM/JUMP2OBJUI'
          EXPORTING
            iv_lgnum     = lcl_controller=>sv_lgnum
            iv_service   = lv_service
            iv_hotspot   = lv_hotspot
            iv_structure = lv_structure
            it_data      = lt_huhdr.

      WHEN TEXT-025.
        READ TABLE lcl_view=>get_instance( )->mt_open_picking_hu INTO DATA(ls_pick) INDEX row.
        IF sy-subrc <> 0 OR ls_pick-who IS INITIAL.
          RETURN.
        ENDIF.

        lv_service = TEXT-026.
        lv_hotspot = TEXT-025.
        lv_structure = TEXT-027.

        DATA(lt_who) = VALUE /scwm/tt_wo_det_mon_out( ( who   = ls_pick-who ) ).

        CALL FUNCTION '/SCWM/JUMP2OBJUI'
          EXPORTING
            iv_lgnum     = lcl_controller=>sv_lgnum
            iv_service   = lv_service
            iv_hotspot   = lv_hotspot
            iv_structure = lv_structure
            it_data      = lt_who.
      WHEN TEXT-041. "SELECTION
        DATA(lo_view) = lcl_view=>get_instance( ).
        READ TABLE lo_view->mt_diff_hu_src_edit ASSIGNING FIELD-SYMBOL(<ls_hu>) INDEX row.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        IF <ls_hu>-selector = abap_false.
          <ls_hu>-selector = abap_true.
        ELSE.
          <ls_hu>-selector = abap_false.
        ENDIF.

        DATA(lo_model) = lcl_model=>get_instance( ).

        READ TABLE lo_model->mt_cons_huhdr INTO DATA(ls_huhdr) WITH KEY huident = <ls_hu>-huident.
        IF sy-subrc = 0 AND <ls_hu>-huident <> '-' AND lo_model->is_pallet( is_huhdr = ls_huhdr ).
          CLEAR lt_hu_content.
          lo_model->build_cons_huitem_table( EXPORTING iv_huident = <ls_hu>-huident it_huitem = lo_model->mt_cons_huitm CHANGING ct_hu_content = lt_hu_content ).

          LOOP AT lt_hu_content INTO DATA(ls_hu_content).
            READ TABLE lo_view->mt_diff_hu_src_edit ASSIGNING FIELD-SYMBOL(<ls_subitem>)
              WITH KEY huident = ls_hu_content-huident
                       matid   = ls_hu_content-matid
                       product = ls_hu_content-product
                       ean     = ls_hu_content-ean
                       mpn     = ls_hu_content-mpn
                       descr   = ls_hu_content-descr
                       quan    = ls_hu_content-quan
                       uom     = ls_hu_content-uom
                       mc      = ls_hu_content-mc
                       serial_status = ls_hu_content-serial_status
                       huident_top = ls_hu_content-huident_top
                       higher_guid = ls_hu_content-higher_guid.
            IF sy-subrc = 0.
              IF <ls_hu>-selector = abap_true.
                <ls_subitem>-selector = abap_true.
              ELSE.
                <ls_subitem>-selector = abap_false.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
*          LOOP AT lo_model->mt_cons_huhdr INTO DATA(ls_subhuhdr)
*            WHERE higher_guid = ls_huhdr-guid_hu.
*            LOOP AT lo_view->mt_diff_hu_src_edit ASSIGNING FIELD-SYMBOL(<ls_subhu>)
*              WHERE huident = ls_subhuhdr-huident.
*              IF <ls_hu>-selection = abap_true.
*                <ls_subhu>-selection = abap_true.
*              ELSE.
*                <ls_subhu>-selection = abap_false.
*              ENDIF.
*            ENDLOOP.
*          ENDLOOP.
*        ENDIF.

        IF lo_view->mo_alv_diff_exp IS NOT INITIAL.
          lo_view->mo_alv_diff_exp->refresh( refresh_mode = if_salv_c_refresh=>full ).
        ENDIF.

      WHEN OTHERS.
        RETURN.
    ENDCASE.

  ENDMETHOD.

  METHOD on_user_command.

    CASE e_salv_function.
      WHEN TEXT-012. "Process

        DATA(lo_selections) = lcl_view=>get_instance( )->mo_alv_cons_headers->get_selections( ).
        DATA(lt_rows) = lo_selections->get_selected_rows( ).

        READ TABLE lt_rows INTO DATA(lv_row) INDEX 1.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        READ TABLE lcl_view=>get_instance( )->mt_cons_headers INTO DATA(ls_line) INDEX lv_row.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        DATA(lo_model) = lcl_model=>get_instance( ).
        lo_model->ms_cons_header = ls_line.
        lo_model->build_cons_hu_table( ls_line-docid ).
        DATA(lo_view) = lcl_view=>get_instance( ).

        CLEAR zstr_consolidation_scr_0200.
        MOVE-CORRESPONDING ls_line TO zstr_consolidation_scr_0200.
        LEAVE TO SCREEN 0200.

      WHEN TEXT-028.
        lcl_controller=>get_instance( )->handle_count_exp( ).
      WHEN TEXT-036.
        "Capture - SN
        lcl_controller=>get_instance( )->handle_capture_serial( ).
      WHEN TEXT-043.
        " Prod.Scan - SN
        lcl_controller=>get_instance( )->handle_prod_scan_serial( ).
      WHEN TEXT-037.
        lcl_controller=>get_instance( )->handle_delete_serial( ).
      WHEN TEXT-038.
        lcl_controller=>get_instance( )->handle_delete_all_serial( ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
