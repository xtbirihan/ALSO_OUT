**********************************************************************
*& Key           : AMOGYORODI-230814
*& Request No.   :
**********************************************************************
*& Description (short)
*&
*&
**********************************************************************
*----------------------------------------------------------------------*
***INCLUDE ZOUT_CONSOLIDATION_CI3.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_report_view
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_view IMPLEMENTATION.
  METHOD get_instance.
    IF so_instance IS INITIAL.
      CREATE OBJECT so_instance.
    ENDIF.
    ro_instance = so_instance.
  ENDMETHOD.

  METHOD show_alv_serial.

    DATA:
      lv_get_sn_huidet TYPE /scwm/de_huident,
      lo_columns       TYPE REF TO cl_salv_columns_table,
      lo_column        TYPE REF TO cl_salv_column_table,
      lo_event_handler TYPE REF TO lcl_event_handler.

    CLEAR lv_get_sn_huidet.

    DATA(lo_model) = lcl_model=>get_instance( ).

    IF ms_cons_hu_serial-huident = '-'.

      ASSIGN lo_model->mt_cons_huhdr[ guid_hu =  ms_cons_hu_serial-guid_hu ]-huident TO FIELD-SYMBOL(<lv_drop_stock>).

      IF sy-subrc = 0.
        lv_get_sn_huidet = <lv_drop_stock>.
      ENDIF.
    ELSE.
      lv_get_sn_huidet =  ms_cons_hu_serial-huident.
    ENDIF.

    mt_serial = lcl_model=>get_instance( )->get_serials_for_hu( iv_huident =  lv_get_sn_huidet
                                                                iv_group_mc = ms_cons_hu_serial-group_mc )."zstr_consolidation_scr_0600-packhu ).

    IF mo_alv_serial IS NOT BOUND.
      TRY.
          cl_salv_table=>factory(
            EXPORTING
              r_container    = io_container
            IMPORTING
              r_salv_table   = mo_alv_serial
            CHANGING
              t_table        = mt_serial ).
        CATCH cx_salv_msg.                              "#EC NO_HANDLER
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDTRY.

      DATA(lo_selections) = mo_alv_serial->get_selections( ).
      lo_selections->set_selection_mode( if_salv_c_selection_mode=>multiple ).

      lo_columns = mo_alv_serial->get_columns( ).
      lo_columns->set_optimize( abap_true ).

      TRY.
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-032 ).  " LGNUM
          lo_column->set_visible( abap_false ).

          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-002 ).  " DOCNO
          lo_column->set_visible( abap_false ).

*          CLEAR lo_column.
*          lo_column ?= lo_columns->get_column( TEXT-033 ).  " SERIAL
*          lo_column->set_visible( abap_false ).

          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-035 ).  " ITEMNO
          lo_column->set_visible( abap_false ).

          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-034 ).  " GUID_HU
          lo_column->set_visible( abap_false ).
        CATCH cx_salv_not_found ##NO_HANDLER.
      ENDTRY.

      TRY.
          DATA(lo_functions) = mo_alv_serial->get_functions( ).
          lo_functions->set_all( space ).
          lo_functions->add_function(
            EXPORTING
              name     = TEXT-037
              icon     = CONV #( icon_execute_object )
              text     = CONV #( TEXT-037 )
              tooltip  = CONV #( TEXT-037 )
              position = 1 ).
          lo_functions->add_function(
            EXPORTING
              name     = TEXT-038
              icon     = CONV #( icon_execute_object )
              text     = CONV #( TEXT-038 )
              tooltip  = CONV #( TEXT-038 )
              position = 1 ).
        CATCH cx_salv_existing ##NO_HANDLER.   " ALV: General Error Class (Checked in Syntax Check)
        CATCH cx_salv_wrong_call ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
      ENDTRY.

      DATA(lo_events) = mo_alv_serial->get_event( ).
      CREATE OBJECT lo_event_handler.
      SET HANDLER lo_event_handler->on_user_command FOR lo_events.

      mo_alv_serial->display( ).
    ELSE.
      mo_alv_serial->refresh( refresh_mode = if_salv_c_refresh=>full ).
    ENDIF.
  ENDMETHOD.

  METHOD show_alv_cons_headers.

    DATA:
      lo_columns       TYPE REF TO cl_salv_columns_table,
      lo_column        TYPE REF TO cl_salv_column_table,
      lo_event_handler TYPE REF TO lcl_event_handler.

    mt_cons_headers = lcl_model=>get_instance( )->mt_cons_headers.

    IF mo_alv_cons_headers IS NOT BOUND.
      TRY.
          cl_salv_table=>factory(
            EXPORTING
              r_container    = io_container
            IMPORTING
              r_salv_table   = mo_alv_cons_headers
            CHANGING
              t_table        = mt_cons_headers ).
        CATCH cx_salv_msg.                              "#EC NO_HANDLER
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDTRY.

      DATA(lo_selections) = mo_alv_cons_headers->get_selections( ).
      lo_selections->set_selection_mode( if_salv_c_selection_mode=>single ).

      lo_columns = mo_alv_cons_headers->get_columns( ).
      lo_columns->set_optimize( abap_true ).

      TRY.
          DATA(lo_functions) = mo_alv_cons_headers->get_functions( ).
          lo_functions->add_function(
            EXPORTING
              name     = TEXT-012
              icon     = CONV #( icon_execute_object )
              text     = CONV #( TEXT-012 )
              tooltip  = CONV #( TEXT-012 )
              position = 1 ).
        CATCH cx_salv_existing ##NO_HANDLER.   " ALV: General Error Class (Checked in Syntax Check)
        CATCH cx_salv_wrong_call ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
      ENDTRY.

      TRY.
          lo_column ?= lo_columns->get_column( TEXT-002 ).  " DOCNO
          lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-006 ).  " DOCID
          lo_column->set_visible( abap_false ).

          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-024 ).  " PARTYNO
          lo_column->set_visible( abap_false ).

          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-039 ).  " CARRIER
          lo_column->set_visible( abap_false ).

        CATCH cx_salv_not_found ##NO_HANDLER.
      ENDTRY.

      DATA(lo_events) = mo_alv_cons_headers->get_event( ).
      CREATE OBJECT lo_event_handler.
      SET HANDLER lo_event_handler->on_link_click FOR lo_events.
      SET HANDLER lo_event_handler->on_user_command FOR lo_events.

      mo_alv_cons_headers->display( ).
    ELSE.
      mo_alv_cons_headers->refresh( refresh_mode = if_salv_c_refresh=>full ).
    ENDIF.
  ENDMETHOD.


  METHOD show_pack_inst.
    " Andriyan Yordanov pack. inst.

    DATA(lo_model) = lcl_model=>get_instance( ).
    DATA(lv_scr_name_cont) =  io_container->get_name( ).

    IF mo_pack_instr_te IS BOUND AND
       mo_pack_instr_te->parent->get_name( ) <> io_container->get_name( ).
      " for the popup we will rebuild the packinst. obj.
      CLEAR mo_pack_instr_te.
    ENDIF.

    IF mo_pack_instr_te IS NOT BOUND.
      mo_pack_instr_te = NEW #(
           parent                     = io_container
           wordwrap_mode              = cl_gui_textedit=>wordwrap_at_windowborder
           wordwrap_to_linebreak_mode = cl_gui_textedit=>false ).

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      mo_pack_instr_te->set_readonly_mode(  ).
      mo_pack_instr_te->set_toolbar_mode( '0' ).
    ENDIF.

    mo_pack_instr_te->set_textstream( EXPORTING text =  COND #( WHEN iv_dlv_level = abap_true AND
                                                                     iv_product_level = abap_false
                                                                  THEN lo_model->get_text_dlv( )
                                                                WHEN iv_product_level = abap_true AND
                                                                     iv_dlv_level = abap_false
                                                                  THEN lo_model->get_text_hu( )
                                                                WHEN iv_dlv_level = abap_true AND
                                                                     iv_product_level = abap_true
                                                                  THEN lo_model->get_text_dlv_hu( ) )
                                      EXCEPTIONS error_cntl_call_method = 1
                                                 not_supported_by_gui   = 2
                                                 OTHERS                 = 3 ).
    IF sy-subrc <> 0.

    ENDIF.

  ENDMETHOD.

  METHOD show_alv_cons_hus.

    DATA:
      lo_columns       TYPE REF TO cl_salv_columns_table,
      lo_column        TYPE REF TO cl_salv_column_table,
      lo_event_handler TYPE REF TO lcl_event_handler.

    mt_cons_hus = lcl_model=>get_instance( )->mt_cons_hus.

    IF mo_alv_cons_hus IS NOT BOUND.
      TRY.
          cl_salv_table=>factory(
            EXPORTING
              r_container    = io_container
            IMPORTING
              r_salv_table   = mo_alv_cons_hus
            CHANGING
              t_table        = mt_cons_hus ).
        CATCH cx_salv_msg.                              "#EC NO_HANDLER
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDTRY.

      DATA(lo_selections) = mo_alv_cons_hus->get_selections( ).
      lo_selections->set_selection_mode( if_salv_c_selection_mode=>single ).

      lo_columns = mo_alv_cons_hus->get_columns( ).
      lo_columns->set_optimize( abap_true ).

      TRY.
          lo_column ?= lo_columns->get_column( TEXT-021 ).  " DOCNO
          lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
        CATCH cx_salv_not_found ##NO_HANDLER.
      ENDTRY.

      DATA(lo_events) = mo_alv_cons_hus->get_event( ).
      CREATE OBJECT lo_event_handler.
      SET HANDLER lo_event_handler->on_link_click FOR lo_events.

      mo_alv_cons_hus->display( ).
    ELSE.
      mo_alv_cons_hus->refresh( refresh_mode = if_salv_c_refresh=>full ).
    ENDIF.
  ENDMETHOD.

  METHOD show_alv_cons_hu_content.

    DATA:
      lo_columns TYPE REF TO cl_salv_columns_table,
      lo_column  TYPE REF TO cl_salv_column_table.

    mt_cons_hu_content = lcl_model=>get_instance( )->mt_cons_hu_content.

    IF mo_alv_cons_content IS NOT BOUND.
      TRY.
          cl_salv_table=>factory(
            EXPORTING
              r_container    = io_container
            IMPORTING
              r_salv_table   = mo_alv_cons_content
            CHANGING
              t_table        = mt_cons_hu_content ).
        CATCH cx_salv_msg.                              "#EC NO_HANDLER
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDTRY.

      DATA(lo_selections) = mo_alv_cons_content->get_selections( ).
      lo_selections->set_selection_mode( if_salv_c_selection_mode=>single ).

      lo_columns = mo_alv_cons_content->get_columns( ).
      lo_columns->set_optimize( abap_true ).

      TRY.
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-042 ). " GUID_HU " Andriyan Yordanov - added new field which should not be on the screen
          lo_column->set_visible( abap_false ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-013 ).  " MATID
          lo_column->set_visible( abap_false ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-029 ).  " HUIDENT_TOP
          lo_column->set_visible( abap_false ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-030 ). " HIGHER_GUID
          lo_column->set_visible( abap_false ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-049 ). " GROUP_MC
          lo_column->set_visible( abap_false ).
        CATCH cx_salv_not_found ##NO_HANDLER.
      ENDTRY.

      mo_alv_cons_content->display( ).
    ELSE.
      mo_alv_cons_content->refresh( refresh_mode = if_salv_c_refresh=>full ).
    ENDIF.
  ENDMETHOD.

  METHOD show_alv_serial_hu_content.

    DATA:
      lv_mc_count               TYPE /scwm/dl_counter,
      lt_alg_sn_cons_hu_content TYPE ztt_cons_hu_item,
      lo_columns                TYPE REF TO cl_salv_columns_table,
      lo_column                 TYPE REF TO cl_salv_column_table,
      lo_event_handler          TYPE REF TO lcl_event_handler.

    DATA(lo_model) = lcl_model=>get_instance( ).

    lo_model->build_cons_huitem_table( EXPORTING iv_huident = zstr_consolidation_scr_0600-packhu
                                                 it_huitem  = lo_model->mt_cons_huitm
                                       IMPORTING et_hu_info_cont = DATA(lt_hu_info_cont)
                                       CHANGING  ct_hu_content = lo_model->mt_cons_hu_content ).

    " Andriyan Yordanov --- SN reverce grouping by SN
    lt_alg_sn_cons_hu_content = lo_model->mt_cons_hu_content.
    CLEAR mt_cons_sn_reques_cont.

    LOOP AT lt_alg_sn_cons_hu_content ASSIGNING FIELD-SYMBOL(<ls_group_sn_mc>).

      IF  <ls_group_sn_mc>-mc > 1.
        DO <ls_group_sn_mc>-mc TIMES.
          <ls_group_sn_mc>-mc = 1.
****          <ls_group_sn_mc>-serial_status = get_serial_status( iv_huident = <ls_huhdr_subhu>-huident ).
          APPEND <ls_group_sn_mc> TO mt_cons_sn_reques_cont.
        ENDDO.
      ELSE.
        APPEND <ls_group_sn_mc> TO mt_cons_sn_reques_cont.
      ENDIF.

    ENDLOOP.

***    SORT mt_cons_sn_reques_cont BY higher_guid.

    LOOP AT mt_cons_sn_reques_cont ASSIGNING FIELD-SYMBOL(<ls_calc_sn_light>).

      ASSIGN lt_hu_info_cont[ matid = <ls_calc_sn_light>-matid
                              higher_guid = <ls_calc_sn_light>-higher_guid
                              quan        = <ls_calc_sn_light>-quan ] TO FIELD-SYMBOL(<ls_map_info_to_reques>).
      CHECK sy-subrc = 0.
      <ls_calc_sn_light>-guid_hu        = <ls_map_info_to_reques>-guid_hu.

      ASSIGN lo_model->mt_cons_huhdr[ guid_hu = <ls_map_info_to_reques>-guid_hu ] TO FIELD-SYMBOL(<ls_huhdr_info>).

      IF sy-subrc = 0 AND
         lo_model->is_pallet( is_huhdr    = <ls_huhdr_info> ) = abap_true.
        " we have stock directly on the HU no MC then we should add the counter
        lv_mc_count += 1.
        <ls_calc_sn_light>-group_mc = lv_mc_count.
        DATA(lv_pall) = abap_true.
      ENDIF.

      IF <ls_calc_sn_light>-serial_status IS NOT INITIAL.
        <ls_calc_sn_light>-serial_status = lo_model->get_serial_status( iv_huident       = <ls_map_info_to_reques>-huident
                                                                        iv_pall_group_mc = COND #( WHEN lv_pall = abap_true
                                                                                             THEN  <ls_calc_sn_light>-group_mc
                                                                                             ELSE 0 )
                                                                        iv_sn_screen_sub  = abap_true ).
      ENDIF.
      CLEAR lv_pall.
      CLEAR <ls_map_info_to_reques>.
    ENDLOOP.
    " End Andriyan Yordanov

    DATA(lv_serial_status) = lo_model->get_serial_status( iv_huident = zstr_consolidation_scr_0300-packhu ).

    IF mo_alv_serial_hu_cont IS NOT BOUND.
      TRY.
          cl_salv_table=>factory(
            EXPORTING
              r_container    = io_container
            IMPORTING
              r_salv_table   = mo_alv_serial_hu_cont
            CHANGING
              t_table        = mt_cons_sn_reques_cont )."mt_cons_hu_content ).
        CATCH cx_salv_msg.                              "#EC NO_HANDLER
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDTRY.

      DATA(lo_selections) = mo_alv_serial_hu_cont->get_selections( ).
      lo_selections->set_selection_mode( if_salv_c_selection_mode=>single ).

      lo_columns = mo_alv_serial_hu_cont->get_columns( ).
      lo_columns->set_optimize( abap_true ).

      TRY.
          DATA(lo_functions) = mo_alv_serial_hu_cont->get_functions( ).

          lo_functions->add_function(
            EXPORTING
              name     = TEXT-036
              icon     = CONV #( icon_execute_object )
              text     = CONV #( TEXT-036 )
              tooltip  = CONV #( TEXT-036 )
              position = 1 ).

          " add new button Product Scanning Andriyan Yordanov
          lo_functions->add_function(
            EXPORTING
              name     = TEXT-043
              icon     = CONV #( icon_execute_object )
              text     = CONV #( TEXT-043 )
              tooltip  = CONV #( TEXT-043 )
              position = 1 ).

        CATCH cx_salv_existing ##NO_HANDLER.   " ALV: General Error Class (Checked in Syntax Check)
        CATCH cx_salv_wrong_call ##NO_HANDLER. " ALV: General Error Class (Checked in Syntax Check)
      ENDTRY.

      TRY.
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-042 ). " GUID_HU " Andriyan Yordanov - added new field which should not be on the screen
          lo_column->set_visible( abap_false ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-013 ).  " MATID
          lo_column->set_visible( abap_false ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-029 ).  " HUIDENT_TOP
          lo_column->set_visible( abap_false ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-030 ).
          lo_column->set_visible( abap_false ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-049 ). " GROUP_MC
          lo_column->set_visible( abap_false ).
        CATCH cx_salv_not_found ##NO_HANDLER.
      ENDTRY.

      DATA(lo_events) = mo_alv_serial_hu_cont->get_event( ).
      CREATE OBJECT lo_event_handler.
      SET HANDLER lo_event_handler->on_user_command FOR lo_events.
***      SET HANDLER lo_event_handler-> FOR lo_events.

      mo_alv_serial_hu_cont->display( ).
    ELSE.
      mo_alv_serial_hu_cont->refresh( refresh_mode = if_salv_c_refresh=>full ).
    ENDIF.
  ENDMETHOD.


  METHOD show_alv_diff_hu_pack.

    DATA:
      lo_columns TYPE REF TO cl_salv_columns_table,
      lo_column  TYPE REF TO cl_salv_column_table.

    mt_diff_hu_dest = lcl_model=>get_instance( )->mt_diff_hu_dest.

    IF mo_alv_diff_pack IS NOT BOUND.
      TRY.
          cl_salv_table=>factory(
            EXPORTING
              r_container    = io_container
            IMPORTING
              r_salv_table   = mo_alv_diff_pack
            CHANGING
              t_table        = mt_diff_hu_dest ).
        CATCH cx_salv_msg.                              "#EC NO_HANDLER
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDTRY.

      DATA(lo_selections) = mo_alv_diff_pack->get_selections( ).
      lo_selections->set_selection_mode( if_salv_c_selection_mode=>single ).

      lo_columns = mo_alv_diff_pack->get_columns( ).
      lo_columns->set_optimize( abap_true ).

      TRY.
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-042 ). " GUID_HU " Andriyan Yordanov - added new field which should not be on the screen
          lo_column->set_visible( abap_false ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-013 ).  " MATID
          lo_column->set_visible( abap_false ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-029 ).  " HUIDENT_TOP
          lo_column->set_visible( abap_false ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-030 ).
          lo_column->set_visible( abap_false ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-049 ). " GROUP_MC
          lo_column->set_visible( abap_false ).
        CATCH cx_salv_not_found ##NO_HANDLER.
      ENDTRY.

      mo_alv_diff_pack->display( ).
    ELSE.
      mo_alv_diff_pack->refresh( refresh_mode = if_salv_c_refresh=>full ).
    ENDIF.
  ENDMETHOD.

  METHOD show_alv_diff_hu_exp.

    DATA:
      lo_event_handler TYPE REF TO lcl_event_handler,
      lo_columns       TYPE REF TO cl_salv_columns_table,
      lo_column        TYPE REF TO cl_salv_column_table.

*    mt_diff_hu_src = lcl_model=>get_instance( )->mt_diff_hu_src.
    CLEAR mt_diff_hu_src_edit.

    LOOP AT mt_diff_hu_src INTO DATA(ls_line).
      IF ls_line-huident = zstr_consolidation_scr_0300-packhu AND
         ls_line-matid IS INITIAL.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO mt_diff_hu_src_edit ASSIGNING FIELD-SYMBOL(<ls_edit>).
      MOVE-CORRESPONDING ls_line TO <ls_edit>.
    ENDLOOP.

    IF mo_alv_diff_exp IS NOT BOUND.
      TRY.
          cl_salv_table=>factory(
            EXPORTING
              r_container    = io_container
            IMPORTING
              r_salv_table   = mo_alv_diff_exp
            CHANGING
              t_table        = mt_diff_hu_src_edit ).
        CATCH cx_salv_msg.                              "#EC NO_HANDLER
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDTRY.

*      DATA(lo_selections) = mo_alv_diff_exp->get_selections( ).
*      lo_selections->set_selection_mode( if_salv_c_selection_mode=>none ).

      lo_columns = mo_alv_diff_exp->get_columns( ).
      lo_columns->set_optimize( abap_true ).

      TRY.
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-042 ). " GUID_HU " Andriyan Yordanov - added new field which should not be on the screen
          lo_column->set_visible( abap_false ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-013 ).  " MATID
          lo_column->set_visible( abap_false ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-029 ).  " HUIDENT_TOP
          lo_column->set_visible( abap_false ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-030 ).
          lo_column->set_visible( abap_false ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-041 ).
          lo_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-049 ). " GROUP_MC
          lo_column->set_visible( abap_false ).
        CATCH cx_salv_not_found ##NO_HANDLER.
      ENDTRY.

      DATA(lo_events) = mo_alv_diff_exp->get_event( ).
      CREATE OBJECT lo_event_handler.
      SET HANDLER lo_event_handler->on_user_command FOR lo_events.
      SET HANDLER lo_event_handler->on_link_click FOR lo_events.

      TRY.
          DATA(lo_selections_diff_exp) = mo_alv_diff_exp->get_selections( ).
          lo_selections_diff_exp->set_selection_mode( if_salv_c_selection_mode=>none ).

          DATA(ls_api) = mo_alv_diff_exp->extended_grid_api( ).
          DATA(ls_edit) = ls_api->editable_restricted( ).
          ls_edit->set_attributes_for_columnname(
            EXPORTING
              columnname = 'SELECTOR'
              all_cells_input_enabled = abap_true ).
        CATCH cx_salv_not_found ##NO_HANDLER.
      ENDTRY.

      mo_alv_diff_exp->display( ).
    ELSE.
      mo_alv_diff_exp->refresh( refresh_mode = if_salv_c_refresh=>full ).
    ENDIF.
  ENDMETHOD.

  METHOD show_alv_open_picking.

    DATA:
      lo_columns       TYPE REF TO cl_salv_columns_table,
      lo_column        TYPE REF TO cl_salv_column_table,
      lo_event_handler TYPE REF TO lcl_event_handler.

    mt_open_picking_hu = lcl_model=>get_instance( )->mt_open_picking_hu.

    IF mo_alv_open_picking IS NOT BOUND.
      TRY.
          cl_salv_table=>factory(
            EXPORTING
              r_container    = io_container
            IMPORTING
              r_salv_table   = mo_alv_open_picking
            CHANGING
              t_table        = mt_open_picking_hu ).
        CATCH cx_salv_msg.                              "#EC NO_HANDLER
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDTRY.

      DATA(lo_selections) = mo_alv_open_picking->get_selections( ).
      lo_selections->set_selection_mode( if_salv_c_selection_mode=>single ).

      lo_columns = mo_alv_open_picking->get_columns( ).
      lo_columns->set_optimize( abap_true ).

      TRY.
          lo_column ?= lo_columns->get_column( TEXT-025 ).  " WHO
          lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

          DATA(lo_events) = mo_alv_open_picking->get_event( ).
          CREATE OBJECT lo_event_handler.
          SET HANDLER lo_event_handler->on_link_click FOR lo_events.
        CATCH cx_salv_not_found ##NO_HANDLER.
      ENDTRY.

      mo_alv_open_picking->display( ).
    ELSE.
      mo_alv_open_picking->refresh( refresh_mode = if_salv_c_refresh=>full ).
    ENDIF.
  ENDMETHOD.

  METHOD show_alv_dest_hu_content.

    DATA:
      lo_columns TYPE REF TO cl_salv_columns_table,
      lo_column  TYPE REF TO cl_salv_column_table.

    mt_dest_hu_content = lcl_model=>get_instance( )->mt_dest_hu_content.

    IF mo_alv_dest_content IS NOT BOUND.
      TRY.
          cl_salv_table=>factory(
            EXPORTING
              r_container    = io_container
            IMPORTING
              r_salv_table   = mo_alv_dest_content
            CHANGING
              t_table        = mt_dest_hu_content ).
        CATCH cx_salv_msg.                              "#EC NO_HANDLER
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDTRY.

      DATA(lo_selections) = mo_alv_dest_content->get_selections( ).
      lo_selections->set_selection_mode( if_salv_c_selection_mode=>single ).

      lo_columns = mo_alv_dest_content->get_columns( ).
      lo_columns->set_optimize( abap_true ).

      TRY.
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-042 ). " GUID_HU " Andriyan Yordanov - added new field which should not be on the screen
          lo_column->set_visible( abap_false ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-013 ).  " MATID
          lo_column->set_visible( abap_false ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-029 ).  " HUIDENT_TOP
          lo_column->set_visible( abap_false ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-030 ).
          lo_column->set_visible( abap_false ).
          CLEAR lo_column.
          lo_column ?= lo_columns->get_column( TEXT-049 ). " GROUP_MC
          lo_column->set_visible( abap_false ).
        CATCH cx_salv_not_found ##NO_HANDLER.
      ENDTRY.

      mo_alv_dest_content->display( ).
    ELSE.
      mo_alv_dest_content->refresh( refresh_mode = if_salv_c_refresh=>full ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
