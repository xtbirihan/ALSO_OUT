**********************************************************************
*& Key           : AMOGYORODI-230814
*& Request No.   :
**********************************************************************
*& Description (short)
*&
*&
**********************************************************************
*----------------------------------------------------------------------*
***INCLUDE ZOUT_CONSOLIDATION_CI1.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_report_controller
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_controller IMPLEMENTATION.
  METHOD get_instance.
    IF so_instance IS INITIAL.
      CREATE OBJECT so_instance.
    ENDIF.
    ro_instance = so_instance.
  ENDMETHOD.

  METHOD selection.

    DATA: ls_workstation  TYPE  /scwm/tworkst.

    lcl_controller=>sv_lgnum = iv_lgnum.
    lcl_controller=>ssout_term_def = is_term_def.

    DATA(lo_model) = lcl_model=>get_instance( ).

    " Andriyan Yordanov
    CALL FUNCTION '/SCWM/TWORKST_READ_SINGLE'
      EXPORTING
        iv_lgnum       = iv_lgnum
        iv_workstation = is_term_def-workstation
      IMPORTING
        es_workst      = ls_workstation
      EXCEPTIONS
        error          = 1
        not_found      = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      sv_lgtyp = ls_workstation-lgtyp.
      sv_procs = ls_workstation-procs.
      sv_aarea = ls_workstation-aarea_plan.
    ENDIF.

    lo_model->set_lgtype( iv_lgtyp = sv_lgtyp ).
***    lo_model->set_aarea( iv_aarea = sv_aarea ).
    lo_model->set_procs( iv_procs = sv_procs ).
    " end Andriyan Yordanov

    lo_model->get_hus_on_cons_bin(
      IMPORTING
        et_docid = DATA(lt_docid) ).

    lo_model->get_open_deliveries( lt_docid ).

  ENDMETHOD.

  METHOD user_command_0100.
    CASE sy-ucomm.
      WHEN 'BACK' OR 'EXIT' OR 'CANC' OR 'LOGOUT'.
        LEAVE PROGRAM.
      WHEN 'ENTER'.
        IF zstr_consolidation_scr_0200-packhu IS INITIAL.
          MESSAGE e509(/scwm/hufunctions).
        ENDIF.

        DATA(lo_model) = lcl_model=>get_instance( ).
        CLEAR lo_model->mt_dlv_headers.
        lo_model->get_delivery_for_hu( zstr_consolidation_scr_0200-packhu ).

        READ TABLE lo_model->mt_cons_headers INTO lo_model->ms_cons_header INDEX 1.
        lo_model->build_cons_hu_table( lo_model->ms_cons_header-docid ).
        DATA(lo_view) = lcl_view=>get_instance( ).

        CLEAR zstr_consolidation_scr_0200.
        MOVE-CORRESPONDING lo_model->ms_cons_header TO zstr_consolidation_scr_0200.
        LEAVE TO SCREEN 0200.

      WHEN 'REFRESH'.
        refresh_screen_0100( ).
    ENDCASE.
  ENDMETHOD.

  METHOD user_command_0200.

    DATA:
      lt_lbox_value TYPE vrm_values.

    CASE sy-ucomm.
      WHEN 'BACK'.
        refresh_screen_0100( ).
        LEAVE TO SCREEN 0100.
      WHEN 'EXIT' OR 'CANC' OR 'LOGOUT'.
        LEAVE PROGRAM.
      WHEN 'PICK'.
        DATA(lo_model) = lcl_model=>get_instance( ).
        lo_model->build_pick_table( iv_docid = zstr_consolidation_scr_0200-docid ).
        LEAVE TO SCREEN 0210.
      WHEN 'SHCOMP'.
        lo_model = lcl_model=>get_instance( ).
        IF sv_show_compl = abap_true.
          CLEAR sv_show_compl.
        ELSE.
          sv_show_compl = abap_true.
        ENDIF.

        lo_model->build_cons_hu_table( iv_docid = zstr_consolidation_scr_0200-docid ).
      WHEN 'REPRT'.
        MESSAGE i001(hrben00forms).

      WHEN 'ENTER'.
        handle_hu_scan_0200( CHANGING cv_huident = zstr_consolidation_scr_0200-packhu ).
      WHEN 'OPENHU'.
        CALL SCREEN 0230.
        handle_open_hu( iv_huident = zstr_consolidation_scr_0200-packhu ).
        CLEAR zstr_consolidation_scr_0200-packhu.
      WHEN 'CHCARR'.

        lo_model = lcl_model=>get_instance( ).

        READ TABLE lo_model->mt_cons_headers INTO DATA(ls_header)
          WITH KEY docid = zstr_consolidation_scr_0200-docid.

        IF sy-subrc <> 0.
          MESSAGE e001(/scmtms/lbn_md_msg).
        ENDIF.

        IF ls_header-status_cons = TEXT-010 OR
           ls_header-status_cons = TEXT-011.
          MESSAGE e002(zewm_cl_msg_con).
        ENDIF.

        " Andriyan Yordanov " change of logic call MOn and validate if step of the top HUs are not confirmed
        IF lo_model->mt_cons_hus IS NOT INITIAL.
          LOOP AT lo_model->mt_cons_huhdr ASSIGNING FIELD-SYMBOL(<ls_check_step_compl>)
                               WHERE ( huident IN VALUE  rseloption( FOR <ls_curr_hus> IN lo_model->mt_cons_hus
                                                         ( sign   = wmegc_sign_inclusive
                                                           option = wmegc_option_eq
                                                           low    = <ls_curr_hus>-tophu ) ) ).

            CHECK <ls_check_step_compl>-copst = abap_true.
            " Step &1 of HU &2 is completed. You can not change the DLV &3.
            MESSAGE e041(zmc_out) WITH <ls_check_step_compl>-procs
                                       |{ <ls_check_step_compl>-huident ALPHA = OUT }|
                                       |{ ls_header-docno  ALPHA = OUT }|.

          ENDLOOP.
        ENDIF.

        " we should call monitor logic ( which should give an option of the user to change the document)
        zcl_outb_packing_wc_base_ui=>init_packing_mon(
                                 it_docno = VALUE #( ( docno = |{ ls_header-docno  ALPHA = IN }| ) ) ).

        " do refresh after we are back from the mon
        refresh_screen_0200( ).
        " End Andriyan Yordanov

        " Andriyan Yordanov - commenting the existsin logic.
        " We should just navigate to the MON.
****        CALL SCREEN 0220.
****
****        CALL FUNCTION 'VRM_GET_VALUES'
****          EXPORTING
****            id     = gc_lbox_carrier
****          IMPORTING
****            values = lt_lbox_value
****          EXCEPTIONS
****            OTHERS = 1.
****        IF sy-subrc <> 0 OR lt_lbox_value IS INITIAL.
****          MESSAGE e001(/scmtms/lbn_md_msg).
****        ENDIF.
****
****        TRY.
****            DATA(ls_value) = lt_lbox_value[ 1 ].
****          CATCH cx_sy_itab_line_not_found.
****            MESSAGE e001(/scmtms/lbn_md_msg).
****        ENDTRY.
****
****        zstr_consolidation_scr_0200-carrier = |{ ls_value-key ALPHA = OUT }|.
****        zstr_consolidation_scr_0200-carrier = |{ zstr_consolidation_scr_0200-carrier ALPHA = IN }|.
****
****        handle_change_carrier(
****          EXPORTING
****            iv_docid   = zstr_consolidation_scr_0200-docid
****            iv_carrier = zstr_consolidation_scr_0200-carrier ).
        " end Andriyan Yordanov
      WHEN 'REFRESH'.
        refresh_screen_0200( ).
    ENDCASE.
  ENDMETHOD.

  METHOD user_command_0210.
    CASE sy-ucomm.
      WHEN 'ENTER'.
        LEAVE TO SCREEN 0.
      WHEN 'BACK'.
        LEAVE TO SCREEN 0200.
      WHEN 'EXIT' OR 'CANC' OR 'LOGOUT'.
        LEAVE PROGRAM.
      WHEN 'REFRESH'.
        DATA(lo_model) = lcl_model=>get_instance( ).
        lo_model->build_pick_table( iv_docid = zstr_consolidation_scr_0200-docid ).
    ENDCASE.
  ENDMETHOD.

  METHOD user_command_popup.
    CASE sy-ucomm.
      WHEN 'ENTER' OR 'OK' OR 'BACK'.
        LEAVE TO SCREEN 0.
      WHEN 'EXIT' OR 'CANC' OR 'LOGOUT'.
        LEAVE PROGRAM.
    ENDCASE.
  ENDMETHOD.

  METHOD user_command_0300.
    DATA:
      lt_lbox_value TYPE vrm_values.

    CASE sy-ucomm.
      WHEN 'BACK'.
        refresh_screen_0200( ).
        LEAVE TO SCREEN 0200.
      WHEN 'EXIT' OR 'CANC' OR 'LOGOUT'.
        LEAVE PROGRAM.
      WHEN 'CLOSE'.
        handle_close_hu( iv_huident = zstr_consolidation_scr_0300-packhu ).
      WHEN 'MERGE'.
        handle_merge_hu( iv_huident = zstr_consolidation_scr_0300-packhu ).
      WHEN 'SPLIT'.
        handle_split_hu( iv_huident = zstr_consolidation_scr_0300-packhu ).
      WHEN 'SERIAL'.
        handle_serials( ).
      WHEN 'REPACK'.

        CALL SCREEN 0370.
****        handle_full_repack( iv_huident_src = zstr_consolidation_scr_0300-packhu
****                    iv_huident_new = zstr_consolidation_scr_0300-newhu
*******                            iv_hutype = zstr_consolidation_scr_0300-hutype
****                    iv_pmatnr = zstr_consolidation_scr_0300-newhu_pmat ).

      WHEN 'REFRESH'.
        refresh_screen_0300( ).
    ENDCASE.
  ENDMETHOD.

  METHOD handle_delete_serial.

    DATA: lt_delete TYPE zcl_crud_ztcross_cap_nums=>tt_ztcross_cap_nums.

    DATA(lo_view) = lcl_view=>get_instance( ).
    DATA(lo_selections) = lo_view->mo_alv_serial->get_selections( ).
    DATA(lt_rows) = lo_selections->get_selected_rows( ).
    DATA(lo_model) = lcl_model=>get_instance( ).

    READ TABLE lo_model->mt_cons_huhdr INTO DATA(ls_huhdr)
      WITH KEY huident = zstr_consolidation_scr_0600-packhu.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<lv_rows>)." INTO DATA(lv_row).

      READ TABLE lo_view->mt_serial INTO DATA(ls_serial) INDEX <lv_rows>.
      CHECK sy-subrc = 0.

      DATA(lt_serial) = zcl_crud_ztcross_cap_nums=>select_multi_by_huid(
        iv_lgnum = lcl_controller=>sv_lgnum
        iv_huid  = ls_serial-guid_hu ).

      " Andriyan Yordanov --- the db data for the serial key should be deleted. Even that user delete just one id_type
      lt_delete = VALUE #( BASE lt_delete FOR <ls_del_lines> IN lt_serial
                                                  WHERE ( lgnum   = lcl_controller=>sv_lgnum AND
                                                          docno   = ls_serial-docno          AND
                                                          itemno  = ls_serial-itemno         AND
                                                          serial  = ls_serial-serial )
                                                          ( <ls_del_lines> ) ).
      " End Andriyan Yordanov
    ENDLOOP.

    IF lt_delete IS NOT INITIAL.
      zcl_crud_ztcross_cap_nums=>delete_multi_by_entries( it_entries = lt_delete ).

      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
        EXPORTING
          functioncode           = 'OK'
        EXCEPTIONS
          function_not_supported = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD handle_delete_all_serial.

    DATA: lt_delete TYPE zcl_crud_ztcross_cap_nums=>tt_ztcross_cap_nums.

    DATA(lo_model) = lcl_model=>get_instance( ).
    DATA(lo_view)  = lcl_view=>get_instance( ).

    READ TABLE lo_model->mt_cons_huhdr INTO DATA(ls_huhdr)
      WITH KEY huident = zstr_consolidation_scr_0600-packhu.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.
        DATA(lt_serial) = zcl_crud_ztcross_cap_nums=>select_multi_by_huid(
                iv_lgnum = lcl_controller=>sv_lgnum
                iv_huid  = lo_view->mt_serial[ 1 ]-guid_hu ).
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    lt_delete = VALUE #( BASE lt_delete FOR <ls_del_lines> IN lt_serial
                                                        ( <ls_del_lines> ) ).

    IF lt_delete IS NOT INITIAL.

      zcl_crud_ztcross_cap_nums=>delete_multi_by_entries( it_entries = lt_delete ).

      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
        EXPORTING
          functioncode           = 'OK'
        EXCEPTIONS
          function_not_supported = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD handle_capture_serial.

    DATA:
      lr_guid_hu TYPE RANGE OF /scwm/guid_hu.

    DATA(lo_view) = lcl_view=>get_instance( ).
    DATA(lo_selections) = lo_view->mo_alv_serial_hu_cont->get_selections( ).
    DATA(lt_rows) = lo_selections->get_selected_rows( ).
    DATA(lo_model) = lcl_model=>get_instance( ).

    CLEAR lo_view->ms_cons_hu_serial.

    READ TABLE lt_rows INTO DATA(lv_row) INDEX 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

***    READ TABLE lo_view->mt_cons_hu_content INTO lo_view->ms_cons_hu_serial INDEX lv_row. Andriyan Yordanov 17.01
***    IF sy-subrc <> 0.
***      RETURN.
***    ENDIF.

    READ TABLE lo_view->mt_cons_sn_reques_cont INTO lo_view->ms_cons_hu_serial INDEX lv_row.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF lo_view->ms_cons_hu_serial-huident <> '-'.
      READ TABLE lo_model->mt_cons_huhdr INTO DATA(ls_huhdr)
        WITH KEY huident = lo_view->ms_cons_hu_serial-huident.

      IF sy-subrc = 0 AND
         ls_huhdr-top = abap_true AND
         ls_huhdr-bottom = abap_false.
        MESSAGE s004(zewm_cl_msg_con) DISPLAY LIKE wmegc_severity_err.
        RETURN.
      ENDIF.

      lo_view->ms_cons_hu_serial-guid_hu = ls_huhdr-guid_hu.
***      zstr_consolidation_scr_0600-packhu = lo_view->ms_cons_hu_serial-huident.

    ELSEIF lo_view->ms_cons_hu_serial-higher_guid IS NOT INITIAL.
      READ TABLE lo_model->mt_cons_huhdr INTO ls_huhdr
        WITH KEY guid_hu = lo_view->ms_cons_hu_serial-higher_guid.
      IF sy-subrc <> 0.

      ENDIF.

      IF ls_huhdr-top = abap_true AND ls_huhdr-bottom = abap_false.
        LOOP AT lo_model->mt_cons_huhdr INTO DATA(ls_huhdr_subhu)
          WHERE higher_guid = ls_huhdr-guid_hu.

          READ TABLE lo_model->mt_cons_huitm  TRANSPORTING NO FIELDS
            WITH KEY guid_parent = ls_huhdr_subhu-guid_hu
                     matid       = lo_view->ms_cons_hu_serial-matid.

          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          IF lo_model->get_serial_status( iv_huident = ls_huhdr_subhu-huident ) = icon_led_red OR
             lo_model->get_serial_status( iv_huident = ls_huhdr_subhu-huident ) = icon_led_yellow.
            DATA(lv_found) = abap_true.
****            lo_view->ms_cons_hu_serial-guid_hu = ls_huhdr_subhu-guid_hu. " Andriyan Yordanov
            EXIT.
          ENDIF.
        ENDLOOP.

      ELSE.
        IF lo_model->is_shipping_carton( is_huhdr = ls_huhdr ).
          lo_view->ms_cons_hu_serial-guid_hu = ls_huhdr-guid_hu.
        ENDIF.
      ENDIF.

***      IF lv_found = abap_true.  " Commented Andriyan Yordanov ... we should not change the HU
***        zstr_consolidation_scr_0600-packhu = ls_huhdr_subhu-huident.
***      ELSE.
***        zstr_consolidation_scr_0600-packhu = ls_huhdr-huident.
***      ENDIF.
    ENDIF.

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = 'OK'
      EXCEPTIONS
        function_not_supported = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD handle_prod_scan_serial.

    DATA: lv_open_sn_flg TYPE abap_bool,
          lv_returncode  TYPE char1 ##NEEDED,
          lt_fields      TYPE STANDARD TABLE OF sval WITH EMPTY KEY.

    DATA(lo_view) = lcl_view=>get_instance( ).

    " check if we have open SN for the relevant records
    CLEAR lv_open_sn_flg.
    LOOP AT lo_view->mt_cons_sn_reques_cont ASSIGNING FIELD-SYMBOL(<ls_check_stat>)
                                              WHERE ( serial_status = icon_led_red OR
                                                      serial_status = icon_led_yellow ) AND
                                                     huident = '-'.
      lv_open_sn_flg = abap_true.
      EXIT.
    ENDLOOP.

    IF lv_open_sn_flg = abap_false.
      RETURN.
    ENDIF.

    " Fill table for popup
    lt_fields = VALUE #(
       ( tabname   = c_ui_matnr_table
         fieldname = c_ui_matnr_field
         fieldtext = TEXT-044
         value     = space
         field_obl = abap_true ) ).

    " use popup to ask for the product
    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = TEXT-001
        start_column    = c_start_col
        start_row       = c_start_row
      IMPORTING
        returncode      = lv_returncode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF lv_returncode = c_cancel_action.
      RETURN.
    ENDIF.

    " get Scanned value
    ASSIGN lt_fields[ fieldname = c_ui_matnr_field ]-value TO FIELD-SYMBOL(<lv_prod_sn>).
    IF sy-subrc <> 0.
      MESSAGE e147(/sapapo/snp).
    ENDIF.

    check_scan_0400(
      EXPORTING
        iv_scan    = CONV #( <lv_prod_sn> )
      IMPORTING
        ev_matid   = DATA(lv_matid) ).

    IF lv_matid IS INITIAL.
      MESSAGE e147(/sapapo/snp).
    ENDIF.

    DATA(lo_model) = lcl_model=>get_instance( ).

    ASSIGN lo_model->mt_cons_huhdr[ guid_hu = <ls_check_stat>-higher_guid ] TO FIELD-SYMBOL(<ls_top_hu>).

    IF sy-subrc = 0 AND
       lo_model->is_shipping_carton( is_huhdr = <ls_top_hu> ).

      LOOP AT lo_view->mt_cons_sn_reques_cont ASSIGNING <ls_check_stat>
                                            WHERE ( serial_status = icon_led_green OR
                                                    serial_status = icon_led_yellow ) AND
                                                   huident = '-' AND
                                                   matid = lv_matid.
        lo_view->ms_cons_hu_serial = <ls_check_stat>.
        EXIT.
      ENDLOOP.

    ELSE.

      LOOP AT lo_view->mt_cons_sn_reques_cont ASSIGNING <ls_check_stat>
                                          WHERE ( serial_status = icon_led_red OR
                                                  serial_status = icon_led_yellow ) AND
                                                 huident = '-' AND
                                                 matid = lv_matid.
        lo_view->ms_cons_hu_serial = <ls_check_stat>.
        EXIT.
      ENDLOOP.
    ENDIF.

    IF sy-subrc <> 0.
      CLEAR lo_view->ms_cons_hu_serial.
    ENDIF.

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = 'OK'
      EXCEPTIONS
        function_not_supported = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD handle_count_exp.

    DATA:
      lv_cnt        TYPE int2,
      lv_quan_moved TYPE /scwm/de_quantity,
      ls_quan       TYPE /scwm/s_quan,
      lt_mat_uom    TYPE /scwm/tt_material_uom,
      lr_guid_hu    TYPE RANGE OF /scwm/guid_hu.

    DATA(lo_view) = lcl_view=>get_instance( ).
    DATA(lo_selections) = lo_view->mo_alv_diff_exp->get_selections( ).
    DATA(lt_rows) = lo_selections->get_selected_rows( ).
    DATA(lo_model) = lcl_model=>get_instance( ).

    LOOP AT lo_view->mt_diff_hu_src_edit INTO DATA(ls_exp) WHERE selector = abap_true.
      IF ls_exp-huident <> '-'.
*        READ TABLE lo_model->mt_cons_huhdr INTO DATA(ls_huhdr)
*          WITH KEY huident = ls_exp-huident.
*        IF sy-subrc <> 0.
*          MESSAGE e050(/scwm/rf_en).
*        ENDIF.
*
*        APPEND VALUE #( sign = /scmb/cl_search=>sc_sign_i option = /scmb/cl_search=>sc_eq low = ls_huhdr-guid_hu ) TO lr_guid_hu.
*
*        IF ls_huhdr-top = abap_true AND ls_huhdr-bottom = abap_false.
*          LOOP AT lo_model->mt_cons_huhdr INTO DATA(ls_huhdr_subhu)
*            WHERE higher_guid = ls_huhdr-guid_hu.
*            APPEND VALUE #( sign = /scmb/cl_search=>sc_sign_i option = /scmb/cl_search=>sc_eq low = ls_huhdr_subhu-guid_hu ) TO lr_guid_hu.
*          ENDLOOP.
*        ENDIF.
*
*        LOOP AT lo_model->mt_cons_huitm INTO DATA(ls_huitem)
*          WHERE guid_parent IN lr_guid_hu.
*          APPEND ls_huitem TO lo_model->mt_scan_huitm.
*        ENDLOOP.
      ELSE.
        CLEAR lv_quan_moved.
        WHILE ls_exp-quan > 0.
          handle_scan_0500(
            EXPORTING
              iv_scan    = CONV #( ls_exp-product )
              iv_huident = zstr_consolidation_scr_0300-packhu
            IMPORTING
              ev_quan_moved = lv_quan_moved ).
          ls_exp-quan = ls_exp-quan - lv_quan_moved.
          lv_cnt = lv_cnt + 1.
          IF lv_cnt = 100.
            EXIT.
          ENDIF.
        ENDWHILE.
      ENDIF.
    ENDLOOP.

    lo_model->build_cons_huitem_table( EXPORTING iv_huident = zstr_consolidation_scr_0300-packhu
                                                 iv_split = abap_true
                                                 it_huitem = lo_model->mt_scan_huitm
                                       CHANGING ct_hu_content = lo_model->mt_diff_hu_dest ).

  ENDMETHOD.

  METHOD handle_popup_conc_mathut.

    " Andriyan Yordanov
    DATA:
      lv_text    TYPE string,
      lt_thutyp  TYPE STANDARD TABLE OF /scwm/t307,
      lt_thutypt TYPE STANDARD TABLE OF /scwm/thutypt.

    CLEAR et_lbox_values.
    " Andriyan Yordanov ---- start change
    DATA(lo_matread) = NEW  /scwm/cl_ui_stock_fields( ).

    DATA(lv_matid) = lo_matread->get_matid_by_no(
        EXPORTING
          iv_matnr = iv_humat  ).                " Material Number

    lo_matread->get_matkey_by_id(
      EXPORTING
        iv_matid = lv_matid                 " Material GUIDs with Conversion Exit
      IMPORTING
        ev_maktx = DATA(lv_maktx) ).

    NEW /scwm/cl_hu_typ( )->/scwm/if_hu_typ~read_hu_type_single(
      EXPORTING
         iv_hutyp = iv_hutype
      IMPORTING
         ev_hutyptxt = DATA(lv_hutyp_txt) ).

    lv_text = |{ lv_hutyp_txt ALPHA = OUT }/{ lv_maktx ALPHA = OUT }| .
    CONDENSE lv_text NO-GAPS.

    et_lbox_values = VALUE #( ( key  = |{ zstr_consolidation_scr_0300-newhu_pmat ALPHA = OUT }|
                                text = lv_text ) ).

  ENDMETHOD.

  METHOD handle_call_repack_scr.

    " Andriyan Yordanov - change re-pack logic load screen 0370
    DATA: ls_matid          TYPE /scmb/mdl_matid_str,
          lt_t307           TYPE /scwm/tt_t307,
          lt_mapp_hutyp_mat TYPE tty_mapp_hutyp_matid,
          lt_hutype_curr_hu TYPE /scwm/tt_t307,
          lt_pmat_new_hu    TYPE /scmb/mdl_matid_tab,
          lt_data_pmat_scr  TYPE tty_pmat_data.

    CLEAR: et_repack_matid,
           et_mapp_hutyp_matid.

    IF iv_hutype_pall IS INITIAL.

      DATA(lo_model) = lcl_model=>get_instance( ).

      ASSIGN lo_model->mt_cons_huhdr[ huident = iv_packhu ] TO FIELD-SYMBOL(<ls_src_hu_repack>).

      IF sy-subrc <> 0.
        " this will not happen error in a process
        RETURN.
      ENDIF.

      DATA(lt_ship_pall_hutypegrp) = zcl_crud_ztcross_cart_type=>select_by_carton_type(
               EXPORTING
                 iv_lgnum             = lcl_controller=>sv_lgnum
                 iv_pack_cartons_type = COND #( WHEN lo_model->is_pallet( is_huhdr = <ls_src_hu_repack> ) = abap_true
                                                   THEN zcl_crud_ztcross_cart_type=>c_pall_type
                                                  WHEN  lo_model->is_shipping_carton( is_huhdr = <ls_src_hu_repack> ) = abap_true
                                                   THEN zcl_crud_ztcross_cart_type=>c_shipping_carton_type ) ).

    ELSE.

      lt_ship_pall_hutypegrp = zcl_crud_ztcross_cart_type=>select_by_carton_type(
               EXPORTING
                 iv_lgnum             = lcl_controller=>sv_lgnum
                 iv_pack_cartons_type = iv_hutype_pall ).

    ENDIF.

    "read HU types for warehouse
    CALL FUNCTION '/SCWM/T307_READ'
      EXPORTING
        iv_lgnum  = lcl_controller=>sv_lgnum
      IMPORTING
        et_t307   = lt_t307
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE wmegc_severity_suc NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE wmegc_severity_err.
      RETURN.
    ENDIF.

    lt_hutype_curr_hu = VALUE #( FOR <ls_t307> IN lt_t307
                                  FOR <ls_hutype_grp> IN lt_ship_pall_hutypegrp
                                         WHERE ( lgnum = lcl_controller=>sv_lgnum AND
                                                 hutypgrp = <ls_t307>-hutypgrp )
                                                 ( <ls_t307> ) ).

    IF lines( lt_hutype_curr_hu ) = 0.
      " add error
      RETURN.
    ENDIF.

    NEW /scwm/cl_hu_typ( )->/scwm/if_hu_typ~read_hu_type_multiple(
      EXPORTING
          it_hutyp = VALUE #( FOR <lv_txt_hutype> IN lt_hutype_curr_hu
                              ( hutyp = <lv_txt_hutype>-letyp ) )
      IMPORTING
          et_hutyp = DATA(lt_hutyp_txt) ).

    SELECT FROM /sapapo/matpack
       FIELDS matid,
              hutyp
      FOR ALL ENTRIES IN @lt_hutype_curr_hu
      WHERE hutyp = @lt_hutype_curr_hu-letyp
      INTO TABLE @DATA(lt_matid_new_hu).

    IF sy-subrc <> 0.
      " add error
      RETURN.
    ENDIF.

    lt_mapp_hutyp_mat = VALUE #( FOR <ls_mapp> IN lt_matid_new_hu
                                  ( hutyp = <ls_mapp>-hutyp
                                    matid_22 = <ls_mapp>-matid ) ).

    " Convert the GUID22 into GUID16
    LOOP AT lt_matid_new_hu ASSIGNING FIELD-SYMBOL(<ls_matid_22>).

      CLEAR ls_matid.

      CALL FUNCTION '/SCMB/MDL_GUID_CONVERT'
        EXPORTING
          iv_guid22 = <ls_matid_22>-matid
        IMPORTING
          ev_guid16 = ls_matid-matid.

      ASSIGN lt_mapp_hutyp_mat[ matid_22 = <ls_matid_22>-matid ] TO FIELD-SYMBOL(<ls_mapp_add_matid16>).
      CHECK sy-subrc = 0.
      <ls_mapp_add_matid16>-matid_16 = ls_matid-matid.

      ASSIGN lt_hutyp_txt[ hutyp = <ls_mapp_add_matid16>-hutyp ] TO FIELD-SYMBOL(<ls_hutyp_txt>).
      IF sy-subrc = 0.
        <ls_mapp_add_matid16>-hutyp_txt = <ls_hutyp_txt>-hutyptext.
      ENDIF.

      IF iv_hutype_pall IS INITIAL.
        IF <ls_src_hu_repack> IS ASSIGNED.
          CHECK <ls_src_hu_repack>-pmat_guid <> ls_matid-matid.
        ENDIF.
      ENDIF.

      APPEND ls_matid TO lt_pmat_new_hu.

    ENDLOOP.

    " Select material number and description from MDL
    TRY.
        CALL FUNCTION '/SCMB/MDL_PRODUCT_READ_MULTI'
          EXPORTING
            it_id   = lt_pmat_new_hu
          IMPORTING
            et_data = lt_data_pmat_scr.
      CATCH /scmb/cx_mdl.
        MESSAGE ID sy-msgid TYPE wmegc_severity_suc NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE wmegc_severity_err.
        RETURN.
    ENDTRY.

    NEW /scwm/cl_ui_stock_fields( )->prefetch_matkey_by_id(
      EXPORTING
        it_matid            =  VALUE #( FOR <ls_pref_matkey> IN lt_mapp_hutyp_mat
                                             ( matid = <ls_pref_matkey>-matid_16 ) )                " Internal Key for Product
      IMPORTING
        et_matid_extkey     = DATA(lt_matid_extkey) ).                 " Product ID and External Key (Number/Short Text)

    LOOP AT lt_matid_extkey ASSIGNING FIELD-SYMBOL(<ls_prodid_key>).
      ASSIGN lt_mapp_hutyp_mat[ matid_16 = <ls_prodid_key>-matid ] TO FIELD-SYMBOL(<ls_mapp_prodid_key>).
      CHECK sy-subrc = 0.
      <ls_mapp_prodid_key>-matnr = <ls_prodid_key>-matnr.
    ENDLOOP.

    et_repack_matid     = lt_data_pmat_scr.
    et_mapp_hutyp_matid = lt_mapp_hutyp_mat.

  ENDMETHOD.

  METHOD handle_full_repack.

    DATA:
      ls_pack_stock TYPE /scwm/s_pack_stock,
      lt_cap_nums   TYPE zcl_crud_ztcross_cap_nums=>tt_ztcross_cap_nums,
      lt_lbox_value TYPE vrm_values.

    CASE sy-ucomm.
      WHEN 'CANCEL'.

        LEAVE TO SCREEN 0300.

      WHEN 'OK'.

        IF iv_pmatnr IS INITIAL.
          MESSAGE e032(/scwm/rf_en). "  DISPLAY LIKE  wmegc_severity_err .
        ELSE.
          DATA(lv_pmatid) =  NEW /scwm/cl_ui_stock_fields( )->get_matid_by_no(
               EXPORTING
                 iv_matnr = iv_pmatnr ).                 " Material Number
        ENDIF.

        IF iv_huident_new IS INITIAL.
          MESSAGE e895(l3). " DISPLAY LIKE  wmegc_severity_err .
        ENDIF.

        IF iv_huident_new = iv_huident_src.
          MESSAGE e187(/scwm/ui_packing)." DISPLAY LIKE  wmegc_severity_err .
        ENDIF.

        CALL FUNCTION '/SCWM/TO_INIT_NEW'
          EXPORTING
            iv_lgnum = sv_lgnum.

        /scwm/cl_tm=>set_lgnum( sv_lgnum ).
        /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = DATA(lo_packing) ).

        lo_packing->init(
          EXPORTING
            iv_lgnum = sv_lgnum
          EXCEPTIONS
            OTHERS   = 1 ).

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        lo_packing->get_hu(
          EXPORTING
            iv_huident = iv_huident_src
          IMPORTING
            es_huhdr   = DATA(ls_huhdr)
          EXCEPTIONS
            OTHERS     = 1 ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        lo_packing->/scwm/if_pack_bas~create_hu(
          EXPORTING
            iv_pmat      = lv_pmatid
            iv_huident   = iv_huident_new
            i_location   = ls_huhdr-lgpla " 'SPED-0001' Andriyan Yordanov
          RECEIVING
            es_huhdr    = DATA(ls_huhdr_new)
          EXCEPTIONS
            OTHERS       = 1 ).

        IF sy-subrc <> 0 OR
           ls_huhdr IS INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.

          SET UPDATE TASK LOCAL. """ ????? leave it like that I don't know why

          lo_packing->/scwm/if_pack_bas~save(
            EXPORTING
              iv_commit = abap_false
             EXCEPTIONS
               OTHERS    = 1 ).

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.

            COMMIT WORK AND WAIT.

            /scwm/cl_tm=>cleanup( ).
            /scwm/cl_tm=>set_lgnum( sv_lgnum ).

            CLEAR lo_packing.

            /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = lo_packing ).

            lo_packing->init(
              EXPORTING
                iv_lgnum = sv_lgnum
              EXCEPTIONS
                OTHERS   = 1 ).

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.

            lo_packing->get_hu(
              EXPORTING
                iv_huident = iv_huident_src
              IMPORTING
                es_huhdr   = ls_huhdr
              EXCEPTIONS
                OTHERS     = 1 ).

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.

            CALL FUNCTION '/SCWM/TO_INIT_NEW'
              EXPORTING
                iv_lgnum = sv_lgnum.

          ENDIF.
        ENDIF.

        DATA(lo_model) = lcl_model=>get_instance( ).

        "Pack all subHUs
        LOOP AT lo_model->mt_cons_huhdr INTO DATA(ls_huhdr_subhu)
          WHERE higher_guid = ls_huhdr-guid_hu.
          lo_packing->pack_hu(
            EXPORTING
              iv_source_hu = ls_huhdr_subhu-guid_hu
              iv_dest_hu   = ls_huhdr_new-guid_hu
            EXCEPTIONS
              OTHERS       = 1 ).

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDLOOP.

        "Pack loose stock
        LOOP AT lo_model->mt_cons_huitm INTO DATA(ls_huitem)
                                  WHERE guid_parent = ls_huhdr-guid_hu.

          CLEAR ls_pack_stock.
          MOVE-CORRESPONDING ls_huitem TO ls_pack_stock.

          lo_packing->repack_stock(
            EXPORTING
              iv_dest_hu    = ls_huhdr_new-guid_hu
              iv_source_hu  = ls_huitem-guid_parent
              iv_stock_guid = ls_huitem-guid_stock
              is_quantity   = VALUE #( quan = ls_huitem-quan unit = ls_huitem-meins )
             EXCEPTIONS
               OTHERS        = 1 ).

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          " update GUID_HU for the serial numbers after repacking in a new HU
          CLEAR lt_cap_nums.
          lt_cap_nums = zcl_crud_ztcross_cap_nums=>select_multi_by_huid(
              iv_lgnum = lcl_controller=>sv_lgnum
              iv_huid  = ls_huitem-guid_parent ).

          CHECK  lines( lt_cap_nums ) > 0.

          LOOP AT lt_cap_nums ASSIGNING FIELD-SYMBOL(<ls_cap_num>).
            <ls_cap_num>-guid_hu = ls_huhdr_new-guid_hu.
          ENDLOOP.

          zcl_crud_ztcross_cap_nums=>modify_multi_entries( it_cap_nums = lt_cap_nums
                                                           iv_commit   = abap_false ).
        ENDLOOP.

        lo_packing->/scwm/if_pack_bas~save(
          EXPORTING
            iv_commit = abap_false
           EXCEPTIONS
             OTHERS    = 1 ).

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          COMMIT WORK.
        ENDIF.

        LEAVE TO SCREEN 0200.

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

  METHOD handle_change_carrier.

    DATA:
      lv_reject              TYPE boole_d,
      lt_return              TYPE /scdl/t_sp_return_code,
      lt_partyloc_head       TYPE /scdl/t_sp_a_head_partyloc,
      ls_key_delete_partyloc TYPE /scdl/s_sp_k_head_partyloc,
      lt_delete_inkey        TYPE /scdl/t_sp_k_head_partyloc,
      ls_outrec_rel	         TYPE /scdl/s_sp_a_head,
      lt_outrec              TYPE /scdl/t_sp_a_head_partyloc.

    IF iv_carrier IS INITIAL.
      MESSAGE e414(/scmtms/fag_msg).
    ENDIF.

    DATA(lo_message_box) = NEW /scdl/cl_sp_message_box( ).
    DATA(lo_sp) = NEW /scdl/cl_sp_prd_out(
        io_message_box = lo_message_box
        iv_mode = /scdl/cl_sp=>sc_mode_classic ).

    "Get Carrier
    lo_sp->select_by_relation(
      EXPORTING
        relation     = /scdl/if_sp_c=>sc_rel_head_to_partyloc
        inrecords    = VALUE /scdl/t_sp_k_head( ( docid = iv_docid ) )
        aspect       = /scdl/if_sp_c=>sc_asp_head
      IMPORTING
        outrecords   = lt_partyloc_head
        rejected     = lv_reject
        return_codes = lt_return ).
    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY failed = abap_true.
    IF sy-subrc = 0 OR lv_reject = abap_true.
      DATA(lt_message) = lo_message_box->get_messages(
        EXPORTING
          iv_msgty = wmegc_severity_err ).
      TRY.
          DATA(ls_message) = lt_message[ 1 ].
          MESSAGE ID ls_message-msgid TYPE ls_message-msgty NUMBER ls_message-msgno
            WITH ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4.
        CATCH cx_sy_itab_line_not_found.
          MESSAGE e019(/sapapo/cfm_msg).
      ENDTRY.
    ENDIF.

    lo_sp->lock(
      EXPORTING
        inkeys       = VALUE /scdl/t_sp_k_head( ( docid = iv_docid ) )
        aspect       = /scdl/if_sp_c=>sc_asp_head
        lockmode     = /scdl/if_sp1_locking=>sc_exclusive_lock
      IMPORTING
        rejected     = lv_reject
        return_codes = lt_return ).

    READ TABLE lt_partyloc_head ASSIGNING FIELD-SYMBOL(<ls_partyloc_carr>)
      WITH KEY party_role = /scdl/if_dl_c=>sc_party_role_carr.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING <ls_partyloc_carr> TO ls_key_delete_partyloc.
      APPEND ls_key_delete_partyloc TO lt_delete_inkey.

      "Delete carrier first
      lo_sp->delete(
        EXPORTING
          inkeys       = lt_delete_inkey
          aspect       = /scdl/if_sp_c=>sc_asp_head_partyloc
        IMPORTING
          rejected     = lv_reject
          return_codes = lt_return ).
      READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY failed = abap_true.
      IF sy-subrc = 0 OR lv_reject = abap_true.
        lt_message = lo_message_box->get_messages(
          EXPORTING
            iv_msgty = wmegc_severity_err ).
        TRY.
            ls_message = lt_message[ 1 ].
            MESSAGE ID ls_message-msgid TYPE ls_message-msgty NUMBER ls_message-msgno
              WITH ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4.
          CATCH cx_sy_itab_line_not_found.
            MESSAGE e019(/sapapo/cfm_msg).
        ENDTRY.
      ENDIF.
    ENDIF.

    lo_sp->insert(
      EXPORTING
        inrecords          = VALUE /scdl/t_sp_a_head_partyloc(
                               ( docid   = iv_docid              party_role = /scdl/if_dl_c=>sc_party_role_carr
                                 partyno = iv_carrier value_ind = 'M' ) )
        aspect             = /scdl/if_sp_c=>sc_asp_head_partyloc
        relation           = /scdl/if_sp_c=>sc_rel_head_to_partyloc
        relation_inkey     = VALUE /scdl/s_sp_k_head( docid = iv_docid )
      IMPORTING
        outrecords         = lt_outrec
        relation_outrecord = ls_outrec_rel ).
    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY failed = abap_true.
    IF sy-subrc = 0 OR lv_reject = abap_true.
      lt_message = lo_message_box->get_messages(
        EXPORTING
          iv_msgty = wmegc_severity_err ).
      TRY.
          ls_message = lt_message[ 1 ].
          MESSAGE ID ls_message-msgid TYPE ls_message-msgty NUMBER ls_message-msgno
            WITH ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4.
        CATCH cx_sy_itab_line_not_found.
          MESSAGE e019(/sapapo/cfm_msg).
      ENDTRY.
    ENDIF.

    lo_sp->save( IMPORTING rejected = lv_reject ).
    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY failed = abap_true.
    IF sy-subrc = 0 OR lv_reject = abap_true.
      ROLLBACK WORK.
      lt_message = lo_message_box->get_messages(
        EXPORTING
          iv_msgty = wmegc_severity_err ).
      TRY.
          ls_message = lt_message[ 1 ].
          MESSAGE ID ls_message-msgid TYPE ls_message-msgty NUMBER ls_message-msgno
            WITH ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4.
        CATCH cx_sy_itab_line_not_found.
          MESSAGE e019(/sapapo/cfm_msg).
      ENDTRY.
    ELSE.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.

  METHOD handle_open_hu.

    IF iv_huident IS INITIAL.
      MESSAGE e074(/scwm/rf_en).
    ENDIF.

    DATA(lo_model) = lcl_model=>get_instance( ).

    READ TABLE lo_model->mt_cons_hus TRANSPORTING NO FIELDS
      WITH KEY huident = iv_huident.
    IF sy-subrc <> 0.
      MESSAGE e076(/scwm/rf_en).
    ENDIF.

    IF lo_model->is_hu_ready( iv_huident = iv_huident ) <> abap_true.
      MESSAGE e001(zewm_cl_msg_con) WITH iv_huident.
    ENDIF.

    /scwm/cl_wm_packing=>get_instance(
      IMPORTING
        eo_instance = DATA(lo_packing) ).

    lo_packing->get_hu(
      EXPORTING
        iv_huident = iv_huident
      IMPORTING
        es_huhdr   = DATA(ls_huhdr)
      EXCEPTIONS
        OTHERS     = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CLEAR ls_huhdr-copst.

    lo_packing->change_huhdr(
      EXPORTING
        is_huhdr   = ls_huhdr
      EXCEPTIONS
        OTHERS     = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lo_packing->save(
      EXCEPTIONS
        OTHERS     = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    refresh_screen_0200( ).

  ENDMETHOD.

  METHOD handle_hu_scan_0200.

    IF cv_huident IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_model) = lcl_model=>get_instance( ).
    lo_model->build_cons_hu_table( iv_docid = lo_model->ms_cons_header-docid ).

    READ TABLE lo_model->mt_cons_huhdr INTO DATA(ls_huhdr)
      WITH KEY huident = cv_huident.
    IF sy-subrc <> 0.
      CLEAR cv_huident.
      MESSAGE s050(/scwm/rf_en)  DISPLAY LIKE wmegc_severity_err.
      RETURN.
    ENDIF.

    IF ls_huhdr-top = abap_false.
      CLEAR cv_huident.
      MESSAGE s252(/scwm/delivery) WITH cv_huident DISPLAY LIKE wmegc_severity_err.
      RETURN.
    ENDIF.

    READ TABLE lo_model->mt_cons_hus INTO DATA(ls_cons_hu)
      WITH KEY huident = cv_huident.

    IF sy-subrc = 0 AND
       ls_cons_hu-cons_compl = TEXT-018.
      CLEAR cv_huident.
      MESSAGE s011(zewm_cl_msg_con) DISPLAY LIKE wmegc_severity_err.
      RETURN.
    ENDIF.

    CLEAR: lo_model->mt_cons_hu_content, lo_model->ms_cons_hu.

    DATA(lo_view) = lcl_view=>get_instance( ).

    MOVE-CORRESPONDING zstr_consolidation_scr_0200 TO zstr_consolidation_scr_0300.
    zstr_consolidation_scr_0300-packhutyp = ls_huhdr-letyp.
    zstr_consolidation_scr_0300-packhutypt  = lo_model->get_hutype( is_huhdr = ls_huhdr ).

    lo_model->build_cons_huitem_table( EXPORTING iv_huident = cv_huident
                                                 it_huitem = lo_model->mt_cons_huitm
                                       CHANGING ct_hu_content = lo_model->mt_cons_hu_content ).

    LEAVE TO SCREEN 0300.

  ENDMETHOD.

  METHOD calc_number_of_cartons.

    DATA:
      lv_carton_per_huitem TYPE p LENGTH 8 DECIMALS 0,
      lt_mat_uom           TYPE /scwm/tt_material_uom,
      lt_guid_hu           TYPE RANGE OF /scwm/guid_hu.

    rv_cartons = 0.

    DATA(lo_model) = lcl_model=>get_instance( ).

    READ TABLE lo_model->mt_cons_huhdr INTO DATA(ls_huhdr)
      WITH KEY huident = iv_huident.
    IF sy-subrc <> 0 OR ls_huhdr-top = abap_false.
      MESSAGE e050(/scwm/rf_en).
    ENDIF.

    APPEND VALUE #( sign = /scmb/cl_search=>sc_sign_i option = /scmb/cl_search=>sc_eq low = ls_huhdr-guid_hu ) TO lt_guid_hu.
    LOOP AT lo_model->mt_cons_huhdr INTO DATA(ls_huhdr_subhu)
      WHERE higher_guid = ls_huhdr-guid_hu.
      APPEND VALUE #( sign = /scmb/cl_search=>sc_sign_i option = /scmb/cl_search=>sc_eq low = ls_huhdr_subhu-guid_hu ) TO lt_guid_hu.
    ENDLOOP.

    LOOP AT lo_model->mt_cons_huitm INTO DATA(ls_huitem)
      WHERE guid_parent IN lt_guid_hu.

      TRY.
          CLEAR lt_mat_uom.
          CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
            EXPORTING
              iv_matid   = ls_huitem-matid
              iv_lgnum   = sv_lgnum
            IMPORTING
              et_mat_uom = lt_mat_uom.
          READ TABLE lt_mat_uom INTO DATA(ls_mat_uom)
            WITH KEY meinh = ls_huitem-altme.
          IF sy-subrc <> 0 OR ls_mat_uom-umrez <= 0.
            rv_cartons = rv_cartons + 1.
            CONTINUE.
          ENDIF.

          CLEAR lv_carton_per_huitem.

          IF ls_huitem-quan MOD ls_mat_uom-umrez = 0.
            lv_carton_per_huitem = ls_huitem-quan DIV ls_mat_uom-umrez.
          ELSE.
            lv_carton_per_huitem = ls_huitem-quan DIV ls_mat_uom-umrez + 1.
          ENDIF.

          rv_cartons = rv_cartons + lv_carton_per_huitem.

        CATCH /scwm/cx_md.
          rv_cartons = rv_cartons + 1.
          CONTINUE.
      ENDTRY.

    ENDLOOP.
  ENDMETHOD.

  METHOD handle_close_hu.

    DATA:
      lv_count       TYPE int1 VALUE 1,
      ls_workstation TYPE /scwm/tworkst.

    DATA(lo_model) = lcl_model=>get_instance( ).

    DATA(lv_serial_status) = lo_model->get_serial_status( iv_huident = zstr_consolidation_scr_0300-packhu ).
    IF lv_serial_status = icon_led_red OR lv_serial_status = icon_led_yellow.
      MESSAGE e010(zewm_cl_msg_con).
    ENDIF.

    CLEAR zstr_consolidation_scr_0300-num_cartons_vrf.
    CALL SCREEN 0310 STARTING AT 10 5 ENDING AT 70 8.
    IF sy-ucomm <> 'ENTER' AND sy-ucomm <> 'OK' AND sy-ucomm <> 'SKIP'.
      RETURN.
    ENDIF.

    READ TABLE lo_model->mt_cons_huhdr INTO DATA(ls_huhdr)
      WITH KEY huident = zstr_consolidation_scr_0300-packhu.

    IF sy-ucomm <> 'SKIP'.
      DATA(lv_cartons) = calc_number_of_cartons( zstr_consolidation_scr_0300-packhu ).
    ELSE.
      lv_cartons = 0.
    ENDIF.

    DO 2 TIMES.
      IF zstr_consolidation_scr_0300-num_cartons_vrf = lv_cartons.
        zstr_consolidation_scr_0300-weight_uom = 'KG'.
        zstr_consolidation_scr_0300-length_uom = zstr_consolidation_scr_0300-widht_uom = zstr_consolidation_scr_0300-height_uom = 'CM'.

        CLEAR:
          zstr_consolidation_scr_0300-length,
          zstr_consolidation_scr_0300-widht,
          zstr_consolidation_scr_0300-height,
          zstr_consolidation_scr_0300-weight,
          zstr_consolidation_scr_0300-length_uom,
          zstr_consolidation_scr_0300-weight_uom.

        IF ls_huhdr-length IS NOT INITIAL.
          zstr_consolidation_scr_0300-length = ls_huhdr-length.
        ELSE.
          zstr_consolidation_scr_0300-length = ls_huhdr-max_length.
        ENDIF.

        IF ls_huhdr-width IS NOT INITIAL.
          zstr_consolidation_scr_0300-widht  = ls_huhdr-width.
        ELSE.
          zstr_consolidation_scr_0300-widht  = ls_huhdr-max_width.
        ENDIF.

        IF ls_huhdr-height IS NOT INITIAL.
          zstr_consolidation_scr_0300-height = ls_huhdr-height.
        ELSE.
          zstr_consolidation_scr_0300-height = ls_huhdr-max_height.
        ENDIF.

        zstr_consolidation_scr_0300-weight = ls_huhdr-g_weight.
        zstr_consolidation_scr_0300-length_uom = ls_huhdr-unit_lwh.
        zstr_consolidation_scr_0300-height_uom = ls_huhdr-unit_lwh.
        zstr_consolidation_scr_0300-widht_uom = ls_huhdr-unit_lwh.
        zstr_consolidation_scr_0300-weight_uom = ls_huhdr-unit_gw.

        CALL SCREEN 0320 STARTING AT 10 5 ENDING AT 90 12.

        IF sy-ucomm = 'OK'.
          cancel_wts_for_hu( iv_huident = ls_huhdr-huident ).
          redetermine_staging_area( iv_huident = ls_huhdr-huident ).

          /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = DATA(lo_packing) ).

          CALL FUNCTION '/SCWM/TWORKST_READ_SINGLE'
            EXPORTING
              iv_lgnum       = sv_lgnum
              iv_workstation = ssout_term_def-workstation
            IMPORTING
              es_workst      = ls_workstation
            EXCEPTIONS
              OTHERS         = 1.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          /scwm/cl_tm=>set_lgnum( iv_lgnum = sv_lgnum ).

          lo_packing->init_by_workstation(
            EXPORTING
              is_workstation   = ls_workstation
              ir_huident       = VALUE #( ( sign = /scmb/cl_search=>sc_sign_i option = /scmb/cl_search=>sc_eq low = ls_huhdr-huident ) )
             EXCEPTIONS
               OTHERS           = 1 ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          lo_packing->print_hu_label(
            EXPORTING
              iv_hu   = ls_huhdr-guid_hu
              is_data = ls_workstation
            EXCEPTIONS
               OTHERS        = 1 ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          IF ls_huhdr-length <> zstr_consolidation_scr_0300-length OR
             ls_huhdr-width <> zstr_consolidation_scr_0300-widht OR
             ls_huhdr-height <> zstr_consolidation_scr_0300-height OR
             ls_huhdr-g_weight <> zstr_consolidation_scr_0300-weight.

            ls_huhdr-length   = zstr_consolidation_scr_0300-length.
            ls_huhdr-width    = zstr_consolidation_scr_0300-widht.
            ls_huhdr-height   = zstr_consolidation_scr_0300-height.
            ls_huhdr-g_weight = zstr_consolidation_scr_0300-weight.
            ls_huhdr-unit_lwh = zstr_consolidation_scr_0300-length_uom.
            ls_huhdr-unit_gw  = zstr_consolidation_scr_0300-weight_uom.

            lo_packing->change_huhdr(
              EXPORTING
                is_huhdr   = ls_huhdr
              EXCEPTIONS
                OTHERS     = 1 ).
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
            DATA(lv_save) = abap_true.
          ENDIF.

          IF ls_huhdr-procs = sv_procs.

            CALL FUNCTION '/SCWM/TO_INIT_NEW'
              EXPORTING
                iv_lgnum = sv_lgnum.

            lo_packing->hu_process_completed(
              EXPORTING
                iv_hu  = ls_huhdr-guid_hu
              EXCEPTIONS
                OTHERS     = 1 ).

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.

            lv_save = abap_true.

          ENDIF.

          IF lv_save = abap_true.

            lo_packing->save(
                EXCEPTIONS
                  OTHERS     = 1 ).

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.

            " Andriyan Yordanov- send ship request after HU Compl. step
            zcl_vce_request=>send_ship_request(
              iv_lgnum   = sv_lgnum                 " Warehouse Number/Warehouse Complex
              iv_huident = ls_huhdr-huident         " Handling Unit Identification
              iv_printer = ssout_term_def-printer ).

            CALL SCREEN 0380 STARTING AT 10 5.
            "end Andriyan Yordanov

          ENDIF.

          CLEAR zstr_consolidation_scr_0300.
          LEAVE TO SCREEN 0200.

        ELSEIF sy-ucomm = 'REFRESH'.

          SELECT SINGLE * FROM /scwm/huhdr
            INTO @DATA(ls_huhdr_db)
            WHERE huident = @zstr_consolidation_scr_0300-packhu.

          IF sy-subrc = 0.
            zstr_consolidation_scr_0300-weight = ls_huhdr_db-g_weight.
            zstr_consolidation_scr_0300-length_uom = ls_huhdr_db-unit_lwh.
            zstr_consolidation_scr_0300-height_uom = ls_huhdr_db-unit_lwh.
            zstr_consolidation_scr_0300-widht_uom = ls_huhdr_db-unit_lwh.
            zstr_consolidation_scr_0300-weight_uom = ls_huhdr_db-unit_gw.
          ENDIF.

        ELSE.
          "LEAVE TO SCREEN 0300.
          RETURN.
        ENDIF.
        EXIT.
      ELSE.
        IF lv_count = 2.
          "Call exception handling
          lo_model->init_diffhu_tables( zstr_consolidation_scr_0300-packhu ).
          DATA(lo_view) = lcl_view=>get_instance( ).
          lo_view->mt_diff_hu_src = lo_model->mt_diff_hu_src.
          CLEAR lo_view->mt_diff_hu_src_edit.

          LOOP AT lo_view->mt_diff_hu_src INTO DATA(ls_line).
            APPEND INITIAL LINE TO lo_view->mt_diff_hu_src_edit ASSIGNING FIELD-SYMBOL(<ls_edit>).
            MOVE-CORRESPONDING ls_line TO <ls_edit>.
          ENDLOOP.

          LEAVE TO SCREEN 0500.
        ELSE.
          lv_count = lv_count + 1.
          CLEAR zstr_consolidation_scr_0300-num_cartons_vrf.
          CALL SCREEN 0330 STARTING AT 10 5 ENDING AT 70 9.
          IF sy-ucomm <> 'ENTER' AND sy-ucomm <> 'OK'.
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD handle_merge_hu.

    DATA: lv_huident TYPE /scwm/de_huident.

    CLEAR: zstr_consolidation_scr_0300-desthu,
           zstr_consolidation_scr_0300-hutype.

    CALL SCREEN 0340 STARTING AT 10 5 ENDING AT 120 10.

    IF sy-ucomm <> 'ENTER'.
      RETURN.
    ENDIF.

    lv_huident = |{ zstr_consolidation_scr_0300-desthu ALPHA = IN }|.

    /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = DATA(lo_packing) ).

    lo_packing->init(
      EXPORTING
        iv_lgnum = sv_lgnum
      EXCEPTIONS
        OTHERS   = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lo_packing->get_hu(
       EXPORTING
         iv_huident = lv_huident
       IMPORTING
         es_huhdr   = DATA(ls_huhdr_dst)
       EXCEPTIONS
         OTHERS     = 1 ).

    IF sy-subrc <> 0.
      CLEAR ls_huhdr_dst.
    ENDIF.

    DATA(lo_model) = lcl_model=>get_instance( ).
    READ TABLE lo_model->mt_cons_huhdr INTO DATA(ls_huhdr_src)
      WITH KEY huident = zstr_consolidation_scr_0300-packhu.

    IF sy-subrc = 0.
      DATA(lv_lgpla) = ls_huhdr_src-lgpla.
    ELSE.
      lv_lgpla = ls_huhdr_dst-lgpla." 'SPED-0001'. Andriyan Yordanov
    ENDIF.

    "Check HU types. Merging pallet with parcel is not allowed
    "Parcel to parcel and pallet to parcel is allowed
    IF ls_huhdr_dst IS NOT INITIAL.
      IF lo_model->is_pallet( is_huhdr = ls_huhdr_src ) = abap_true AND
         lo_model->is_pallet( is_huhdr = ls_huhdr_dst ) = abap_false.
        MESSAGE e006(zewm_cl_msg_con).
      ENDIF.
    ENDIF.

    IF ls_huhdr_dst IS INITIAL.

      IF zstr_consolidation_scr_0300-newhu_pmat IS INITIAL.
        MESSAGE e017(zewm_cl_msg_con).
      ENDIF.

      " create a new HU for merge logic
      create_hu(
        EXPORTING
          iv_lgpla   = lv_lgpla
          iv_pmat    = zstr_consolidation_scr_0300-newhu_pmat
          iv_huident = lv_huident ).

    ENDIF.

    pack_all_stock_to_hu(
      EXPORTING
        iv_move_hu     = lo_model->is_shipping_carton( is_huhdr = ls_huhdr_src ) " Andriyan Yordanov move whole source HU to the destination
        iv_huident_src = zstr_consolidation_scr_0300-packhu
        iv_huident_des = lv_huident ).

    refresh_screen_0300( lv_huident ).
    zstr_consolidation_scr_0300-packhu = lv_huident.

  ENDMETHOD.

  METHOD handle_split_hu.

    DATA: lv_huident TYPE /scwm/de_huident.

    CLEAR zstr_consolidation_scr_0300-newhu_pmat.

    DATA(lo_model) = lcl_model=>get_instance( ).

    READ TABLE lo_model->mt_cons_huhdr INTO DATA(ls_huhdr_src)
      WITH KEY huident = zstr_consolidation_scr_0300-packhu.

    IF sy-subrc = 0 AND lo_model->is_pallet( is_huhdr = ls_huhdr_src ) = abap_false.
      MESSAGE e008(zewm_cl_msg_con).
    ENDIF.

    CALL SCREEN 0350 STARTING AT 10 5 ENDING AT 120 10.

    IF sy-ucomm <> 'ENTER' AND
       sy-ucomm <> 'OK'.
      RETURN.
    ENDIF.

    lv_huident = |{ zstr_consolidation_scr_0300-desthu ALPHA = IN }|.

    /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = DATA(lo_packing) ).

    lo_packing->init(
      EXPORTING
        iv_lgnum = sv_lgnum
      EXCEPTIONS
        OTHERS   = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lo_packing->get_hu(
       EXPORTING
         iv_huident = lv_huident
       IMPORTING
         es_huhdr   = DATA(ls_huhdr_dst)
       EXCEPTIONS
         OTHERS     = 1 ).
    IF sy-subrc <> 0.
      CLEAR ls_huhdr_dst.
    ENDIF.

    IF ls_huhdr_dst IS INITIAL.
      " Andriyan Yordanov - craete HU Directly from the pop-up
      DATA(lv_packmat) = zstr_consolidation_scr_0300-newhu_pmat.
***      SELECT SINGLE matnr FROM mara INTO @DATA(lv_packmat)
***        WHERE hutyp = @zstr_consolidation_scr_0300-hutype.
      IF lv_packmat IS INITIAL.
        MESSAGE e012(zewm_cl_msg_con) WITH lv_huident.
      ENDIF.

      create_hu(
        EXPORTING
          iv_lgpla   = ls_huhdr_src-lgpla
          iv_pmat    = lv_packmat
          iv_huident = lv_huident ).

    ELSEIF ls_huhdr_dst-top = abap_false.
      MESSAGE e252(/scwm/delivery) WITH lv_huident.
    ENDIF.

    CLEAR zstr_consolidation_scr_0400.
    MOVE-CORRESPONDING zstr_consolidation_scr_0300 TO zstr_consolidation_scr_0400.
    zstr_consolidation_scr_0400-source_hu = zstr_consolidation_scr_0300-packhu.
    zstr_consolidation_scr_0400-dest_hu = zstr_consolidation_scr_0300-desthu.

    LEAVE TO SCREEN 0400.
  ENDMETHOD.

  METHOD handle_serials.

    DATA(lo_view) = lcl_view=>get_instance( ).
    LOOP AT lo_view->mt_cons_hu_content TRANSPORTING NO FIELDS
      WHERE serial_status <> space.
      EXIT.
    ENDLOOP.

***    LOOP AT lo_view->mt_cons_sn_reques_cont TRANSPORTING NO FIELDS
***    WHERE serial_status <> space.
***      EXIT.
***    ENDLOOP.
    IF sy-subrc <> 0.
      MESSAGE e005(zewm_cl_msg_con).
    ENDIF.

    CLEAR: zstr_consolidation_scr_0600,
           lo_view->ms_cons_hu_serial. " Andriyan Yordanov

    CALL SCREEN 0600.

  ENDMETHOD.

  METHOD refresh_screen_0100.

    DATA(lo_view) = lcl_view=>get_instance( ).
    CLEAR: zstr_consolidation_scr_0200, zstr_consolidation_scr_0300, zstr_consolidation_scr_0400,
      lo_view->mt_cons_hus, lo_view->mt_cons_hu_content, lo_view->mt_cons_sn_reques_cont, lo_view->mt_cons_headers.

    DATA(lo_model) = lcl_model=>get_instance( ).
    CLEAR: lo_model->ms_cons_hu, lo_model->mt_cons_hus, lo_model->ms_cons_hu_content,
      lo_model->mt_cons_hu_content, lo_model->mt_cons_headers, lo_model->mt_cons_huitm,
      lo_model->mt_dlv_headers, lo_model->mt_dlv_items, lo_model->mt_cons_huhdr, lo_model->ms_cons_header.

    CALL FUNCTION '/SCWM/HUMAIN_REFRESH'.

    selection( iv_lgnum    = sv_lgnum
               is_term_def = ssout_term_def ).

  ENDMETHOD.

  METHOD refresh_screen_0200.

    DATA(lo_view) = lcl_view=>get_instance( ).
    CLEAR: zstr_consolidation_scr_0200, zstr_consolidation_scr_0300, zstr_consolidation_scr_0400,
      lo_view->mt_cons_hus, lo_view->mt_cons_hu_content, lo_view->mt_cons_sn_reques_cont.

    DATA(lo_model) = lcl_model=>get_instance( ).

    CLEAR: lo_model->ms_cons_hu, lo_model->mt_cons_hus, lo_model->mt_cons_huhdr, lo_model->mt_cons_huitm,
      lo_model->ms_cons_hu_content, lo_model->mt_cons_hu_content.

    lo_model->set_lgtype( sv_lgtyp ).
***    lo_model->set_aarea( iv_aarea = sv_aarea ).
    lo_model->set_procs( iv_procs = sv_procs ).
    lo_model->get_hus_on_cons_bin( ).

    MOVE-CORRESPONDING lo_model->ms_cons_header TO zstr_consolidation_scr_0200.
    lo_model->build_cons_hu_table( lo_model->ms_cons_header-docid ).

  ENDMETHOD.

  METHOD refresh_screen_0300.

    DATA:
      lv_huident TYPE /scwm/de_huident.

    CLEAR: zstr_consolidation_scr_0300, zstr_consolidation_scr_0400.
    MOVE-CORRESPONDING zstr_consolidation_scr_0200 TO zstr_consolidation_scr_0300.

    DATA(lo_view) = lcl_view=>get_instance( ).
    CLEAR: lo_view->mt_cons_hus, lo_view->mt_cons_hu_content, lo_view->mt_cons_sn_reques_cont.

    DATA(lo_model) = lcl_model=>get_instance( ).
    CLEAR: lo_model->ms_cons_hu_content, lo_model->mt_cons_hu_content.

    lo_model->build_cons_hu_table( iv_docid = lo_model->ms_cons_header-docid ).

    IF iv_huident IS NOT INITIAL.
      lv_huident = iv_huident.
    ELSE.
      lv_huident = lo_model->ms_cons_hu-huident.
    ENDIF.

    lo_model->build_cons_huitem_table( EXPORTING iv_huident = lv_huident
                                                 it_huitem = lo_model->mt_cons_huitm
                                       CHANGING ct_hu_content = lo_model->mt_cons_hu_content ).

    DATA(lo_container_huitem) = NEW cl_gui_custom_container( container_name = 'CONT_HU_ITEMS' ).
    lo_view = lcl_view=>get_instance( ).
    lo_view->show_alv_cons_hu_content( lo_container_huitem ).

  ENDMETHOD.

  METHOD user_command_0400.
    CASE sy-ucomm.
      WHEN 'BACK'.
        refresh_screen_0300( ).
        LEAVE TO SCREEN 0300.
      WHEN 'REFRESH'.
        "refresh_screen_0400( ).
      WHEN 'EXIT' OR 'CANC' OR 'LOGOUT'.
        LEAVE PROGRAM.
      WHEN 'ENTER'.
        handle_scan_0400( zstr_consolidation_scr_0400-scan_input ).
    ENDCASE.
  ENDMETHOD.

  METHOD handle_scan_0400.

    DATA:
      lv_src_hu_split_wt TYPE /scwm/ltap_vlenr,
      lv_huident_dest    TYPE /scwm/de_huident,
      ls_huitem          TYPE /scwm/s_huitm_int,
      ls_huheader_dest   TYPE /scwm/s_huhdr_int,
      ls_quan            TYPE /scwm/s_quan,
      lt_mat_uom         TYPE /scwm/tt_material_uom,
      lt_src_hu_content  TYPE ztt_cons_hu_item,
      lt_guid_subhu_r    TYPE RANGE OF /scwm/guid_hu,
      lt_guid_hu_r       TYPE RANGE OF /scwm/guid_hu.

    DATA(lo_model) = lcl_model=>get_instance( ).

    check_scan_0400(
      EXPORTING
        iv_scan  = iv_scan
      IMPORTING
        ev_matid   = DATA(lv_matid)
        ev_huident = DATA(lv_huident) ).

    IF lv_matid IS NOT INITIAL.

      READ TABLE lo_model->mt_cons_huhdr INTO DATA(ls_huhdr)
        WITH KEY huident = zstr_consolidation_scr_0300-packhu.

      IF sy-subrc <> 0.
        MESSAGE e050(/scwm/rf_en).
      ENDIF.

      " Andriyan Yordanov
      lt_src_hu_content = VALUE #( FOR <ls_cont> IN lo_model->mt_cons_hu_content
                                                WHERE ( matid = lv_matid )
                                                ( <ls_cont> ) ).

      LOOP AT lt_src_hu_content ASSIGNING FIELD-SYMBOL(<ls_check_amazon>)
                                                 WHERE huident <> '-'.
        ASSIGN lo_model->mt_cons_huhdr[ huident = <ls_check_amazon>-huident ] TO FIELD-SYMBOL(<ls_huhdr_mc_amazon>).
        CHECK sy-subrc = 0.
        CHECK  lo_model->is_master_carton( is_huhdr = <ls_huhdr_mc_amazon> ) = abap_true.
        DELETE lt_src_hu_content.
      ENDLOOP.

      CLEAR zstr_consolidation_scr_0410.
      zstr_consolidation_scr_0410-matid = lv_matid.

      IF lines( lt_src_hu_content ) > 1.

        CALL SCREEN 0410 STARTING AT 10 5 ENDING AT 50 10.

        IF sy-ucomm <> 'OK' AND
           sy-ucomm <> 'ENTER'.
          " if user did not populated the exact. qty we will jump back to the split 400 screen
          CLEAR zstr_consolidation_scr_0400-scan_input.
          RETURN.
        ENDIF.

        IF lo_model->check_sn_track( ) = abap_true. " AND

          CALL SCREEN 0420 STARTING AT 10 5.

          IF sy-ucomm <> 'OK' AND
             sy-ucomm <> 'ENTER'.
            " if user did not populated the exact. SN we will jump back to the split 400 screen
            CLEAR zstr_consolidation_scr_0400-scan_input.
            RETURN.
          ENDIF.
        ENDIF.

        lv_src_hu_split_wt = zstr_consolidation_scr_0410-vlenr.
        ls_quan = VALUE #( quan = zstr_consolidation_scr_0410-quan
                           unit = zstr_consolidation_scr_0410-meins ).
        ls_huitem-guid_stock  = zstr_consolidation_scr_0410-guid_stock.
        ls_huitem-guid_parent = zstr_consolidation_scr_0410-sguid_hu.

        IF lv_src_hu_split_wt   IS INITIAL OR
           ls_huitem-guid_stock IS INITIAL .
          MESSAGE e015(zewm_cl_msg_con).
        ENDIF.

        " End Andriyan Yordanov
      ELSE.

        APPEND VALUE #( sign   = /scmb/cl_search=>sc_sign_i
                        option = /scmb/cl_search=>sc_eq
                        low    = ls_huhdr-guid_hu ) TO lt_guid_hu_r.

        IF lo_model->check_sn_track( ) = abap_true. " AND
          CLEAR lo_model->mt_sn_split_pall.
          CALL SCREEN 0420 STARTING AT 10 5.

          IF sy-ucomm <> 'OK' AND
             sy-ucomm <> 'ENTER'.
            " if user did not populated the exact. SN we will jump back to the split 400 screen
            CLEAR zstr_consolidation_scr_0400-scan_input.
            RETURN.
          ENDIF.

        ENDIF.

        "end check sn scan
        IF ls_huhdr-top = abap_true AND ls_huhdr-bottom = abap_false.
          LOOP AT lo_model->mt_cons_huhdr INTO DATA(ls_huhdr_subhu)
                                              WHERE higher_guid = ls_huhdr-guid_hu.

            APPEND VALUE #( sign   = /scmb/cl_search=>sc_sign_i
                            option = /scmb/cl_search=>sc_eq
                            low    = ls_huhdr_subhu-guid_hu ) TO lt_guid_hu_r.
          ENDLOOP.
        ENDIF.

        LOOP AT lo_model->mt_cons_huitm INTO ls_huitem
          WHERE matid = lv_matid AND
                guid_parent IN lt_guid_hu_r.
          EXIT.
        ENDLOOP.

        IF sy-subrc = 0.
          TRY.
              CLEAR lt_mat_uom.

              CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
                EXPORTING
                  iv_matid   = ls_huitem-matid
                  iv_lgnum   = sv_lgnum
                IMPORTING
                  et_mat_uom = lt_mat_uom.

              READ TABLE lt_mat_uom INTO DATA(ls_mat_uom)
                WITH KEY meinh = ls_huitem-altme.

              IF sy-subrc <> 0 OR ls_mat_uom-umrez <= 0.
                ls_quan-quan = ls_huitem-quan.
                ls_quan-unit = ls_huitem-meins.
              ELSE.

                IF ls_huitem-quan >= ls_mat_uom-umrez.
                  ls_quan-quan = ls_mat_uom-umrez.
                  ls_quan-unit = ls_huitem-altme.
                ELSE.
                  ls_quan-quan = ls_huitem-quan.
                  ls_quan-unit = ls_huitem-meins.
                ENDIF.

              ENDIF.

            CATCH /scwm/cx_md.
              ls_quan-quan = ls_huitem-quan.
              ls_quan-unit = ls_huitem-meins.
          ENDTRY.

        ENDIF.
      ENDIF.

      pack_stock(
        EXPORTING
          iv_huident_src = COND #( WHEN lv_src_hu_split_wt IS INITIAL
                                    THEN zstr_consolidation_scr_0400-source_hu
                                    ELSE lv_src_hu_split_wt )
          iv_huident_des = zstr_consolidation_scr_0400-dest_hu
          iv_guid_stock  = ls_huitem-guid_stock
          is_quan        = ls_quan ).

      " move SN
      ASSIGN lo_model->mt_cons_huhdr[ huident = COND #( WHEN lv_src_hu_split_wt IS INITIAL
                             THEN zstr_consolidation_scr_0400-source_hu
                             ELSE lv_src_hu_split_wt )  ] TO FIELD-SYMBOL(<ls_src_hu_key>).

      IF sy-subrc = 0.

        lv_huident_dest = |{ zstr_consolidation_scr_0400-dest_hu ALPHA = IN }|.

        CALL FUNCTION '/SCWM/HUHEADER_READ'
          EXPORTING
            iv_appl     = wmegc_huappl_wme
            iv_huident  = lv_huident_dest
          IMPORTING
            es_huheader = ls_huheader_dest
          EXCEPTIONS
            OTHERS      = 1.

        IF sy-subrc = 0 OR
           ls_huheader_dest IS NOT INITIAL.

****          ASSIGN lo_model->mt_cons_huhdr[ huident = zstr_consolidation_scr_0400-dest_hu ] TO FIELD-SYMBOL(<ls_dest_hu_key>).
****
****          IF sy-subrc = 0.
          zcl_crud_ztcross_cap_nums=>modify_multi_huid_sn(
            iv_lgnum            = lcl_controller=>sv_lgnum                     " Warehouse Number/Warehouse Complex
            iv_src_guid_hu      = <ls_src_hu_key>-guid_hu                      " Unique Internal Identification of a Handling Unit
            iv_dest_guid_hu     = ls_huheader_dest-guid_hu                     " Unique Internal Identification of a Handling Unit
            iv_sn_group         = zstr_consolidation_scr_0400-serial_number    " Serial Number
            iv_commit           = abap_true  ).

          IF lo_model->is_pallet( is_huhdr = ls_huheader_dest ).
            " do regruping destination of the SN
            zcl_crud_ztcross_cap_nums=>modify_regroup_sn(
              iv_lgnum        = lcl_controller=>sv_lgnum
              iv_dest_guid_hu = ls_huheader_dest-guid_hu
             iv_commit       = abap_true  ).
          ENDIF.

          IF lo_model->is_pallet( is_huhdr = <ls_src_hu_key> ).
            " we should re-group source if we have some remainign QTY inside
            zcl_crud_ztcross_cap_nums=>modify_regroup_sn(
                        iv_lgnum        = lcl_controller=>sv_lgnum
                        iv_dest_guid_hu = <ls_src_hu_key>-guid_hu
                        iv_commit       = abap_true  ).
          ENDIF.

        ENDIF.
      ENDIF.


    ELSEIF lv_huident IS NOT INITIAL.

      check_scanned(
        iv_huident_src = lv_huident
        iv_huident_des = |{ zstr_consolidation_scr_0400-packhu ALPHA = IN }| ).

      pack_hu(
        EXPORTING
          iv_huident_src = lv_huident
          iv_huident_des = zstr_consolidation_scr_0400-dest_hu ).

      " move SN
      ASSIGN lo_model->mt_cons_huhdr[ huident = lv_huident ] TO <ls_src_hu_key>.

      IF sy-subrc = 0.

        lv_huident_dest = |{ zstr_consolidation_scr_0400-dest_hu ALPHA = IN }|.

        CALL FUNCTION '/SCWM/HUHEADER_READ'
          EXPORTING
            iv_appl     = wmegc_huappl_wme
            iv_huident  = lv_huident_dest
          IMPORTING
            es_huheader = ls_huheader_dest
          EXCEPTIONS
            OTHERS      = 1.

        IF sy-subrc = 0 OR
           ls_huheader_dest IS NOT INITIAL.

          zcl_crud_ztcross_cap_nums=>modify_multi_huid_sn(
            iv_lgnum            = lcl_controller=>sv_lgnum                     " Warehouse Number/Warehouse Complex
            iv_src_guid_hu      = <ls_src_hu_key>-guid_hu                      " Unique Internal Identification of a Handling Unit
            iv_dest_guid_hu     = ls_huheader_dest-guid_hu                     " Unique Internal Identification of a Handling Unit
            iv_commit           = abap_true  ).


          IF lo_model->is_pallet( is_huhdr = ls_huheader_dest ).
            " do regruping of the SN
            zcl_crud_ztcross_cap_nums=>modify_regroup_sn(
              iv_lgnum        = lcl_controller=>sv_lgnum
              iv_dest_guid_hu = ls_huheader_dest-guid_hu
             iv_commit        = abap_true  ).
          ENDIF.

          IF lo_model->is_pallet( is_huhdr = <ls_src_hu_key> ).
            " we should re-group source if we have some remainign QTY inside
            zcl_crud_ztcross_cap_nums=>modify_regroup_sn(
                        iv_lgnum        = lcl_controller=>sv_lgnum
                        iv_dest_guid_hu = <ls_src_hu_key>-guid_hu
                        iv_commit       = abap_true  ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR: lo_model->mt_cons_huhdr,
           lo_model->mt_cons_huitm,
           lo_model->mt_cons_hu_content,
           lo_model->mt_dest_hu_content.

    lo_model->set_lgtype( sv_lgtyp ).
    lo_model->set_procs( iv_procs = sv_procs ).
    lo_model->get_hus_on_cons_bin( ).

    lo_model->build_cons_huitem_table( EXPORTING iv_huident    = zstr_consolidation_scr_0400-source_hu
                                                 it_huitem     = lo_model->mt_cons_huitm
                                       CHANGING  ct_hu_content = lo_model->mt_cons_hu_content ).

    lo_model->build_cons_huitem_table( EXPORTING iv_huident    = zstr_consolidation_scr_0400-dest_hu
                                                 it_huitem     = lo_model->mt_cons_huitm
                                                 iv_split      = abap_true
                                       CHANGING  ct_hu_content = lo_model->mt_dest_hu_content ).

    CLEAR zstr_consolidation_scr_0400-scan_input.

  ENDMETHOD.

  METHOD user_command_0410.

    CASE sy-ucomm.
      WHEN 'OK' OR 'ENTER'.

        split_popup_find_mc_matnr( EXPORTING iv_hu_top     = zstr_consolidation_scr_0300-packhu
                                   CHANGING cs_qty_prod_mc = zstr_consolidation_scr_0410 ).

        LEAVE TO SCREEN 0.

      WHEN 'CANC' OR 'CANCEL' OR 'LOGOUT'.
        LEAVE TO SCREEN 0.
    ENDCASE.

  ENDMETHOD.


  METHOD user_command_0420.

    CASE sy-ucomm.
      WHEN 'OK' OR 'ENTER'.

        IF zstr_consolidation_scr_0400-serial_number IS INITIAL.
          MESSAGE s021(zewm_cl_msg_con) DISPLAY LIKE wmegc_severity_err.
          RETURN.
        ENDIF.

        DATA(lt_sn_cap_num) = zcl_crud_ztcross_cap_nums=>select_multi_by_full_serials(
            EXPORTING
              iv_lgnum    = lcl_controller=>sv_lgnum
              it_serials  = VALUE #( ( full_serial_number = zstr_consolidation_scr_0400-serial_number ) ) ).

        IF lines( lt_sn_cap_num ) = 0.
          CLEAR zstr_consolidation_scr_0400-serial_number.
          MESSAGE s022(zewm_cl_msg_con) DISPLAY LIKE wmegc_severity_err.
          RETURN.
        ENDIF.

        DATA(lo_model) = lcl_model=>get_instance( ).

        ASSIGN lo_model->mt_cons_huhdr[ guid_hu = lt_sn_cap_num[ 1 ]-guid_hu ] TO FIELD-SYMBOL(<ls_hu_mc>).

        IF sy-subrc <> 0.
          CLEAR zstr_consolidation_scr_0400-serial_number.
          MESSAGE s023(zewm_cl_msg_con) DISPLAY LIKE wmegc_severity_err.
          RETURN.
        ENDIF.

        IF lo_model->mt_sn_split_pall IS NOT INITIAL.
          " qty field scanned
          ASSIGN  lo_model->mt_sn_split_pall[ sguid_hu = <ls_hu_mc>-guid_hu ] TO FIELD-SYMBOL(<ls_scan_mat_qty>).
          IF sy-subrc = 0.
            zstr_consolidation_scr_0410 = <ls_scan_mat_qty>.
          ELSE.
            CLEAR zstr_consolidation_scr_0400-serial_number.
            MESSAGE s024(zewm_cl_msg_con) DISPLAY LIKE wmegc_severity_err.
            RETURN.
          ENDIF.

        ELSE.
          " no QTY field scan
          IF NOT line_exists( lo_model->mt_cons_huitm[ guid_parent = <ls_hu_mc>-guid_hu
                                                       matid       =  zstr_consolidation_scr_0410-matid ] ).
            CLEAR zstr_consolidation_scr_0400-serial_number.
            MESSAGE s024(zewm_cl_msg_con) DISPLAY LIKE wmegc_severity_err.
            RETURN.
          ELSE.
            zstr_consolidation_scr_0400-source_hu = <ls_hu_mc>-huident.
          ENDIF.
        ENDIF.

        LEAVE TO SCREEN 0.

      WHEN 'CANC' OR 'CANCEL' OR 'LOGOUT'.
        LEAVE TO SCREEN 0.
    ENDCASE.

  ENDMETHOD.


  METHOD user_command_0610.

    DATA: lv_serial   TYPE string,
          lt_cap_nums TYPE zcl_crud_ztcross_cap_nums=>tt_ztcross_cap_nums.

    CASE sy-ucomm.
      WHEN 'OK' OR 'ENTER'.

        IF zstr_consolidation_scr_0600-vendor_huident IS INITIAL.
          MESSAGE s029(zewm_cl_msg_con) DISPLAY LIKE wmegc_severity_err.
          RETURN.
        ENDIF.

        DATA(lo_model) = lcl_model=>get_instance( ).

        ASSIGN lo_model->mt_cons_huhdr[ huident = zstr_consolidation_scr_0600-packhu ] TO FIELD-SYMBOL(<ls_huhdr>).

        IF sy-subrc <> 0.
          MESSAGE s025(zewm_cl_msg_con) DISPLAY LIKE wmegc_severity_err.
          RETURN.
        ENDIF.

        ASSIGN   lo_model->mt_cons_huitm[ guid_parent = <ls_huhdr>-guid_hu ] TO  FIELD-SYMBOL(<ls_huitem>) .

        IF sy-subrc <> 0.
          MESSAGE s025(zewm_cl_msg_con) DISPLAY LIKE wmegc_severity_err.
          RETURN.
        ENDIF.

        ASSIGN lo_model->mt_dlv_items[ docid  = <ls_huitem>-qdocid
                                     itemid = <ls_huitem>-qitmid ] TO FIELD-SYMBOL(<ls_dlv_item>).
        IF sy-subrc <> 0.
          MESSAGE s025(zewm_cl_msg_con) DISPLAY LIKE wmegc_severity_err.
          RETURN.
        ENDIF.

        SELECT FROM ztcross_preadvic
           FIELDS serial
           WHERE lgnum = @lcl_controller=>sv_lgnum AND
                 huident = @zstr_consolidation_scr_0600-vendor_huident
          INTO TABLE @DATA(lt_sn_vendor).

        IF sy-subrc <> 0.
          MESSAGE s030(zewm_cl_msg_con) DISPLAY LIKE wmegc_severity_err.
          RETURN.
        ENDIF.

        ASSIGN lo_model->mt_cons_hu_content[ 1 ] TO FIELD-SYMBOL(<ls_cons_qty_check>).

        IF sy-subrc <> 0.
          MESSAGE s025(zewm_cl_msg_con) DISPLAY LIKE wmegc_severity_err.
          RETURN.
        ENDIF.

        IF lines( lt_sn_vendor ) <> ( <ls_cons_qty_check>-quan * <ls_cons_qty_check>-mc ).
          CLEAR zstr_consolidation_scr_0600-vendor_huident.
          MESSAGE s031(zewm_cl_msg_con) DISPLAY LIKE wmegc_severity_err.
          RETURN.
        ENDIF.

        zcl_param=>get_parameter(
          EXPORTING
            iv_lgnum     = lcl_controller=>sv_lgnum
            iv_process   = zif_param_const=>c_zout_0008
            iv_parameter = zif_param_const=>c_indenttab01
          IMPORTING
            ev_constant  = DATA(lv_ident) ).

        GET TIME STAMP FIELD DATA(lv_cre_time).

        LOOP AT lt_sn_vendor ASSIGNING FIELD-SYMBOL(<ls_sn_vendor>).

          DATA(lv_len) = strlen( <ls_sn_vendor>-serial ).

          lv_len = COND #( WHEN lv_len > 18
                              THEN 18
                              ELSE lv_len ).

          lv_serial = substring( val = <ls_sn_vendor>-serial
                                 off = strlen( <ls_sn_vendor>-serial ) - lv_len
                                 len = lv_len ).

          APPEND VALUE #( lgnum   = lcl_controller=>sv_lgnum
                          docno   = <ls_dlv_item>-docno
                          itemno  = <ls_dlv_item>-itemno
                          serial  = lv_serial
                          id_type = lv_ident
                          full_serial_number = <ls_sn_vendor>-serial
                          guid_hu    = <ls_huhdr>-guid_hu
***                          group_mc   = lv_count_pall
                          created_by = sy-uname
                          created_at = lv_cre_time ) TO lt_cap_nums.
        ENDLOOP.

        DATA(lo_view) = lcl_view=>get_instance( ).

        DATA lv_line TYPE sy-index.
        " add grouping
        LOOP AT lo_view->mt_cons_sn_reques_cont ASSIGNING FIELD-SYMBOL(<ls_cons_sn_request>).
          DO <ls_cons_sn_request>-quan TIMES.
            lv_line = lv_line + 1.
            ASSIGN lt_cap_nums[ lv_line ] TO FIELD-SYMBOL(<ls_cap_num_group>).
            IF sy-subrc = 0.
              <ls_cap_num_group>-group_mc = <ls_cons_sn_request>-group_mc.
            ENDIF.
          ENDDO.
        ENDLOOP.

        IF lt_cap_nums IS NOT INITIAL.
          zcl_crud_ztcross_cap_nums=>insert_multi_entries(
            it_cap_nums  = lt_cap_nums ).
        ENDIF.

        LEAVE TO SCREEN 0.

      WHEN 'CANC' OR 'CANCEL' OR 'LOGOUT'.
        LEAVE TO SCREEN 0.
    ENDCASE.

  ENDMETHOD.

  METHOD split_popup_find_mc_matnr.
    " Andriyan Yordanov
    " during split when user scan product which is packed in more then one HUs
    " we should find exact MC which we should move to the destination HU

    DATA: lv_qty TYPE /scwm/de_quantity.

    IF cs_qty_prod_mc-quan IS INITIAL.
      MESSAGE e014(zewm_cl_msg_con).
    ENDIF.

    DATA(lo_model) = lcl_model=>get_instance( ).

    ASSIGN lo_model->mt_cons_hu_content[ matid = cs_qty_prod_mc-matid
                                         quan  = cs_qty_prod_mc-quan ] TO FIELD-SYMBOL(<ls_cons_hu_content>).

    IF sy-subrc <> 0.
      CLEAR cs_qty_prod_mc-quan.
      MESSAGE e014(zewm_cl_msg_con).
    ENDIF.

    " get top HU
    ASSIGN lo_model->mt_cons_huhdr[ huident = iv_hu_top ]  TO FIELD-SYMBOL(<ls_top_hu>).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CLEAR lo_model->mt_sn_split_pall.

    " Get all sub-Hus
    LOOP AT lo_model->mt_cons_huhdr ASSIGNING  FIELD-SYMBOL(<ls_subhus_cont>)
                                                     WHERE  ( higher_guid = <ls_top_hu>-guid_hu OR
                                                              guid_hu     = <ls_top_hu>-guid_hu ).

      CHECK NOT line_exists( lo_model->mt_cons_hu_content[ huident = <ls_subhus_cont>-huident ] ).

      ASSIGN lo_model->mt_cons_huitm[ guid_parent = <ls_subhus_cont>-guid_hu
                                      matid       = cs_qty_prod_mc-matid ] TO FIELD-SYMBOL(<ls_huitm_stock>).

      IF sy-subrc = 0.
        TRY.
            lv_qty = <ls_huitm_stock>-quan DIV <ls_huitm_stock>-quana.
          CATCH cx_sy_zerodivide.
            CLEAR lv_qty.
            CONTINUE.
        ENDTRY.

        IF lv_qty = cs_qty_prod_mc-quan.

          cs_qty_prod_mc-guid_stock    = <ls_huitm_stock>-guid_stock.
          cs_qty_prod_mc-sguid_hu      = <ls_subhus_cont>-guid_hu.
          cs_qty_prod_mc-vlenr         = <ls_subhus_cont>-huident.
          cs_qty_prod_mc-meins         = <ls_huitm_stock>-meins.
          cs_qty_prod_mc-serial_status = <ls_cons_hu_content>-serial_status.

          APPEND cs_qty_prod_mc TO lo_model->mt_sn_split_pall.
***          EXIT.

        ENDIF.

        CLEAR lv_qty.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD redetermine_staging_area.

    DATA: ls_read_options TYPE /scwm/dlv_query_contr_str,
          ls_include_data TYPE /scwm/dlv_query_incl_str_prd,
          lt_pdo_items    TYPE /scwm/dlv_item_out_prd_tab,
          lt_docid_query  TYPE /scwm/dlv_docid_item_tab,
          lt_docid        TYPE /scwm/dlv_docid_item_tab,
          lo_prd_man      TYPE REF TO /scwm/cl_dlv_management_prd.

    IF iv_huident IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_model) = lcl_model=>get_instance( ).

    ASSIGN lo_model->mt_cons_huhdr[ huident = iv_huident ] TO FIELD-SYMBOL(<ls_hdr_closed>).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lt_docid_query = VALUE #( FOR <ls_docid_item> IN lo_model->mt_cons_huitm
                                            WHERE ( guid_parent = <ls_hdr_closed>-guid_hu )
                                             ( docid = <ls_docid_item>-qdocid ) ).

    IF lines( lt_docid_query ) = 0.
      RETURN.
    ENDIF.

    lo_prd_man = NEW #( ).

    ls_read_options-data_retrival_only = abap_true.
    ls_read_options-item_part_select   = abap_true.

    " Do a query just to be sure
    TRY.
        CALL METHOD lo_prd_man->query
          EXPORTING
            it_docid        = lt_docid_query
            iv_whno         = lcl_controller=>sv_lgnum
            is_read_options = ls_read_options
            is_include_data = ls_include_data
          IMPORTING
            et_items        = lt_pdo_items.
      CATCH /scdl/cx_delivery.
    ENDTRY.


    DATA: ls_t333   TYPE /scwm/t333,
          ls_docid  TYPE /scwm/dlv_docid_item_str,
          lv_procty TYPE  /scwm/de_procty.

    LOOP AT lt_pdo_items ASSIGNING FIELD-SYMBOL(<ls_pdo_item>).

      lv_procty = <ls_pdo_item>-sapext-/scwm/procty.

      CALL FUNCTION '/SCWM/T333_READ_SINGLE'
        EXPORTING
          iv_lgnum    = lcl_controller=>sv_lgnum
          iv_procty   = lv_procty
        IMPORTING
          es_t333     = ls_t333
        EXCEPTIONS
          not_found   = 1
          wrong_input = 2
          OTHERS      = 3.

      CHECK sy-subrc = 0.

      CHECK ls_t333-pld <> abap_false.

      APPEND VALUE #( docid  = <ls_pdo_item>-docid
                      itemid = <ls_pdo_item>-itemid
                      doccat = <ls_pdo_item>-doccat ) TO lt_docid.

    ENDLOOP.

    IF lines( lt_docid ) = 0.
      RETURN.
    ENDIF.

    lo_prd_man->det_pickloc_stag_door(
      EXPORTING
        iv_whno                    = lcl_controller=>sv_lgnum
        iv_internal_commit_control = abap_true
        iv_det_rbd                 = abap_false
        iv_det_stga_door           = abap_true
        iv_doccat                  = wmegc_doccat_pdo
        it_docid_itemid            = lt_docid
      IMPORTING
        et_entries_not_changed_err = DATA(lt_entries_not_changed_err)
        et_lock_errors             = DATA(lt_lock_errors)
        et_entries_saved           = DATA(lt_entries_saved)
        et_entries_stga            = DATA(lt_entries_stga)          "stga/door executed
        et_entries_rbd_not_found   = DATA(lt_entries_rbd_not_found) "rbd execute but no bin found
        eo_message                 = DATA(lo_message) ).

  ENDMETHOD.

  METHOD cancel_wts_for_hu.

    DATA:
      lt_cancel   TYPE /scwm/tt_cancl,
      lt_ordim_o  TYPE /scwm/tt_ordim_o,
      lt_bapiret  TYPE bapirettab,
      lv_severity TYPE bapi_mtype.

    CALL FUNCTION '/SCWM/TO_READ_SRC'
      EXPORTING
        iv_lgnum   = sv_lgnum
        iv_huident = iv_huident
      IMPORTING
        et_ordim_o = lt_ordim_o
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0 OR lt_ordim_o IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_ordim_o INTO DATA(ls_ordim_o).
      APPEND VALUE #( tanum = ls_ordim_o-tanum ) TO lt_cancel.
    ENDLOOP.

    IF lt_cancel IS INITIAL.
      RETURN.
    ENDIF.

    SET UPDATE TASK LOCAL.

    CALL FUNCTION '/SCWM/TO_CANCEL'
      EXPORTING
        iv_lgnum    = sv_lgnum
        it_cancl    = lt_cancel
      IMPORTING
        et_bapiret  = lt_bapiret
        ev_severity = lv_severity.
    IF lv_severity CA wmegc_severity_eax.
      ROLLBACK WORK.
      LOOP AT lt_bapiret INTO DATA(ls_bapiret)
        WHERE type CA wmegc_severity_eax.
        MESSAGE ID ls_bapiret-id TYPE ls_bapiret-type NUMBER ls_bapiret-number
          WITH ls_bapiret-message_v1 ls_bapiret-message_v2 ls_bapiret-message_v3 ls_bapiret-message_v4.
      ENDLOOP.
    ENDIF.

    COMMIT WORK AND WAIT.

  ENDMETHOD.

  METHOD check_scan_0400.

    DATA:
      lv_matnr     TYPE /sapapo/matnr,
      lv_matnr_ex  TYPE /sapapo/matnr_ex,
      ls_product   TYPE /scmb/mdl_matid_str,
      ls_matnr_key TYPE /scmb/mdl_matnr_str,
      lv_huident   TYPE /scwm/de_huident,
      lv_ean       TYPE ean11.

    lv_matnr = |{ iv_scan ALPHA = IN }|.
    lv_matnr_ex = |{ iv_scan ALPHA = IN }|.

    DO 2 TIMES.
      TRY.
          IF ls_matnr_key IS INITIAL.
            ls_matnr_key-matnr = lv_matnr.
          ENDIF.

          CALL FUNCTION '/SCMB/MDL_PRODUCT_READ'
            EXPORTING
              is_key  = ls_matnr_key
            IMPORTING
              es_data = ls_product.
          IF ls_product-matid IS NOT INITIAL.
            ev_matid = ls_product-matid.
            RETURN.
          ENDIF.
        CATCH /scmb/cx_mdl.
          ls_matnr_key-matnr = lv_matnr_ex.
      ENDTRY.
    ENDDO.

    IF ev_matid IS INITIAL.

* UM Start 30.10.2023 - Change conversion exit
*      lv_ean = |{ iv_scan ALPHA = IN }|.
      lv_ean = |{ iv_scan ALPHA = OUT }|.
* UM End 30.10.2023 - Change conversion exit

      CALL FUNCTION '/SCWM/GET_MATID_FROM_EAN11'
        EXPORTING
          iv_ean11 = lv_ean
        IMPORTING
          ev_matid = ev_matid.
    ENDIF.

    IF ev_matid IS INITIAL.
      SELECT SINGLE * FROM mara INTO @DATA(ls_mara)
        WHERE mfrpn = @iv_scan.
      IF sy-subrc = 0.
        TRY.
            CLEAR: ls_product, ls_matnr_key.
            ls_matnr_key-matnr = ls_mara-matnr.

            CALL FUNCTION '/SCMB/MDL_PRODUCT_READ'
              EXPORTING
                is_key  = ls_matnr_key
              IMPORTING
                es_data = ls_product.
            IF ls_product-matid IS NOT INITIAL.
              ev_matid = ls_product-matid.
              RETURN.
            ENDIF.
          CATCH /scmb/cx_mdl ##NO_HANDLER.
        ENDTRY.
      ENDIF.
    ENDIF.

    IF ev_matid IS INITIAL.
      lv_huident = |{ iv_scan ALPHA = IN }|.
      CALL FUNCTION '/SCWM/HUHEADER_READ'
        EXPORTING
          iv_appl    = wmegc_huappl_wme
          iv_huident = lv_huident
        EXCEPTIONS
          OTHERS     = 1.
      IF sy-subrc <> 0.
        CLEAR ev_huident.
      ELSEIF sy-subrc = 0.
        ev_huident = lv_huident.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD handle_scan_0600.

    DATA:
      lv_serial   TYPE string,
      lt_cap_nums TYPE zcl_crud_ztcross_cap_nums=>tt_ztcross_cap_nums.

    CLEAR ev_no_clear_field.

    DATA(lo_model) = lcl_model=>get_instance( ).
    DATA(lo_view) = lcl_view=>get_instance( ).

    READ TABLE lo_model->mt_cons_huhdr INTO DATA(ls_huhdr)
      WITH KEY huident = zstr_consolidation_scr_0600-packhu.

    IF sy-subrc <> 0.
      MESSAGE s050(/scwm/rf_en) DISPLAY LIKE wmegc_severity_err.
      RETURN.
    ENDIF.

    IF ls_huhdr-top = abap_true AND ls_huhdr-bottom = abap_false.

      LOOP AT lo_model->mt_cons_huhdr INTO DATA(ls_huhdr_subhu)
        WHERE " higher_guid = ls_huhdr-guid_hu AND
              guid_hu     = lo_view->ms_cons_hu_serial-guid_hu. " Andriyan Yordanov.

        READ TABLE lo_model->mt_cons_huitm INTO DATA(ls_huitem)
          WITH KEY guid_parent = ls_huhdr_subhu-guid_hu
                   matid       = lo_view->ms_cons_hu_serial-matid.

        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        IF lo_model->is_pallet( is_huhdr = ls_huhdr_subhu ) = abap_true.
          " We have stock directly on the pallet or ship cart is not a MC
          DATA(lv_count_pall) = lo_view->ms_cons_hu_serial-group_mc.
        ENDIF.

        IF lo_model->get_serial_status( iv_huident = ls_huhdr_subhu-huident ) = icon_led_red OR
          lo_model->get_serial_status( iv_huident = ls_huhdr_subhu-huident )  = icon_led_yellow.
          DATA(lv_found) = abap_true.
          EXIT.
        ENDIF.

      ENDLOOP.

      IF lv_found <> abap_true.
        MESSAGE s317(/scwm/rf_en)  DISPLAY LIKE wmegc_severity_err.
        RETURN.
      ELSE.
        ls_huhdr = ls_huhdr_subhu.
      ENDIF.

    ELSE.
      READ TABLE lo_model->mt_cons_huitm INTO ls_huitem
        WITH KEY guid_parent = ls_huhdr-guid_hu
                 matid       = lo_view->ms_cons_hu_serial-matid. " VALUE #( lo_view->mt_cons_hu_content[ huident = ls_huhdr-huident ]-matid  OPTIONAL )."""  ???? Andriyan Yordanov

      IF lo_model->is_pallet( is_huhdr = ls_huhdr ) = abap_true.
        " We have stock directly on the pallet or ship cart is not a MC
        lv_count_pall = lo_view->ms_cons_hu_serial-group_mc.
      ENDIF.
    ENDIF.

    IF ls_huitem-qdocid IS INITIAL OR
       ls_huitem-qitmid IS INITIAL.
      MESSAGE s187(/scwm/rf_en)  DISPLAY LIKE wmegc_severity_err.
      RETURN.
    ENDIF.

    READ TABLE lo_model->mt_dlv_items INTO DATA(ls_dlv_item)
      WITH KEY docid  = ls_huitem-qdocid
               itemid = ls_huitem-qitmid.

    IF sy-subrc <> 0.
      MESSAGE s187(/scwm/rf_en)  DISPLAY LIKE wmegc_severity_err.
      RETURN.
    ENDIF.

    IF ( ls_dlv_item-eew-zzindenttab01 = iv_sn_main_selnum AND
         ls_dlv_item-eew-zzindenttab01 IS NOT INITIAL AND
         iv_sn_main IS INITIAL ) OR
       ( ls_dlv_item-eew-zzindenttab02 = iv_sn_selnum_02 AND
         ls_dlv_item-eew-zzindenttab02 IS NOT INITIAL AND
         iv_zzindenttab02_scan IS INITIAL ) OR
       ( ls_dlv_item-eew-zzindenttab03 = iv_sn_selnum_03 AND
         ls_dlv_item-eew-zzindenttab03 IS NOT INITIAL AND
         iv_zzindenttab03_scan IS INITIAL ) OR
       ( ls_dlv_item-eew-zzindenttab04 = iv_sn_selnum_04 AND
         ls_dlv_item-eew-zzindenttab04 IS NOT INITIAL AND
         iv_zzindenttab04_scan IS INITIAL ) OR
       ( ls_dlv_item-eew-zzindenttab05 = iv_sn_selnum_05 AND
         ls_dlv_item-eew-zzindenttab05 IS NOT INITIAL AND
         iv_zzindenttab05_scan IS INITIAL ) .
**      MESSAGE s018(zewm_cl_msg_con) DISPLAY LIKE wmegc_severity_err .
      ev_no_clear_field = abap_true.
      RETURN.
    ENDIF.

    DATA(lt_serial) = zcl_crud_ztcross_cap_nums=>select_multi_by_docno_itemno(
                        iv_lgnum  = lcl_controller=>sv_lgnum
                        iv_docno  = ls_dlv_item-docno
                        iv_itemno = ls_dlv_item-itemno ).

    ASSIGN lt_serial[ 1 ] TO FIELD-SYMBOL(<ls_sn_db>).

    IF sy-subrc = 0.
      lv_serial = <ls_sn_db>-serial.
    ENDIF.

    " Andriyan Yordanov new logic save SN
    GET TIME STAMP FIELD DATA(lv_cre_time).

    IF iv_sn_main_selnum IS NOT INITIAL AND
       iv_sn_main        IS NOT INITIAL.

      " ALSO check SN
      DATA(lt_check_sn) = NEW zcl_serial_number_check( sv_lgnum )->extract_serial_number(
                          iv_mateialnumber = lo_view->ms_cons_hu_serial-product
                          iv_barcode       = CONV #( iv_sn_main ) ).

      IF lines( lt_check_sn ) = 0 .
        MESSAGE s061(zmc_rfui)  DISPLAY LIKE wmegc_severity_err.
        RETURN.
      ENDIF.
      " End ALSO SN Check


      DATA(lv_len) = strlen( iv_sn_main ).

      lv_len = COND #( WHEN lv_len > 18
                          THEN 18
                          ELSE lv_len ).

      lv_serial = substring( val = iv_sn_main
                             off = strlen( iv_sn_main ) - lv_len
                             len = lv_len ).

      APPEND VALUE #( lgnum   = lcl_controller=>sv_lgnum
                      docno   = ls_dlv_item-docno
                      itemno  = ls_dlv_item-itemno
                      serial  = lv_serial
                      id_type = iv_sn_main_selnum
                      full_serial_number = iv_sn_main
                      guid_hu    = ls_huhdr-guid_hu
                      group_mc   = lv_count_pall
                      created_by = sy-uname
                      created_at = lv_cre_time ) TO lt_cap_nums.
    ENDIF.

    IF iv_sn_selnum_02 IS NOT INITIAL AND
       iv_zzindenttab02_scan IS NOT INITIAL.

      APPEND VALUE #( lgnum   = lcl_controller=>sv_lgnum
                      docno   = ls_dlv_item-docno
                      itemno  = ls_dlv_item-itemno
                      serial  = lv_serial
                      id_type = iv_sn_selnum_02
                      full_serial_number = iv_zzindenttab02_scan
                      guid_hu = ls_huhdr-guid_hu
                      group_mc   = lv_count_pall
                      created_by = sy-uname
                      created_at = lv_cre_time ) TO lt_cap_nums.
    ENDIF.

    IF iv_sn_selnum_03 IS NOT INITIAL AND
       iv_zzindenttab03_scan IS NOT INITIAL.

      APPEND VALUE #( lgnum   = lcl_controller=>sv_lgnum
                      docno   = ls_dlv_item-docno
                      itemno  = ls_dlv_item-itemno
                      serial  = lv_serial
                      id_type = iv_sn_selnum_03
                      full_serial_number = iv_zzindenttab03_scan
                      guid_hu = ls_huhdr-guid_hu
                      group_mc   = lv_count_pall
                      created_by = sy-uname
                      created_at = lv_cre_time ) TO lt_cap_nums.
    ENDIF.

    IF iv_sn_selnum_04 IS NOT INITIAL AND
       iv_zzindenttab04_scan IS NOT INITIAL.

      APPEND VALUE #( lgnum   = lcl_controller=>sv_lgnum
                      docno   = ls_dlv_item-docno
                      itemno  = ls_dlv_item-itemno
                      serial  = lv_serial
                      id_type = iv_sn_selnum_04
                      full_serial_number = iv_zzindenttab04_scan
                      guid_hu = ls_huhdr-guid_hu
                      group_mc   = lv_count_pall
                      created_by = sy-uname
                      created_at = lv_cre_time ) TO lt_cap_nums.
    ENDIF.

    IF iv_sn_selnum_05 IS NOT INITIAL AND
       iv_zzindenttab05_scan IS NOT INITIAL.

      APPEND VALUE #( lgnum   = lcl_controller=>sv_lgnum
                      docno   = ls_dlv_item-docno
                      itemno  = ls_dlv_item-itemno
                      serial  = lv_serial
                      id_type = iv_sn_selnum_05
                      full_serial_number = iv_zzindenttab05_scan
                      guid_hu = ls_huhdr-guid_hu
                      group_mc   = lv_count_pall
                      created_by = sy-uname
                      created_at = lv_cre_time ) TO lt_cap_nums.
    ENDIF.

    " Check if SN was scanned before
    DATA(lt_sn_cap_num_check) = zcl_crud_ztcross_cap_nums=>select_multi_by_full_serials(
        EXPORTING
          iv_lgnum    = lcl_controller=>sv_lgnum
          it_serials  = VALUE #( FOR <ls_check_cap_nums> IN lt_cap_nums
               ( full_serial_number = <ls_check_cap_nums>-full_serial_number ) ) ).

    IF lines( lt_sn_cap_num_check ) > 0.
      MESSAGE s020(zmc_out_ui_packing) WITH lt_sn_cap_num_check[ 1 ]-full_serial_number
                                       DISPLAY LIKE wmegc_severity_err.
      RETURN.
    ENDIF.
    "end Andriyan Yordanov new logic save SN

    IF lt_cap_nums IS NOT INITIAL.
      zcl_crud_ztcross_cap_nums=>insert_multi_entries(
        it_cap_nums  = lt_cap_nums ).
    ENDIF.

  ENDMETHOD.

  METHOD handle_scan_0600_vendor.

    DATA: lv_ident    TYPE zde_param_low.

    DATA(lo_model) =  lcl_model=>get_instance( ).

    IF lines( lo_model->mt_cons_hu_content ) <> 1.
      MESSAGE e025(zewm_cl_msg_con).
    ENDIF.

    IF lo_model->mt_cons_hu_content[ 1 ]-serial_status <> icon_led_red.
      MESSAGE e032(zewm_cl_msg_con).
    ENDIF.

    ASSIGN lo_model->mt_cons_huhdr[ huident = zstr_consolidation_scr_0600-packhu ] TO FIELD-SYMBOL(<ls_huhdr>).

    IF sy-subrc <> 0.
      MESSAGE e025(zewm_cl_msg_con).
    ENDIF.

    " get main ident type of the SN from the param.
    IF lv_ident IS INITIAL.
      zcl_param=>get_parameter(
        EXPORTING
          iv_lgnum     = lcl_controller=>sv_lgnum
          iv_process   = zif_param_const=>c_zout_0008
          iv_parameter = zif_param_const=>c_indenttab01
        IMPORTING
          ev_constant  = lv_ident ).
    ENDIF.

    LOOP AT lo_model->mt_cons_huitm ASSIGNING FIELD-SYMBOL(<ls_huitem>) WHERE guid_parent = <ls_huhdr>-guid_hu.

      ASSIGN lo_model->mt_dlv_items[ docid  = <ls_huitem>-qdocid
                                     itemid = <ls_huitem>-qitmid ] TO FIELD-SYMBOL(<ls_dlv_item>).

      IF sy-subrc <> 0.
        MESSAGE e026(zewm_cl_msg_con).
      ENDIF.

      IF <ls_dlv_item>-eew-zzindenttab01 <> lv_ident.
        MESSAGE e027(zewm_cl_msg_con) WITH |{ <ls_dlv_item>-docno ALPHA = OUT }|
                                           |{ <ls_dlv_item>-itemno ALPHA = OUT }| .
      ENDIF.

      CHECK <ls_dlv_item>-eew-zzindenttab02 IS NOT INITIAL OR
            <ls_dlv_item>-eew-zzindenttab03 IS NOT INITIAL OR
            <ls_dlv_item>-eew-zzindenttab04 IS NOT INITIAL OR
            <ls_dlv_item>-eew-zzindenttab05 IS NOT INITIAL.

      MESSAGE e028(zewm_cl_msg_con) WITH |{ <ls_dlv_item>-docno ALPHA = OUT }|
                                         |{ <ls_dlv_item>-itemno ALPHA = OUT }| .
    ENDLOOP.

    CALL SCREEN 0610 STARTING AT 10 5.

  ENDMETHOD.

  METHOD handle_scan_0500.

    DATA:
      ls_quan    TYPE /scwm/s_quan,
      lt_fields  TYPE STANDARD TABLE OF sval,
      lt_mat_uom TYPE /scwm/tt_material_uom,
      lr_guid_hu TYPE RANGE OF /scwm/guid_hu.

    DATA(lo_model) = lcl_model=>get_instance( ).

    check_scan_0400(
      EXPORTING
        iv_scan    = iv_scan
      IMPORTING
        ev_matid   = DATA(lv_matid)
        ev_huident = DATA(lv_huident) ).
    IF lv_matid IS NOT INITIAL.
*      get_huitem_by_matid_exc_handling( ).
      READ TABLE lo_model->mt_cons_huhdr INTO DATA(ls_huhdr)
        WITH KEY huident = iv_huident.
      IF sy-subrc <> 0.
        MESSAGE e050(/scwm/rf_en).
      ENDIF.

      APPEND VALUE #( sign = /scmb/cl_search=>sc_sign_i option = /scmb/cl_search=>sc_eq low = ls_huhdr-guid_hu ) TO lr_guid_hu.

      IF ls_huhdr-top = abap_true AND ls_huhdr-bottom = abap_false.
        LOOP AT lo_model->mt_cons_huhdr INTO DATA(ls_huhdr_subhu)
          WHERE higher_guid = ls_huhdr-guid_hu.
          APPEND VALUE #( sign = /scmb/cl_search=>sc_sign_i option = /scmb/cl_search=>sc_eq low = ls_huhdr_subhu-guid_hu ) TO lr_guid_hu.
        ENDLOOP.
      ENDIF.

      LOOP AT lo_model->mt_cons_huitm_exc INTO DATA(ls_huitem)
        WHERE guid_parent IN lr_guid_hu AND matid = lv_matid.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        TRY.
            CLEAR lt_mat_uom.
            CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
              EXPORTING
                iv_matid   = ls_huitem-matid
                iv_lgnum   = sv_lgnum
              IMPORTING
                et_mat_uom = lt_mat_uom.
            READ TABLE lt_mat_uom INTO DATA(ls_mat_uom)
              WITH KEY meinh = ls_huitem-altme.
            IF sy-subrc <> 0 OR ls_mat_uom-umrez <= 0.
              ls_quan-quan = ls_huitem-quan.
              ls_quan-unit = ls_huitem-meins.
            ELSE.
              IF ls_huitem-quan >= ls_mat_uom-umrez.
                ls_quan-quan = ls_mat_uom-umrez.
                ls_quan-unit = ls_huitem-meins.
              ELSE.
                ls_quan-quan = ls_huitem-quan.
                ls_quan-unit = ls_huitem-meins.
              ENDIF.
            ENDIF.
          CATCH /scwm/cx_md.
            ls_quan-quan = ls_huitem-quan.
            ls_quan-unit = ls_huitem-meins.
        ENDTRY.

        LOOP AT lo_model->mt_cons_huitm_exc INTO DATA(ls_huitem_quan)
          WHERE guid_parent IN lr_guid_hu AND matid = lv_matid AND quan <> ls_quan-quan.
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
          APPEND VALUE #( tabname = '/SCWM/S_QUAN'
                          fieldname = 'QUAN'
                          value = ''
                          field_obl = '' ) TO lt_fields.

          CALL FUNCTION 'POPUP_GET_VALUES'
            EXPORTING
              popup_title = TEXT-020
            TABLES
              fields      = lt_fields
            EXCEPTIONS
              OTHERS      = 1.
          IF sy-subrc <> 0.
            CLEAR lt_fields.
          ENDIF.

          TRY.
              ls_quan-quan = lt_fields[ 1 ]-value.
              ls_quan-unit = ls_huitem-meins.
            CATCH cx_sy_itab_line_not_found.
              MESSAGE e175(/scwm/rf_en).
          ENDTRY.

          IF ls_quan-quan <= 0.
            MESSAGE e175(/scwm/rf_en).
          ENDIF.
        ENDIF.

        CLEAR ev_quan_moved.
        ls_huitem-quan = ev_quan_moved = ls_quan-quan.
        ls_huitem-meins = ls_quan-unit.

        APPEND ls_huitem TO lo_model->mt_scan_huitm.

        LOOP AT lo_model->mt_cons_huitm_exc ASSIGNING FIELD-SYMBOL(<ls_src>)
          WHERE guid_parent = ls_huitem-guid_parent AND
                guid_stock  = ls_huitem-guid_stock.
          IF <ls_src>-quan > ls_huitem-quan.
            <ls_src>-quan = <ls_src>-quan - ls_huitem-quan.
          ELSE.
            DATA(lv_del) = abap_true.
          ENDIF.
        ENDLOOP.
        IF sy-subrc = 0 AND lv_del = abap_true.
          DELETE lo_model->mt_cons_huitm_exc
            WHERE guid_parent = ls_huitem-guid_parent AND
                  guid_stock  = ls_huitem-guid_stock.
        ENDIF.

      ELSE.
        MESSAGE e007(zewm_cl_msg_con).
      ENDIF.
    ELSEIF lv_huident IS NOT INITIAL.
      LOOP AT lo_model->mt_cons_huhdr INTO ls_huhdr
        WHERE huident = lv_huident.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.
        MESSAGE e050(/scwm/rf_en).
      ENDIF.

      APPEND VALUE #( sign = /scmb/cl_search=>sc_sign_i option = /scmb/cl_search=>sc_eq low = ls_huhdr-guid_hu ) TO lr_guid_hu.

      IF ls_huhdr-top = abap_true AND ls_huhdr-bottom = abap_false.
        LOOP AT lo_model->mt_cons_huhdr INTO ls_huhdr_subhu
          WHERE higher_guid = ls_huhdr-guid_hu.
          APPEND VALUE #( sign = /scmb/cl_search=>sc_sign_i option = /scmb/cl_search=>sc_eq low = ls_huhdr_subhu-guid_hu ) TO lr_guid_hu.
        ENDLOOP.
      ENDIF.

      LOOP AT lo_model->mt_cons_huitm_exc INTO ls_huitem
        WHERE guid_parent IN lr_guid_hu.
        APPEND ls_huitem TO lo_model->mt_scan_huitm.
        CLEAR lv_del.
        LOOP AT lo_model->mt_cons_huitm_exc ASSIGNING <ls_src>
          WHERE guid_parent = ls_huitem-guid_parent AND
                guid_stock  = ls_huitem-guid_stock.
          IF <ls_src>-quan > ls_huitem-quan.
            <ls_src>-quan = <ls_src>-quan - ls_huitem-quan.
          ELSE.
            lv_del = abap_true.
          ENDIF.
        ENDLOOP.
        IF sy-subrc = 0 AND lv_del = abap_true.
          DELETE lo_model->mt_cons_huitm_exc
            WHERE guid_parent = ls_huitem-guid_parent AND
                  guid_stock  = ls_huitem-guid_stock.
        ENDIF.
      ENDLOOP.
    ENDIF.

    lo_model->build_cons_huitem_table( EXPORTING iv_huident = iv_huident
                                                 it_huitem = lo_model->mt_cons_huitm_exc
                                       CHANGING ct_hu_content = lo_model->mt_diff_hu_src ).

    lo_model->build_cons_huitem_table( EXPORTING iv_huident = zstr_consolidation_scr_0300-packhu
                                                 iv_split = abap_true
                                                 it_huitem = lo_model->mt_scan_huitm
                                       CHANGING ct_hu_content = lo_model->mt_diff_hu_dest ).

    DATA(lt_diff_hu_src) = lo_model->mt_diff_hu_src.
    LOOP AT lt_diff_hu_src INTO DATA(ls_diff_hu_src) WHERE huident <> '-'.
      CLEAR: ls_huhdr, lr_guid_hu.
      LOOP AT lo_model->mt_cons_huhdr INTO ls_huhdr
        WHERE huident = ls_diff_hu_src-huident.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      APPEND VALUE #( sign = /scmb/cl_search=>sc_sign_i option = /scmb/cl_search=>sc_eq low = ls_huhdr-guid_hu ) TO lr_guid_hu.

      IF ls_huhdr-top = abap_true AND ls_huhdr-bottom = abap_false.
        LOOP AT lo_model->mt_cons_huhdr INTO ls_huhdr_subhu
          WHERE higher_guid = ls_huhdr-guid_hu.
          APPEND VALUE #( sign = /scmb/cl_search=>sc_sign_i option = /scmb/cl_search=>sc_eq low = ls_huhdr_subhu-guid_hu ) TO lr_guid_hu.
        ENDLOOP.
      ENDIF.

      LOOP AT lo_model->mt_cons_huitm_exc INTO ls_huitem
        WHERE guid_parent IN lr_guid_hu.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.
        DELETE lo_model->mt_diff_hu_src WHERE huident = ls_diff_hu_src-huident.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD user_command_0600.
    CASE sy-ucomm.
      WHEN 'BACK'.
        "refresh_screen_0300( ).
        LEAVE TO SCREEN 0300.
      WHEN 'EXIT' OR 'CANC'.
        LEAVE PROGRAM.
      WHEN 'ENTER'.
        lcl_controller=>get_instance( )->handle_scan_0600(
          EXPORTING
            iv_scan_serial = zstr_consolidation_scr_0600-scan_serial
            iv_scan_imei   = zstr_consolidation_scr_0600-scan_imei
            iv_scan_mac    = zstr_consolidation_scr_0600-scan_mac
            iv_scan_cpu    = zstr_consolidation_scr_0600-scan_cpu
            iv_scan_lic    = zstr_consolidation_scr_0600-scan_lic

            iv_sn_main_selnum = zstr_consolidation_scr_0600-sn_selnum_key
            iv_sn_main        = zstr_consolidation_scr_0600-zzindenttab01_scan
            iv_sn_selnum_02   = zstr_consolidation_scr_0600-sn_selnum_02
            iv_zzindenttab02_scan = zstr_consolidation_scr_0600-zzindenttab02_scan
            iv_sn_selnum_03 = zstr_consolidation_scr_0600-sn_selnum_03
            iv_zzindenttab03_scan = zstr_consolidation_scr_0600-zzindenttab03_scan
            iv_sn_selnum_04 = zstr_consolidation_scr_0600-sn_selnum_04
            iv_zzindenttab04_scan = zstr_consolidation_scr_0600-zzindenttab04_scan
            iv_sn_selnum_05 = zstr_consolidation_scr_0600-sn_selnum_05
            iv_zzindenttab05_scan = zstr_consolidation_scr_0600-zzindenttab05_scan
          IMPORTING ev_no_clear_field = DATA(lv_no_clear_field) ).

        IF lv_no_clear_field = abap_false.
          CLEAR: zstr_consolidation_scr_0600-scan_serial,
                 zstr_consolidation_scr_0600-scan_imei,
                 zstr_consolidation_scr_0600-scan_mac,
                 zstr_consolidation_scr_0600-scan_cpu,
                 zstr_consolidation_scr_0600-scan_lic,
                 zstr_consolidation_scr_0600-sn_selnum_key,
                 zstr_consolidation_scr_0600-zzindenttab01_scan,
                 zstr_consolidation_scr_0600-sn_selnum_02,
                 zstr_consolidation_scr_0600-zzindenttab02_scan,
                 zstr_consolidation_scr_0600-sn_selnum_03,
                 zstr_consolidation_scr_0600-zzindenttab03_scan,
                 zstr_consolidation_scr_0600-sn_selnum_04,
                 zstr_consolidation_scr_0600-zzindenttab04_scan,
                 zstr_consolidation_scr_0600-sn_selnum_05,
                 zstr_consolidation_scr_0600-zzindenttab05_scan.
        ENDIF.


      WHEN 'BARCODE'.

      WHEN 'VENDOR'.

        lcl_controller=>get_instance( )->handle_scan_0600_vendor( ).

    ENDCASE.
  ENDMETHOD.

  METHOD user_command_0500.
    CASE sy-ucomm.
      WHEN 'BACK'.
        "refresh_screen_0300( ).
        LEAVE TO SCREEN 0300.
      WHEN 'EXIT' OR 'CANC' OR 'LOGOUT'.
        LEAVE PROGRAM.
      WHEN 'COUNT'.
        handle_count_exp( ).
      WHEN 'ENTER'.
        handle_scan_0500( iv_scan = zstr_consolidation_scr_0500-scan_input iv_huident = zstr_consolidation_scr_0300-packhu ).
      WHEN 'DELETE'.
        delete_and_close(
          iv_docid   = zstr_consolidation_scr_0300-docid
          iv_huident = zstr_consolidation_scr_0300-packhu ).
      WHEN 'PRINT'.
        MESSAGE i001(hrben00forms).
      WHEN 'REFRESH' OR 'RESTART'.
        DATA(lo_model) = lcl_model=>get_instance( ).
        lo_model->init_diffhu_tables( zstr_consolidation_scr_0300-packhu ).
        DATA(lo_view) = lcl_view=>get_instance( ).
        lo_view->mt_diff_hu_src = lo_model->mt_diff_hu_src.
        CLEAR lo_view->mt_diff_hu_src_edit.

        LOOP AT lo_view->mt_diff_hu_src INTO DATA(ls_line).
          APPEND INITIAL LINE TO lo_view->mt_diff_hu_src_edit ASSIGNING FIELD-SYMBOL(<ls_edit>).
          MOVE-CORRESPONDING ls_line TO <ls_edit>.
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.

  METHOD delete_and_close.

    TYPES:
      BEGIN OF lty_diff_quan,
        docid     TYPE  /scdl/dl_docid,
        itemid    TYPE  /scdl/dl_itemid,
        quan_diff TYPE /scwm/de_quantity,
        unit      TYPE /scwm/de_unit,
        quan      TYPE /scwm/de_quantity,
      END OF lty_diff_quan.

    DATA:
      lv_quan_diff TYPE /scwm/de_quantity,
      ls_quan      TYPE /scwm/s_quan,
      ls_huhdr_new TYPE /scwm/s_huhdr_int,
      lr_guid_hu   TYPE RANGE OF /scwm/guid_hu,
      lv_pmat_guid TYPE /scwm/de_pmatid,
      lt_item_quan TYPE /scdl/t_sp_a_item_quantity,
      ls_diff_quan TYPE lty_diff_quan,
      lt_diff_quan TYPE STANDARD TABLE OF lty_diff_quan.

    DATA(lo_model) = lcl_model=>get_instance( ).

    READ TABLE lo_model->mt_cons_huhdr INTO DATA(ls_huhdr)
      WITH KEY huident = iv_huident.
    IF sy-subrc <> 0.
      MESSAGE e050(/scwm/rf_en).
    ENDIF.

    APPEND VALUE #( sign = /scmb/cl_search=>sc_sign_i option = /scmb/cl_search=>sc_eq low = ls_huhdr-guid_hu ) TO lr_guid_hu.

    IF ls_huhdr-top = abap_true AND ls_huhdr-bottom = abap_false.
      LOOP AT lo_model->mt_cons_huhdr INTO DATA(ls_huhdr_subhu)
        WHERE higher_guid = ls_huhdr-guid_hu.
        APPEND VALUE #( sign = /scmb/cl_search=>sc_sign_i option = /scmb/cl_search=>sc_eq low = ls_huhdr_subhu-guid_hu ) TO lr_guid_hu.
      ENDLOOP.
    ENDIF.

    LOOP AT lo_model->mt_cons_huitm INTO DATA(ls_huitm)
      WHERE guid_parent IN lr_guid_hu.

      CLEAR: ls_diff_quan.

      READ TABLE lo_model->mt_scan_huitm INTO DATA(ls_scan_huitm)
        WITH KEY guid_parent = ls_huitm-guid_parent
                 matid       = ls_huitm-matid
                 guid_stock  = ls_huitm-guid_stock.
      IF sy-subrc <> 0.
        ls_diff_quan-docid = ls_huitm-qdocid.
        ls_diff_quan-itemid = ls_huitm-qitmid.
        ls_diff_quan-quan = ls_huitm-quan.
        ls_diff_quan-quan_diff = ls_huitm-quan * -1.
        ls_diff_quan-unit = ls_huitm-meins.
      ELSEIF ls_scan_huitm-quan < ls_huitm-quan.
        ls_diff_quan-docid = ls_huitm-qdocid.
        ls_diff_quan-itemid = ls_huitm-qitmid.
        ls_diff_quan-quan = ls_huitm-quan - ls_scan_huitm-quan.
        ls_diff_quan-quan_diff = ls_scan_huitm-quan - ls_huitm-quan.
        ls_diff_quan-unit = ls_huitm-meins.
      ENDIF.

      IF ls_diff_quan IS NOT INITIAL.
        APPEND ls_diff_quan TO lt_diff_quan.
        IF ls_huhdr_new IS INITIAL.
          SET UPDATE TASK LOCAL.

          CALL FUNCTION '/SCWM/TO_INIT_NEW'
            EXPORTING
              iv_lgnum = lcl_controller=>sv_lgnum.

          /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = DATA(lo_packing) ).
          lo_packing->init(
            EXPORTING
              iv_lgnum = sv_lgnum
            EXCEPTIONS
              OTHERS   = 1 ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          CALL FUNCTION 'CONVERSION_EXIT_MDLPD_INPUT'
            EXPORTING
              input  = '9500331'
            IMPORTING
              output = lv_pmat_guid.

          lo_packing->/scwm/if_pack_bas~create_hu(
            EXPORTING
              iv_pmat      = lv_pmat_guid
              i_location   = ssout_term_def-lostnfound_stbin "'SPED.LOST.FOUND'
            RECEIVING
              es_huhdr    = ls_huhdr_new
            EXCEPTIONS
              OTHERS       = 1 ).
          IF sy-subrc <> 0 OR ls_huhdr_new IS INITIAL.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.

        READ TABLE lo_model->mt_cons_huhdr INTO DATA(ls_huhdr_parent)
          WITH KEY guid_hu = ls_huitm-guid_parent.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        lo_packing->repack_stock(
          EXPORTING
            iv_dest_hu    = ls_huhdr_new-guid_hu
            iv_source_hu  = ls_huhdr_parent-guid_hu
            iv_stock_guid = ls_huitm-guid_stock
            is_quantity   = VALUE #( quan = ls_diff_quan-quan unit = ls_diff_quan-unit )
           EXCEPTIONS
             OTHERS        = 1 ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF ls_huhdr_new IS NOT INITIAL.
      SET UPDATE TASK LOCAL.
      lo_packing->/scwm/if_pack_bas~save(
        EXPORTING
          iv_commit = abap_true
          iv_wait   = abap_true
         EXCEPTIONS
           OTHERS    = 1 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      SET UPDATE TASK LOCAL.
      cancel_pick(
        iv_huident = ls_huhdr_new-huident
        iv_commit = abap_true ).

      LOOP AT lt_diff_quan INTO ls_diff_quan.
        set_process_code( is_item_quan = VALUE #( docid = ls_diff_quan-docid itemid = ls_diff_quan-itemid qty = ls_diff_quan-quan_diff uom = ls_diff_quan-unit ) ).
      ENDLOOP.

      CLEAR lo_packing.
      /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = lo_packing ).
      lo_packing->init(
        EXPORTING
          iv_lgnum = sv_lgnum
        EXCEPTIONS
          OTHERS   = 1 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      lo_packing->empty_hu(
        EXPORTING
          iv_hu         = ls_huhdr_new-guid_hu
          iv_dest_lgpla = ssout_term_def-lostnfound_stbin
        EXCEPTIONS
          OTHERS        = 1 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      lo_packing->/scwm/if_pack_bas~save(
          EXPORTING
            iv_commit = abap_true
            iv_wait   = abap_true
           EXCEPTIONS
             OTHERS    = 1 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

    handle_close_hu( iv_huident = iv_huident ).

  ENDMETHOD.

  METHOD confirm_hu_step.

    CALL FUNCTION '/SCWM/TO_INIT_NEW'
      EXPORTING
        iv_lgnum = lcl_controller=>sv_lgnum.

    /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = DATA(lo_packing) ).
    lo_packing->init(
      EXPORTING
        iv_lgnum = sv_lgnum
      EXCEPTIONS
        OTHERS   = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL METHOD lo_packing->hu_process_completed
      EXPORTING
        iv_hu  = iv_guid_hu
      EXCEPTIONS
        error  = 1
        OTHERS = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lo_packing->save(
      EXCEPTIONS
        error     = 1                " See Log
        OTHERS    = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD create_hu.

    DATA:
      lv_pmat_guid TYPE /scwm/de_pmatid.

    CALL FUNCTION '/SCWM/TO_INIT_NEW'
      EXPORTING
        iv_lgnum = lcl_controller=>sv_lgnum.

    /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = DATA(lo_packing) ).

    lo_packing->init(
      EXPORTING
        iv_lgnum = sv_lgnum
      EXCEPTIONS
        OTHERS   = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_MDLPD_INPUT'
      EXPORTING
        input  = iv_pmat
      IMPORTING
        output = lv_pmat_guid.

    lo_packing->/scwm/if_pack_bas~create_hu(
      EXPORTING
        iv_pmat      = lv_pmat_guid
        iv_huident   = iv_huident
        i_location   = iv_lgpla
      RECEIVING
        es_huhdr    = DATA(ls_huhdr)
      EXCEPTIONS
        OTHERS       = 1 ).
    IF sy-subrc <> 0 OR ls_huhdr IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lo_packing->/scwm/if_pack_bas~save(
      EXPORTING
        iv_commit = iv_commit
       EXCEPTIONS
         OTHERS    = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    ev_huident = ls_huhdr-huident.
    ev_guid_hu = ls_huhdr-guid_hu.

  ENDMETHOD.

  METHOD cancel_pick.

    DATA:
      ls_dynprodata        TYPE /scmb/s_dynpro_data,
      ls_default           TYPE /scmb/s_default_values_det,
      lt_huhdr             TYPE /scwm/tt_huhdr_int,
      lo_attribute_handler TYPE REF TO /scmb/cl_attribute_handler,
      lo_message_handler   TYPE REF TO /scmb/cl_message_handler,
      lo_sp                TYPE REF TO /scwm/cl_cancpick_sp,
      ls_dynpro_data       TYPE /scmb/s_dynpro_data,
      ls_default_values    TYPE /scmb/s_default_values,
      lt_cancpick_stock    TYPE /scwm/tt_asp_stock_cancpick,
      lt_cancpick_hu       TYPE /scwm/tt_asp_hu_cancpick,
      lt_outrecords_rel    TYPE /scwm/tt_asp_dlv_cancpick,
      lv_rejected          TYPE boole_d,
      lt_return_code       TYPE /scmb/t_sp_return_code.

    /scwm/cl_tm=>set_lgnum( iv_lgnum = sv_lgnum ).

    "Set default data
    ls_default_values-v_identifier       = /scwm/if_sp_cancpick_c=>sc_identifier_cancpick.
    ls_default_values-v_values_structure = /scwm/if_sp_cancpick_c=>sc_default_str.
    ls_default_values-v_extended         = abap_true.

    CLEAR ls_default.
    ls_default-v_fieldname    = 'LGNUM'.
    ls_default-v_memoryid     = '/SCWM/LGN'.
    ls_default-v_mandatory    = abap_true.
    ls_default-v_checkbox     = abap_false.
    ls_default-v_no_dropdown  = abap_true.
    APPEND ls_default TO ls_default_values-t_details.

    CLEAR ls_default.
    ls_default-v_fieldname    = 'CDHU'.
    ls_default-v_memoryid     = '/SCWM/CDHU'.
    ls_default-v_mandatory    = abap_false.
    ls_default-v_checkbox     = abap_true.
    ls_default-v_no_dropdown  = abap_true.
    APPEND ls_default TO ls_default_values-t_details.

    CREATE OBJECT lo_attribute_handler.
    CREATE OBJECT lo_message_handler.

    "Create SP for cancel pick
    CREATE OBJECT lo_sp
      EXPORTING
        iv_mode              = /scwm/if_sp_cancpick_c=>sc_mode_dynp
        io_attribute_handler = lo_attribute_handler
        io_message_handler   = lo_message_handler.

    "Set process specific attributes
    CALL METHOD /scmb/cl_base=>set_process_data
      EXPORTING
        io_attribute_handler     = lo_attribute_handler
        io_service_provider      = lo_sp
        io_message_handler       = lo_message_handler
        is_dynpro_data           = ls_dynpro_data
        is_default_values        = ls_default_values
        iv_disable_standard_save = abap_true.

    /scmb/cl_base=>end_of_initialization( iv_no_message_display = abap_true ).

    CALL FUNCTION '/SCWM/HU_READ'
      EXPORTING
        iv_appl    = wmegc_huappl_wme
        iv_top     = abap_true
        iv_lgnum   = sv_lgnum
        iv_huident = iv_huident
      IMPORTING
        et_huhdr   = lt_huhdr
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      MESSAGE e003(zewm_cl_msg_con).
    ENDIF.

    READ TABLE lt_huhdr INTO DATA(ls_huhdr) WITH KEY huident = iv_huident.
    IF ls_huhdr-top <> abap_true.
      READ TABLE lt_huhdr INTO DATA(ls_tophu) WITH KEY guid_hu = ls_huhdr-higher_guid.
    ELSE.
      ls_tophu = ls_huhdr.
    ENDIF.

    CALL METHOD lo_sp->query
      EXPORTING
        selections = VALUE /scdl/t_sp_selection( ( fieldname = 'HUIDENT' sign = /scmb/cl_search=>sc_sign_i option = /scmb/cl_search=>sc_eq low = iv_huident ) )
        query      = 'QRY_CANCPICK_HU'
      IMPORTING
        outrecords = lt_cancpick_hu
        rejected   = lv_rejected.

    DATA(lt_inkey_rel) = VALUE /scwm/tt_aspk_hu_cancpick( ( guid_hu = ls_tophu-guid_hu ) ).

    CALL METHOD lo_sp->select_by_relation
      EXPORTING
        relation   = '/SCWM/REL_HU_STOCK'
        inrecords  = lt_inkey_rel
        aspect     = '/SCWM/S_SP_A_HU'
        options    = VALUE #( )
      IMPORTING
        outrecords = lt_cancpick_stock
        rejected   = lv_rejected.

    CLEAR lt_inkey_rel.
    lt_inkey_rel = VALUE /scwm/tt_aspk_hu_cancpick( ( guid_hu = ls_huhdr-guid_hu ) ).

    CALL METHOD lo_sp->execute
      EXPORTING
        aspect       = '/SCWM/S_SP_A_HU'
        inkeys       = lt_inkey_rel
        action       = '/SCWM/ACT_UNASSIGN_STOCK'
      IMPORTING
        outrecords   = lt_cancpick_hu
        rejected     = lv_rejected
        return_codes = lt_return_code.

    lo_sp->save(
      EXPORTING
        synchronously = abap_true
      IMPORTING
        rejected      = lv_rejected ).
    IF check_sp_error( EXPORTING
      iv_rejected     = lv_rejected
      it_return_codes = lt_return_code ) = abap_true.
      MESSAGE e003(zewm_cl_msg_con).
    ENDIF.

    /scwm/cl_tm=>cleanup( ).

    IF iv_commit = abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.

*    DELETE lt_cancpick_stock WHERE guid_parent <> ls_huhdr-guid_hu.
*    READ TABLE lt_cancpick_stock ASSIGNING FIELD-SYMBOL(<ls_stock>)
*      WITH KEY docid  = iv_docid
*               itemid = iv_itemid.
*    IF sy-subrc <> 0 OR <ls_stock> IS NOT ASSIGNED.
*      MESSAGE e003(zewm_cl_msg_con).
*    ELSE.
*      "Modify the quantity
*      <ls_stock>-qty_move = iv_quan.
*
*      DELETE lt_cancpick_stock WHERE guid_stock NE <ls_stock>-guid_stock.
**      lt_inrecords = lt_outrecords.
*      lt_inkeys = VALUE /scwm/tt_aspk_stock_cancpick( ( guid_stock  = <ls_stock>-guid_stock
*                                                        guid_parent = <ls_stock>-guid_parent ) ).
*    ENDIF.
*
*    lo_sp->update(
*      EXPORTING
*        aspect    = /scwm/if_sp_cancpick_c=>sc_asp_item_stock
*        inrecords = lt_cancpick_stock
*      IMPORTING
*        rejected = lv_rejected ).
*    IF check_sp_error( EXPORTING
*      iv_rejected     = lv_rejected
*      it_return_codes = lt_return_code ) = abap_true.
*      MESSAGE e003(zewm_cl_msg_con).
*    ENDIF.
*
*    DATA:
*      lt_outrecords_stock      TYPE /scwm/tt_asp_stock_cancpick.
*      "lt_outrecords_dlv  TYPE /scwm/tt_asp_dlv_cancpick.
*
*    "Unassign the stock and create WT
*    CALL METHOD lo_sp->execute
*      EXPORTING
*        aspect             = /scwm/if_sp_cancpick_c=>sc_asp_item_stock
*        inkeys             = lt_inkeys
*        action             = /scwm/if_sp_cancpick_c=>sc_act_unassign_stock_to
*        relation_inkey     = lt_inkeys_doc
*        relation           = /scwm/if_sp_cancpick_c=>sc_rel_item_stock
*      IMPORTING
*        outrecords         = lt_outrecords_stock
*        relation_outrecord = lt_outrecords_dlv
*        rejected           = lv_rejected
*        return_codes       = lt_return_code.
*    IF check_sp_error( EXPORTING
*      iv_rejected     = lv_rejected
*      it_return_codes = lt_return_code ) = abap_true.
*      MESSAGE e003(zewm_cl_msg_con).
*    ENDIF.



*    IF iv_itemid IS NOT INITIAL.
*      READ TABLE lt_outrecords ASSIGNING FIELD-SYMBOL(<ls_stock>)
*        WITH KEY docid  = iv_docid
*                 itemid = iv_itemid.
*      IF sy-subrc <> 0 OR <ls_stock> IS NOT ASSIGNED.
*        MESSAGE e003(zewm_cl_msg_con).
*      ELSE.
*        "Modify the quantity
*        <ls_stock>-qty_move = iv_quan.
*        DELETE lt_outrecords WHERE guid_stock NE <ls_stock>-guid_stock.
*        lt_inrecords = lt_outrecords.
*        lt_inkeys = VALUE #( ( guid_stock  = <ls_stock>-guid_stock
*                               guid_parent = <ls_stock>-guid_parent ) ).
*      ENDIF.
**    ELSEIF it_items IS NOT INITIAL.
**      LOOP AT it_items INTO ls_item.
**        LOOP AT lt_outrecords  ASSIGNING <ls_stock>
**          WHERE docid    = ls_item-docid   AND
**                itemid   = ls_item-itemid  AND
**                batchno  = ls_item-batchno AND
**                qty_move > ls_item-qty_returned.
**          <ls_stock>-qty_move = ls_item-qty_returned.
**          APPEND <ls_stock> TO lt_inrecords.
**          DATA(ls_inkey) = VALUE /scwm/s_aspk_stock_cancpick( guid_stock  = <ls_stock>-guid_stock
**                                                              guid_parent = <ls_stock>-guid_parent ).
**          APPEND ls_inkey TO lt_inkeys.
**          EXIT.
**        ENDLOOP.
**      ENDLOOP.
*    ENDIF.
*
*    IF lt_inrecords IS INITIAL OR lt_inkeys IS INITIAL.
*      MESSAGE e003(zewm_cl_msg_con).
*    ENDIF.
*
*    lo_sp->update(
*      EXPORTING
*        aspect = /scwm/if_sp_cancpick_c=>sc_asp_item_stock
*        inrecords = lt_inrecords
*      IMPORTING
*        rejected = lv_rejected ).
*    IF check_sp_error( EXPORTING
*      iv_rejected     = lv_rejected
*      it_return_codes = lt_return_code ) = abap_true.
*      MESSAGE e003(zewm_cl_msg_con).
*    ENDIF.
*
*    "Unassign the stock and create WT
*    CALL METHOD lo_sp->execute
*      EXPORTING
*        aspect             = /scwm/if_sp_cancpick_c=>sc_asp_item_stock
*        inkeys             = lt_inkeys
*        action             = /scwm/if_sp_cancpick_c=>sc_act_unassign_stock_to
*        relation_inkey     = lt_inkeys_doc
*        relation           = /scwm/if_sp_cancpick_c=>sc_rel_item_stock
*      IMPORTING
*        outrecords         = lt_outrecords
*        relation_outrecord = lt_outrecords_rel
*        rejected           = lv_rejected
*        return_codes       = lt_return_code.
*    IF check_sp_error( EXPORTING
*      iv_rejected     = lv_rejected
*      it_return_codes = lt_return_code ) = abap_true.
*      MESSAGE e003(zewm_cl_msg_con).
*    ENDIF.

  ENDMETHOD.

  METHOD set_process_code.

    DATA:
      lt_key_head       TYPE /scdl/t_sp_k_head,
      lt_keys_item      TYPE /scdl/t_sp_k_item,
      lt_outrecords     TYPE /scdl/t_sp_a_head,
      lt_outrec_item    TYPE /scdl/t_sp_a_item,
      lr_action_control TYPE REF TO /scwm/dlv_prcode_add_str.

    DATA(lo_message_box) = NEW /scdl/cl_sp_message_box( ).
    DATA(lo_sp) = NEW /scdl/cl_sp_prd_out(
        io_message_box = lo_message_box
        iv_mode = /scdl/cl_sp=>sc_mode_classic ).

    APPEND VALUE #( docid = is_item_quan-docid ) TO lt_key_head.

    lo_sp->select(
      EXPORTING
        inkeys       = lt_key_head
        aspect       = /scdl/if_sp_c=>sc_asp_head
      IMPORTING
        outrecords   = lt_outrecords
        rejected     = DATA(lv_reject)
        return_codes = DATA(lt_return) ).
    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY failed = abap_true.
    IF sy-subrc = 0 OR lv_reject = abap_true.
      DATA(lt_message) = lo_message_box->get_messages(
        EXPORTING
          iv_msgty = wmegc_severity_err ).
      TRY.
          DATA(ls_message) = lt_message[ 1 ].
          MESSAGE ID ls_message-msgid TYPE ls_message-msgty NUMBER ls_message-msgno
            WITH ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4.
        CATCH cx_sy_itab_line_not_found.
          MESSAGE e019(/sapapo/cfm_msg).
      ENDTRY.
    ENDIF.

    lo_sp->lock(
      EXPORTING
        inkeys       = lt_key_head
        aspect       = /scdl/if_sp_c=>sc_asp_head
        lockmode     = /scdl/if_sp1_locking=>sc_exclusive_lock
      IMPORTING
        rejected     = lv_reject
        return_codes = lt_return ).

    CREATE DATA lr_action_control.
    lr_action_control->prcode = /scwm/if_bw_extr_c=>sc_dlv_proc_gi_so."'O001'.
    lr_action_control->qty = is_item_quan-qty.
    lr_action_control->uom = is_item_quan-uom.

    DATA(ls_action) = VALUE /scdl/s_sp_act_action( action_code = /scwm/if_dl_c=>sc_ac_prcode_add
                                                   action_control = lr_action_control ).
    APPEND VALUE #( docid  = is_item_quan-docid
                    itemid = is_item_quan-itemid ) TO lt_keys_item.

    lo_sp->execute(
      EXPORTING aspect       = /scdl/if_sp_c=>sc_asp_item
                inkeys       = lt_keys_item
                inparam      = ls_action
                action       = /scdl/if_sp_c=>sc_act_execute_action
      IMPORTING outrecords   = lt_outrec_item
                rejected     = lv_reject
                return_codes = lt_return ).
    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY failed = abap_true.
    IF sy-subrc = 0 OR lv_reject = abap_true.
      lt_message = lo_message_box->get_messages(
        EXPORTING
          iv_msgty = wmegc_severity_err ).
      TRY.
          ls_message = lt_message[ 1 ].
          MESSAGE ID ls_message-msgid TYPE ls_message-msgty NUMBER ls_message-msgno
            WITH ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4.
        CATCH cx_sy_itab_line_not_found.
          MESSAGE e019(/sapapo/cfm_msg).
      ENDTRY.
    ENDIF.

    lo_sp->save( IMPORTING rejected = lv_reject ).
    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY failed = abap_true.
    IF sy-subrc = 0 OR lv_reject = abap_true.
      ROLLBACK WORK.
      lt_message = lo_message_box->get_messages(
        EXPORTING
          iv_msgty = wmegc_severity_err ).
      TRY.
          ls_message = lt_message[ 1 ].
          MESSAGE ID ls_message-msgid TYPE ls_message-msgty NUMBER ls_message-msgno
            WITH ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4.
        CATCH cx_sy_itab_line_not_found.
          MESSAGE e019(/sapapo/cfm_msg).
      ENDTRY.
    ELSE.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.

  METHOD check_sp_error.
    IF iv_rejected = abap_true.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    READ TABLE it_return_codes TRANSPORTING NO FIELDS
      WITH KEY failed = abap_true.
    IF sy-subrc = 0.
      rv_error = abap_true.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD check_scanned .

    DATA: lv_huident_dest TYPE /scwm/de_huident,
          ls_huhdr_dest   TYPE  /scwm/s_huhdr_int,
          lt_huhdr_int    TYPE /scwm/tt_huhdr_int.

    DATA(lo_model) = lcl_model=>get_instance( ).
    CALL FUNCTION '/SCWM/HUMAIN_REFRESH'.

    CALL FUNCTION '/SCWM/HU_READ'
      EXPORTING
        iv_appl    = wmegc_huappl_wme
        iv_huident = iv_huident_des
      IMPORTING
        es_huhdr   = ls_huhdr_dest
        et_huhdr   = lt_huhdr_int
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF NOT line_exists( lt_huhdr_int[ huident = iv_huident_src ] ).
      MESSAGE e020(zewm_cl_msg_con) WITH |{ iv_huident_src ALPHA = OUT }|.
    ENDIF.

  ENDMETHOD.

  METHOD pack_hu.

    /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = DATA(lo_packing) ).
    lo_packing->init(
      EXPORTING
        iv_lgnum = sv_lgnum
      EXCEPTIONS
        OTHERS   = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lo_packing->get_hu(
       EXPORTING
         iv_huident = iv_huident_src
       IMPORTING
         es_huhdr   = DATA(ls_huhdr_src)
       EXCEPTIONS
         OTHERS     = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lo_packing->get_hu(
       EXPORTING
         iv_huident = iv_huident_des
       IMPORTING
         es_huhdr   = DATA(ls_huhdr_des)
       EXCEPTIONS
         OTHERS     = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION '/SCWM/TO_INIT_NEW'
      EXPORTING
        iv_lgnum = sv_lgnum.

    lo_packing->pack_hu(
      EXPORTING
        iv_source_hu = ls_huhdr_src-guid_hu
        iv_dest_hu   = ls_huhdr_des-guid_hu
      EXCEPTIONS
        OTHERS       = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lo_packing->/scwm/if_pack_bas~save(
      EXPORTING
        iv_commit = iv_commit
       EXCEPTIONS
         OTHERS    = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD pack_all_stock_to_hu.

    /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = DATA(lo_packing) ).

    lo_packing->init(
      EXPORTING
        iv_lgnum = sv_lgnum
      EXCEPTIONS
        OTHERS   = 1 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lo_packing->get_hu(
       EXPORTING
         iv_huident = iv_huident_src
       IMPORTING
         es_huhdr   = DATA(ls_huhdr_src)
       EXCEPTIONS
         OTHERS     = 1 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lo_packing->get_hu(
       EXPORTING
         iv_huident = iv_huident_des
       IMPORTING
         es_huhdr   = DATA(ls_huhdr_des)
       EXCEPTIONS
         OTHERS     = 1 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION '/SCWM/TO_INIT_NEW'
      EXPORTING
        iv_lgnum = sv_lgnum.

    IF iv_move_hu = abap_false.

      " move SN from source to destination HU
      zcl_crud_ztcross_cap_nums=>modify_multi_huid_sn(
        iv_lgnum            = lcl_controller=>sv_lgnum                 " Warehouse Number/Warehouse Complex
        iv_src_guid_hu      = ls_huhdr_src-guid_hu                     " Unique Internal Identification of a Handling Unit
        iv_dest_guid_hu     = ls_huhdr_des-guid_hu                     " Unique Internal Identification of a Handling Unit
        iv_commit           = abap_false  ).

      " repack the content of the HU and the source will be deleted
      lo_packing->empty_hu(
        EXPORTING
          iv_hu         = ls_huhdr_src-guid_hu
          iv_dest_hu    = ls_huhdr_des-guid_hu
        EXCEPTIONS
          OTHERS        = 1 ).

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ELSE.

      " source HU should be packed to the destination like a subHU
      lo_packing->/scwm/if_pack_bas~pack_hu(
        EXPORTING
          iv_source_hu = ls_huhdr_src-guid_hu
          iv_dest_hu   = ls_huhdr_des-guid_hu
        EXCEPTIONS
          error        = 1
          OTHERS       = 2 ).

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDIF.

    lo_packing->/scwm/if_pack_bas~save(
      EXPORTING
        iv_commit = iv_commit
       EXCEPTIONS
         OTHERS    = 1 ).

    IF sy-subrc <> 0.
      ROLLBACK WORK.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD pack_stock.

    /scwm/cl_wm_packing=>get_instance( IMPORTING eo_instance = DATA(lo_packing) ).

    lo_packing->init(
      EXPORTING
        iv_lgnum = sv_lgnum
      EXCEPTIONS
        OTHERS   = 1 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lo_packing->get_hu(
       EXPORTING
         iv_huident = iv_huident_src
       IMPORTING
         es_huhdr   = DATA(ls_huhdr_src)
       EXCEPTIONS
         OTHERS     = 1 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lo_packing->get_hu(
       EXPORTING
         iv_huident = iv_huident_des
       IMPORTING
         es_huhdr   = DATA(ls_huhdr_des)
       EXCEPTIONS
         OTHERS     = 1 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION '/SCWM/TO_INIT_NEW'
      EXPORTING
        iv_lgnum = sv_lgnum.

    lo_packing->repack_stock(
      EXPORTING
        iv_dest_hu    = ls_huhdr_des-guid_hu
        iv_source_hu  = ls_huhdr_src-guid_hu
        iv_stock_guid = iv_guid_stock
        is_quantity   = is_quan
       EXCEPTIONS
         OTHERS        = 1 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lo_packing->/scwm/if_pack_bas~save(
      EXPORTING
        iv_commit = iv_commit
        iv_wait   = iv_commit
       EXCEPTIONS
         OTHERS    = 1 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
