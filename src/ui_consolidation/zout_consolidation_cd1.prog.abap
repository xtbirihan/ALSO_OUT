**********************************************************************
*& Key           : AMOGYORODI-230814
*& Request No.   :
**********************************************************************
*& Description (short)
*&
*&
**********************************************************************
*----------------------------------------------------------------------*
***INCLUDE ZOUT_CONSOLIDATION_CD1.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class lcl_report_controller
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_controller DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-DATA:
      sv_lgnum       TYPE /scwm/lgnum,
      sv_show_compl  TYPE xfeld VALUE abap_true,
      sv_lgtyp       TYPE /scwm/de_lgtyp,
      sv_aarea       TYPE /scwm/de_aarea_plan,
      sv_procs       TYPE /scwm/de_procs,
      ssout_term_def TYPE zout_term_def,
      so_instance    TYPE REF TO lcl_controller.

    CLASS-METHODS:
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_controller.

    TYPES:
      BEGIN OF ty_pmat_data,
        matnr TYPE /scmb/mdl_product_no,
        txt   TYPE /scmb/mdl_product_text_tab1,
      END OF ty_pmat_data,

      BEGIN OF ty_mapp_hutyp_matid,
        matnr     TYPE /scmb/mdl_product_no,
        matid_22  TYPE /sapapo/matid,
        matid_16  TYPE /scmb/mdl_matid,
        hutyp     TYPE /scwm/de_hutyp,
        hutyp_txt TYPE /scwm/de_desc40,
      END OF ty_mapp_hutyp_matid.

    CONSTANTS: c_start_col      TYPE char2 VALUE '30' ##NO_TEXT,
               c_start_row      TYPE char2 VALUE '10' ##NO_TEXT,
               c_cancel_action  TYPE char1 VALUE 'A' ##NO_TEXT,
               c_ui_matnr_table TYPE tabname VALUE '/SCMB/MDL_MATNR_STR',
               c_ui_matnr_field TYPE lvc_fname VALUE 'MATNR'.

    TYPES tty_pmat_data TYPE STANDARD TABLE OF ty_pmat_data.
    TYPES tty_mapp_hutyp_matid TYPE STANDARD TABLE OF ty_mapp_hutyp_matid.

    METHODS:
      selection IMPORTING iv_lgnum    TYPE /scwm/lgnum
                          is_term_def TYPE zout_term_def, " Andriyan Yordanov
      user_command_0100,

      user_command_0200,

      user_command_0210,

      user_command_0300,

      user_command_0400,

      user_command_0410,

      user_command_0420,

      user_command_0500,

      user_command_0600,

      user_command_0610,

      user_command_popup,

      refresh_screen_0100,

      refresh_screen_0200,

      refresh_screen_0300
        IMPORTING iv_huident TYPE /scwm/de_huident OPTIONAL,

      handle_hu_scan_0200
        CHANGING cv_huident TYPE /scwm/de_huident,

      handle_scan_0400
        IMPORTING iv_scan TYPE /scwm/de_rf_huident,

      handle_scan_0500
        IMPORTING iv_scan       TYPE /scwm/de_rf_huident
                  iv_huident    TYPE /scwm/de_huident
        EXPORTING ev_quan_moved TYPE /scwm/de_quantity,

      handle_scan_0600
        IMPORTING iv_scan_serial        TYPE /scwm/de_rf_huident
                  iv_scan_imei          TYPE /scwm/de_rf_huident
                  iv_scan_mac           TYPE /scwm/de_rf_huident
                  iv_scan_cpu           TYPE /scwm/de_rf_huident
                  iv_scan_lic           TYPE /scwm/de_rf_huident

                  iv_sn_main_selnum     TYPE zde_selnum_ui
                  iv_sn_main            TYPE /scwm/de_serid
                  iv_sn_selnum_02       TYPE zde_selnum_ui
                  iv_zzindenttab02_scan TYPE /scwm/de_serid
                  iv_sn_selnum_03       TYPE zde_selnum_ui
                  iv_zzindenttab03_scan TYPE /scwm/de_serid
                  iv_sn_selnum_04       TYPE zde_selnum_ui
                  iv_zzindenttab04_scan TYPE /scwm/de_serid
                  iv_sn_selnum_05       TYPE zde_selnum_ui
                  iv_zzindenttab05_scan TYPE /scwm/de_serid
        EXPORTING ev_no_clear_field     TYPE abap_bool,

      handle_scan_0600_vendor,

      handle_close_hu
        IMPORTING iv_huident TYPE /scwm/de_huident,

      handle_merge_hu
        IMPORTING iv_huident TYPE /scwm/de_huident,

      handle_split_hu
        IMPORTING iv_huident TYPE /scwm/de_huident,

      handle_open_hu
        IMPORTING iv_huident TYPE /scwm/de_huident,

      handle_count_exp,

      handle_capture_serial,

      handle_prod_scan_serial, " Andriyan Yordanov


      handle_delete_serial,

      handle_delete_all_serial,

      handle_call_repack_scr " Andriyan Yordanov
        IMPORTING iv_packhu           TYPE zde_cons_packhu OPTIONAL
                  iv_hutype_pall      TYPE zde_pack_cartons_type OPTIONAL
        EXPORTING et_repack_matid     TYPE tty_pmat_data
                  et_mapp_hutyp_matid TYPE tty_mapp_hutyp_matid,

      handle_popup_conc_mathut
        IMPORTING iv_humat       TYPE /scwm/de_matnr
                  iv_hutype      TYPE /scwm/de_hutyp
        EXPORTING et_lbox_values TYPE vrm_values,

      handle_full_repack
        IMPORTING iv_huident_src TYPE /scwm/de_huident
                  iv_huident_new TYPE /scwm/de_huident
                  iv_pmatnr      TYPE /scwm/de_matnr,   " Andriyan Yordanov /// we should do repack. base on pmatid

      handle_change_carrier
        IMPORTING iv_docid   TYPE /scdl/dl_docid
                  iv_carrier TYPE /scdl/dl_partyno,
      handle_serials,

      calc_number_of_cartons
        IMPORTING iv_huident        TYPE /scwm/de_huident
        RETURNING VALUE(rv_cartons) TYPE int2,

      cancel_wts_for_hu
        IMPORTING iv_huident TYPE /scwm/de_huident,

      redetermine_staging_area
        IMPORTING iv_huident TYPE /scwm/de_huident,

      check_scan_0400
        IMPORTING iv_scan    TYPE /scwm/de_rf_huident
        EXPORTING ev_matid   TYPE /scwm/de_matid
                  ev_huident TYPE /scwm/de_huident,

      split_popup_find_mc_matnr
        IMPORTING iv_hu_top      TYPE /scwm/de_huident
        CHANGING  cs_qty_prod_mc TYPE zstr_consolidation_scr_0410,

      create_hu
        IMPORTING iv_lgpla   TYPE /scwm/de_lgpla
                  iv_pmat    TYPE /scwm/de_pmat
                  iv_huident TYPE /scwm/de_huident OPTIONAL
                  iv_commit  TYPE xfeld DEFAULT abap_true
        EXPORTING ev_huident TYPE /scwm/de_huident
                  ev_guid_hu TYPE /scwm/guid_hu,

      delete_and_close
        IMPORTING iv_docid   TYPE /scdl/dl_docid
                  iv_huident TYPE /scwm/de_huident,

      confirm_hu_step
        IMPORTING iv_guid_hu TYPE /scwm/guid_hu,

      cancel_pick
        IMPORTING iv_huident TYPE /scwm/de_huident
                  iv_commit  TYPE xfeld,

      check_sp_error
        IMPORTING iv_rejected     TYPE boole_d
                  it_return_codes TYPE /scmb/t_sp_return_code
        RETURNING VALUE(rv_error) TYPE xfeld,

      set_process_code
        IMPORTING is_item_quan TYPE /scdl/s_sp_a_item_quantity,

      pack_hu
        IMPORTING iv_huident_src TYPE /scwm/de_huident
                  iv_huident_des TYPE /scwm/de_huident
                  iv_commit      TYPE xfeld DEFAULT abap_true,

      check_scanned
        IMPORTING iv_huident_src TYPE /scwm/de_huident
                  iv_huident_des TYPE /scwm/de_huident,

      pack_all_stock_to_hu
        IMPORTING iv_huident_src TYPE /scwm/de_huident
                  iv_huident_des TYPE /scwm/de_huident
                  iv_move_hu     TYPE abap_bool OPTIONAL
                  iv_commit      TYPE xfeld DEFAULT abap_true,

      pack_stock
        IMPORTING iv_huident_src TYPE /scwm/de_huident
                  iv_huident_des TYPE /scwm/de_huident
                  iv_guid_stock  TYPE /lime/guid_stock
                  iv_commit      TYPE xfeld DEFAULT abap_true
                  is_quan        TYPE /scwm/s_quan.

ENDCLASS.
