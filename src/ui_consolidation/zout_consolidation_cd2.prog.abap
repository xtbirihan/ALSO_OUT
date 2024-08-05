**********************************************************************
*& Key           : AMOGYORODI-230814
*& Request No.   :
**********************************************************************
*& Description (short)
*&
*&
**********************************************************************
*----------------------------------------------------------------------*
***INCLUDE ZOUT_CONSOLIDATION_CD2.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class lcl_report_model
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_model DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      so_instance      TYPE REF TO lcl_model.

    CLASS-METHODS:
      get_instance RETURNING VALUE(ro_instance) TYPE REF TO lcl_model.

    CONSTANTS:
      c_actty           TYPE /scwm/de_actty VALUE 'SPED',
      c_cust_partno_spr TYPE /scdl/dl_party_role  VALUE 'ZCUSPR'.

    DATA:
***      mv_aarea           TYPE /scwm/de_aarea, " Andriyan Yordanov
      mv_lgtyp           TYPE /scwm/de_lgtyp,
      mv_procs           TYPE /scwm/de_procs,
      mt_cross_numbers   TYPE zewm_tt_cross_numbers, " end Andriyan Yordanov
      mt_dlv_headers     TYPE /scwm/dlv_header_out_prd_tab,
      mt_dlv_items       TYPE /scwm/dlv_item_out_prd_tab,
      mt_open_picking    TYPE ztt_consolidation_open_pick,
      mt_open_picking_hu TYPE ztt_consolidation_open_pick_hu,
      ms_cons_header     TYPE zstr_cons_dlv_header,
      mt_cons_headers    TYPE ztt_cons_dlv_header,
      mt_serials         TYPE STANDARD TABLE OF ztcross_cap_nums,
      ms_cons_hu         TYPE zstr_cons_hu_header,
      mt_cons_hus        TYPE ztt_cons_hu_header,
      mt_cons_huhdr      TYPE /scwm/tt_huhdr_int,
      mt_cons_huitm      TYPE /scwm/tt_huitm_int,
      mt_cons_huitm_exc  TYPE /scwm/tt_huitm_int,
      mt_scan_huitm      TYPE /scwm/tt_huitm_int,
      ms_cons_hu_content TYPE zstr_cons_hu_item,
      mt_cons_hu_content TYPE ztt_cons_hu_item,
      mt_diff_hu_src     TYPE ztt_cons_hu_item,
      mt_diff_hu_dest    TYPE ztt_cons_hu_item,
      mt_dest_hu_content TYPE ztt_cons_hu_item,
      mt_sn_split_pall   TYPE ztt_sn_split_pall.
    METHODS:
      set_lgtype
        IMPORTING iv_lgtyp TYPE /scwm/de_lgtyp,

***      set_aarea
***        IMPORTING iv_aarea TYPE /scwm/de_aarea,

      set_procs
        IMPORTING iv_procs TYPE /scwm/de_procs,

      get_open_deliveries
        IMPORTING it_docid TYPE /scdl/dl_docid_tab OPTIONAL,

      build_cons_dlv_table,

      build_cons_huitem_table
        IMPORTING iv_huident      TYPE /scwm/de_huident
                  iv_split        TYPE xfeld OPTIONAL
                  it_huitem       TYPE /scwm/tt_huitm_int
        EXPORTING et_hu_info_cont TYPE ztt_cons_hu_item
        CHANGING  ct_hu_content   TYPE ztt_cons_hu_item,

      build_cons_hu_table
        IMPORTING iv_docid TYPE /scdl/dl_docid,

      build_pick_table
        IMPORTING iv_docid TYPE /scdl/dl_docid,

      get_party_role
        IMPORTING is_dlv_header TYPE /scwm/dlv_header_out_prd_str
                  iv_party_role TYPE /scdl/dl_party_role
        EXPORTING ev_party_no   TYPE bu_partner
                  ev_party_text TYPE bu_descrip,

      get_status
        IMPORTING is_dlv_header    TYPE /scwm/dlv_header_out_prd_str
                  iv_status_type   TYPE /scdl/dl_status_type
        RETURNING VALUE(rv_status) TYPE zde_status_picking,

      get_hus_on_cons_bin
        IMPORTING "  iv_lgtyp TYPE /scwm/de_lgtyp OPTIONAL " Andriyan Yordanov - now we have set method
                  iv_lgpla TYPE /scwm/de_lgpla OPTIONAL
        EXPORTING et_docid TYPE /scdl/dl_docid_tab,

      get_delivery_for_hu
        IMPORTING iv_huident TYPE /scwm/de_huident,

      get_pick_open_who
        IMPORTING is_who             TYPE /scwm/s_who_int
                  it_whohu           TYPE /scwm/tt_whohu
                  it_ordim_o         TYPE /scwm/tt_ordim_o
                  it_ordim_c         TYPE /scwm/tt_ordim_c
                  it_huhdr           TYPE /scwm/tt_huhdr_int
        CHANGING  ct_open_picking_hu TYPE ztt_consolidation_open_pick_hu,

      get_real_aa
        IMPORTING is_who          TYPE /scwm/s_who_int
                  it_ordim_o      TYPE /scwm/tt_ordim_o
                  it_ordim_c      TYPE /scwm/tt_ordim_c
        RETURNING VALUE(rv_aarea) TYPE /scwm/de_aarea,

      get_serials_for_hu
        IMPORTING iv_huident       TYPE /scwm/de_huident
                  iv_group_mc      TYPE /scwm/dl_counter
        RETURNING VALUE(rt_serial) TYPE ztt_consolidation_serial,

      insert_serials
        IMPORTING it_serial TYPE ztt_serial
                  iv_commit TYPE xfeld DEFAULT abap_true,

      update_serials
        IMPORTING it_serial TYPE ztt_serial
                  iv_commit TYPE xfeld DEFAULT abap_true,

      delete_serials_for_hu
        IMPORTING iv_huident TYPE /scwm/de_huident
                  iv_commit  TYPE xfeld DEFAULT abap_true,

      is_hu_ready
        IMPORTING iv_huident      TYPE /scwm/de_huident
        RETURNING VALUE(rv_ready) TYPE xfeld,

      get_hutype
        IMPORTING is_huhdr         TYPE /scwm/s_huhdr_int
        RETURNING VALUE(rv_hutype) TYPE string,

      get_hutype_for_pmat
        IMPORTING iv_pmatid TYPE /scwm/de_matid
        EXPORTING ev_hutype TYPE /scwm/de_hutyp
                  ev_descr  TYPE string,

      is_pallet
        IMPORTING is_huhdr         TYPE /scwm/s_huhdr_int OPTIONAL
                  iv_hutypgrp      TYPE /scwm/de_hutypgrp OPTIONAL
        RETURNING VALUE(rv_pallet) TYPE xfeld,

      is_master_carton
        IMPORTING is_huhdr     TYPE /scwm/s_huhdr_int OPTIONAL
                  iv_hutypgrp  TYPE /scwm/de_hutypgrp OPTIONAL
        RETURNING VALUE(rv_mc) TYPE xfeld,

      is_shipping_carton
        IMPORTING is_huhdr     TYPE /scwm/s_huhdr_int OPTIONAL
                  iv_hutypgrp  TYPE /scwm/de_hutypgrp OPTIONAL
        RETURNING VALUE(rv_sc) TYPE xfeld,

      get_serials
        IMPORTING iv_docid TYPE /scdl/dl_docid,

      add_huitem
        IMPORTING is_huitem     TYPE /scwm/s_huitm_int
                  iv_split      TYPE xfeld
        CHANGING  ct_hu_content TYPE ztt_cons_hu_item,

      sum_hu_quantity IMPORTING iv_guid_hu     TYPE /scwm/guid_hu
                                iv_matid       TYPE /scwm/de_matid  OPTIONAL
                      RETURNING VALUE(rv_quan) TYPE /scwm/de_quantity,

      get_serial_status
        IMPORTING iv_huident         TYPE /scwm/de_huident
                  iv_pall_group_mc   TYPE /scwm/dl_counter OPTIONAL
                  iv_calc_stock_onhu TYPE abap_bool        OPTIONAL
                  iv_sn_screen_sub   TYPE abap_bool        OPTIONAL
        RETURNING VALUE(rv_status)   TYPE zde_serial_status,

      init_diffhu_tables
        IMPORTING iv_huident TYPE /scwm/de_huident,

      get_text_dlv
        RETURNING VALUE(rv_text_dlv) TYPE string,

      get_text_hu
        RETURNING VALUE(rv_text_dlv) TYPE string,

      get_text_dlv_hu
        RETURNING VALUE(rv_text_dlv_hu) TYPE string,

      check_sn_track
        RETURNING VALUE(rv_sn_scan) TYPE abap_bool.

ENDCLASS.
