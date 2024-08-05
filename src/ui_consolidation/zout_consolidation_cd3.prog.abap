**********************************************************************
*& Key           : AMOGYORODI-230814
*& Request No.   :
**********************************************************************
*& Description (short)
*&
*&
**********************************************************************
*----------------------------------------------------------------------*
***INCLUDE ZOUT_CONSOLIDATION_CD3.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class lcl_report_view
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_view DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      so_instance TYPE REF TO lcl_view.
    CLASS-METHODS:
      get_instance RETURNING VALUE(ro_instance) TYPE REF TO lcl_view.

    DATA:
      mt_cons_headers        TYPE ztt_cons_dlv_header,
      mt_cons_hus            TYPE ztt_cons_hu_header,
      mt_cons_hu_content     TYPE ztt_cons_hu_item,
      mt_cons_sn_reques_cont TYPE ztt_cons_hu_item, " Andriyan Yordanov
      ms_cons_hu_serial      TYPE zstr_cons_hu_item,
      mt_diff_hu_src         TYPE ztt_cons_hu_item,
      mt_diff_hu_src_edit    TYPE ztt_cons_hu_item_sel,
      mt_diff_hu_dest        TYPE ztt_cons_hu_item,
      mt_open_picking        TYPE ztt_consolidation_open_pick,
      mt_open_picking_hu     TYPE ztt_consolidation_open_pick_hu,
      mt_dest_hu_content     TYPE ztt_cons_hu_item,
      mt_serial              TYPE ztt_consolidation_serial,
      mo_alv_cons_headers    TYPE REF TO cl_salv_table,
      mo_alv_cons_hus        TYPE REF TO cl_salv_table,
      mo_alv_cons_content    TYPE REF TO cl_salv_table,
      mo_alv_diff_exp        TYPE REF TO cl_salv_table,
      mo_alv_diff_pack       TYPE REF TO cl_salv_table,
      mo_alv_dest_content    TYPE REF TO cl_salv_table,
      mo_alv_open_picking    TYPE REF TO cl_salv_table,
      mo_alv_serial_hu_cont  TYPE REF TO cl_salv_table,
      mo_alv_serial          TYPE REF TO cl_salv_table,
      mo_pack_instr_te       TYPE REF TO cl_gui_textedit.

    METHODS:
      show_alv_cons_headers    IMPORTING io_container TYPE REF TO cl_gui_container,
      show_alv_cons_hus        IMPORTING io_container TYPE REF TO cl_gui_container,
      show_alv_cons_hu_content IMPORTING io_container TYPE REF TO cl_gui_container,
      show_alv_serial_hu_content IMPORTING io_container TYPE REF TO cl_gui_container,
      show_alv_diff_hu_pack    IMPORTING io_container TYPE REF TO cl_gui_container,
      show_alv_diff_hu_exp     IMPORTING io_container TYPE REF TO cl_gui_container,
      show_alv_dest_hu_content IMPORTING io_container TYPE REF TO cl_gui_container,
      show_alv_open_picking    IMPORTING io_container TYPE REF TO cl_gui_container,
      show_alv_serial          IMPORTING io_container TYPE REF TO cl_gui_container,

      show_pack_inst           IMPORTING iv_dlv_level     TYPE abap_bool OPTIONAL
                                         iv_product_level TYPE abap_bool OPTIONAL
                                         io_container     TYPE REF TO cl_gui_container.
ENDCLASS.
