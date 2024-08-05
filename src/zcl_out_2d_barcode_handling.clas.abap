CLASS zcl_out_2d_barcode_handling DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS c_mode_text TYPE char255 VALUE 'TEXT'.
    CONSTANTS c_mode_range TYPE char255 VALUE 'RANGE'.
    CONSTANTS c_dynnr_main TYPE dynnr VALUE '0100'.
    CONSTANTS c_dynnr_range TYPE dynnr VALUE '0102'.
    CONSTANTS c_dynnr_text TYPE dynnr VALUE '0103'.

    DATA mv_report_id TYPE sy-repid READ-ONLY.
    DATA mv_dynpro_no TYPE dynnr.

    METHODS init_dynpro_objects IMPORTING iv_repid                   TYPE sy-repid
                                CHANGING  co_custom_container_input  TYPE REF TO cl_gui_custom_container
                                          co_custom_container_output TYPE REF TO cl_gui_custom_container
                                          co_editor_input            TYPE REF TO cl_gui_textedit
                                          co_editor_output           TYPE REF TO cl_gui_textedit.

    METHODS pbo.
    METHODS pai IMPORTING iv_ucomm TYPE sy-ucomm
                          iv_mode  TYPE char255.
    METHODS init_dynpro_fields IMPORTING iv_ucomm            TYPE sy-ucomm
                                         iv_mode             TYPE char255
                               CHANGING  cv_input_range_low  TYPE string
                                         cv_input_range_high TYPE string
                                         co_editor_input     TYPE REF TO cl_gui_textedit
                                         co_editor_output    TYPE REF TO cl_gui_textedit.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_serial_numbers,
             serialnumber TYPE gernr,
           END OF ty_serial_numbers.

    CONSTANTS c_ucomm_mode TYPE sy-ucomm VALUE 'MODE'.
    CONSTANTS c_ucomm_read TYPE sy-ucomm VALUE 'READ'.
    CONSTANTS c_ucomm_exit TYPE sy-ucomm VALUE 'EXIT'.
    CONSTANTS c_input_range_low TYPE dynfnam VALUE 'INP_INPUT_RANGE_LOW'.
    CONSTANTS c_input_range_high TYPE dynfnam VALUE 'INP_INPUT_RANGE_HIGH'.
    CONSTANTS c_input_matnr TYPE dynfnam VALUE 'MARA-MATNR'.
    CONSTANTS c_input_lgnum TYPE dynfnam VALUE '/SCWM/T300-LGNUM'.

    DATA mv_selected_mode TYPE char255.
    DATA mv_input_range_low TYPE string.
    DATA mv_input_range_high TYPE string.
    DATA mv_matnr TYPE matnr.
    DATA mv_lgnum TYPE /scwm/lgnum.

    DATA mt_output TYPE TABLE OF char1024.

    DATA mo_editor_input TYPE REF TO cl_gui_textedit.
    DATA mo_editor_output TYPE REF TO cl_gui_textedit.

    METHODS set_mode IMPORTING iv_mode  TYPE char255.
    METHODS read_parameters.
    METHODS read_barcode.
ENDCLASS.



CLASS ZCL_OUT_2D_BARCODE_HANDLING IMPLEMENTATION.


  METHOD init_dynpro_fields.
**********************************************************************
*& Key           : AD-2301012
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Initilazies the dynpro fields, depending on the selected mode.
*&
**********************************************************************
    IF iv_ucomm <> c_ucomm_mode.
      RETURN.
    ENDIF.

    co_editor_output->set_textstream( text = '' ).

    CASE mv_selected_mode.
      WHEN c_mode_text.
        CLEAR cv_input_range_low.
        CLEAR cv_input_range_high.
      WHEN c_mode_range.
        co_editor_input->set_textstream( text = '' ).
    ENDCASE.
  ENDMETHOD.


  METHOD init_dynpro_objects.
**********************************************************************
*& Key           : AD-230908
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Initializes the dynamic dynpro objects.
*&
**********************************************************************
    co_custom_container_input = NEW cl_gui_custom_container( container_name = 'CCONTAINER_INPUT' ).
    co_custom_container_output = NEW cl_gui_custom_container( container_name = 'CCONTAINER_OUTPUT' ).

    IF co_custom_container_input IS BOUND.
*      co_editor_input = NEW cl_gui_textedit( wordwrap_mode = cl_gui_textedit=>wordwrap_at_windowborder
      co_editor_input = NEW cl_gui_textedit( wordwrap_mode = cl_gui_textedit=>wordwrap_off
                                             wordwrap_to_linebreak_mode = cl_gui_textedit=>true
                                             parent = co_custom_container_input ).
    ENDIF.

    IF co_custom_container_output IS BOUND.
      co_editor_output = NEW cl_gui_textedit( wordwrap_mode = cl_gui_textedit=>wordwrap_at_windowborder
                                              wordwrap_to_linebreak_mode = cl_gui_textedit=>true
                                              parent = co_custom_container_output ).
    ENDIF.

    IF co_custom_container_input IS BOUND AND co_editor_input IS BOUND.
* Text lesen und schreiben
      co_editor_input->set_readonly_mode( readonly_mode = cl_gui_textedit=>false ).
* Anzeige von Toolbar und Statusbar des Texteditors unterdrücken
      co_editor_input->set_toolbar_mode( toolbar_mode = cl_gui_textedit=>false ).

      mo_editor_input =  co_editor_input.
    ENDIF.

    IF co_custom_container_output IS BOUND AND co_editor_output IS BOUND.
* Text nur lesen
      co_editor_output->set_readonly_mode( readonly_mode = cl_gui_textedit=>true ).
* Anzeige von Toolbar und Statusbar des Texteditors unterdrücken
      co_editor_output->set_toolbar_mode( toolbar_mode = cl_gui_textedit=>false ).

      mo_editor_output = co_editor_output.
    ENDIF.

    mv_report_id = iv_repid.
    mv_selected_mode = c_mode_text.
    mv_dynpro_no = c_dynnr_text.
  ENDMETHOD.


  METHOD pai.
**********************************************************************
*& Key           : AD-2301012
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Process After Input
*&
**********************************************************************
    CASE iv_ucomm.
      WHEN c_ucomm_mode.
        set_mode( iv_mode ).
      WHEN c_ucomm_read.
        read_parameters(  ).
        read_barcode( ).
      WHEN c_ucomm_exit.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.


  METHOD pbo.
**********************************************************************
*& Key           : AD-2301012
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Process Before Output
*&
**********************************************************************

  ENDMETHOD.


  METHOD read_barcode.
**********************************************************************
*& Key           : AD-2301012
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Reads the entered barcode
*&
**********************************************************************
    DATA lv_dynpro_value_low TYPE string.
    DATA lv_dynpro_value_high TYPE string.
    DATA lt_input TYPE TABLE OF char1024.
    DATA lt_result TYPE stringtab.
    DATA lo_sn_process TYPE REF TO zcl_serial_number_check.

    REFRESH mt_output.

    lo_sn_process = NEW #( mv_lgnum ).

    CASE mv_selected_mode.
      WHEN c_mode_range.
        CALL FUNCTION 'GET_DYNP_VALUE'
          EXPORTING
            i_field = c_input_range_low
            i_repid = mv_report_id
            i_dynnr = mv_dynpro_no
          CHANGING
            o_value = lv_dynpro_value_low.

        CALL FUNCTION 'GET_DYNP_VALUE'
          EXPORTING
            i_field = c_input_range_high
            i_repid = mv_report_id
            i_dynnr = mv_dynpro_no
          CHANGING
            o_value = lv_dynpro_value_high.

        lt_result = lo_sn_process->create_range( iv_mateialnumber = mv_matnr
                                                 iv_barcode_low = lv_dynpro_value_low
                                                 iv_barcode_high = lv_dynpro_value_high ).
      WHEN c_mode_text.
        mo_editor_input->get_text_as_stream( IMPORTING text = lt_input ).

        LOOP AT lt_input ASSIGNING FIELD-SYMBOL(<ls_line>).
          lv_dynpro_value_low = lv_dynpro_value_low && <ls_line>.
        ENDLOOP.

        lt_result = lo_sn_process->extract_serial_number( iv_mateialnumber = mv_matnr
                                                          iv_barcode = lv_dynpro_value_low  ).
    ENDCASE.

    mt_output = lt_result.

    mo_editor_output->set_text_as_stream( text = mt_output  ).
  ENDMETHOD.


  METHOD read_parameters.
**********************************************************************
*& Key           : AD-2301012
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Reads the application parameter
*&
**********************************************************************
    DATA lv_value TYPE string.

    CALL FUNCTION 'GET_DYNP_VALUE'
      EXPORTING
        i_field = c_input_matnr
        i_repid = mv_report_id
        i_dynnr = c_dynnr_main
      CHANGING
        o_value = lv_value.
    mv_matnr = lv_value.

    CALL FUNCTION 'GET_DYNP_VALUE'
      EXPORTING
        i_field = c_input_lgnum
        i_repid = mv_report_id
        i_dynnr = c_dynnr_main
      CHANGING
        o_value = lv_value.
    mv_lgnum = lv_value.
  ENDMETHOD.


  METHOD set_mode.
**********************************************************************
*& Key           : AD-2301012
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Set the process mode depending on the users choice.
*&
**********************************************************************
    mv_selected_mode = iv_mode.

    CASE mv_selected_mode.
      WHEN c_mode_range.
        mv_dynpro_no = c_dynnr_range.
      WHEN c_mode_text.
        mv_dynpro_no = c_dynnr_text.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
