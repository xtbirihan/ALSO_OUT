**********************************************************************
*& Report zout_2d_barcode_handling
**********************************************************************
*& Key           : AD-231012
*& Request No.   :
**********************************************************************
*& Description (short)
*& Programm to handle serial numbers form 2D barcodes
*& 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
REPORT zout_2d_barcode_handling.

DATA gv_is_inital TYPE abap_bool VALUE abap_true.

DATA go_custom_container_input TYPE REF TO cl_gui_custom_container.
DATA go_custom_container_output TYPE REF TO cl_gui_custom_container.
DATA go_editor_input TYPE REF TO cl_gui_textedit.
DATA go_editor_output TYPE REF TO cl_gui_textedit.
DATA go_2d_barcode_handler TYPE REF TO zcl_out_2d_barcode_handling.

" Global variables for interacting with the dynpro
DATA ok_code LIKE sy-ucomm.
DATA lst_mode TYPE char255.
DATA inp_input_range_low TYPE string.
DATA inp_input_range_high TYPE string.
DATA inp_single_line_barcode TYPE string.
DATA matnr TYPE matnr.
DATA lgnum TYPE /scwm/lgnum.

INITIALIZATION.
  go_2d_barcode_handler = NEW zcl_out_2d_barcode_handling(  ).
  go_2d_barcode_handler->init_dynpro_objects( EXPORTING iv_repid = sy-repid
                                               CHANGING co_custom_container_input =  go_custom_container_input
                                                        co_custom_container_output = go_custom_container_output
                                                        co_editor_input = go_editor_input
                                                        co_editor_output = go_editor_output ).

START-OF-SELECTION.
  SET SCREEN zcl_out_2d_barcode_handling=>c_dynnr_main.
  lst_mode = zcl_out_2d_barcode_handling=>c_mode_text.


MODULE user_command_0100 INPUT.
  go_2d_barcode_handler->pai( iv_ucomm = ok_code
                          iv_mode = lst_mode ).

  go_2d_barcode_handler->init_dynpro_fields( EXPORTING
                                                iv_ucomm =  ok_code
                                                iv_mode = lst_mode
                                              CHANGING
                                                 cv_input_range_low = inp_input_range_low
                                                 cv_input_range_high = inp_input_range_high
                                                 co_editor_input = go_editor_input
                                                 co_editor_output = go_editor_output ).

ENDMODULE.


MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.

  IF gv_is_inital = abap_true.

    DATA(lt_modes) = VALUE vrm_values( ( key = zcl_out_2d_barcode_handling=>c_mode_text text = '(Multi) Text line barcdoe' )
                                       ( key = zcl_out_2d_barcode_handling=>c_mode_range text = 'Range creation' ) ).

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = 'LST_MODE'
        values          = lt_modes
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 99.

    gv_is_inital = abap_false.
  ENDIF.

  go_2d_barcode_handler->pbo( ).
ENDMODULE.
