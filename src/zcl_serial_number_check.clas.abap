CLASS zcl_serial_number_check DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor IMPORTING iv_warehousenumber TYPE /scwm/lgnum.
    METHODS extract_serial_number IMPORTING iv_mateialnumber        TYPE matnr
                                            iv_barcode              TYPE string
                                  RETURNING VALUE(rt_serialnumbers) TYPE stringtab.
    METHODS create_range IMPORTING iv_mateialnumber        TYPE matnr
                                   iv_barcode_low          TYPE string
                                   iv_barcode_high         TYPE string
                         RETURNING VALUE(rt_serialnumbers) TYPE stringtab.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_sn_template,
             first_digit TYPE string,
             lenght      TYPE int4,
           END OF ty_sn_template.

    CONSTANTS c_common_seperators TYPE string VALUE ',;:_|'.
    CONSTANTS c_template_wildcard TYPE c LENGTH 1 VALUE '_'.

    DATA mv_warehousenumber TYPE /scwm/lgnum.
    DATA mv_barcode_low TYPE string.
    DATA mv_barcode_high TYPE string.
    DATA mv_materialnumber TYPE matnr.

    DATA mt_templates TYPE TABLE OF ty_sn_template.
    DATA mt_serialnumbers TYPE stringtab.
    DATA mt_barcode_split TYPE stringtab.

    METHODS get_material_templates.
    METHODS split_barcode.
    METHODS search_for_sn_pattern.
    METHODS find_numberic_part IMPORTING iv_barcode      TYPE string
                               RETURNING VALUE(rv_value) TYPE int8.
ENDCLASS.



CLASS ZCL_SERIAL_NUMBER_CHECK IMPLEMENTATION.


  METHOD constructor.
**********************************************************************
*& Key           : AD-231130
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Constructor. Creates a new instance for a specific warehouse.
*&
**********************************************************************
    mv_warehousenumber = iv_warehousenumber.
  ENDMETHOD.


  METHOD create_range.
**********************************************************************
*& Key           : AD-231130
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Create the barcode range between the two provided values.
*&
**********************************************************************
    DATA lx_exception TYPE REF TO cx_root.
    DATA lv_barcode_low TYPE string.
    DATA lv_barcode_high TYPE string.
    DATA lv_value_low TYPE int8.
    DATA lv_value_high TYPE int8.
    DATA lv_static_part TYPE string.

    mv_materialnumber = iv_mateialnumber.
    mv_barcode_low = iv_barcode_low.
    mv_barcode_high = iv_barcode_high.

    REFRESH mt_serialnumbers.

    get_material_templates(  ).

    LOOP AT mt_templates ASSIGNING FIELD-SYMBOL(<ls_template>).
      TRY.
          " Search for pattern - low value
          lv_barcode_low = substring_from( val = mv_barcode_low  sub = <ls_template>-first_digit len = <ls_template>-lenght ).
          " Search for pattern - high value
          lv_barcode_high = substring_from( val = mv_barcode_high  sub = <ls_template>-first_digit len = <ls_template>-lenght ).
          EXIT.
        CATCH cx_root INTO lx_exception.
          CLEAR lv_barcode_low.
          CLEAR lv_barcode_high.
      ENDTRY.
    ENDLOOP.

    " Stop process, if either of the entered barcode doesn't match the pattern
    IF lv_barcode_low IS INITIAL OR lv_barcode_high IS INITIAL.
      RETURN.
    ENDIF.

    " Get numeric part of the barcodes
    lv_value_low = find_numberic_part( lv_barcode_low ).
    lv_value_high = find_numberic_part( lv_barcode_high ).

    " Check, if the barcode ranges are in the correct order
    IF lv_value_high < lv_value_low.
      RETURN.
    ENDIF.

    " Get static part of the barcode
    DATA(lv_sub) = condense( CONV string( lv_value_low ) ).
    lv_static_part = substring_before( val = lv_barcode_low sub = lv_sub ).

    DATA(lv_numbers_to_create) = lv_value_high - lv_value_low.

    DO lv_numbers_to_create TIMES.
      mt_serialnumbers = VALUE #( BASE mt_serialnumbers ( |{ lv_static_part }{ lv_value_low }|  ) ).
      lv_value_low = lv_value_low + 1.
    ENDDO.
    mt_serialnumbers = VALUE #( BASE mt_serialnumbers ( |{ lv_static_part }{ lv_value_high }|  ) ).


    rt_serialnumbers = mt_serialnumbers.
  ENDMETHOD.


  METHOD extract_serial_number.
**********************************************************************
*& Key           : AD-231130
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Checks the provided string for possible serial number(s) by applying
*& material specific templates. The templates are stored in table xyz.
**********************************************************************
    mv_materialnumber = iv_mateialnumber.
    mv_materialnumber = |{ mv_materialnumber ALPHA = OUT  }|. " Remove leading zeros
    mv_materialnumber = |{ mv_materialnumber ALIGN = RIGHT WIDTH = 18 PAD = '0'  }|. " Fill up to 18 digit to match with the template table

    mv_barcode_low = iv_barcode.

    REFRESH mt_barcode_split.
    REFRESH mt_serialnumbers.

    get_material_templates(  ).

    split_barcode(  ).
    search_for_sn_pattern(  ).

    " Remove leading zeros form serial numbers
    LOOP AT mt_serialnumbers ASSIGNING FIELD-SYMBOL(<lv_serial_number>).
      SHIFT <lv_serial_number> LEFT DELETING LEADING '0'.
    ENDLOOP.

    rt_serialnumbers = mt_serialnumbers.

  ENDMETHOD.


  METHOD find_numberic_part.
**********************************************************************
*& Key           : AD-231130
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Finds, and returns the numeric value of the given barcode.
*& The search beginns at the end of the barcode
**********************************************************************
    DATA(lv_barcode_length)  = strlen( iv_barcode ).
    DATA(lv_offset) = 1.
    DATA(lv_substring_is_numeric) = abap_true.

    WHILE lv_substring_is_numeric = abap_true.
      IF lv_offset > lv_barcode_length.
        EXIT. "The entire barcode ist numeric.
      ENDIF.

      IF substring( val = iv_barcode off = lv_barcode_length - lv_offset ) CO '0123456789'.
        TRY.
            rv_value = substring( val = iv_barcode  off = lv_barcode_length - lv_offset ).
          CATCH cx_root INTO DATA(lx_exception).
            CLEAR rv_value.
        ENDTRY.

        lv_offset = lv_offset + 1.
      ELSE.
        lv_substring_is_numeric = abap_false.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.


  METHOD get_material_templates.
**********************************************************************
*& Key           : AD-231130
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Reads the serial number templates for the provided material number
*&
**********************************************************************
    REFRESH mt_templates.

    SELECT template
      INTO TABLE @DATA(lt_patterns)
      FROM ztcross_patterns
     WHERE lgnum = @mv_warehousenumber
       AND matnr = @mv_materialnumber
  ORDER BY id_type, counter.

    LOOP AT lt_patterns ASSIGNING FIELD-SYMBOL(<ls_pattern>).
      DATA(ls_template) = VALUE ty_sn_template( ).

      FIND FIRST OCCURRENCE OF c_template_wildcard IN <ls_pattern>-template MATCH OFFSET DATA(lv_offset).
      IF lv_offset > 0.
        ls_template-first_digit = substring( val = <ls_pattern>-template len = lv_offset ).
      ENDIF.

      ls_template-lenght = strlen( <ls_pattern>-template ).

      IF ls_template-lenght IS NOT INITIAL.
        APPEND ls_template TO mt_templates.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD search_for_sn_pattern.
**********************************************************************
*& Key           : AD-231130
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Search for serial number patterns within the barcode strings.
*&
**********************************************************************
    DATA lt_result TYPE stringtab.
    DATA lv_serialnumber TYPE string.
    DATA lx_exception TYPE REF TO cx_root.

    " If there are no SN templates avaiable for the material number,
    " simply pass the imported strings to the output table.
    IF mt_templates IS INITIAL.
      mt_serialnumbers  = VALUE #( FOR <serial_number> IN mt_barcode_split ( <serial_number> )  ).
      RETURN.
    ENDIF.

    LOOP AT mt_templates ASSIGNING FIELD-SYMBOL(<ls_template>).
      lt_result = mt_barcode_split.

      IF <ls_template>-first_digit IS INITIAL.
        LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_line>).
          TRY.
              lv_serialnumber = substring( val = <ls_line> len = <ls_template>-lenght ).
              mt_serialnumbers = VALUE #( BASE mt_serialnumbers ( lv_serialnumber ) ).
            CATCH cx_root INTO lx_exception.
          ENDTRY.
        ENDLOOP.
      ELSE.
        DELETE lt_result WHERE table_line NS <ls_template>-first_digit.

        FIND ALL OCCURRENCES OF <ls_template>-first_digit IN TABLE  lt_result RESULTS DATA(lt_findings).

        LOOP AT lt_findings ASSIGNING FIELD-SYMBOL(<ls_finding>).
          TRY.
              lv_serialnumber = substring( val = lt_result[ <ls_finding>-line ] off = <ls_finding>-offset len = <ls_template>-lenght ).
              mt_serialnumbers = VALUE #( BASE mt_serialnumbers ( lv_serialnumber ) ).
            CATCH cx_root INTO lx_exception.
          ENDTRY.
        ENDLOOP.
      ENDIF.

      IF mt_serialnumbers IS NOT INITIAL.
        RETURN.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD split_barcode.
**********************************************************************
*& Key           : AD-231130
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Splits the barcode by the the most common sperators
*&
**********************************************************************
    DATA lv_counter TYPE int8 VALUE 0.
    DATA lv_current_line_index TYPE sy-tabix.
    DATA lt_split TYPE stringtab.

    FIELD-SYMBOLS <ls_line> TYPE string.

    " First of all, split by carrage return and linefeed.
    " (THe User might have entered an multi line barcode.)
    SPLIT mv_barcode_low AT cl_abap_char_utilities=>cr_lf INTO TABLE mt_barcode_split.

    " If there is none of the common seperators in the string, but there is spaces,
    " then split by space. (The space is probably the seperator)
    IF mv_barcode_low NA c_common_seperators.
      LOOP AT mt_barcode_split ASSIGNING <ls_line>.
        lv_current_line_index = sy-tabix.

        IF <ls_line> CA space.
          SPLIT <ls_line> AT space INTO TABLE lt_split.
          INSERT LINES OF lt_split INTO mt_barcode_split INDEX lv_current_line_index.
        ENDIF.
      ENDLOOP.

      DELETE mt_barcode_split WHERE table_line CA space.
    ENDIF.

    " Then split by the most common seperators
    DATA(lv_number_of_split_chars) = strlen( c_common_seperators ).

    WHILE lv_counter < lv_number_of_split_chars.
      LOOP AT mt_barcode_split ASSIGNING <ls_line>.
        lv_current_line_index = sy-tabix.

        IF contains( val = <ls_line> sub = c_common_seperators+lv_counter(1) ).
          SPLIT <ls_line> AT c_common_seperators+lv_counter(1) INTO TABLE lt_split.
          INSERT LINES OF lt_split INTO mt_barcode_split INDEX lv_current_line_index.
        ENDIF.
      ENDLOOP.

      DELETE mt_barcode_split WHERE table_line CS c_common_seperators+lv_counter(1).

      lv_counter = lv_counter + 1.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.
