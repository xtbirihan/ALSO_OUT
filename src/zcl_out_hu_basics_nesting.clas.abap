CLASS zcl_out_hu_basics_nesting DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES /scwm/if_ex_hu_basics_nesting.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA st_source_hu_header TYPE /scwm/tt_huhdr_int.
    CLASS-DATA st_source_hu_item TYPE /scwm/tt_huitm_int.
    CLASS-DATA st_destination_hu_header TYPE /scwm/tt_huhdr_int.
    CLASS-DATA st_destination_hu_item TYPE /scwm/tt_huitm_int.

    CLASS-METHODS get_handling_unit IMPORTING is_hu    TYPE /scwm/s_huhdr_int
                                    EXPORTING et_huhdr TYPE /scwm/tt_huhdr_int
                                              et_huitm TYPE  /scwm/tt_huitm_int.

    CLASS-METHODS get_carrier IMPORTING iv_huident        TYPE /scwm/de_huident
                                        it_huhdr          TYPE /scwm/tt_huhdr_int
                                        it_huitm          TYPE /scwm/tt_huitm_int
                              RETURNING VALUE(rv_carrier) TYPE bu_partner.

    CLASS-METHODS check_is_necessary IMPORTING iv_huident                   TYPE /scwm/de_huident
                                     RETURNING VALUE(rv_check_is_necessary) TYPE abap_bool.
ENDCLASS.



CLASS ZCL_OUT_HU_BASICS_NESTING IMPLEMENTATION.


  METHOD /scwm/if_ex_hu_basics_nesting~check.
**********************************************************************
*& Key           : AD-231026
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Entry point for BAdI implementation /SCWM/EX_HU_BASICS_NESTING
*&
**********************************************************************
    REFRESH st_source_hu_header.
    REFRESH st_source_hu_item.
    REFRESH st_destination_hu_header.
    REFRESH st_destination_hu_item.

    get_handling_unit( EXPORTING
                          is_hu = cs_huhdr
                       IMPORTING
                          et_huhdr = st_source_hu_header
                          et_huitm = st_source_hu_item ).

    IF NOT check_is_necessary( iv_huident = cs_huhdr-huident ).
      RETURN.
    ENDIF.

    DATA(lv_carrier) = get_carrier( iv_huident = cs_huhdr-huident
                                    it_huhdr = st_source_hu_header
                                    it_huitm = st_source_hu_item ).

    get_handling_unit( EXPORTING
                          is_hu = cs_destination
                       IMPORTING
                          et_huhdr = st_destination_hu_header
                          et_huitm = st_destination_hu_item ).

    " Remove the destination HU from the list.
    " (If no other HUs are in the list, it means the destination HU is still empty
    "  an therfore we don't have to perform the carrier check).
    DELETE st_destination_hu_header WHERE huident = cs_destination-huident.

    LOOP AT st_destination_hu_header ASSIGNING FIELD-SYMBOL(<ls_destination_hu>).
      DATA(lv_carrier_to_compare) = get_carrier( iv_huident = <ls_destination_hu>-huident
                                                 it_huhdr = st_destination_hu_header
                                                 it_huitm = st_destination_hu_item ).

      IF lv_carrier <> lv_carrier_to_compare.
        sy-msgid = 'ZMC_OUT'.
        sy-msgno = '035'. "Carrier &1 of HU &2 is not equal to carrier &3 of destination HU &4
        sy-msgty = 'E'.
        sy-msgv1 = |{ lv_carrier ALPHA = OUT }|.
        sy-msgv2 = |{ cs_huhdr-huident ALPHA = OUT }|.
        sy-msgv3 = |{ lv_carrier_to_compare ALPHA = OUT }|.
        sy-msgv4 = |{ <ls_destination_hu>-huident ALPHA = OUT }|.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        RAISE EXCEPTION TYPE /scwm/cx_basics.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_is_necessary.
**********************************************************************
*& Key           : AD-231103
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Determines, if the check has to be performed
*&
**********************************************************************
    DATA(lv_procs) = VALUE /scwm/de_procs( st_source_hu_header[ huident = iv_huident ]-procs OPTIONAL ).
    DATA(lv_copst) = VALUE /scwm/de_copst( st_source_hu_header[ huident = iv_huident ]-copst OPTIONAL ).

    rv_check_is_necessary = abap_false.

    " Get the internal process step
    SELECT SINGLE iproc
      INTO @DATA(lv_iproc)
      FROM /scwm/tprocs
     WHERE procs = @lv_procs.

    " If no table entry found, no check is necessary.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " If the internal process step is STAG or LOAD, the check is necessary.
    IF lv_iproc = 'STAG' OR lv_iproc = 'LOAD'.
      rv_check_is_necessary = abap_true.
      RETURN.
    ENDIF.

    " If the process step is PAC and the step is completed (copst = X), the check is necessary.
    IF lv_iproc = 'PAC' AND lv_copst = 'X'.
      rv_check_is_necessary = abap_true.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD get_carrier.
**********************************************************************
*& Key           : AD-231026
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Determines the carrier for the delivery using the stock reference
*& from the handling unit.
**********************************************************************
    DATA(lv_hu_guid) = VALUE /scwm/guid_hu( it_huhdr[ huident = iv_huident ]-guid_hu OPTIONAL ).
    DATA(lv_qdocid)  = VALUE /scwm/de_docid( it_huitm[ guid_parent = lv_hu_guid ]-qdocid  OPTIONAL ).

    SELECT SINGLE partnercarr_id
       INTO @DATA(lv_carrier_guid)
       FROM /scdl/db_proch_o
      WHERE docid = @lv_qdocid.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BUPA_NUMBERS_GET'
      EXPORTING
        iv_partner_guid = lv_carrier_guid
      IMPORTING
        ev_partner      = rv_carrier.

    IF sy-subrc <> 0.
      CLEAR rv_carrier.
    ENDIF.
  ENDMETHOD.


  METHOD get_handling_unit.
**********************************************************************
*& Key           : AD-231103
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Reads the information for the provided handling unit.
*&
**********************************************************************
    DATA lv_severity  TYPE bapi_mtype.

    DATA(lt_huident) = VALUE  rseloption( ( sign = 'I'
                                               option = 'EQ'
                                               low = is_hu-huident  ) ).

    CALL FUNCTION '/SCWM/HU_SELECT_GEN'
      EXPORTING
        iv_lgnum      = is_hu-lgnum
        ir_huident    = lt_huident
      IMPORTING
        et_huhdr      = et_huhdr
        et_huitm      = et_huitm
        e_rc_severity = lv_severity
      EXCEPTIONS
        error         = 1
        not_possible  = 2
        wrong_input   = 3
        error_message = 1
        OTHERS        = 99.

    IF sy-subrc <> 0 OR lv_severity CA 'EXA'.
      REFRESH et_huhdr.
      REFRESH et_huitm.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
