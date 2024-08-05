*----------------------------------------------------------------------*
***INCLUDE ZOUT_UI_PACK_GENER_I_CL.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_out_ui_pack_gen_prog
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
**********************************************************************
*& Key           : <AYORDANOV>-140723
*& Request No.   : GAPs 22 - Packing UI general program. local class
* implementation main logic
**********************************************************************
CLASS lcl_out_ui_pack_gen_prog IMPLEMENTATION.

  METHOD at_sell_get_wst.
    IF ss_workstation-workstation <> iv_wrkst.
      CALL FUNCTION '/SCWM/TWORKST_READ_SINGLE'
        EXPORTING
          iv_lgnum       = iv_lgnum
          iv_workstation = iv_wrkst
        IMPORTING
          es_workst      = ss_workstation
          es_wrktyp      = ss_worksttyp
        EXCEPTIONS
          OTHERS         = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      IF ss_worksttyp-trtyp <> '1'.
        MESSAGE e114(/scwm/ui_packing).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD  exec_pack_sel.

    DATA: lv_top_hu         TYPE /scwm/de_huident,
          ls_hu_doc_ref_qty TYPE ty_hu_doc_ref_qty,
          lt_hu_doc_ref_qty TYPE tty_hu_doc_ref_qty,
          lo_pack_wm        TYPE REF TO /scwm/cl_wm_packing.

    IF ss_workstation IS INITIAL.
      at_sell_get_wst( iv_lgnum = iv_lgnum
                       iv_wrkst = iv_wrkst ).
    ENDIF.

    IF ss_workstation IS INITIAL.
      RETURN.
    ENDIF.

    /scwm/cl_wm_packing=>get_instance(
        IMPORTING eo_instance = lo_pack_wm ).

    lo_pack_wm->init_by_workstation(
      EXPORTING
        is_workstation   = ss_workstation                 " Work Station Profile
      EXCEPTIONS
        error            = 1                " Error, see log
        OTHERS           = 2 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    /scwm/cl_wm_packing=>get_workcenter_bins(
      EXPORTING
        is_workstation = ss_workstation
      IMPORTING
        et_lagp        = DATA(lt_lagp)
      EXCEPTIONS
        OTHERS         = 99 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF NOT line_exists( lt_lagp[ lgnum = iv_lgnum
                                 lgpla = iv_lgpla ] ).
      "Storage bin &1 is not part of work station &2
      MESSAGE s014(zmc_out) WITH iv_lgpla
                                 iv_wrkst DISPLAY LIKE wmegc_severity_err .
      RETURN.
    ENDIF.

    " Get all data for the relevant HU """""" I don't know what will need
    lo_pack_wm->get_hu(
      EXPORTING
        iv_huident = iv_huident                 " Handling Unit Identification
*        iv_lock    =    ???????              " Single-Character Indicator
      IMPORTING
**        et_huident = DATA(lt_huidetn)
        et_huitm   = DATA(lt_huitm)                " Material Items in the HU
        es_huhdr   = DATA(ls_huhdr)                 " Internal Structure for Processing the HU Header
        et_huhdr   = DATA(lt_hu_hdr)                 " Table Type for HU Headers in the Internal Structure
**        et_hutree  = DATA(lt_hutree)                 " Table with HU Hierarchy Entries
**        et_huref   = DATA(lt_huref)                 " Table with HU References
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE   wmegc_severity_suc   NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE wmegc_severity_err .
    ENDIF.


    ASSIGN lt_hu_hdr[ top = abap_true
                      lgpla = iv_lgpla ] TO FIELD-SYMBOL(<ls_top_hu>).

    IF sy-subrc <> 0.
      "Relevant HU is not located in selected bin
      MESSAGE s015(zmc_out) DISPLAY LIKE wmegc_severity_err .
      RETURN.
    ENDIF.

    LOOP AT lt_huitm ASSIGNING FIELD-SYMBOL(<ls_huitem>).
      ASSIGN lt_hu_hdr[ guid_hu = <ls_huitem>-guid_parent ]-zz_packtyp TO FIELD-SYMBOL(<lv_zzpacktype>).
      CHECK sy-subrc = 0.
      DATA(lv_pack_type_scenario) = <lv_zzpacktype>.
      EXIT.
    ENDLOOP.

    CASE lv_pack_type_scenario.
      WHEN mc_pack_appl_slo.
*        zcl_outb_packing_workcenter_ui=>start_ui_slo(
*          EXPORTING
*            iv_lgnum = iv_lgnum
*            iv_wrkst = iv_wrkst
*            iv_lgpla = iv_lgpla
*            iv_huid  = iv_huident
*        ).
      WHEN mc_pack_appl_spo.
*        zcl_outb_packing_workcenter_ui=>start_ui_spo(
*          EXPORTING
*            iv_lgnum = iv_lgnum
*            iv_wrkst = iv_wrkst
*            iv_lgpla = iv_lgpla
*            iv_huid  = iv_huident
*        ).
      WHEN mc_pack_appl_sc.
        "" TODO CALL SCREEN SC
      WHEN OTHERS.
        "Error during process determination. HU not relevant.
        MESSAGE s017(zmc_out) DISPLAY LIKE wmegc_severity_err .
    ENDCASE.

*** Commented this because the logic was changed
***    IF lines( lt_huref ) > 0.
***
***      LOOP AT  lt_huref ASSIGNING FIELD-SYMBOL(<ls_hu_Ref>).
***        LOOP AT lt_huitm ASSIGNING FIELD-SYMBOL(<ls_hu_stock>)
***                                      WHERE qdocid = <ls_hu_Ref>-docid
***                                       GROUP BY ( qdocid = <ls_hu_stock>-qdocid )
***                                     ASSIGNING FIELD-SYMBOL(<ls_group_qdocid_stock>).
***
***          ls_hu_doc_ref_qty-docid   = <ls_hu_stock>-qdocid.
***          ls_hu_doc_ref_qty-huident  = COND #( WHEN ls_huhdr-higher_guid IS INITIAL THEN ls_huhdr-huident
***                                         ELSE VALUE #( lt_hu_hdr[ guid_hu = ls_huhdr-higher_guid ]-huident OPTIONAL ) ).
***
***          ls_hu_doc_ref_qty-qty = REDUCE i( INIT lv_cound = 0 FOR <ls_refhuitm_count> IN GROUP <ls_group_qdocid_stock>
***                                                  NEXT lv_cound = lv_cound + <ls_refhuitm_count>-quan ).
***
***          APPEND ls_hu_doc_ref_qty TO lt_hu_doc_ref_qty.
***          CLEAR ls_hu_doc_ref_qty.
***
***        ENDLOOP.
***      ENDLOOP.
***
***    ELSE.
***      MESSAGE s016(zmc_out) WITH <ls_top_hu>-huident DISPLAY LIKE wmegc_severity_err .
***      RETURN.
***    ENDIF.
***
***    IF lines( lt_hu_doc_ref_qty ) = 0.
***      MESSAGE s016(zmc_out) WITH <ls_top_hu>-huident DISPLAY LIKE wmegc_severity_err .
***      RETURN.
***    ENDIF.
***
***    " ====> check Shipping carton scenarion <====
***    IF lines( lt_hu_doc_ref_qty ) = 1.
***      IF 1 > lt_hu_doc_ref_qty[ 1 ]-qty.
***        " TODO " Call logci -  Shipping carton – The HU has only one delivery and quantity greater that 1PC
***
***        RETURN.
***      ELSE.
***        " TODO " -  SPO -  the HU can have multiple deliveries with one line and one PC
***        RETURN.
***      ENDIF.
***
***    ENDIF.
***
***    " ====> check SPO scenarion <====
***    LOOP AT lt_hu_doc_ref_qty ASSIGNING FIELD-SYMBOL(<ls_hu_check_qty_doc>) WHERE qty > 1.
***      DATA(lv_no_call_spo) = abap_true.
***      EXIT.
***    ENDLOOP.
***
***    IF lv_no_call_spo = abap_false.
***      " TODO " -  SPO -  the HU can have multiple deliveries with one line and one PC
***      RETURN.
***    ENDIF.
***
***    " ====> check  SLO scenarion <====
***    LOOP AT lt_hu_doc_ref_qty ASSIGNING <ls_hu_check_qty_doc> WHERE qty = 1.
***      DATA(lv_no_call_slo) = abap_true.
***      EXIT.
***    ENDLOOP.
***
***    IF lv_no_call_slo = abap_true.
***      " we have some mixing scenarion here ... what to do ?
***    ELSE.
***
***    ENDIF.


  ENDMETHOD.

ENDCLASS.
