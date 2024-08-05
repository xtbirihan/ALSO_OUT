CLASS zcl_out_core_psc_process_posc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /scwm/if_ex_core_psc_process .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_pmat_hutypgrp
      IMPORTING
        !iv_lgnum          TYPE /scwm/lgnum
        !iv_matid          TYPE /scwm/de_matid
      RETURNING
        VALUE(rv_hutypgrp) TYPE /scwm/de_hutypgrp .
    METHODS get_vas_orders
      IMPORTING
        !iv_rdocid       TYPE /scwm/de_docid
        !iv_rdoccat      TYPE /scwm/de_doccat
        !iv_itemid       TYPE /scdl/dl_itemid
      RETURNING
        VALUE(rt_vas_hd) TYPE /scwm/tt_vas_header_int .
    METHODS read_doc_item
      IMPORTING
        !iv_rdocid      TYPE /scwm/de_docid
        !iv_rdoccat     TYPE /scwm/de_doccat
        !iv_itemid      TYPE /scwm/de_itmid
      RETURNING
        VALUE(rs_items) TYPE /scwm/dlv_item_out_prd_str .
ENDCLASS.



CLASS ZCL_OUT_CORE_PSC_PROCESS_POSC IMPLEMENTATION.


  METHOD /scwm/if_ex_core_psc_process~process.
**********************************************************************
*& Key           : RM-230810
*& Request No.   : GAP 54 - Process-Oriented Storage Control custom settings
**********************************************************************
*& Description (short)
*&
**********************************************************************

    DATA:
          lo_impl_whohu TYPE REF TO zcl_out_whohu_posc.

    IF zcl_switch=>get_switch_state( iv_lgnum = iv_lgnum
                                     iv_devid = zif_switch_const=>c_zout_017 ) EQ abap_false. "WOCR: POSC custom settings
      RETURN.
    ENDIF.

    IF cv_procs IS INITIAL.
      RETURN.
    ENDIF.

    DATA(ls_posc_cmap) = NEW zcl_crud_ztout_posc_cmap( )->select_single_by_keys( iv_lgnum = iv_lgnum
                                                                                 iv_prces = iv_prces
                                                                                 iv_procs = cv_procs ).

    IF ls_posc_cmap IS INITIAL.
      RETURN.
    ENDIF.


    lo_impl_whohu = zcl_out_whohu_posc=>get_inst( ).
    " HU-Type Group to which the HU-Type of the Packaging material determined for the WO belongs
    " Table /SCWM/T307
    READ TABLE lo_impl_whohu->mt_whohu INTO DATA(ls_whohu) WITH KEY huid = is_wt-huid.

    IF sy-subrc = 0.
      " check if pmat in column pmatif of ZTOUT_TOTE_PMAT
      " if yes use the tote column to find hu type grp
      zcl_crud_ztout_tote_pmat=>select_totes_for_pmat(
        EXPORTING
          iv_lgnum  = iv_lgnum
          iv_pmatid = ls_whohu-pmat_guid
        IMPORTING
          et_tote_pmat_map = DATA(lt_tote_pmat) ).

      IF lt_tote_pmat IS NOT INITIAL.
        DATA(lv_pmat_hutypgrp) = get_pmat_hutypgrp( iv_lgnum = iv_lgnum
                                                    iv_matid = lt_tote_pmat[ 1 ]-tote ).
      ELSE.

        lv_pmat_hutypgrp = get_pmat_hutypgrp( iv_lgnum = iv_lgnum
                                              iv_matid = ls_whohu-pmat_guid ).
      ENDIF.
* UM Start - 18.09.2023
    ELSE.
      lv_pmat_hutypgrp = get_pmat_hutypgrp( iv_lgnum = iv_lgnum
                                            iv_matid = is_huhdr-pmat_guid ).
* UM End - 18.09.2023

    ENDIF.

    " KEP WC determination HU type group
    IF ls_posc_cmap-kep_wc_determ = abap_true
* UM Start - 18.09.2023
      AND cv_nlpla IS INITIAL
* UM End - 18.09.2023
      .
      DATA(ls_kepwc_hutg) = NEW zcl_crud_ztout_kepwc_hutg( )->select_single_by_key( iv_lgnum    = iv_lgnum
                                                                                    iv_hutypgrp = lv_pmat_hutypgrp ).
      IF ls_kepwc_hutg IS NOT INITIAL.
        cv_nlpla = ls_kepwc_hutg-kep_wc_bin.
      ENDIF.
    ENDIF.

    " Read dlv item
    DATA(ls_item) = read_doc_item( iv_rdocid  = is_wt-rdocid
                                   iv_itemid  = is_wt-ritmid
                                   iv_rdoccat = is_wt-rdoccat ).
    " if cannot read with task read with HUitem
    IF ls_item IS INITIAL.
      ls_item = read_doc_item( iv_rdocid  = it_huitm[ 1 ]-qdocid
                               iv_itemid  = it_huitm[ 1 ]-qitmid
                               iv_rdoccat = it_huitm[ 1 ]-qdoccat ).
    ENDIF.

    " SVK WC determination based on staging
    IF ls_posc_cmap-svk_wc_determ = abap_true.

* UM Start - 09.10.2023 - Add check on non-conveyable Master Cartons
      zcl_param=>get_parameter(
        EXPORTING
          iv_lgnum     = iv_lgnum
          iv_process   = zif_param_const=>c_zout_0006
          iv_parameter = zif_param_const=>c_posc_svk_hutgr_nonc
        IMPORTING
          ev_constant  = DATA(lv_hutgr_nonc) ).

      IF lv_pmat_hutypgrp <> lv_hutgr_nonc OR lv_hutgr_nonc IS INITIAL.

        DATA(ls_svkwc_stag) = NEW zcl_crud_ztout_svkwc_stag( )->select_single_by_keys(
              iv_lgnum      = iv_lgnum
              iv_stare_a_gr = ls_item-sapext-/scwm/stagarea ).

        " Which one to use if there are more than 1 items?!
        IF ls_svkwc_stag IS NOT INITIAL.
          cv_nlpla = ls_svkwc_stag-svk_wc_bin.
        ENDIF.

      ENDIF.
* UM End - 09.10.2023 - Add check on non-conveyable Master Cartons

    ENDIF.

    zcl_param=>get_parameter(
          EXPORTING
            iv_lgnum     = iv_lgnum
            iv_process   = zif_param_const=>c_zout_0001
            iv_parameter = zif_param_const=>c_procty_kep
          IMPORTING
            ev_constant  = DATA(lv_kep_wpt) ).


    " Skip if KEP + WPT of the delivery
    IF ls_posc_cmap-skip_kep = abap_true AND ls_item-sapext-/scwm/procty = lv_kep_wpt.
      ev_not_this_step = abap_true.
    ENDIF.

    " VAS L2/L3
    IF ls_posc_cmap-vas_chk = abap_true.
      DATA(ls_vasare_det) = NEW zcl_crud_ztout_vasare_det( )->select_single_by_keys(
                                  iv_lgnum = iv_lgnum
                                  iv_procs = cv_procs ).

      IF ls_vasare_det IS NOT INITIAL.
        " Check VAS Order type according to table.

        DATA(lt_vas_hd) = get_vas_orders( iv_rdocid  = is_wt-rdocid
                                          iv_itemid  = is_wt-ritmid
                                          iv_rdoccat = is_wt-rdoccat ).
        " if cannot read with task read with HUitem
        IF lt_vas_hd IS INITIAL.
          lt_vas_hd = get_vas_orders( iv_rdocid  = it_huitm[ 1 ]-qdocid
                                      iv_itemid  = it_huitm[ 1 ]-qitmid
                                      iv_rdoccat = it_huitm[ 1 ]-qdoccat ).
        ENDIF.

        READ TABLE lt_vas_hd INDEX 1 INTO DATA(ls_vas).
        IF sy-subrc = 0 AND ls_vas-vas_art <> ls_vasare_det-vas_art.
          ev_not_this_step = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.

    " SPED VAS Bin chk
    IF ls_posc_cmap-sped_vas_bin_chk = abap_true.

      " Convoyable
      DATA(ls_spedvasbin) = NEW zcl_crud_ztout_spedvasbin( )->select_single_by_key(
                                    iv_lgnum     = iv_lgnum
                                    iv_curr_step = is_huhdr-procs
                                    iv_curr_bin  = is_huhdr-lgpla
                                    iv_next_step = cv_procs ).

      IF ls_spedvasbin IS NOT INITIAL.
        cv_nlpla = ls_spedvasbin-next_dst.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD get_pmat_hutypgrp.
**********************************************************************
*& Key           : RM-230810
*& Request No.   : GAP 54 - Process-Oriented Storage Control custom settings
**********************************************************************
*& Description (short)
*&
**********************************************************************

    DATA:
      ls_t307       TYPE  /scwm/t307,
      ls_mat_pack   TYPE /scwm/s_material_pack.

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_lgnum    = iv_lgnum
            iv_matid    = iv_matid
          IMPORTING
            es_mat_pack = ls_mat_pack.
        ##NO_HANDLER
      CATCH /scwm/cx_md.
    ENDTRY.

    CALL FUNCTION '/SCWM/T307_READ_SINGLE'
      EXPORTING
        iv_lgnum    = iv_lgnum
        iv_letyp    = ls_mat_pack-hutyp
      IMPORTING
        es_t307     = ls_t307
      EXCEPTIONS
        not_found   = 1
        wrong_input = 2
        OTHERS      = 3.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    rv_hutypgrp = ls_t307-hutypgrp.

  ENDMETHOD.


  METHOD get_vas_orders.
**********************************************************************
*& Key           : RM-230810
*& Request No.   : GAP 54 - Process-Oriented Storage Control custom settings
**********************************************************************
*& Description (short)
*&
**********************************************************************
    DATA:
      lt_vas_guid TYPE /scwm/tt_vas_guid.

    " Check VAS Order type according to table.

    CALL METHOD /scwm/cl_vas_db=>get_list
      EXPORTING
        it_refid       = VALUE /scwm/dlv_docid_item_tab( ( doccat = iv_rdoccat
                                                           itemid = iv_itemid
                                                           docid  = iv_rdocid ) )
      IMPORTING
        et_header_keys = lt_vas_guid
      EXCEPTIONS
        OTHERS         = 99.

    IF sy-subrc = 0.
      CALL METHOD /scwm/cl_vas_db=>read
        EXPORTING
          it_vas    = lt_vas_guid
        IMPORTING
          et_header = rt_vas_hd
        EXCEPTIONS
          OTHERS    = 4.
      IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD read_doc_item.
**********************************************************************
*& Key           : RM-230810
*& Request No.   : GAP 54 - Process-Oriented Storage Control custom settings
**********************************************************************
*& Description (short)
*&
**********************************************************************

    DATA:
      ls_read_options TYPE /scwm/dlv_query_contr_str,
      ls_include_data TYPE /scwm/dlv_query_incl_str_prd,
      lo_prd          TYPE REF TO /scwm/cl_dlv_management_prd.

    lo_prd = /scwm/cl_dlv_management_prd=>get_instance( ).

    ls_read_options-data_retrival_only      = abap_true.
    ls_read_options-mix_in_object_instances = abap_true.

    ls_include_data-head_refdoc  = abap_true.
    ls_include_data-item_refdoc  = abap_true.

    TRY.
        lo_prd->query(
          EXPORTING
            it_docid        = VALUE #( ( docid = iv_rdocid ) )
            iv_doccat       = iv_rdoccat
            is_read_options = ls_read_options
            is_include_data = ls_include_data
          IMPORTING
            et_items        = DATA(lt_items) ).

        READ TABLE lt_items WITH KEY itemid = iv_itemid INTO rs_items.
        ##NO_HANDLER
      CATCH /scdl/cx_delivery.

    ENDTRY.

  ENDMETHOD.
ENDCLASS.
