class ZCL_OUT_WHO_DSTGRP_SPO_SLO definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_WHO_DSTGRP .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_OUT_WHO_DSTGRP_SPO_SLO IMPLEMENTATION.


  METHOD /scwm/if_ex_who_dstgrp~dstgrp.
**********************************************************************
*& Key           : RM-230407
*& Request No.   : GAPs 57 Outbound: Custom Warehouse Order Creation
*&                 Rule filters for SPO/SLO flows
**********************************************************************
*& Description (short)
*& The aim of this class is to create an instance of an singleton class
*& with a table that will store all WHO's WTs to be created during the wave
*& release. In case a dlv has more than 1 WT with different activity areas
*& all the tasks of this dlv will be marked as "different areawho".
*& This table will be later used in the BAdI Implementation of /SCWM/EX_WHO_FLT_IL
**********************************************************************

    DATA:
      lt_areawho         TYPE TABLE OF /scwm/de_whoaa,
      lt_temp_to         TYPE zcl_out_who_wt_areawho=>tt_wt_areawho,
      lo_impl_wt_areawho TYPE REF TO zcl_out_who_wt_areawho.

    IF zcl_switch=>get_switch_state( iv_lgnum = /scwm/cl_tm=>sv_lgnum
                                     iv_devid = zif_switch_const=>c_zout_002 ) EQ abap_false.
      RETURN.
    ENDIF.

    lo_impl_wt_areawho = zcl_out_who_wt_areawho=>get_inst( ).

    IF lo_impl_wt_areawho->mt_wt_areawho IS NOT INITIAL.
      DELETE lo_impl_wt_areawho->mt_wt_areawho WHERE lgnum = /scwm/cl_tm=>sv_lgnum.
    ENDIF.

    lt_temp_to = it_to.
    SORT lt_temp_to BY rdocid.

    LOOP AT lt_temp_to ASSIGNING FIELD-SYMBOL(<ls_dummy>)
                                    GROUP BY ( rdocid = <ls_dummy>-rdocid )
                                    INTO DATA(ls_docno_grps).

      IF ls_docno_grps-rdocid IS INITIAL.
        CONTINUE.
      ENDIF.

      " Find all areawho for one document
      LOOP AT GROUP ls_docno_grps INTO DATA(ls_docno_grp).
        APPEND ls_docno_grp-areawho TO lt_areawho.
      ENDLOOP.

      DELETE ADJACENT DUPLICATES FROM lt_areawho.

      LOOP AT GROUP ls_docno_grps INTO ls_docno_grp.

        " If activity areas are more than 1 -> SLO/SPO not allowed
        IF lines( lt_areawho ) > 1.
          ls_docno_grp-diff_areawho = abap_true.
        ENDIF.

        APPEND ls_docno_grp TO lo_impl_wt_areawho->mt_wt_areawho.
      ENDLOOP.

      CLEAR: lt_areawho.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
