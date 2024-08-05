*&---------------------------------------------------------------------*
*& Include          ZOUT_WO_QUEUE_DET_CL_D
*&---------------------------------------------------------------------*
********************************************************************
*& Key          : <aahmedov>-130623
*& Request No.   : GAPs 17 - Picking WO Bundling
********************************************************************
*& Description  :
********************************************************************
CLASS lcl_wo_queue_det DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          iv_lgnum TYPE /scwm/lgnum
        RAISING
          zcx_core_exception,

      main,

      sel_wo EXPORTING et_who     TYPE /scwm/tt_who_int
                       et_whohu   TYPE  /scwm/tt_whohu_int
                       et_ordim_o TYPE /scwm/tt_ordim_o,

      update_wo IMPORTING it_who     TYPE /scwm/tt_who_int
                          it_ordim_o TYPE /scwm/tt_ordim_o,

      validate_lgnum IMPORTING iv_lgnum        TYPE /scwm/lgnum
                     RETURNING VALUE(rv_lgnum) TYPE /scwm/lgnum.

  PRIVATE SECTION.
    TYPES: BEGIN OF ls_prule_pmat_mapping,
             ppmat  TYPE zde_ppmat, "Picking Package Material
             pmatid TYPE /scwm/de_pmatid, "Package Material GUID
           END OF ls_prule_pmat_mapping.

    TYPES: lt_prule_pmat_mapping TYPE STANDARD TABLE OF ls_prule_pmat_mapping.

    DATA: mo_log           TYPE REF TO /scwm/cl_log,
          mo_packmmat_algo TYPE REF TO zcl_packmmat_algo.

    DATA: mv_lgnum        TYPE /scwm/lgnum,
          mv_message      TYPE bapi_msg ##NEEDED,
          mt_queue_det    TYPE zcl_crud_ztout_queue_det=>tt_queue_det,
          mt_queue_selopt TYPE /scwm/tt_queue_r.

    METHODS save_log.

ENDCLASS.
