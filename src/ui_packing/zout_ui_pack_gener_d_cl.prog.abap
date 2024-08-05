*----------------------------------------------------------------------*
***INCLUDE ZOUT_UI_PACK_GENER_D_CL.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class lcl_out_ui_pack_gen_prog
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
**********************************************************************
*& Key           : <AYORDANOV>-140723
*& Request No.   : GAPs 22 - Packing UI general program. local class
* definitian main logic
**********************************************************************
CLASS lcl_out_ui_pack_gen_prog DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS: at_sell_get_wst
      IMPORTING iv_lgnum TYPE /scwm/lgnum
                iv_wrkst TYPE /scwm/de_workstation.

    METHODS: exec_pack_sel
      IMPORTING iv_lgnum   TYPE /scwm/lgnum
                iv_wrkst   TYPE /scwm/de_workstation
                iv_lgpla   TYPE /scwm/lgpla
                iv_huident TYPE /scwm/de_huident.

  PRIVATE SECTION.

    CONSTANTS: mc_pack_appl_slo TYPE zde_packtyp VALUE 'SLO',
               mc_pack_appl_spo TYPE zde_packtyp VALUE 'SPO',
               mc_pack_appl_sc  TYPE zde_packtyp VALUE 'SC'.

    TYPES:
      BEGIN OF ty_hu_doc_ref_qty,
        docid   TYPE /scdl/dl_docid,
***        itemid  TYPE /scdl/dl_itemid,
        huident TYPE /scwm/de_huident,
        qty     TYPE /scwm/de_quantity,
***        uom     TYPE /scwm/de_base_uom,
      END OF ty_hu_doc_ref_qty .

    TYPES:
      tty_hu_doc_ref_qty  TYPE STANDARD TABLE OF ty_hu_doc_ref_qty .

    CLASS-DATA: ss_workstation TYPE /scwm/tworkst,
                ss_worksttyp   TYPE /scwm/twrktyp.

ENDCLASS.
