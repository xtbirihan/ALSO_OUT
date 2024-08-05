**********************************************************************
*& Key           : AMOGYORODI-230814
*& Request No.   :
**********************************************************************
*& Description (short)
*&
*&
**********************************************************************
*&---------------------------------------------------------------------*
*& Include          ZOUT_CONSOLIDATION_TOP
*&---------------------------------------------------------------------*

TABLES: zstr_consolidation_scr_0200,
        zstr_consolidation_scr_0300,
        zstr_consolidation_scr_0400,
        zstr_consolidation_scr_0410,
        zstr_consolidation_scr_0500,
        zstr_consolidation_scr_0600.

CONSTANTS:
  gc_lbox_carrier      TYPE vrm_id VALUE 'CARRIER',
  gc_lbox_hutyp        TYPE vrm_id VALUE 'ZSTR_CONSOLIDATION_SCR_0300-HUTYPE',
  gc_lbox_pmat         TYPE vrm_id VALUE 'ZSTR_CONSOLIDATION_SCR_0300-NEWHU_PMAT', " Andriyan Yordanov
  gc_lbox_newhu        TYPE vrm_id VALUE 'ZSTR_CONSOLIDATION_SCR_0300-NEWHU', " Andriyan Yordanov

  gc_sn_field_ind1     TYPE string   VALUE   'ZSTR_CONSOLIDATION_SCR_0600-SN_SELNUM_KEY',
  gc_sn_field_indtxt1  TYPE string   VALUE  'ZSTR_CONSOLIDATION_SCR_0600-ZZINDENTTAB01_TXT01',
  gc_sn_field_indscan1 TYPE string   VALUE  'ZSTR_CONSOLIDATION_SCR_0600-ZZINDENTTAB01_SCAN',

  gc_sn_field_ind2     TYPE string   VALUE   'ZSTR_CONSOLIDATION_SCR_0600-SN_SELNUM_02',
  gc_sn_field_indtxt2  TYPE string   VALUE  'ZSTR_CONSOLIDATION_SCR_0600-ZZINDENTTAB02_TXT02',
  gc_sn_field_indscan2 TYPE string   VALUE  'ZSTR_CONSOLIDATION_SCR_0600-ZZINDENTTAB02_SCAN',

  gc_sn_field_ind3     TYPE string   VALUE   'ZSTR_CONSOLIDATION_SCR_0600-SN_SELNUM_03',
  gc_sn_field_indtxt3  TYPE string   VALUE  'ZSTR_CONSOLIDATION_SCR_0600-ZZINDENTTAB03_TXT03',
  gc_sn_field_indscan3 TYPE string   VALUE  'ZSTR_CONSOLIDATION_SCR_0600-ZZINDENTTAB03_SCAN',

  gc_sn_field_ind4     TYPE string   VALUE   'ZSTR_CONSOLIDATION_SCR_0600-SN_SELNUM_04',
  gc_sn_field_indtxt4  TYPE string   VALUE  'ZSTR_CONSOLIDATION_SCR_0600-ZZINDENTTAB04_TXT04',
  gc_sn_field_indscan4 TYPE string   VALUE  'ZSTR_CONSOLIDATION_SCR_0600-ZZINDENTTAB04_SCAN',


  gc_sn_field_ind5     TYPE string   VALUE   'ZSTR_CONSOLIDATION_SCR_0600-SN_SELNUM_05',
  gc_sn_field_indtxt5  TYPE string   VALUE  'ZSTR_CONSOLIDATION_SCR_0600-ZZINDENTTAB05_TXT05',
  gc_sn_field_indscan5 TYPE string   VALUE  'ZSTR_CONSOLIDATION_SCR_0600-ZZINDENTTAB05_SCAN',


  gc_sn_hu             TYPE string VALUE 'ZSTR_CONSOLIDATION_SCR_0600-HUIDENT_CURR',
  gc_sn_matnr          TYPE string VALUE 'ZSTR_CONSOLIDATION_SCR_0600-SN_MATNR',
  gc_sn_matx           TYPE string VALUE 'ZSTR_CONSOLIDATION_SCR_0600-SN_MAKTX',
  gc_sn_selno          TYPE string VALUE 'ZSTR_CONSOLIDATION_SCR_0600-SELNO',
  gc_sn_selsn          TYPE string VALUE 'ZSTR_CONSOLIDATION_SCR_0600-SELSN'.
