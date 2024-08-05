interface ZIF_WHSE_ORDER
  public .


  types:
    BEGIN OF ty_to,
      who           TYPE /scwm/who-who,
      count_to      TYPE /scwm/s_wo_det_mon_out-count_to,
      sum_weight    TYPE /scwm/s_wo_det_mon_out-sum_weight,
      unit_w        TYPE /scwm/s_wo_det_mon_out-unit_w,
      sum_volum     TYPE /scwm/s_wo_det_mon_out-sum_volum,
      unit_v        TYPE /scwm/s_wo_det_mon_out-unit_v,
      sum_reachtime TYPE /scwm/s_wo_det_mon_out-sum_reachtime,
      unit_rt       TYPE /scwm/s_wo_det_mon_out-unit_rt,
      pick_comp_dt  TYPE /scwm/s_wo_det_mon-pick_comp_dt,
      zz_bundled    TYPE  zde_bndl,
    END OF ty_to .
  types:
    tty_to TYPE STANDARD TABLE OF ty_to WITH EMPTY KEY.
  types:
**********************************************************************
  "<aahmedov>-28.06.2023
    BEGIN OF ty_to_cmp,
      who           TYPE /scwm/who-who,
      count_to      TYPE /scwm/s_wo_det_mon_out-count_to,
      sum_weight    TYPE /scdl/dl_qty,
      unit_w        TYPE /scwm/s_wo_det_mon_out-unit_w,
      sum_volum     TYPE /scdl/dl_qty,
      unit_v        TYPE /scwm/s_wo_det_mon_out-unit_v,
      sum_reachtime TYPE /scwm/s_wo_det_mon_out-sum_reachtime,
      unit_rt       TYPE /scwm/s_wo_det_mon_out-unit_rt,
      pick_comp_dt  TYPE /scwm/s_wo_det_mon-pick_comp_dt,
    END OF ty_to_cmp .
  types:
    tty_to_cmp TYPE STANDARD TABLE OF ty_to_cmp .

  constants:
**********************************************************************
    BEGIN OF wo_mapping_fieldname,
      lgnum      TYPE /scwm/s_map_selopt2field-fieldname VALUE 'LGNUM',
      who        TYPE /scwm/s_map_selopt2field-fieldname VALUE 'WHO',
      wcr        TYPE /scwm/s_map_selopt2field-fieldname VALUE 'WCR',
      type       TYPE /scwm/s_map_selopt2field-fieldname VALUE 'TYPE',
      hdr_procty TYPE /scwm/s_map_selopt2field-fieldname VALUE 'HDR_PROCTY',
      queue      TYPE /scwm/s_map_selopt2field-fieldname VALUE 'QUEUE',
      status     TYPE /scwm/s_map_selopt2field-fieldname VALUE 'STATUS',
      wostop     TYPE /scwm/s_map_selopt2field-fieldname VALUE 'WOSTOP',
      wostca     TYPE /scwm/s_map_selopt2field-fieldname VALUE 'WOSTCA',
      wostho     TYPE /scwm/s_map_selopt2field-fieldname VALUE 'WOSTHO',
      wostco     TYPE /scwm/s_map_selopt2field-fieldname VALUE 'WOSTCO',
      wostpr     TYPE /scwm/s_map_selopt2field-fieldname VALUE 'WOSTPR',
      areawho    TYPE /scwm/s_map_selopt2field-fieldname VALUE 'AREAWHO',
      processor  TYPE /scwm/s_map_selopt2field-fieldname VALUE 'PROCESSOR',
      started_at TYPE /scwm/s_map_selopt2field-fieldname VALUE 'STARTED_AT',
      lsd        TYPE /scwm/s_map_selopt2field-fieldname VALUE 'LSD',
      created_at TYPE /scwm/s_map_selopt2field-fieldname VALUE 'CREATED_AT',
    END OF wo_mapping_fieldname .
  constants:
    BEGIN OF wo_mapping_selname,
      s_who    TYPE /scwm/s_map_selopt2field-selname VALUE 'S_WHO',
      s_wcr    TYPE /scwm/s_map_selopt2field-selname VALUE 'S_WCR',
      s_type   TYPE /scwm/s_map_selopt2field-selname VALUE 'S_TYPE',
      s_hdrwpt TYPE /scwm/s_map_selopt2field-selname VALUE 'S_HDRWPT',
      s_queue  TYPE /scwm/s_map_selopt2field-selname VALUE 'S_QUEUE',
      s_status TYPE /scwm/s_map_selopt2field-selname VALUE 'S_STATUS',
      p_wostop TYPE /scwm/s_map_selopt2field-selname VALUE 'P_WOSTOP',
      p_wostca TYPE /scwm/s_map_selopt2field-selname VALUE 'P_WOSTCA',
      p_wostho TYPE /scwm/s_map_selopt2field-selname VALUE 'P_WOSTHO',
      p_wostco TYPE /scwm/s_map_selopt2field-selname VALUE 'P_WOSTCO',
      p_wostpr TYPE /scwm/s_map_selopt2field-selname VALUE 'P_WOSTPR',
      s_aawho  TYPE /scwm/s_map_selopt2field-selname VALUE 'S_AAWHO',
      s_proc   TYPE /scwm/s_map_selopt2field-selname VALUE 'S_PROC',
      s_wostdt TYPE /scwm/s_map_selopt2field-selname VALUE 'S_WOSTDT',
      s_wolsdt TYPE /scwm/s_map_selopt2field-selname VALUE 'S_WOLSDT',
      s_wocrdt TYPE /scwm/s_map_selopt2field-selname VALUE 'S_WOCRDT',
    END OF wo_mapping_selname .
  constants:
    BEGIN OF wo_att_fieldname,
      who            TYPE fieldname VALUE 'WHO',
      huid           TYPE fieldname VALUE 'HUID',
      whoseq         TYPE fieldname VALUE 'WHOSEQ',
      wcr            TYPE fieldname VALUE 'WCR',
      zzputwall      TYPE fieldname VALUE 'ZZPUTWALL',
      dlogpos_ext_wt TYPE fieldname VALUE 'DLOGPOS_EXT_WT',
      dstgrp         TYPE fieldname VALUE 'DSTGRP',
    END OF wo_att_fieldname .
  constants:
    BEGIN OF wo_cr,
      kshc TYPE /scwm/de_wcr VALUE 'KSHC',
      bshc TYPE /scwm/de_wcr VALUE 'BSHC',
    END OF wo_cr .
  constants:
    BEGIN OF wo_aarea,
      aa01 TYPE /scwm/de_whoaa VALUE 'AA01',
      aa02 TYPE /scwm/de_whoaa VALUE 'AA02',
    END OF wo_aarea .
  constants:
    BEGIN OF wo_status,
      open       TYPE /scwm/de_whostat VALUE ' ',
      canc       TYPE /scwm/de_whostat VALUE 'A',
      on_hold    TYPE /scwm/de_whostat VALUE 'B',
      conf       TYPE /scwm/de_whostat VALUE 'C',
      in_process TYPE /scwm/de_whostat VALUE 'D',
    END OF wo_status .
  constants:
    BEGIN OF wo_mapping_prop,
      tablename    TYPE /scwm/s_map_selopt2field-tablename VALUE '/SCWM/WHO',
      is_key       TYPE abap_boolean VALUE 'X',
      is_timestamp TYPE abap_boolean VALUE 'X',
      date_from    TYPE /scwm/s_map_selopt2field-p_date_from VALUE 'P_CDTFR',
      time_from    TYPE /scwm/s_map_selopt2field-p_time_from VALUE 'P_CTMFR',
      date_to      TYPE /scwm/s_map_selopt2field-p_date_to   VALUE 'P_CDTTO',
      time_to      TYPE /scwm/s_map_selopt2field-p_time_to   VALUE 'P_CDTTO',
    END OF wo_mapping_prop .
  constants:
    BEGIN OF wo_report,
      report   TYPE syrepid VALUE 'Z_SPO_SLO_OPTIMIZATION',
      pfstatus TYPE sypfkey VALUE 'STANDARD',
    END OF wo_report .
  constants:
    BEGIN OF wo_time_conversion,
      seconds TYPE meins VALUE 'S',
      minutes TYPE meins VALUE 'M',
      hours   TYPE meins VALUE 'H',
    END OF wo_time_conversion .
  constants:
    BEGIN OF message_severity,
      warning     TYPE symsgty VALUE 'W',
      error       TYPE symsgty VALUE 'E',
      abort       TYPE symsgty VALUE 'A',
      exit        TYPE symsgty VALUE 'X',
      success     TYPE symsgty VALUE 'S',
      information TYPE symsgty VALUE 'I',
    END OF message_severity .
  constants:
    BEGIN OF update_indicator,
      insert TYPE updkz_d VALUE 'I',
    END OF update_indicator .
  constants:
    BEGIN OF log,
      object                 TYPE balobj_d VALUE 'ZEWM',
      subobject              TYPE balsubobj VALUE 'ZWHO_SPO_SLO_OPT',
      subobject_qdet         TYPE balsubobj VALUE 'ZWHO_QUEUE_DET',
      subobject_wave_release TYPE balsubobj VALUE 'ZWAVERELEASE',
      textformat             TYPE bapitga-textformat VALUE 'ASC',
    END OF log .
  constants:
    BEGIN OF to_mapping_fieldname,
      lgnum  TYPE /scwm/s_map_selopt2field-fieldname VALUE 'TO~LGNUM',
      who    TYPE /scwm/s_map_selopt2field-fieldname VALUE 'WO~WHO',
      status TYPE /scwm/s_map_selopt2field-fieldname VALUE 'TO~TOSTAT',
    END OF to_mapping_fieldname .
  constants:
    BEGIN OF wo_attribute,
      attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
      attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
      attr3 TYPE scx_attrname VALUE 'MV_MSGV3',
      attr4 TYPE scx_attrname VALUE 'MV_MSGV4',
    END OF wo_attribute .
  constants:
    BEGIN OF wo_duplicate_key,
      msgid TYPE symsgid VALUE 'ZMC_WHSE_ORDER',
      msgno TYPE symsgno VALUE '000',
      attr1 TYPE scx_attrname VALUE '',
      attr2 TYPE scx_attrname VALUE '',
      attr3 TYPE scx_attrname VALUE '',
      attr4 TYPE scx_attrname VALUE '',
    END OF wo_duplicate_key .
  constants:
    BEGIN OF wo_unexpected_field,
      msgid TYPE symsgid VALUE 'ZMC_WHSE_ORDER',
      msgno TYPE symsgno VALUE '001',
      attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
      attr2 TYPE scx_attrname VALUE '',
      attr3 TYPE scx_attrname VALUE '',
      attr4 TYPE scx_attrname VALUE '',
    END OF wo_unexpected_field .
  constants:
    BEGIN OF wo_empty_key,
      msgid TYPE symsgid VALUE 'ZMC_WHSE_ORDER',
      msgno TYPE symsgno VALUE '002',
      attr1 TYPE scx_attrname VALUE '',
      attr2 TYPE scx_attrname VALUE '',
      attr3 TYPE scx_attrname VALUE '',
      attr4 TYPE scx_attrname VALUE '',
    END OF wo_empty_key .
  constants:
    BEGIN OF wo_no_customizing_found,
      msgid TYPE symsgid VALUE 'ZMC_WHSE_ORDER',
      msgno TYPE symsgno VALUE '003',
      attr1 TYPE scx_attrname VALUE '',
      attr2 TYPE scx_attrname VALUE '',
      attr3 TYPE scx_attrname VALUE '',
      attr4 TYPE scx_attrname VALUE '',
    END OF wo_no_customizing_found .
  constants:
    BEGIN OF invalid_time_unit,
      msgid TYPE symsgid VALUE 'ZMC_WHSE_ORDER',
      msgno TYPE symsgno VALUE '004',
      attr1 TYPE scx_attrname VALUE '',
      attr2 TYPE scx_attrname VALUE '',
      attr3 TYPE scx_attrname VALUE '',
      attr4 TYPE scx_attrname VALUE '',
    END OF invalid_time_unit .
  constants:
    BEGIN OF whoid_not_found,
      msgid TYPE symsgid VALUE 'ZMC_WHSE_ORDER',
      msgno TYPE symsgno VALUE '005',
      attr1 TYPE scx_attrname VALUE '',
      attr2 TYPE scx_attrname VALUE '',
      attr3 TYPE scx_attrname VALUE '',
      attr4 TYPE scx_attrname VALUE '',
    END OF whoid_not_found .
  constants:
    BEGIN OF who_creation_error,
      msgid TYPE symsgid VALUE 'ZMC_WHSE_ORDER',
      msgno TYPE symsgno VALUE '006',
      attr1 TYPE scx_attrname VALUE '',
      attr2 TYPE scx_attrname VALUE '',
      attr3 TYPE scx_attrname VALUE '',
      attr4 TYPE scx_attrname VALUE '',
    END OF who_creation_error .
  constants:
    BEGIN OF who_block_successfull,
      msgid TYPE symsgid VALUE 'ZMC_WHSE_ORDER',
      msgno TYPE symsgno VALUE '007',
      attr1 TYPE scx_attrname VALUE '',
      attr2 TYPE scx_attrname VALUE '',
      attr3 TYPE scx_attrname VALUE '',
      attr4 TYPE scx_attrname VALUE '',
    END OF who_block_successfull .
  constants:
    BEGIN OF who_unblock_successfull,
      msgid TYPE symsgid VALUE 'ZMC_WHSE_ORDER',
      msgno TYPE symsgno VALUE '008',
      attr1 TYPE scx_attrname VALUE '',
      attr2 TYPE scx_attrname VALUE '',
      attr3 TYPE scx_attrname VALUE '',
      attr4 TYPE scx_attrname VALUE '',
    END OF who_unblock_successfull .
  constants:
    BEGIN OF who_block_error,
      msgid TYPE symsgid VALUE 'ZMC_WHSE_ORDER',
      msgno TYPE symsgno VALUE '009',
      attr1 TYPE scx_attrname VALUE '',
      attr2 TYPE scx_attrname VALUE '',
      attr3 TYPE scx_attrname VALUE '',
      attr4 TYPE scx_attrname VALUE '',
    END OF who_block_error .
  constants:
    BEGIN OF who_unblock_error,
      msgid TYPE symsgid VALUE 'ZMC_WHSE_ORDER',
      msgno TYPE symsgno VALUE '010',
      attr1 TYPE scx_attrname VALUE '',
      attr2 TYPE scx_attrname VALUE '',
      attr3 TYPE scx_attrname VALUE '',
      attr4 TYPE scx_attrname VALUE '',
    END OF who_unblock_error .
  constants:
    BEGIN OF who_rule_error,
      msgid TYPE symsgid VALUE 'ZMC_WHSE_ORDER',
      msgno TYPE symsgno VALUE '011',
      attr1 TYPE scx_attrname VALUE '',
      attr2 TYPE scx_attrname VALUE '',
      attr3 TYPE scx_attrname VALUE '',
      attr4 TYPE scx_attrname VALUE '',
    END OF who_rule_error .
  constants:
    BEGIN OF who_creation_rule_successfull,
      msgid TYPE symsgid VALUE 'ZMC_WHSE_ORDER',
      msgno TYPE symsgno VALUE '012',
      attr1 TYPE scx_attrname VALUE '',
      attr2 TYPE scx_attrname VALUE '',
      attr3 TYPE scx_attrname VALUE '',
      attr4 TYPE scx_attrname VALUE '',
    END OF who_creation_rule_successfull .

  methods WO_DATA_SELECT
    exporting
      !ET_WHO type /SCWM/TT_WO_DET_MON
      !ET_ORDIM_O type TTY_TO
      !ET_ORDIM_C type TTY_TO
      !ET_ORDIM_O_ALL_FIELDS type /SCWM/TT_ORDIM_O .
  methods TO_DATA_SELECT
    importing
      !IT_WHO type /SCWM/TT_WHO
    exporting
      !ET_TO type /SCWM/TT_TO_DET_MON .
  methods UPDATE_QUEUE
    importing
      !IT_WHO type /SCWM/TT_WHO_INT
      !IT_ORDIM_O type /SCWM/TT_ORDIM_O
    exporting
      !ET_BAPIRET type BAPIRETTAB .
endinterface.
