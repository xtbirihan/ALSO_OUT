class ZCL_WHSE_ORDER definition
  public
  final
  create public .

public section.

  interfaces ZIF_WHSE_ORDER .

  aliases TO_DATA_SELECT
    for ZIF_WHSE_ORDER~TO_DATA_SELECT .
  aliases UPDATE_QUEUE
    for ZIF_WHSE_ORDER~UPDATE_QUEUE .
  aliases WO_DATA_SELECT
    for ZIF_WHSE_ORDER~WO_DATA_SELECT .

  types:
    BEGIN OF ts_named_dref,
        name TYPE string,
        dref TYPE REF TO data,
      END OF ts_named_dref .
  types:
    tt_named_dref      TYPE STANDARD TABLE OF ts_named_dref WITH DEFAULT KEY .
  types TT_NAMED_SELTABLES type TT_NAMED_DREF .
  types TS_NAMED_SELTABLE type TS_NAMED_DREF .

  class-data MV_LGNUM type /SCWM/LGNUM .

  methods CONSTRUCTOR
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IT_SELECTION_PARAMETERS type ZTT_SELECT_OPTION optional .
  class-methods COMBINE_SELTABS
    importing
      !IT_NAMED_SELTABS type TT_NAMED_SELTABLES
      !IV_CLIENT_FIELD type STRING optional
    returning
      value(RV_WHERE) type STRING .
  PROTECTED SECTION.
private section.

  data MV_WHERE type STRING .

  methods RAISE_EXCEPTION_FROM_SY .
  methods FILL_MAPPING_TABLE
    returning
      value(RT_MAPPING) type /SCWM/TT_MAP_SELOPT2FIELD .
  methods FILL_WO_STATUS
    importing
      !IV_WO_STATUS_OPEN type /SCWM/DE_WIP_WOSTAT_OPEN optional
      !IV_WO_STATUS_CANCEL type /SCWM/DE_WIP_WOSTAT_CANC optional
      !IV_WO_STATUS_HOLD type /SCWM/DE_WIP_WOSTAT_HOLD optional
      !IV_WO_STATUS_CONFIRM type /SCWM/DE_WIP_WOSTAT_CONF optional
      !IV_WO_STATUS_PROCESSING type /SCWM/DE_WIP_WOSTAT_PROC optional
    returning
      value(RT_STATUS) type ZTT_SELECT_OPTION_FIELDS .
  methods CHECK_SELECTION_PARAMETERS
    importing
      !IT_SELECTION_PARAMETERS type ZTT_SELECT_OPTION .
  methods GENERATE_DYNAMIC_WHERE
    importing
      !IT_SELECTION_PARAMETERS type ZTT_SELECT_OPTION .
ENDCLASS.



CLASS ZCL_WHSE_ORDER IMPLEMENTATION.


  METHOD check_selection_parameters.
********************************************************************
*& Key          : <TBIRIHAN>-January 17, 2024
*& Request No.  : GAP-063
********************************************************************
*& Description  :
********************************************************************
    CALL FUNCTION '/SCWM/T340D_READ_SINGLE'
      EXPORTING
        iv_lgnum  = me->mv_lgnum
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      raise_exception_from_sy( ).
    ENDIF.

    IF it_selection_parameters IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lt_selection_parameters)  = it_selection_parameters.
    SORT lt_selection_parameters BY field.
    DELETE ADJACENT DUPLICATES FROM lt_selection_parameters COMPARING field.
    IF sy-subrc EQ 0.
      RAISE EXCEPTION TYPE zcx_whse_order
        EXPORTING
          textid = zif_whse_order=>wo_duplicate_key.
    ENDIF.

    DATA(lt_mapping) = me->fill_mapping_table( ).
    LOOP AT it_selection_parameters ASSIGNING FIELD-SYMBOL(<ls_selection_parameters>).
      IF NOT line_exists( lt_mapping[ fieldname = <ls_selection_parameters>-field  ] ).
        RAISE EXCEPTION TYPE zcx_whse_order
          EXPORTING
            textid   = zif_whse_order=>wo_unexpected_field
            mv_msgv1 = CONV #( <ls_selection_parameters>-field ).
      ENDIF.
    ENDLOOP.

    LOOP AT it_selection_parameters ASSIGNING <ls_selection_parameters> WHERE field IS INITIAL.
      EXIT.
    ENDLOOP.
    IF sy-subrc EQ 0.
      RAISE EXCEPTION TYPE zcx_whse_order
        EXPORTING
          textid = zif_whse_order=>wo_empty_key.
    ENDIF.
  ENDMETHOD.


  METHOD combine_seltabs.
********************************************************************
*& Key          : <TBIRIHAN>-January 17, 2024
*& Request No.  : GAP-063
********************************************************************
*& Description  :
********************************************************************

    DATA:
      ls_seltab TYPE ts_named_seltable,
      lr_seltab TYPE REF TO cl_shdb_seltab,
      lv_sep    TYPE string.
    FIELD-SYMBOLS
      <ls_seltab> TYPE STANDARD TABLE.

    IF iv_client_field IS NOT INITIAL.
      rv_where = |{ iv_client_field } = '{ sy-mandt }'|.
      lv_sep = ` AND `.
    ENDIF.

    LOOP AT it_named_seltabs INTO ls_seltab.
      ASSIGN ls_seltab-dref->* TO <ls_seltab>.
      IF <ls_seltab> IS NOT INITIAL.
        lr_seltab = cl_shdb_seltab=>new( <ls_seltab> ).
        TRY.
            rv_where = |{ rv_where }{ lv_sep }({ lr_seltab->sql_where_condition( ls_seltab-name ) } )|.
            ##NO_HANDLER
          CATCH cx_shdb_exception.
        ENDTRY.

        lv_sep = ` AND `.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
********************************************************************
*& Key          : <TBIRIHAN>-January 17, 2024
*& Request No.  : GAP-063
********************************************************************
*& Description  :
********************************************************************

    me->mv_lgnum = iv_lgnum.
    me->check_selection_parameters( it_selection_parameters ).
    me->generate_dynamic_where(  it_selection_parameters  ).

  ENDMETHOD.


  METHOD fill_mapping_table.
********************************************************************
*& Key          : <TBIRIHAN>-January 17, 2024
*& Request No.  : GAP-063
********************************************************************
*& Description  :
********************************************************************

    APPEND VALUE #( tablename = zif_whse_order=>wo_mapping_prop-tablename
                    selname   = zif_whse_order=>wo_mapping_selname-s_who
                    fieldname = zif_whse_order=>wo_mapping_fieldname-who
                    is_key    = zif_whse_order=>wo_mapping_prop-is_key
                  ) TO rt_mapping.

    APPEND VALUE #( tablename = zif_whse_order=>wo_mapping_prop-tablename
                    selname   = zif_whse_order=>wo_mapping_selname-s_wcr
                    fieldname = zif_whse_order=>wo_mapping_fieldname-wcr
                  ) TO rt_mapping.

    APPEND VALUE #( tablename = zif_whse_order=>wo_mapping_prop-tablename
                    selname   = zif_whse_order=>wo_mapping_selname-s_type
                    fieldname = zif_whse_order=>wo_mapping_fieldname-type
                  ) TO rt_mapping.

    APPEND VALUE #( tablename = zif_whse_order=>wo_mapping_prop-tablename
                    selname   = zif_whse_order=>wo_mapping_selname-s_hdrwpt
                    fieldname = zif_whse_order=>wo_mapping_fieldname-hdr_procty
                  ) TO rt_mapping.

    APPEND VALUE #( tablename = zif_whse_order=>wo_mapping_prop-tablename
                    selname   = zif_whse_order=>wo_mapping_selname-s_queue
                    fieldname = zif_whse_order=>wo_mapping_fieldname-queue
                  ) TO rt_mapping.

    APPEND VALUE #( tablename = zif_whse_order=>wo_mapping_prop-tablename
                    selname   = zif_whse_order=>wo_mapping_selname-s_status
                    fieldname = zif_whse_order=>wo_mapping_fieldname-status
                  ) TO rt_mapping.

    APPEND VALUE #( tablename = zif_whse_order=>wo_mapping_prop-tablename
                    selname   = zif_whse_order=>wo_mapping_selname-p_wostop
                    fieldname = zif_whse_order=>wo_mapping_fieldname-wostop
                  ) TO rt_mapping.

    APPEND VALUE #( tablename = zif_whse_order=>wo_mapping_prop-tablename
                    selname   = zif_whse_order=>wo_mapping_selname-p_wostca
                    fieldname = zif_whse_order=>wo_mapping_fieldname-wostca
                  ) TO rt_mapping.

    APPEND VALUE #( tablename = zif_whse_order=>wo_mapping_prop-tablename
                selname   = zif_whse_order=>wo_mapping_selname-p_wostho
                fieldname = zif_whse_order=>wo_mapping_fieldname-wostho
              ) TO rt_mapping.


    APPEND VALUE #( tablename = zif_whse_order=>wo_mapping_prop-tablename
                selname   = zif_whse_order=>wo_mapping_selname-p_wostco
                fieldname = zif_whse_order=>wo_mapping_fieldname-wostco
              ) TO rt_mapping.

    APPEND VALUE #( tablename = zif_whse_order=>wo_mapping_prop-tablename
                selname   = zif_whse_order=>wo_mapping_selname-p_wostpr
                fieldname = zif_whse_order=>wo_mapping_fieldname-wostpr
              ) TO rt_mapping.

    APPEND VALUE #( tablename = zif_whse_order=>wo_mapping_prop-tablename
                selname   = zif_whse_order=>wo_mapping_selname-s_aawho
                fieldname = zif_whse_order=>wo_mapping_fieldname-areawho
              ) TO rt_mapping.

    APPEND VALUE #( tablename = zif_whse_order=>wo_mapping_prop-tablename
                selname   = zif_whse_order=>wo_mapping_selname-s_proc
                fieldname = zif_whse_order=>wo_mapping_fieldname-processor
              ) TO rt_mapping.

    APPEND VALUE #( tablename = zif_whse_order=>wo_mapping_prop-tablename
                selname   = zif_whse_order=>wo_mapping_selname-s_wostdt
                fieldname = zif_whse_order=>wo_mapping_fieldname-started_at
              ) TO rt_mapping.

    APPEND VALUE #( tablename = zif_whse_order=>wo_mapping_prop-tablename
                selname   = zif_whse_order=>wo_mapping_selname-s_wolsdt
                fieldname = zif_whse_order=>wo_mapping_fieldname-lsd
              ) TO rt_mapping.

    APPEND VALUE #( tablename = zif_whse_order=>wo_mapping_prop-tablename
                selname   = zif_whse_order=>wo_mapping_selname-s_wocrdt
                fieldname = zif_whse_order=>wo_mapping_fieldname-created_at
                is_timestamp = zif_whse_order=>wo_mapping_prop-is_timestamp
                p_date_from  = zif_whse_order=>wo_mapping_prop-date_from
                p_time_from  = zif_whse_order=>wo_mapping_prop-time_from
                p_date_to    = zif_whse_order=>wo_mapping_prop-date_to
                p_time_to    = zif_whse_order=>wo_mapping_prop-time_to
              ) TO rt_mapping.

  ENDMETHOD.


  METHOD fill_wo_status.
********************************************************************
*& Key          : <TBIRIHAN>-January 17, 2024
*& Request No.  : GAP-063
********************************************************************
*& Description  :
********************************************************************
    IF iv_wo_status_open EQ abap_true .
      APPEND VALUE #( sign   = wmegc_sign_inclusive
                      option = wmegc_option_eq
                      low    = wmegc_to_open ) TO rt_status.
    ENDIF.

    IF iv_wo_status_hold EQ abap_true.
      APPEND VALUE #( sign   = wmegc_sign_inclusive
                      option = wmegc_option_eq
                      low    = wmegc_to_inactiv ) TO rt_status.
    ENDIF.

    IF iv_wo_status_cancel EQ abap_true.
      APPEND VALUE #( sign   = wmegc_sign_inclusive
                      option = wmegc_option_eq
                      low    = wmegc_to_canceled ) TO rt_status.
    ENDIF.

    IF iv_wo_status_confirm EQ abap_true.
      APPEND VALUE #( sign   = wmegc_sign_inclusive
                      option = wmegc_option_eq
                      low    = wmegc_to_confirmed ) TO rt_status.
    ENDIF.
    IF iv_wo_status_processing EQ abap_true.
      APPEND VALUE #( sign   = wmegc_sign_inclusive
                      option = wmegc_option_eq
                      low    = wmegc_wo_in_process ) TO rt_status.
    ENDIF.

  ENDMETHOD.


  METHOD generate_dynamic_where.
********************************************************************
*& Key          : <TBIRIHAN>-January 17, 2024
*& Request No.  : GAP-063
********************************************************************
*& Description  :
********************************************************************
    TYPES tt_named_seltables TYPE if_shdb_def=>tt_named_dref .
    DATA: lt_name  TYPE tt_named_seltables,
          lr_lgnum TYPE ztt_select_option_fields.

    APPEND VALUE zstr_select_option_fields( sign   = wmegc_sign_inclusive
                                            option = wmegc_option_eq
                                            low    = me->mv_lgnum ) TO lr_lgnum.

    DATA(lr_status) =
      me->fill_wo_status(
      EXPORTING
        iv_wo_status_open       = VALUE #( it_selection_parameters[ field = zif_whse_order=>wo_mapping_fieldname-wostop ]-select_options[ 1 ]-low OPTIONAL )
        iv_wo_status_cancel     = VALUE #( it_selection_parameters[ field = zif_whse_order=>wo_mapping_fieldname-wostca ]-select_options[ 1 ]-low OPTIONAL )
        iv_wo_status_hold       = VALUE #( it_selection_parameters[ field = zif_whse_order=>wo_mapping_fieldname-wostho ]-select_options[ 1 ]-low OPTIONAL )
        iv_wo_status_confirm    = VALUE #( it_selection_parameters[ field = zif_whse_order=>wo_mapping_fieldname-wostco ]-select_options[ 1 ]-low OPTIONAL )
        iv_wo_status_processing = VALUE #( it_selection_parameters[ field = zif_whse_order=>wo_mapping_fieldname-wostpr ]-select_options[ 1 ]-low OPTIONAL )
    ).

    DATA(lt_selection_parameters) = VALUE ztt_select_option( ( field = zif_whse_order=>wo_mapping_fieldname-lgnum
                                                               select_options = lr_lgnum )
                                                             ( field = zif_whse_order=>wo_mapping_fieldname-status
                                                               select_options = lr_status ) ).

    lt_selection_parameters = VALUE ztt_select_option( BASE lt_selection_parameters FOR ls_parameters IN it_selection_parameters
                                                         WHERE ( field NE zif_whse_order=>wo_mapping_fieldname-wostop
                                                            AND  field NE zif_whse_order=>wo_mapping_fieldname-wostca
                                                            AND  field NE zif_whse_order=>wo_mapping_fieldname-wostho
                                                            AND  field NE zif_whse_order=>wo_mapping_fieldname-wostco
                                                            AND  field NE zif_whse_order=>wo_mapping_fieldname-wostpr )
                                                               ( field = ls_parameters-field
                                                                 select_options =  ls_parameters-select_options ) ).



    LOOP AT lt_selection_parameters ASSIGNING FIELD-SYMBOL(<ls_selection_parameters>).
      APPEND VALUE #( name = <ls_selection_parameters>-field
                      dref = REF #( <ls_selection_parameters>-select_options ) ) TO lt_name.
    ENDLOOP.
    mv_where =  me->combine_seltabs( it_named_seltabs = lt_name ).

  ENDMETHOD.


  METHOD raise_exception_from_sy.
********************************************************************
*& Key          : <TBIRIHAN>-January 17, 2024
*& Request No.  : GAP-063
********************************************************************
*& Description  :
********************************************************************
    DATA: ls_t100key TYPE scx_t100key.

    ls_t100key-msgid = sy-msgid.
    ls_t100key-msgno = sy-msgno.
    ls_t100key-attr1 = zif_whse_order=>wo_attribute-attr1.
    ls_t100key-attr2 = zif_whse_order=>wo_attribute-attr2.
    ls_t100key-attr3 = zif_whse_order=>wo_attribute-attr3.
    ls_t100key-attr3 = zif_whse_order=>wo_attribute-attr4.

    RAISE EXCEPTION TYPE zcx_whse_order
      EXPORTING
        textid   = ls_t100key
        mv_msgv1 = sy-msgv1
        mv_msgv2 = sy-msgv2
        mv_msgv3 = sy-msgv3
        mv_msgv4 = sy-msgv4.
  ENDMETHOD.


  METHOD zif_whse_order~to_data_select.
********************************************************************
*& Key          : <TBIRIHAN>-January 17, 2024
*& Request No.  : GAP-063
********************************************************************
*& Description  :
********************************************************************
    TYPES tt_named_seltables TYPE if_shdb_def=>tt_named_dref .
    DATA: lt_name  TYPE tt_named_seltables.
    DATA: lr_lgnum TYPE ztt_select_option_fields.
    DATA: lr_who   TYPE ztt_select_option_fields.
    DATA: lt_who TYPE /scwm/tt_who.

    CLEAR: et_to.

    APPEND VALUE zstr_select_option_fields( sign   = wmegc_sign_inclusive
                                            option = wmegc_option_eq
                                            low    = me->mv_lgnum ) TO lr_lgnum.

    DATA(lr_status) =
      me->fill_wo_status(
      EXPORTING
        iv_wo_status_open       = abap_true
        iv_wo_status_cancel     = abap_true
        iv_wo_status_hold       = abap_true
        iv_wo_status_confirm    = abap_true
        iv_wo_status_processing = abap_false
    ).

    lr_who = VALUE ztt_select_option_fields( FOR ls_who IN it_who
                                                 ( sign   = wmegc_sign_inclusive
                                                   option = wmegc_option_eq
                                                   low    = ls_who-who ) ).

    DATA(lt_selection_parameters) = VALUE ztt_select_option( ( field = zif_whse_order=>to_mapping_fieldname-lgnum
                                                               select_options = lr_lgnum )
                                                             ( field = zif_whse_order=>to_mapping_fieldname-who
                                                               select_options = lr_who )
                                                             ( field = zif_whse_order=>to_mapping_fieldname-status
                                                               select_options = lr_status )  ).


    LOOP AT lt_selection_parameters ASSIGNING FIELD-SYMBOL(<ls_selection_parameters>).
      APPEND VALUE #( name = <ls_selection_parameters>-field
                      dref = REF #( <ls_selection_parameters>-select_options ) ) TO lt_name.
    ENDLOOP.
    DATA(lv_where) =  me->combine_seltabs( it_named_seltabs = lt_name ).

    SELECT  * FROM /scwm/who AS wo
       INNER JOIN /scwm/ordim_o AS to
       ON to~lgnum = wo~lgnum AND
          to~who   = wo~who
       INTO CORRESPONDING FIELDS OF TABLE lt_who
       WHERE (lv_where).
    IF sy-subrc = 0.
      LOOP AT lt_who ASSIGNING FIELD-SYMBOL(<ls_who>).
        APPEND INITIAL LINE TO et_to ASSIGNING FIELD-SYMBOL(<ls_to_new>).

        MOVE-CORRESPONDING <ls_who> TO <ls_to_new>.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD zif_whse_order~update_queue.
********************************************************************
*& Key          : <aahmedov>-130623
*& Request No.  : GAP 17 - Picking WO Bundling
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA:
      ##NEEDED
      lv_msg         TYPE string,
      lt_who         TYPE /scwm/tt_who_int,
      ls_ordim_o     TYPE /scwm/ordim_o,
      lt_ordim_o     TYPE /scwm/tt_ordim_o,
      ls_ordim_o_int TYPE /scwm/s_ordim_o_int,
      lt_ordim_o_int TYPE /scwm/tt_ordim_o_int,
      lt_wo_rsrc_ty  TYPE /scwm/tt_wo_rsrc_ty,
      ls_wo_rsrc_ty  TYPE /scwm/wo_rsrc_ty,
      ls_attributes  TYPE /scwm/s_who_att,
      ls_t346        TYPE /scwm/t346,
      lv_content     TYPE /scwm/de_content,
      et_wo_rsrc_ty  TYPE /scwm/tt_wo_rsrc_ty,
      lv_subrc       TYPE sy-subrc.

    FIELD-SYMBOLS: <who> TYPE /scwm/s_who_int.

    lt_who = it_who.

    LOOP AT lt_who ASSIGNING <who>.
      ls_wo_rsrc_ty-who = <who>-who.
      APPEND ls_wo_rsrc_ty TO lt_wo_rsrc_ty.
    ENDLOOP.
    " Get index records for all WOs
    CALL FUNCTION '/SCWM/RSRC_WHO_RSTYP_GET'
      EXPORTING
        iv_lgnum      = mv_lgnum
        iv_rfind      = abap_true
      CHANGING
        ct_wo_rsrc_ty = lt_wo_rsrc_ty.

    LOOP AT lt_who ASSIGNING <who>.
      " Don't create index records for Super-WOs of category Load/Unload
      IF ( <who>-type = wmegc_wcr_lu AND <who>-flgwho = abap_true ).
        CONTINUE.
      ENDIF.

      CALL FUNCTION '/SCWM/T346_READ_SINGLE'
        EXPORTING
          iv_lgnum  = mv_lgnum
          iv_queue  = <who>-queue
        IMPORTING
          es_t346   = ls_t346
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF sy-subrc <> 0.
        et_bapiret = VALUE #( BASE et_bapiret
            ( id = sy-msgid type = sy-msgty number = sy-msgno
              message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 message_v4 = sy-msgv4 ) ).
        RETURN.
      ENDIF.

      CLEAR: ls_ordim_o_int,
             lt_ordim_o_int,
             lt_ordim_o,
             et_wo_rsrc_ty.
      " Read WO to get open WTs

      lt_ordim_o = VALUE #( FOR <ordim_o> IN it_ordim_o
                                WHERE ( who = <who>-who )
                                ( CORRESPONDING #( <ordim_o> ) ) ).

      " For the queue change of Load/unload subWO the topWO's WT must be collected
      IF <who>-type = wmegc_wcr_lu AND <who>-topwhoid IS NOT INITIAL.
        TRY.
            CALL FUNCTION '/SCWM/WHO_SELECT'
              EXPORTING
                iv_to      = abap_true
                iv_lgnum   = mv_lgnum
                iv_who     = <who>-topwhoid
              IMPORTING
                et_ordim_o = lt_ordim_o.
          CATCH /scwm/cx_core.
            et_bapiret = VALUE #( BASE et_bapiret
                ( id = sy-msgid type = sy-msgty number = sy-msgno
                  message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 message_v4 = sy-msgv4 ) ).

            CLEAR lt_ordim_o.
        ENDTRY.
      ENDIF.

      READ TABLE lt_wo_rsrc_ty INTO ls_wo_rsrc_ty
        WITH KEY who = <who>-who.

      IF ( ( sy-subrc NE 0 ) AND
           ( ls_t346-rfrsrc = wmegc_rfrsrc_rs OR
               ls_t346-rfrsrc = wmegc_rfrsrc_rf OR
               ls_t346-rfrsrc = wmegc_rfrsrc_mfs_with_rsrc ) ).

        CLEAR lt_ordim_o_int.

        LOOP AT lt_ordim_o INTO ls_ordim_o ##INTO_OK.
          MOVE-CORRESPONDING ls_ordim_o TO ls_ordim_o_int ##ENH_OK.
          APPEND ls_ordim_o_int TO lt_ordim_o_int.
        ENDLOOP.

        IF <who>-flginv   = abap_true AND
           lt_ordim_o_int IS INITIAL.
          " get all activity types for inventory
          SELECT a~act_type INTO ls_ordim_o_int-act_type
                 UP TO 1 ROWS
                 FROM /scwm/tactty AS a
                 INNER JOIN /scwm/taareas AS b
                 ON  a~lgnum    = b~lgnum
                 AND a~act_type = b~act_type
                 WHERE a~lgnum   = mv_lgnum
                 AND   a~trart   = wmegc_trart_inv
                 AND   b~area    = <who>-areawho.      "#EC CI_BUFFJOIN
          ENDSELECT.

          ls_ordim_o_int-trart      = wmegc_trart_inv.
          ls_ordim_o_int-queue      = VALUE #( it_who[ who = <who>-who ]-queue OPTIONAL ).
          ls_ordim_o_int-lgnum      = <who>-lgnum.
          ls_ordim_o_int-aarea      = <who>-areawho.
          ls_ordim_o_int-areawho    = <who>-areawho.
          APPEND ls_ordim_o_int TO lt_ordim_o_int.
        ENDIF.
        " Determine the RF content
        CALL FUNCTION '/SCWM/WHO_DET_RF_RSRC_CONTENT'
          EXPORTING
            iv_lgnum    = mv_lgnum
            is_who      = <who>
            it_ordim_o  = lt_ordim_o_int
          IMPORTING
            ev_content  = lv_content
          EXCEPTIONS
            wrong_trart = 1
            wrong_iproc = 2
            tproc_error = 3
            OTHERS      = 4.
        IF sy-subrc <> 0.
          et_bapiret = VALUE #( BASE et_bapiret
              ( id = sy-msgid type = sy-msgty number = sy-msgno
                message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 message_v4 = sy-msgv4 ) ).

          RETURN.
        ENDIF.

        " Set queue temporary otherwise next fm works on the "old" data
        <who>-queue = VALUE #( it_who[ who = <who>-who ]-queue OPTIONAL ).

        " Create the index entries
        CALL FUNCTION '/SCWM/RSRC_WHO_CREATE'
          EXPORTING
            iv_lgnum        = mv_lgnum
            iv_content      = lv_content
            is_who          = <who>
          IMPORTING
            ev_lowest_lsd   = <who>-lsd
            et_wo_rsrc_ty   = et_wo_rsrc_ty
          CHANGING
            ct_ordim_o      = lt_ordim_o_int
          EXCEPTIONS
            no_qualif_rstyp = 1
            OTHERS          = 2.
        IF sy-subrc <> 0.
          et_bapiret = VALUE #( BASE et_bapiret
              ( id = sy-msgid type = sy-msgty number = sy-msgno
                message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 message_v4 = sy-msgv4 ) ).

          RETURN.
        ENDIF.
        IF et_wo_rsrc_ty[] IS INITIAL.
          MESSAGE s039(/scwm/rsrc) WITH <who>-who INTO lv_msg.
          et_bapiret = VALUE #( BASE et_bapiret
              ( id = sy-msgid type = sy-msgty number = sy-msgno
                message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 message_v4 = sy-msgv4 ) ).

          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " creating whoid table from who table
    LOOP AT lt_who ASSIGNING <who>.
      " Clearing for security, than setting the queue
      CLEAR: ls_attributes, lv_subrc.

      MOVE-CORRESPONDING <who> TO ls_attributes ##ENH_OK.
      ls_attributes-queue  = VALUE #( it_who[ who = <who>-who ]-queue OPTIONAL ).
      ls_attributes-lsd    = <who>-lsd.

      CALL FUNCTION '/SCWM/WHO_UPDATE'
        EXPORTING
          iv_lgnum      = mv_lgnum
          iv_db_update  = abap_true
          iv_who        = <who>-who
          iv_queue      = abap_true
          is_attributes = ls_attributes
        EXCEPTIONS
          read_error    = 1
          attributes    = 2
          OTHERS        = 3.
      IF sy-subrc <> 0.
        lv_subrc = sy-subrc.

        et_bapiret = VALUE #( BASE et_bapiret
            ( id = sy-msgid type = sy-msgty number = sy-msgno
              message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 message_v4 = sy-msgv4 ) ).
        EXIT.
      ELSE.

        DATA(lv_queue_old) = VALUE /scwm/de_queue( lt_wo_rsrc_ty[ who = <who>-who ]-queue OPTIONAL ).

        MESSAGE s014(zmc_whse_order) WITH <who>-who lv_queue_old ls_attributes-queue INTO lv_msg.

        et_bapiret = VALUE #( BASE et_bapiret
            ( id = sy-msgid type = sy-msgty number = sy-msgno
              message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 message_v4 = sy-msgv4 ) ).
      ENDIF.
    ENDLOOP.

    " If error occurs - rollback
    IF lv_subrc <> 0.
      ROLLBACK WORK.
      MESSAGE s002(/scwm/l3) INTO lv_msg.

      et_bapiret = VALUE #( BASE et_bapiret
          ( id = sy-msgid type = sy-msgty number = sy-msgno
            message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 message_v4 = sy-msgv4 ) ).
      RETURN.
    ELSE.
      " commit work and wait to display new data after auto-refresh
      COMMIT WORK AND WAIT.
    ENDIF.

    CALL FUNCTION 'DEQUEUE_ALL'.
  ENDMETHOD.


  METHOD zif_whse_order~wo_data_select.
********************************************************************
*& Key          : <TBIRIHAN>-January 17, 2024
*& Request No.  : GAP-063
********************************************************************
*& Description  :
********************************************************************
    DATA: lt_who TYPE /scwm/tt_who.

    CLEAR: et_who, et_ordim_o, et_ordim_c, et_ordim_o_all_fields.

    SELECT *
           FROM /scwm/who AS wo
           INTO TABLE lt_who
       WHERE (me->mv_where).
    IF sy-subrc <> 0.
      REFRESH et_who.
      RETURN.
    ENDIF.

    LOOP AT lt_who ASSIGNING FIELD-SYMBOL(<ls_who>).
      APPEND INITIAL LINE TO et_who ASSIGNING FIELD-SYMBOL(<ls_new_who>).
      MOVE-CORRESPONDING <ls_who> TO <ls_new_who>.
    ENDLOOP.

    IF et_ordim_o IS SUPPLIED.
      SELECT o~who,
           COUNT( * ) AS count_to,
           SUM( weight ) AS sum_weight,
           o~unit_w AS unit_w,
           SUM( o~volum ) AS sum_volum,
           o~unit_v AS unit_v,
           SUM( o~solpo ) AS sum_reachtime,
           o~zeiei AS unit_rt,
           MIN( o~pick_comp_dt ) AS pick_comp_dt,
           w~zz_bundled
           FROM /scwm/ordim_o AS o
           INNER JOIN @et_who  AS w ON o~who EQ w~who ##ITAB_KEY_IN_SELECT
           WHERE o~lgnum = @me->mv_lgnum
           GROUP BY o~who, o~unit_w, o~unit_v, o~zeiei,  w~zz_bundled
           APPENDING TABLE @et_ordim_o.
      SORT et_ordim_o BY who.
    ENDIF.

    IF et_ordim_c IS SUPPLIED.
      SELECT toc~who,
             COUNT( * ) AS count_to,
             SUM( toc~weight ) AS sum_weight,
             toc~unit_w AS unit_w,
             SUM( toc~volum ) AS sum_volum,
             toc~unit_v AS unit_v,
             SUM( toc~solpo ) AS sum_reachtime,
             toc~zeiei AS unit_rt,
             MIN( tol~pick_comp_dt ) AS pick_comp_dt,
             w~zz_bundled
             FROM /scwm/ordim_c AS toc
             INNER JOIN /scwm/ordim_l AS tol ON tol~lgnum = toc~lgnum ##ITAB_KEY_IN_SELECT
                                            AND tol~tanum = toc~tanum
             INNER JOIN @et_who  AS w ON toc~who EQ w~who
             WHERE toc~lgnum = @me->mv_lgnum
             GROUP BY toc~who, toc~unit_w, toc~unit_v, toc~zeiei, w~zz_bundled
             APPENDING TABLE @et_ordim_c.
      SORT et_ordim_c BY who.
    ENDIF.

    IF et_ordim_o_all_fields IS SUPPLIED.
      SELECT o~*
       FROM /scwm/ordim_o AS o
       INNER JOIN @et_who AS w ON o~who EQ w~who ##ITAB_KEY_IN_SELECT
       WHERE o~lgnum = @me->mv_lgnum
       APPENDING TABLE @et_ordim_o_all_fields.
      SORT et_ordim_o_all_fields BY who.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
