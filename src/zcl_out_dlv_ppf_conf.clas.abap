CLASS zcl_out_dlv_ppf_conf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES /scwm/if_ex_dlv_ppf_conf.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_bsark TYPE /sapcnd/fieldname VALUE 'ZZBSARK'.
    CONSTANTS c_nali TYPE /sapcnd/fieldname VALUE 'ZZNALI'.
    CONSTANTS c_augru TYPE /sapcnd/fieldname VALUE 'ZZAUGRU'.
    CONSTANTS c_dti TYPE /sapcnd/fieldname VALUE 'ZZDTI'.
    CONSTANTS c_logcon TYPE /sapcnd/fieldname VALUE 'ZZLOGCON'.
ENDCLASS.



CLASS ZCL_OUT_DLV_PPF_CONF IMPLEMENTATION.


  METHOD /scwm/if_ex_dlv_ppf_conf~add_catalog_fields.
    DATA lv_data_loaded TYPE bool.
    DATA lv_value TYPE /sapcnd/det_value.

    DATA(lt_field_range) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = c_bsark )
                                               ( sign = 'I' option = 'EQ' low = c_nali )
                                               ( sign = 'I' option = 'EQ' low = c_augru )
                                               ( sign = 'I' option = 'EQ' low = c_dti )
                                               ( sign = 'I' option = 'EQ' low = c_logcon ) ).


    BREAK-POINT ID zcg_dlv_ppf_conf.

    " Loop at all requested fields
    LOOP  AT it_headerfields ASSIGNING FIELD-SYMBOL(<lv_headerfield>).

      IF line_exists( ct_request[ 1 ]-header_attributes[ fieldname  = <lv_headerfield> ] ).
        " The Standard logic allready determined a value for the field
        CONTINUE.
      ENDIF.

      IF <lv_headerfield> NOT IN lt_field_range.
        " Currently processed field is not relevant for custom data determination
        CONTINUE.
      ENDIF.

      " Load delivery data from DB only once.
      IF lv_data_loaded = abap_false.

        DATA(go_inb_delivery) = /scwm/cl_dlv_management_prd=>get_instance( ).

        DATA(lt_selection) = VALUE /scwm/dlv_selection_tab( ( fieldname = /scdl/if_dl_logfname_c=>sc_docid_h
                                                                   sign = 'I'
                                                                 option  = 'EQ'
                                                                    low = iv_docid )  ).

        " Set up the read options
        DATA(ls_read_options) = VALUE /scwm/dlv_query_contr_str(  ).
        ls_read_options-data_retrival_only = abap_true. " We only want to read data.
        ls_read_options-mix_in_object_instances = /scwm/if_dl_c=>sc_mix_in_load_instance.

        " Execute Query
        TRY.
            go_inb_delivery->query(
                EXPORTING iv_doccat = /scdl/if_dl_c=>sc_doccat_out_prd
                          it_selection = lt_selection
                          is_read_options = ls_read_options
                IMPORTING et_headers = DATA(lt_headers) ).

            " Set flag after successfully loading the delivery data.
            " (This avodis unnecessary db reads)
            lv_data_loaded = abap_true.
          CATCH /scdl/cx_delivery INTO DATA(lx_delivery).
            CONTINUE.
        ENDTRY.

      ENDIF.

      " Fill field
      CLEAR lv_value.
      CASE <lv_headerfield>.
        WHEN c_bsark.
          lv_value = VALUE #(  lt_headers[ 1 ]-eew-zzbsark OPTIONAL ).
        WHEN c_nali.
          lv_value = VALUE #( lt_headers[ 1 ]-eew-zznali OPTIONAL ).
        WHEN c_augru.
          lv_value = VALUE #( lt_headers[ 1 ]-eew-zzaugru OPTIONAL ).
        WHEN c_dti.
          lv_value = VALUE #( lt_headers[ 1 ]-eew-zzdti OPTIONAL ).
        WHEN c_logcon.
          lv_value = VALUE #( lt_headers[ 1 ]-eew-zzlogcon OPTIONAL ).
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

      ct_request[ 1 ]-header_attributes = VALUE #( BASE ct_request[ 1 ]-header_attributes ( fieldname = <lv_headerfield> value = lv_value ) ).

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
