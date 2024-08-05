"Name: \PR:/SCWM/SAPLL03A\FO:ROUTING_CREATE\SE:BEGIN\EI
ENHANCEMENT 0 ZIE_ROUTING_CREATE.
*
    BREAK-POINT ID zcg_ex_layout_routing_cr_pick.

    IF abap_true = zcl_switch=>get_switch_state(
                      iv_lgnum = is_ltap-lgnum
                      iv_devid = zif_switch_const=>c_zout_019
                      it_fields = VALUE #( ( field = zif_switch_const=>c_ltrans
                                             field_value = /scwm/cl_rf_bll_srvc=>get_ltrans( ) ) ) ).

      PERFORM routing_create_cust USING    is_ltap
                                  CHANGING et_ltap_vb.
      RETURN.
    ENDIF.

ENDENHANCEMENT.
