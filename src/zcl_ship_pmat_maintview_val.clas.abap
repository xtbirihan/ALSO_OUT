class ZCL_SHIP_PMAT_MAINTVIEW_VAL definition
  public
  final
  create public .

public section.

  methods VALIDATE_INPUT
    importing
      !IS_DATA type ZTOUT_SHIP_PMAT
      !IT_TOTAL type ANY TABLE .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SHIP_PMAT_MAINTVIEW_VAL IMPLEMENTATION.


  METHOD VALIDATE_INPUT.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    TYPES: BEGIN OF ty_total,
             action TYPE xfeld.
             INCLUDE TYPE zmv_ship_pmat.
    TYPES: END OF ty_total.

    DATA: lt_totals TYPE STANDARD TABLE OF ty_total WITH EMPTY KEY.

    IF is_data IS NOT INITIAL AND is_data-lgnum IS INITIAL.
      MESSAGE e092(/scwm/monitor).
    ENDIF.

    lt_totals = VALUE #( FOR <l> IN it_total ( CORRESPONDING #( <l> ) ) ).

    DELETE lt_totals WHERE action = 'D'.

    DATA(lv_nr_clients) = 0.

    IF is_data IS NOT INITIAL.
      IF is_data-carrier IS NOT INITIAL.
        lv_nr_clients += 1.
      ENDIF.

      IF is_data-ship_to IS NOT INITIAL.
        lv_nr_clients += 1.
      ENDIF.

      IF is_data-sold_to IS NOT INITIAL.
        lv_nr_clients += 1.
      ENDIF.

      IF lv_nr_clients <> 1.
        MESSAGE e001(zmc_out).
      ENDIF.

      LOOP AT lt_totals ASSIGNING FIELD-SYMBOL(<ls_exist>) WHERE lgnum = is_data-lgnum
                                                             AND carrier = is_data-carrier
                                                             AND ship_to = is_data-ship_to
                                                             AND sold_to = is_data-sold_to.
        IF <ls_exist>-inc_excl <> is_data-inc_excl.
          MESSAGE e002(zmc_out).
        ENDIF.
      ENDLOOP.

      RETURN.
    ENDIF.

    LOOP AT lt_totals ASSIGNING FIELD-SYMBOL(<ls_dummy>) GROUP BY ( lgnum   = <ls_dummy>-lgnum
                                                                    carrier = <ls_dummy>-carrier
                                                                    ship_to = <ls_dummy>-ship_to
                                                                    sold_to = <ls_dummy>-sold_to )
                      ASSIGNING FIELD-SYMBOL(<ls_group>).
      DATA(lv_counter) = 0.

      LOOP AT GROUP <ls_group> ASSIGNING FIELD-SYMBOL(<ls_line>).
        lv_counter += 1.

        IF lv_counter = 1.
          DATA(ls_base) = <ls_line>.
          CONTINUE.
        ENDIF.

        IF ls_base-inc_excl <> <ls_line>-inc_excl.
          MESSAGE e002(zmc_out).
        ENDIF.
      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
