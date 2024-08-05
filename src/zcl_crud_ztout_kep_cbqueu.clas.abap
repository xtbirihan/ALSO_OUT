class ZCL_CRUD_ZTOUT_KEP_CBQUEU definition
  public
  final
  create public .

public section.

  class-methods SELECT_SINGLE_BY_QUEUE
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_QUEUE type /SCWM/DE_QUEUE
    exporting
      !ES_CONV_BEL_QUEUEU type ZTOUT_KEP_CBQUEU .
protected section.
private section.

  class-data SS_OUT_KEP_CBQUEU type ZTOUT_KEP_CBQUEU .
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_KEP_CBQUEU IMPLEMENTATION.


  METHOD select_single_by_queue.
********************************************************************
*& Key          : <AYORDANOV> 25.08.2023
*& Request No.  :
********************************************************************
*& Description
*& REad cust table ztout_kep_cbqueu
********************************************************************

    CLEAR es_conv_bel_queueu.

    IF ss_out_kep_cbqueu-lgnum = iv_lgnum AND
       ss_out_kep_cbqueu-queue = iv_queue.
      es_conv_bel_queueu = ss_out_kep_cbqueu.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM ztout_kep_cbqueu
      WHERE lgnum = @iv_lgnum AND
            queue = @iv_queue
      INTO @DATA(ls_kep_queue_conv).

    IF sy-subrc <> 0.
      RETURN.
    ELSE.
      ss_out_kep_cbqueu = ls_kep_queue_conv.
    ENDIF.

    es_conv_bel_queueu = ls_kep_queue_conv.

  ENDMETHOD.
ENDCLASS.
