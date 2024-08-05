CLASS zcx_whse_order DEFINITION
  PUBLIC
  INHERITING FROM cx_no_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    DATA mv_msgv1 TYPE symsgv.
    DATA mv_msgv2 TYPE symsgv.
    DATA mv_msgv3 TYPE symsgv.
    DATA mv_msgv4 TYPE symsgv.


    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !mv_msgv1 TYPE symsgv   OPTIONAL
        !mv_msgv2 TYPE symsgv   OPTIONAL
        !mv_msgv3 TYPE symsgv   OPTIONAL
        !mv_msgv4 TYPE symsgv   OPTIONAL .
protected section.
private section.
ENDCLASS.



CLASS ZCX_WHSE_ORDER IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MV_MSGV1 = MV_MSGV1 .
me->MV_MSGV2 = MV_MSGV2 .
me->MV_MSGV3 = MV_MSGV3 .
me->MV_MSGV4 = MV_MSGV4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
