CLASS zcl_crud_zout_term_def DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_zout_term_def TYPE STANDARD TABLE OF zout_term_def WITH EMPTY KEY .

    CLASS-METHODS select_single_by_key
      IMPORTING
        !iv_terminal       TYPE /scwm/de_wc_terminal
      RETURNING
        VALUE(rs_terminal) TYPE zout_term_def .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CRUD_ZOUT_TERM_DEF IMPLEMENTATION.


  METHOD select_single_by_key.
********************************************************************
*& Key          : <BSUGAREV>-Nov 21, 2023
*& Request No.  :
********************************************************************
*& Description  :
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE * FROM zout_term_def
      WHERE terminal = @iv_terminal
       INTO @rs_terminal.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'zout_term_def' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
