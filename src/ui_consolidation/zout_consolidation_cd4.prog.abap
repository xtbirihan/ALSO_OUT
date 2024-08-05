*----------------------------------------------------------------------*
***INCLUDE ZOUT_CONSOLIDATION_CD4.
**********************************************************************
*& Key           : AMOGYORODI-230814
*& Request No.   :
**********************************************************************
*& Description (short)
*&
*&
**********************************************************************
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class lcl_event_handler
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      on_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column,
      on_user_command FOR EVENT added_function OF cl_salv_events
          IMPORTING !e_salv_function .
ENDCLASS.
