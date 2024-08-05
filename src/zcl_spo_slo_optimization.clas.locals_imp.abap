*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS:
      ##NEEDED
      on_link_click  FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column,
      ##NEEDED
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.
  PRIVATE SECTION.
ENDCLASS.                    "lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.
    ##NEEDED
  METHOD on_link_click.

  ENDMETHOD.                    "on_link_click

    ##NEEDED
  METHOD on_user_command.

  ENDMETHOD.                    "on_user_command

ENDCLASS.                    "lcl_handle_events IMPLEMENTATION
