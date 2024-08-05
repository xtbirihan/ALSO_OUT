**********************************************************************
*& Key           : AMOGYORODI-230814
*& Request No.   :
**********************************************************************
*& Description (short)
*&
*&
**********************************************************************
REPORT zout_consolidation.

INCLUDE:
  zout_consolidation_top,
  zout_consolidation_sel,
  zout_consolidation_cd1,
  zout_consolidation_cd2,
  zout_consolidation_cd3,
  zout_consolidation_cd4,
  zout_consolidation_ci1,
  zout_consolidation_ci2,
  zout_consolidation_ci3,
  zout_consolidation_ci4,
  zout_consolidation_o01,
  zout_consolidation_o02,
  zout_consolidation_o03,
  zout_consolidation_o04,
  zout_consolidation_o05,
  zout_consolidation_o06,
  zout_consolidation_i01,
  zout_consolidation_i02,
  zout_consolidation_i03,
  zout_consolidation_i04,
  zout_consolidation_i05,
  zout_consolidation_i06.

INITIALIZATION.
  CLEAR: lcl_model=>so_instance, lcl_view=>so_instance,
         lcl_controller=>so_instance, lcl_controller=>sv_lgnum.

  DATA: lv_terminal TYPE /scwm/de_wc_terminal.
  CALL FUNCTION 'TH_USER_INFO'
    EXPORTING
      client   = sy-mandt
      user     = sy-uname
    IMPORTING
      terminal = lv_terminal.
  IF lv_terminal IS NOT INITIAL.
    SELECT SINGLE * INTO @DATA(ls_term_def) FROM zout_term_def
      WHERE terminal = @lv_terminal.
    IF sy-subrc = 0.
      p_lgnum = ls_term_def-lgnum.
      p_works = ls_term_def-workstation.
      p_lost  = ls_term_def-lostnfound_stbin.
      p_prin  = ls_term_def-printer.
    ENDIF.
  ENDIF.

START-OF-SELECTION.

  lcl_controller=>get_instance( )->selection( iv_lgnum    = p_lgnum
                                              is_term_def = VALUE #( lgnum            = p_lgnum
                                                                     workstation      = p_works
                                                                     lostnfound_stbin = p_lost
                                                                     printer          = p_prin ) ).

  CALL SCREEN 0100.
