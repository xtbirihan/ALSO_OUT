CLASS zcl_crud_ztout_wocr_pckin DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_ztout_wocr_pckin TYPE STANDARD TABLE OF ztout_wocr_pckin WITH EMPTY KEY .

    CLASS-METHODS select_multi_by_lgnum
      IMPORTING
        !iv_lgnum        TYPE /scwm/lgnum
      RETURNING
        VALUE(rt_result) TYPE tt_ztout_wocr_pckin.

    CLASS-METHODS select_single_by_key
      IMPORTING
        !iv_lgnum        TYPE /scwm/lgnum
        !iv_wocr         TYPE ztout_wocr_pckin-wocr
      RETURNING
        VALUE(rs_result) TYPE ztout_wocr_pckin.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      mv_lgnum       TYPE /scwm/lgnum,
      mt_wocr_pckind TYPE tt_ztout_wocr_pckin.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_WOCR_PCKIN IMPLEMENTATION.


  METHOD select_multi_by_lgnum.
********************************************************************
*& Key          : <BSUGAREV>-21.06.2023 09:49:20
*& Request No.  :
********************************************************************
*& Description
*&
********************************************************************
    STATICS: sv_lgnum       TYPE /scwm/lgnum,
             st_wocr_pckind TYPE tt_ztout_wocr_pckin.

    BREAK-POINT ID zcg_db_crud.

    IF sv_lgnum <> iv_lgnum.
      sv_lgnum = iv_lgnum.

      CLEAR: st_wocr_pckind.

      SELECT * FROM ztout_wocr_pckin
        INTO TABLE @st_wocr_pckind
       WHERE lgnum = @iv_lgnum.
      IF sy-subrc <> 0.
        LOG-POINT ID zcg_db_crud FIELDS 'ZTOUT_WOCR_PCKIN' sy-datum sy-uzeit.
      ENDIF.

    ENDIF.

    rt_result = st_wocr_pckind.

  ENDMETHOD.


  METHOD select_single_by_key.
********************************************************************
*& Key          : <BSUGAREV>-21.06.2023 09:49:20
*& Request No.  :
********************************************************************
*& Description
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    rs_result = VALUE #( mt_wocr_pckind[ lgnum = iv_lgnum wocr = iv_wocr ] OPTIONAL ).

    IF rs_result IS INITIAL.
      SELECT SINGLE * FROM ztout_wocr_pckin
        INTO @rs_result
       WHERE lgnum = @iv_lgnum
         AND wocr  = @iv_wocr.
      IF sy-subrc <> 0.
        LOG-POINT ID zcg_db_crud FIELDS 'ZTOUT_WOCR_PCKIN' sy-datum sy-uzeit.
        RETURN.
      ENDIF.

      APPEND rs_result TO mt_wocr_pckind.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
