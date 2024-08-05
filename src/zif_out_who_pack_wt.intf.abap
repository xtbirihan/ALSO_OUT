INTERFACE zif_out_who_pack_wt
  PUBLIC .

  TYPES:
    BEGIN OF ty_bin_auom,
      bin  TYPE /scwm/de_lgpla,
      quan TYPE /scwm/s_quan-quan,
      uom  TYPE /scwm/s_quan-unit,
    END OF ty_bin_auom.

  TYPES: tt_bin_auom TYPE STANDARD TABLE OF ty_bin_auom WITH DEFAULT KEY.

  METHODS pack_tasks_in_carton
    IMPORTING
      VALUE(it_tasks)  TYPE /scwm/tt_ordim_o_int
      VALUE(is_carton) TYPE zcl_ship_pmat_algorithm_base=>ty_pmat_qty
    EXPORTING
      !et_packed       TYPE /scwm/tt_ordim_o_int
      !et_to_next_who  TYPE /scwm/tt_ordim_o_int
      !ev_pmatid       TYPE /scwm/de_pmatid
      !ev_split        TYPE sy-tabix .
  METHODS pack_tasks_in_shippmat
    IMPORTING
      VALUE(it_tasks)     TYPE /scwm/tt_ordim_o_int
      VALUE(it_shipping)  TYPE zcl_ship_pmat_algorithm_base=>tt_pmat_qty
      VALUE(it_bin_aarea) TYPE /scwm/tt_lagps OPTIONAL
    EXPORTING
      !et_packed          TYPE /scwm/tt_ordim_o_int
      !et_to_next_who     TYPE /scwm/tt_ordim_o_int
      !ev_pmatid          TYPE /scwm/de_pmatid
      !ev_split           TYPE sy-tabix .
  METHODS pack_mc_in_pallet
    IMPORTING
      !is_hucrea_param        TYPE /scwm/hum_create_attributes_s
      !it_pmats               TYPE /scwm/tt_who_pmat
      !it_cartons             TYPE /scwm/tt_ordim_o_int
    RETURNING
      VALUE(rt_pallet_packed) TYPE /scwm/tt_ordim_o_int .
  METHODS create_ship_hu
    IMPORTING
      !is_param       TYPE /scwm/hum_create_attributes_s
      !is_huhdr_crea  TYPE /scwm/s_huhdr_create_int
    RETURNING
      VALUE(rs_huhdr) TYPE /scwm/s_huhdr_int .
ENDINTERFACE.
