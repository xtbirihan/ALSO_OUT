interface ZIF_OUTB_WEIGHT_SCALE_INTF
  public .


  methods GET_WEIGTH_IN_KG
    importing
      !IV_WEIGHT_SCALE_IP type ZDE_WEIGHT_SCALE_IP
    returning
      value(RV_WEIGHT_IN_KG) type ZDE_SCALE_HU_WEIGHT .
endinterface.
