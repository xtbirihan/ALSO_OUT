class ZCL_CRUD_ZOUT_HU_CONT definition
  public
  final
  create public .

public section.

  types:
    tt_hu_content TYPE STANDARD TABLE OF zout_hu_cont WITH EMPTY KEY .

  class-methods INSERT_HU_CONTENT
    importing
      !IT_HU_CONTENT type TT_HU_CONTENT .
  class-methods SELECT_HU_CONTENT
    importing
      !IV_GUID_HU type /SCWM/GUID_HU
    returning
      value(RT_HU_CONTENT) type TT_HU_CONTENT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZOUT_HU_CONT IMPLEMENTATION.


  METHOD insert_hu_content.
    INSERT zout_hu_cont FROM TABLE it_hu_content.
    COMMIT WORK.
  ENDMETHOD.


  METHOD select_hu_content.
    SELECT * FROM zout_hu_cont INTO TABLE rt_hu_content
             WHERE guid_hu EQ iv_guid_hu.
  ENDMETHOD.
ENDCLASS.
