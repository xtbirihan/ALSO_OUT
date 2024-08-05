CLASS zcl_out_dlv_ppf_add_pdf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_ex_exec_methodcall_ppf.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPE-POOLS sppf.
ENDCLASS.



CLASS ZCL_OUT_DLV_PPF_ADD_PDF IMPLEMENTATION.


  METHOD if_ex_exec_methodcall_ppf~execute.
**********************************************************************
*& Key           : AD-230717
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Exceutes the PPF Action to print additional, static, PDF Documents
*& during the packing process.
**********************************************************************
    DATA lo_delivery_ppf TYPE REF TO /scdl/cl_dlv_ppf.

    DATA ls_document_key TYPE /scdl/mp_dockey_str.

    DATA lv_filename TYPE c LENGTH 1024.
    DATA lv_output_device TYPE rspopname.

    " Down- Cast Object to delivery ppf class.
    lo_delivery_ppf ?= io_appl_object.

    TRY.
        " Get key information from delivery
        ls_document_key-docid = lo_delivery_ppf->get_docid(  ).
        ls_document_key-doccat = lo_delivery_ppf->get_doccat(  ).

        " ToDo: Determine filename and output device.


        DATA(lo_print_pdf) = NEW zcl_core_print_pdf( iv_fname = lv_filename
                                                      iv_prnds = lv_output_device ).
        lo_print_pdf->main(  ).

        " Set Action status to successful.
        rp_status = sppf_status_processed.
      CATCH cx_os_object_not_found.                     "#EC NO_HANDLER
        " Set Action status to error
        rp_status = sppf_status_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
