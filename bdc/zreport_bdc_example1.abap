*&---------------------------------------------------------------------*
*& Report ZREPORT_BDC_EXAMPLE1
*&
*&---------------------------------------------------------------------*
*& SAP-ABAP-Utilities/bdc
*& Batch Input example
*&
*& Bruno Neuman
*& http://www.linkedin.com/in/bruno-n-a51213a3/
*&
*&---------------------------------------------------------------------*

REPORT zreport_bdc_example1.

DATA o_bdc TYPE REF TO zcl_abap_util_bdc.

* Mandatory macro to call method "APPEND" like old version with perform
DEFINE append_bdc.

  CALL METHOD o_bdc->append
    EXPORTING
      im_dbg = &1
      im_nam = &2
      im_val = &3.

END-OF-DEFINITION.

SELECTION-SCREEN BEGIN OF BLOCK b1.
  PARAMETERS:
    p_belnr TYPE belnr_d,
    p_bukrs TYPE bukrs,
    p_gjahr TYPE gjahr.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

* Create the instance of object BDC
  CREATE OBJECT o_bdc
    EXPORTING
      im_tcode   = 'FB03'
    EXCEPTIONS
      invalid_tcode = 1
      OTHERS        = 2.

* Invalid is raised when TCODE isn't exits
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

* BDC data
  append_bdc:
    'X'  'SAPMF05L'     '0100',
    ' '  'RF05L-BELNR'  p_belnr,
    ' '  'RF05L-BUKRS'  p_bukrs,
    ' '  'RF05L-GJAHR'  p_gjahr,
    ' '  'BDC_OKCODE'   '/00'.

* Call Transaction
  CALL METHOD o_bdc->execute
    EXCEPTIONS
      failed = 1.

END-OF-SELECTION.