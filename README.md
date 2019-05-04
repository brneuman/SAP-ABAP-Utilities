# SAP-ABAP-Utilities
My SAP ABAP Utility objects repository.

## BDC Object

Usability of class [<ZCL_ABAP_UTIL_BDC>](/bdc/src/zcl_abap_util_bdc.abap) is demonstrated in report program [<ZREPORT_BDC_EXAMPLE1>](/bdc/zreport_bdc_example1.abap).

### Advantages:
- Validation of Transaction Code at runtime;
- No declaration needed of variables used to perform the Call Transaction;
- No declaration needed of local variables on dynamic screen-element name;
- Fast return message search.

#### Tips

To use the *APPEND* method like the old version as a *PERFORM*, and to turn your code more readable, just include the macro below in your code:

OBS: Macro usage does not allow you to call methods that return value at importing parameters, like *IM_NAM* or *IM_VAL*. Use the conventional *APPEND* method call to do it with this approach.

##### Macro Declaration:
```
DEFINE m_append.

  CALL METHOD <your_bdc_object_name>->append
    EXPORTING
      im_dbg = &1
      im_nam = &2
      im_val = &3.

END-OF-DEFINITION.
```

##### Macro Utilization example:
```
"Basic FB03 BDC
m_append:
  'X'  'SAPMF05L'     '0100'
  ' '  'RF05L-BELNR'  '<document_number>'
  ' '  'RF05L-BUKRS'  '<company_code>'
  ' '  'RF05L-GJAHR'  '<year>'
  ' '  'BDC_OKCODE'   '/00'
```

##### Dynamic parameter values with return method data example:
```
<your_bdc_object_name>->append( 
  im_dbg = 'X' 
  im_nam = <local_object>->get_screen_name( ) 
  im_val = <local_object>->get_value( )
).
```

##### String templates in ABAP 7.40 example:
```
<your_bdc_object_name>->append( 
  im_dbg = 'X' 
  im_nam = |<SCREEN_NAME>{ lv_screen_number }|
  im_val = |<VALUE>{ lv_complement }|
).
```

## TVARV Object

Usability of class [<ZCL_ABAP_UTIL_TVARV>](/tvarv/src/zcl_abap_util_tvarv) is demonstrated in report program [<ZREPORT_TVARV_EXAMPLE1>](/tvarv/zreport_tvarv_example1.abap).

### Advantages:
- Validation of variant name at runtime;
- No declaration needed for variant data container;
- No declaration needed for simple parameters check;
- Multiple parameters selection with a separator
- Expression on IF statement;
- Clean code.

##### Add new variant example:
```
"Add new variants to our TVARV Object
DATA lo_tvarv TYPE REF TO zcl_abap_util_tvarv.

CREATE OBJECT lo_tvarv.
lo_tvarv->add_variant( im_name = 'TVARV_PARAMETER_NAME_1' ).

"Handling exceptions
lo_tvarv->add_variant(
  EXPORTING
    im_name = 'TVARV_PARAMETER_NAME_2'
  EXCEPTIONS
    inconsistent = 1
    not_found    = 2
).
IF sy-subrc <> 0.
  "Workaround...
ENDIF.
```
