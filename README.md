# SAP-ABAP-Utilities
SAP ABAP Utilities codes

## BDC Object

Usability of class *ZCL_ABAP_UTIL_BDC* is demonstrated in report program *ZREPORT_BDC_EXAMPLE1*.

### Advantages:
- Validation of Transaction Code at runtime;
- No declaration needed of variables used to perform the Call Transaction;
- No declaration needed of local variables on dynamic screen-element name;
- Fast return message search.

#### Tips

To use the *APPEND* method like old version as a PERFORM like, include the macro below in your code:
OBS: Using macro isn`t allow you to call methods that returns a value of importing parameter *IM_NAM* or *IM_VAL*. Use the conventional *APPEND* method call to do it with this approarch.

##### Macro Declaration:
```
DEFINE m_append.

  CALL METHOD <your_bdc_object_name->append
    EXPORTING
      im_dbg = &1
      im_nam = &2
      im_val = &3.

END-OF-DEFINITION.
```

##### Macro Utilization example:
```
m_append: "Basic FB03 BDC
  'X'  'SAPMF05L'     '0100'
  ' '  'RF05L-BELNR'  '<DOCUMENT_NUMBER>'
  ' '  'RF05L-BUKRS'  '<COMPANY_CODE>'
  ' '  'RF05L-GJAHR'  '<YEAR>'
  ' '  'BDC_OKCODE'   '/00'
```

##### Dynamic parameter values with return method data example:
```
<your_bdc_object_name->append( 
  im_dbg = 'X' 
  im_nam = <local_object>->get_screen_name( ) 
  im_val = <local_object>->get_value( )
).
```

##### String templates in ABAP 7.40 example:
```
<your_bdc_object_name->append( 
  im_dbg = 'X' 
  im_nam = |<SCREEN_NAME>{ lv_screen_number }|
  im_val = |<VALUE>{ lv_complement }|
).
```
