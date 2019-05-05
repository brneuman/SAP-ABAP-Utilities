# SAP-ABAP-Utilities
My SAP ABAP Utility objects repository.

- [SAP ABAP Async Handler](src/zabap_util_async/)
- [SAP ABAP TVARV Object](src/zabap_util_tvarv/)
- [SAP ABAP BDC Object](src/zabap_util_bdc/)

# Table of Contents

* [Getting Started](#getting-started)
* [Async Handler Usage](#async-handler-usage)
* [TVARV Object Usage](#tvarv-object-usage)
* [BDC Object Usage](#bdc-object-usage)

## Getting Started

You must implement ZABAPGIT program into your SAP System to import this repository. Visit https://docs.abapgit.org/

## Async Handler Usage

Check the explanation below of [Async Handler](src/zabap_util_async/).

### Pre requisites

1. Your code must be writted in ABAP-Object Oriented concept. Visit for more information about SAP ABAP-OO development: [Getting comfortable using the Object-Oriented design model with ABAP](https://blogs.sap.com/2018/07/15/getting-comfortable-using-the-object-oriented-design-model-with-abap-part-1/).

2. Your model class must implement the interface ```ZIF_ABAP_UTIL_ASYNC_TASK```.

3. Implement your logic into interface methods: ```ZIF_ABAP_UTIL_ASYNC_TASK~EXECUTE_TASK``` and ```ZIF_ABAP_UTIL_ASYNC_TASK~GET_DATA``` (if your logic expects a result data).

4. Create Async Handler Object and add your tasks.

### Example

Running 3 objects asyncronously, ```o_task1``` to send e-mails, ```o_task2``` to load data and ```o_task3``` to update a table.

```
" Async Handler Object
DATA: o_async_handler TYPE REF TO zcl_abap_util_async_handler.

" Interface Task
DATA: o_task  TYPE REF TO zif_abap_util_async_task.

" Your objects with the ZIF_ABAP_UTIL_ASYNC_TASK interface implemented.
DATA: o_task1 TYPE REF TO zcl_email_sender,
      o_task2 TYPE REF TO zcl_load_routine,
      o_task3 TYPE REF TO zcl_update_table.
      
CREATE OBJECT o_async_handler.

CREATE OBJECT o_task1.
o_task ?= o_task1.
o_async_handler->add_new_task ( im_id_full = 'TASK1' im_task = o_task ).

CREATE OBJECT o_task2.
o_task ?= o_task2.
o_async_handler->add_new_task ( im_id_full = 'TASK2' im_task = o_task ).

CREATE OBJECT o_task3.
o_task ?= o_task3.
o_async_handler->add_new_task ( im_id_full = 'TASK3' im_task = o_task ).
```

- Run your Tasks via Job:
```
o_async_handler->start_jobs( ).
```

- Run your Tasks expecting callback:
```
"Setup your server group to run paralell tasks
o_async_handler->set_server_group( 'YOUR_SERVER_GROUP_NAME' ).
  
SET HANDLER your_callback_method FOR o_async_handler.
o_async_handler->start_tasks( ).
```

### Async setups

- Set job owner
Default: current user -> ```SY-UNAME```.
```
o_async_handler->set_job_owner( 'SCHEDULER' ).
```

- Set server group
Mandatory for paralell processing ( using method ```START_TASKS( )```.
Reference: [RZLLITAB](https://www.se80.co.uk/saptables/r/rzll/rzllitab.htm).
```
o_async_handler->set_server_group( 'parallel_executors' ).
```

- Set timeout
Optional for paralell processing  ( using method ```START_TASKS( )``` ).
Default: 120 seconds.
```
o_async_handler->set_timeout( 600 ). "10 minutes
```

## TVARV Object Usage

Check the explanation below of [TVARV Object](src/zabap_util_tvarv/).

##### Add new variant:
```
"Add new variants to our TVARV Object
DATA lo_tvarv TYPE REF TO zcl_abap_util_tvarv.

CREATE OBJECT lo_tvarv.
lo_tvarv->add_variant( im_name = 'TVARV_PARAMETER_NAME_1' ).

" ----------------------------------------------------- //
"Another way

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

##### Add multiple variants
```
"Add new variants to our TVARV Object
DATA: lo_tvarv  TYPE REF TO zcl_abap_util_tvarv.
DATA: lv_active TYPE boolean,
      lv_email  TYPE string.

" Parameters:                      | Values:
" 'TVARV_PARAMETER_NAME_1-ACTIVE'  | 'X'
" 'TVARV_PARAMETER_NAME_1-EMAIL'   | 'test@domain.com.br'

CREATE OBJECT lo_tvarv.
lo_tvarv->add_variant( im_name = 'TVARV_PARAMETER_NAME_1' im_separator = '-' ).

lv_active = lo_tvarv->get_parameter( 'ACTIVE' ).
lv_email  = lo_tvarv->get_parameter( 'EMAIL' ).
```

##### Simple checks with boolean value from method for a PARAMETER variant type
```
"Add new variants to our TVARV Object
DATA: lo_tvarv TYPE REF TO zcl_abap_util_tvarv.
DATA: lv_email TYPE string.

" Parameters:                      | Values:
" 'TVARV_PARAMETER_NAME_1-ACTIVE'  | 'X'
" 'TVARV_PARAMETER_NAME_1-EMAIL'   | 'test@domain.com.br'

CREATE OBJECT lo_tvarv.
lo_tvarv->add_variant( im_name = 'TVARV_PARAMETER_NAME_1' im_separator = '-' ).

IF lo_tvarv->is_value_exists( im_name = 'ACTIVE' im_value = abap_true ) = abap_true.
  lv_email = lo_tvarv->get_parameter( 'EMAIL' ).
  "Send email...
ELSE.
  EXIT.
ENDIF.
```

##### Simple checks with boolean value from method for RANGE variant type
```
"Add new variants to our TVARV Object
DATA: lo_tvarv TYPE REF TO zcl_abap_util_tvarv.

" Parameters:                             | Values:
" 'TVARV_PARAMETER_NAME_1-ACTIVE'         | 'X'
" 'TVARV_PARAMETER_NAME_1-EMAIL_LIST' L1  | 'test1@domain.com.br'
" 'TVARV_PARAMETER_NAME_1-EMAIL_LIST' L2  | 'test2@domain.com.br'
" 'TVARV_PARAMETER_NAME_1-EMAIL_LIST' L3  | 'test3@domain.com.br'

CREATE OBJECT lo_tvarv.
lo_tvarv->add_variant( im_name = 'TVARV_PARAMETER_NAME_1' im_separator = '-' ).

IF lo_tvarv->is_value_exists( im_name = 'EMAIL_LIST' im_value = 'test3@domain.com.br' ) = abap_true.
  "Send email...
ELSE.
  EXIT.
ENDIF.
```

##### Using static methods to get the newest value from Database (bypass buffer)
```
" Parameters:                             | Values:
" 'TVARV_PARAMETER_NAME_1-ACTIVE'         | 'X'

IF zcl_abap_util_tvarv=>s_is_value_exists( im_name = 'ROUTINE-ACTIVE' im_value = abap_true ) = abap_true.
  "Process...
ELSE.
  EXIT.
ENDIF.
```

##### SAP ABAP 7.40 facilities
```
"Add new variants to our TVARV Object
DATA: lo_tvarv TYPE REF TO zcl_abap_util_tvarv.

" Parameters:                             | Values:
" 'TVARV_PARAMETER_NAME_1-ACTIVE'         | 'X'
" 'TVARV_PARAMETER_NAME_1-EMAIL_LIST' L1  | 'test1@domain.com.br'
" 'TVARV_PARAMETER_NAME_1-EMAIL_LIST' L2  | 'test2@domain.com.br'
" 'TVARV_PARAMETER_NAME_1-EMAIL_LIST' L3  | 'test3@domain.com.br'

CREATE OBJECT lo_tvarv.
lo_tvarv->add_variant( im_name = 'TVARV_PARAMETER_NAME_1' im_separator = '-' ).

IF lo_tvarv->is_value_exists( im_name = 'EMAIL_LIST' im_value = 'test3@domain.com.br' ). "For boolean type, no comparison required
  "Send email...
ELSEIF lo_tvarv->is_value_exists( im_name = 'EMAIL_LIST' im_value = object->get_email( ) ). "
  EXIT.
ENDIF.
```
[Check SAP ABAP 7.40 release features here](https://blogs.sap.com/2015/10/25/abap-740-quick-reference/).


## BDC Object Object

Check the explanation below of [BDC Object](src/zabap_util_bdc/).

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

##### SAP ABAP 7.40 facilities
```
<your_bdc_object_name>->append( 
  im_dbg = 'X' 
  im_nam = |<SCREEN_NAME>{ lv_screen_number }|
  im_val = |<VALUE>{ lv_complement }|
).
```
