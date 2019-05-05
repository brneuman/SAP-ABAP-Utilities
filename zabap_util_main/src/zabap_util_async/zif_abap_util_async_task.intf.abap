*----------------------------------------------------------------------*
*       INTERFACE ZIF_ABAP_UTIL_ASYNC_TASK
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
interface ZIF_ABAP_UTIL_ASYNC_TASK
  public .


  types:
    BEGIN OF ty_s_task_data,
           id   TYPE string,
           dref TYPE REF TO data,
         END OF ty_s_task_data .
  types:
    ty_t_task_data TYPE STANDARD TABLE OF ty_s_task_data .
  types TY_TASK_RESULT type STRING .
  types TY_TASK_STATUS type STRING .

  data V_TASK_ID type BTCJOB .
  data V_TASK_STATUS type TY_TASK_STATUS .
  constants C_TASK_STATUS_WAITING type TY_TASK_STATUS value 'WAITING'. "#EC NOTEXT
  constants C_TASK_STATUS_STARTED type TY_TASK_STATUS value 'STARTED'. "#EC NOTEXT
  constants C_TASK_STATUS_FINISHED type TY_TASK_STATUS value 'FINISHED'. "#EC NOTEXT
  data V_TASK_RESULT type TY_TASK_RESULT .
  constants C_TASK_RESULT_SUCCESS type TY_TASK_RESULT value 'SUCCESS'. "#EC NOTEXT
  constants C_TASK_RESULT_FAILED type TY_TASK_RESULT value 'FAILED'. "#EC NOTEXT
  data V_TASK_DATA_EXISTS type BOOLEAN .

  events STATUS_CHANGE
    exporting
      value(P_TASK_STATUS) type TY_TASK_STATUS .

  methods EXECUTE_TASK
    exceptions
      FAILED .
  methods GET_DATA
    exporting
      !EX_TASK_DATA type TY_T_TASK_DATA .
  methods GET_SERIAL
    exporting
      !EX_TASK_SERIAL_TAB type Z_ABAP_UTIL_T_TASK_SERIAL
      !EX_TASK_SERIAL_LINE type STRING .
endinterface.
