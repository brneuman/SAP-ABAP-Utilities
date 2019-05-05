FUNCTION zfm_abap_util_async_executor.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(T_TASK_SERIAL) TYPE  Z_ABAP_UTIL_T_TASK_SERIAL OPTIONAL
*"     VALUE(V_TASK_SERIAL) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     VALUE(EX_TASK_SERIAL) TYPE  Z_ABAP_UTIL_T_TASK_SERIAL
*"     VALUE(RFCSI_EXPORT) TYPE  RFCSI
*"----------------------------------------------------------------------

* DEFINITION SECTION -------------------------- //

  DATA: lo_task        TYPE REF TO zif_abap_util_async_task.
  DATA: lt_task_serial TYPE z_abap_util_t_task_serial,
        lt_task_data   TYPE zif_abap_util_async_task=>ty_t_task_data.

* LOGIC SECTION ------------------------------- //

  "Call kernel
  CALL 'RFCSystemInfo' ID 'RFCSI' FIELD rfcsi_export.

*  "Infinity loop for background debugging (suggestion)
*  DO 120 TIMES. "2 minutes limit
*    SELECT COUNT( * ) UP TO 1 ROWS
*      FROM tvarvc
*      BYPASSING BUFFER
*      WHERE name = 'ZABAP_UTIL_ASYNC-INFINITY_LOOP_ACTIVE'
*        AND type = 'P'
*        AND low  = abap_true.
*    IF sy-subrc <> 0.
*      EXIT.
*    ENDIF.
*
*    SELECT COUNT( * ) UP TO 1 ROWS
*      FROM tvarvc
*      BYPASSING BUFFER
*      WHERE name = 'ZABAP_UTIL_ASYNC-INFINITY_LOOP_UNAME'
*        AND type = 'P'
*        AND low  = sy-uname.
*    IF sy-subrc <> 0.
*      EXIT.
*    ENDIF.
*
*    WAIT UP TO 1 SECONDS.
*  ENDDO.

  "Convert serial to object
  IF t_task_serial[] IS NOT INITIAL.
    zcl_abap_util_async_base_task=>s_get_task_from_serial( EXPORTING im_task_serial_tab = t_task_serial[]
                                                           IMPORTING ex_task_object     = lo_task ).
  ELSE.
    zcl_abap_util_async_base_task=>s_get_task_from_serial( EXPORTING im_task_serial_line = v_task_serial
                                                           IMPORTING ex_task_object      = lo_task ).
  ENDIF.

  "Start processing
  lo_task->v_task_status = zif_abap_util_async_task=>c_task_status_started.

  "Async logic
  lo_task->execute_task( EXCEPTIONS failed = 1 ).
  IF sy-subrc = 0.
    "On success
    lo_task->v_task_result = zif_abap_util_async_task=>c_task_result_success.
  ELSE.
    "On failed
    lo_task->v_task_result = zif_abap_util_async_task=>c_task_result_failed.
  ENDIF.

  "Check if return data exists
  lo_task->get_data( IMPORTING ex_task_data = lt_task_data[] ).
  IF lt_task_data[] IS NOT INITIAL.
    lo_task->v_task_data_exists = abap_true.
  ENDIF.

  "Finished processing
  lo_task->v_task_status = zif_abap_util_async_task=>c_task_status_finished.

  "Convert object to serial
  lo_task->get_serial( IMPORTING ex_task_serial_tab = lt_task_serial[] ).

  ex_task_serial = lt_task_serial[].

ENDFUNCTION.
