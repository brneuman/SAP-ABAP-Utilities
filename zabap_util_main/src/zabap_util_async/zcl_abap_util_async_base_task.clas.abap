*----------------------------------------------------------------------*
*       CLASS ZCL_ABAP_UTIL_ASYNC_BASE_TASK DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_ABAP_UTIL_ASYNC_BASE_TASK definition
  public
  abstract
  create public .

public section.

  interfaces ZIF_ABAP_UTIL_ASYNC_TASK
      abstract methods EXECUTE_TASK
                       GET_DATA
      final methods GET_SERIAL .
  interfaces IF_SERIALIZABLE_OBJECT .

  class-methods S_GET_TASK_FROM_SERIAL
    importing
      !IM_TASK_SERIAL_TAB type Z_ABAP_UTIL_T_TASK_SERIAL optional
      !IM_TASK_SERIAL_LINE type STRING optional
    exporting
      !EX_TASK_OBJECT type ref to ZIF_ABAP_UTIL_ASYNC_TASK .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ABAP_UTIL_ASYNC_BASE_TASK IMPLEMENTATION.


METHOD s_get_task_from_serial.

* DEFINITION SECTION -------------------------- //

  DATA: lo_task        TYPE REF TO zif_abap_util_async_task.
  DATA: lv_task_serial TYPE string.

  FIELD-SYMBOLS: <ls_task_serial> LIKE LINE OF im_task_serial_tab[].

* LOGIC SECTION ------------------------------- //

  IF im_task_serial_tab[] IS NOT INITIAL.
    LOOP AT im_task_serial_tab[] ASSIGNING <ls_task_serial>.
      CONCATENATE lv_task_serial <ls_task_serial> INTO lv_task_serial RESPECTING BLANKS.
    ENDLOOP.
  ELSE.
    lv_task_serial = im_task_serial_line.
  ENDIF.

  CALL TRANSFORMATION id_indent SOURCE XML lv_task_serial RESULT task = lo_task.

  ex_task_object = lo_task.

ENDMETHOD.


METHOD zif_abap_util_async_task~get_serial.

* DEFINITION SECTION -------------------------- //

  DATA: lt_task_serial         TYPE z_abap_util_t_task_serial.
  DATA: lv_serialized_task     TYPE string,
        lv_serial_aux          TYPE c LENGTH 200,
        lv_serial_size         TYPE i,
        lv_serial_offset       TYPE i,
        lv_serial_offset_total TYPE i,
        lv_serial_length       TYPE i VALUE 200.

* LOGIC SECTION ------------------------------- //

  CALL TRANSFORMATION id_indent SOURCE task = me RESULT XML lv_serialized_task.
  lv_serial_size = strlen( lv_serialized_task ).

  DO.
    IF lv_serial_offset > lv_serial_size.
      EXIT.
    ENDIF.

    lv_serial_offset_total = lv_serial_offset + lv_serial_length.
    IF lv_serial_offset_total > lv_serial_size.
      lv_serial_length = lv_serial_size - lv_serial_offset.
    ENDIF.

    lv_serial_aux = lv_serialized_task+lv_serial_offset(lv_serial_length).
    APPEND lv_serial_aux TO lt_task_serial[].

    lv_serial_offset = lv_serial_offset + 200.

    FREE lv_serial_offset_total.
  ENDDO.

  ex_task_serial_tab[] = lt_task_serial[].
  ex_task_serial_line  = lv_serialized_task.

ENDMETHOD.
ENDCLASS.
