
REPORT  zrep_abap_util_async_executor.

*----------------------------------------------------------------------*
*       CLASS lcl_main DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_main DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS: s_start.

ENDCLASS.                    "lcl_main DEFINITION

PARAMETERS p_serial TYPE string NO-DISPLAY.

START-OF-SELECTION.

  lcl_main=>s_start( ).

END-OF-SELECTION.

*----------------------------------------------------------------------*
*       CLASS lcl_main IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_main IMPLEMENTATION.

  METHOD s_start.

* DEFINITION SECTION -------------------------- //

* LOGIC SECTION ------------------------------- //

    CALL FUNCTION 'ZFM_ABAP_UTIL_ASYNC_EXECUTOR'
      EXPORTING
        v_task_serial = p_serial.

  ENDMETHOD.                    "s_start

ENDCLASS.                    "lcl_main IMPLEMENTATION
