*----------------------------------------------------------------------*
*       CLASS ZCL_ABAP_UTIL_ASYNC_HANDLER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_ABAP_UTIL_ASYNC_HANDLER definition
  public
  final
  create public .

public section.

  types TY_TASK_STATUS type STRING .
  types:
    BEGIN OF ty_s_task_container,
               name TYPE btcjob,
               task TYPE REF TO zif_abap_util_async_task,
             END OF ty_s_task_container .
  types:
    ty_t_task_container TYPE STANDARD TABLE OF ty_s_task_container .

  events CALLBACK
    exporting
      value(P_T_TASK_CONTAINER) type TY_T_TASK_CONTAINER .
  events STATUS_CHANGE
    exporting
      value(P_TASK_STATUS) type TY_TASK_STATUS .

  class-methods S_GET_COUNT_WP_AVAILABLE
    returning
      value(RE_VALUE) type INTEGER .
  class-methods S_IS_WORKPROCESS_AVAILABLE
    returning
      value(RE_VALUE) type BOOLEAN .
  methods ADD_NEW_TASK
    importing
      !IM_ID_PREFIX type BTCJOB optional
      !IM_ID_FULL type BTCJOB optional
      !IM_TASK type ref to ZIF_ABAP_UTIL_ASYNC_TASK .
  methods FREE .
  methods GET_STATUS
    returning
      value(RE_STATUS) type TY_TASK_STATUS .
  methods ON_TASK_END
    importing
      !P_TASK type CLIKE .
  methods SET_JOB_OWNER
    importing
      !IM_NAME type UNAME .
  methods SET_SERVER_GROUP
    importing
      !IM_SERVER_GROUP type RZLLITAB-CLASSNAME .
  methods SET_TIMEOUT
    importing
      !IM_TIMEOUT type INTEGER .
  methods SET_WORKPROCESS_LIMIT
    importing
      !IM_MAX_QUANTITY type I optional
      !IM_MAX_PERCENT type I optional .
  methods START_JOBS
    exceptions
      SUBMIT_JOB_ERROR
      CLOSE_JOB_ERROR
      TIMEOUT .
  methods START_TASKS
    exceptions
      WP_NOT_AVAILABLE
      SERVER_GROUP_NOT_INITIALIZED
      TIMEOUT .
protected section.
private section.

  constants C_TASK_STATUS_INITIAL type TY_TASK_STATUS value 'INITIAL'. "#EC NOTEXT
  constants C_TASK_STATUS_SUBMITTING type TY_TASK_STATUS value 'SUBMITTING'. "#EC NOTEXT
  constants C_TASK_STATUS_WAITING_CALLBACK type TY_TASK_STATUS value 'WAITING_CALLBACK'. "#EC NOTEXT
  constants C_TASK_STATUS_WAITING_WP_FREE type TY_TASK_STATUS value 'WAITING_WP_FREE'. "#EC NOTEXT
  constants C_TASK_STATUS_WAITING_RETRY type TY_TASK_STATUS value 'WAITING_RETRY'. "#EC NOTEXT
  constants C_TASK_STATUS_FINISHED type TY_TASK_STATUS value 'FINISHED'. "#EC NOTEXT
  data V_TIMEOUT type INTEGER value 120. "#EC NOTEXT .  .  .  .  .  .  .  .  .  . " .
  data V_SERVER_GROUP type RZLLITAB-CLASSNAME .
  data V_JOB_OWNER type UNAME .               " .
  data V_WP_LIMIT_DEFINED type BOOLEAN .
  data V_WP_LIMIT_QUANTITY type INTEGER .
  data V_WP_LIMIT_PERCENT type INTEGER .
  data V_WP_USING_ALL_AVAILABLES type BOOLEAN .
  data V_TASK_STATUS type TY_TASK_STATUS value C_TASK_STATUS_INITIAL. "#EC NOTEXT .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  . " .
  data V_TASK_COUNT type INTEGER .
  data V_TASK_ACTIVE type INTEGER .
  data V_TASK_EXISTS type BOOLEAN .
  data T_TASK_CONTAINER type TY_T_TASK_CONTAINER .
  data T_TASK_CONTAINER_CALLBACK type TY_T_TASK_CONTAINER .

  methods _GET_TASK_NAME
    importing
      !IM_ID type BTCJOB
    returning
      value(RE_NAME) type BTCJOB .
  methods _SET_TASK_STATUS
    importing
      !IM_TASK_STATUS type TY_TASK_STATUS .
  methods _GET_WP_LIMIT
    importing
      !IM_WP_AVAILABLE type INTEGER
    returning
      value(RE_WP_QUANTITY) type INTEGER .
ENDCLASS.



CLASS ZCL_ABAP_UTIL_ASYNC_HANDLER IMPLEMENTATION.


METHOD add_new_task.

* DEFINITION SECTION -------------------------- //

  DATA: lo_task TYPE REF TO zif_abap_util_async_task.
  FIELD-SYMBOLS: <ls_task_container> LIKE LINE OF me->t_task_container[].

* LOGIC SECTION ------------------------------- //

  lo_task = im_task.

  IF im_id_prefix IS NOT INITIAL.
    lo_task->v_task_id = _get_task_name( im_id_prefix ).
  ELSEIF im_id_full IS NOT INITIAL.
    lo_task->v_task_id = im_id_full.
  ENDIF.

  ASSERT lo_task IS BOUND.
  APPEND INITIAL LINE TO t_task_container[] ASSIGNING <ls_task_container>.
  <ls_task_container>-name = lo_task->v_task_id.
  <ls_task_container>-task = lo_task.

  v_task_count  = v_task_count + 1.
  v_task_exists = abap_true.

ENDMETHOD.


METHOD free.

* DEFINITION SECTION -------------------------- //

* LOGIC SECTION ------------------------------- //

  FREE: v_server_group,
        v_job_owner,
        v_task_count,
        v_task_active,
        v_task_exists,
        t_task_container[],
        t_task_container_callback[].

  v_task_status = c_task_status_initial.

ENDMETHOD.


METHOD get_status.

* DEFINITION SECTION -------------------------- //

* LOGIC SECTION ------------------------------- //

  re_status = v_task_status.

ENDMETHOD.


METHOD on_task_end.

* DEFINITION SECTION -------------------------- //

  DATA: lo_task        TYPE REF TO zif_abap_util_async_task.
  DATA: lt_task_serial TYPE z_abap_util_t_task_serial.

  FIELD-SYMBOLS: <ls_task_container_callback> LIKE LINE OF me->t_task_container_callback[].

* LOGIC SECTION ------------------------------- //

  RECEIVE RESULTS FROM FUNCTION 'ZFM_ABAP_UTIL_ASYNC_EXECUTOR'
    IMPORTING
      ex_task_serial = lt_task_serial[].

  zcl_abap_util_async_base_task=>s_get_task_from_serial( EXPORTING im_task_serial_tab = lt_task_serial[]
                                                         IMPORTING ex_task_object     = lo_task ).

  APPEND INITIAL LINE TO me->t_task_container_callback[] ASSIGNING <ls_task_container_callback>.
  <ls_task_container_callback>-name = lo_task->v_task_id.
  <ls_task_container_callback>-task = lo_task.

  v_task_active = v_task_active - 1.

ENDMETHOD.


METHOD set_job_owner.

* DEFINITION SECTION -------------------------- //

* LOGIC SECTION ------------------------------- //

  me->v_job_owner = im_name.

ENDMETHOD.


METHOD set_server_group.

* DEFINITION SECTION -------------------------- //

* LOGIC SECTION ------------------------------- //

  SELECT COUNT( * ) UP TO 1 ROWS
    FROM rzllitab
    WHERE classname = im_server_group.

  ASSERT sy-subrc = 0.

  me->v_server_group = im_server_group.

ENDMETHOD.


METHOD set_timeout.

* DEFINITION SECTION -------------------------- //

* LOGIC SECTION ------------------------------- //

  me->v_timeout = im_timeout + 1.

ENDMETHOD.


METHOD set_workprocess_limit.

* DEFINITION SECTION -------------------------- //

* LOGIC SECTION ------------------------------- //

  IF im_max_quantity IS NOT INITIAL.
    ASSERT im_max_quantity > 0.
    v_wp_limit_defined  = abap_true.
    v_wp_limit_quantity = im_max_quantity.

  ELSEIF im_max_percent IS NOT INITIAL.
    ASSERT im_max_percent > 0.
    v_wp_limit_defined  = abap_true.
    v_wp_limit_percent = im_max_percent.

  ENDIF.

ENDMETHOD.


METHOD start_jobs.

* DEFINITION SECTION -------------------------- //

  DATA: lo_task        TYPE REF TO zif_abap_util_async_task.
  DATA: lv_task_serial TYPE string,
        lv_jobcount    TYPE btcjobcnt.

  FIELD-SYMBOLS: <ls_task_container> LIKE LINE OF me->t_task_container[].

* LOGIC SECTION ------------------------------- //

  IF v_job_owner IS INITIAL.
    v_job_owner = sy-uname.
  ENDIF.

  LOOP AT t_task_container[] ASSIGNING <ls_task_container>.
    lo_task = <ls_task_container>-task.
    CHECK lo_task IS BOUND.

    lo_task->get_serial( IMPORTING ex_task_serial_line = lv_task_serial ).

    DO v_timeout TIMES.

      IF sy-index = v_timeout.
        MESSAGE text-m05 TYPE 'E' RAISING timeout.
      ENDIF.

      IF s_is_workprocess_available( ) <> abap_true.
        v_task_status = c_task_status_waiting_wp_free.
        WAIT UP TO 1 SECONDS.
        CONTINUE.
      ENDIF.

      v_task_status = c_task_status_submitting.

      CALL FUNCTION 'JOB_OPEN'
        EXPORTING
          jobname          = <ls_task_container>-name
        IMPORTING
          jobcount         = lv_jobcount
        EXCEPTIONS
          cant_create_job  = 1
          invalid_job_data = 2
          jobname_missing  = 3
          OTHERS           = 4.

      IF sy-subrc <> 0.
        v_task_status = c_task_status_waiting_retry.
        WAIT UP TO 1 SECONDS.
        CONTINUE.
      ENDIF.

      SUBMIT zrep_abap_util_async_executor
        WITH p_serial = lv_task_serial
        USER v_job_owner
        VIA JOB <ls_task_container>-name
        NUMBER  lv_jobcount
        AND RETURN.

      IF sy-subrc <> 0.
        MESSAGE text-m03 TYPE 'E' RAISING submit_job_error.
      ENDIF.

      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = lv_jobcount
          jobname              = <ls_task_container>-name
          strtimmed            = abap_true
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          OTHERS               = 8.

      IF sy-subrc <> 0.
        MESSAGE text-m04 TYPE 'E' RAISING close_job_error.
      ENDIF.

      FREE: lv_jobcount.
      EXIT.
    ENDDO.

    FREE: lv_task_serial.
  ENDLOOP.

  v_task_status = c_task_status_finished.

ENDMETHOD.


METHOD start_tasks.

* DEFINITION SECTION -------------------------- //

  DATA: lo_task          TYPE REF TO zif_abap_util_async_task.
  DATA: lt_task_serial   TYPE z_abap_util_t_task_serial.
  DATA: lv_task_active   TYPE i,
        lv_wp_total      TYPE i,
        lv_wp_available  TYPE i,
        lv_wp_limit      TYPE i,
        lv_rfcdest       TYPE rfcsi-rfcdest.

  FIELD-SYMBOLS: <ls_task_container> LIKE LINE OF me->t_task_container[].

* LOGIC SECTION ------------------------------- //

  ASSERT v_server_group IS NOT INITIAL.

  CALL FUNCTION 'SPBT_INITIALIZE'
    EXPORTING
      group_name                     = v_server_group  "Server group
    IMPORTING
      max_pbt_wps                    = lv_wp_total     "Total number of dialog work
      free_pbt_wps                   = lv_wp_available "Number of dialog work processes
    EXCEPTIONS
      invalid_group_name             = 1
      internal_error                 = 2
      pbt_env_already_initialized    = 3
      currently_no_resources_avail   = 4
      no_pbt_resources_found         = 5
      cant_init_different_pbt_groups = 6
      OTHERS                         = 7.

  IF sy-subrc <> 0.
    MESSAGE text-m01 TYPE 'E' RAISING server_group_not_initialized.
  ELSEIF lv_wp_available = 0.
    MESSAGE text-m02 TYPE 'E' RAISING wp_not_available.
  ENDIF.

  lv_wp_limit = _get_wp_limit( lv_wp_available ).

  LOOP AT t_task_container[] ASSIGNING <ls_task_container>.
    lo_task = <ls_task_container>-task.
    lo_task->get_serial( IMPORTING ex_task_serial_tab = lt_task_serial[] ).

    DO v_timeout TIMES.

      IF sy-index = v_timeout.
        MESSAGE text-m05 TYPE 'E' RAISING timeout.
      ENDIF.

      "Workprocess in limit usage
      IF lv_task_active > 0.
        IF lv_task_active = lv_wp_limit.
          _set_task_status( c_task_status_waiting_wp_free ).
          WAIT UNTIL v_task_active < lv_wp_limit UP TO v_timeout SECONDS.
          IF sy-subrc = 8.
            MESSAGE text-m05 TYPE 'E' RAISING timeout.
          ENDIF.
        ENDIF.
      ENDIF.

      _set_task_status( c_task_status_submitting ).

      "Start task
      CALL FUNCTION 'ZFM_ABAP_UTIL_ASYNC_EXECUTOR'
        STARTING NEW TASK <ls_task_container>-name
        DESTINATION IN GROUP v_server_group
        CALLING on_task_end ON END OF TASK
        EXPORTING
          t_task_serial         = lt_task_serial[]
        EXCEPTIONS
          system_failure        = 1
          communication_failure = 2
          resource_failure      = 3.

      CASE sy-subrc.
        WHEN 0.
          "Get server name
          CALL FUNCTION 'SPBT_GET_PP_DESTINATION'
            IMPORTING
              rfcdest = lv_rfcdest
            EXCEPTIONS
              OTHERS  = 1.

          v_task_active = v_task_active + 1.
          lv_task_active = v_task_active.
          EXIT.

        WHEN 1 OR 2.
          "Remove current server from available server list
          CALL FUNCTION 'SPBT_DO_NOT_USE_SERVER'
            IMPORTING
              server_name                 = lv_rfcdest
            EXCEPTIONS
              invalid_server_name         = 1
              no_more_resources_left      = 2
              pbt_env_not_initialized_yet = 3
              OTHERS                      = 4.

          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

        WHEN 3.
          "Wait submitted tasks finish
          IF lv_task_active > 1.
            _set_task_status( c_task_status_waiting_wp_free ).
            WAIT UNTIL v_task_active < lv_task_active UP TO v_timeout SECONDS.
            IF sy-subrc = 8.
              MESSAGE text-m05 TYPE 'E' RAISING timeout.
            ENDIF.
          ELSE.
            EXIT.
          ENDIF.

        WHEN OTHERS.
          EXIT.

      ENDCASE.

      _set_task_status( c_task_status_waiting_retry ).
      WAIT UP TO 1 SECONDS.
    ENDDO.

    FREE: lt_task_serial[].
  ENDLOOP.

  _set_task_status( c_task_status_waiting_callback ).
  WAIT UNTIL v_task_active = 0 UP TO v_timeout SECONDS.
  IF sy-subrc = 8.
    MESSAGE text-m05 TYPE 'E' RAISING timeout.
  ENDIF.

  _set_task_status( c_task_status_finished ).
  RAISE EVENT callback EXPORTING p_t_task_container = t_task_container_callback[].

ENDMETHOD.


METHOD S_GET_COUNT_WP_AVAILABLE.

* DEFINITION SECTION -------------------------- //

  DATA: lt_wplist TYPE STANDARD TABLE OF wpinfos.
  DATA: lv_free   TYPE i.

* LOGIC SECTION ------------------------------- //

  CALL FUNCTION 'RZL_SYSTEMWIDE_WPINFO'
    TABLES
      wplist         = lt_wplist[]
    EXCEPTIONS
      argument_error = 1
      send_error     = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  DELETE lt_wplist[] WHERE wp_typ     NE 'BTC'.
  DELETE lt_wplist[] WHERE wp_istatus NE '2'.

  DESCRIBE TABLE lt_wplist[] LINES lv_free.
  re_value = lv_free.

ENDMETHOD.


METHOD S_IS_WORKPROCESS_AVAILABLE.

* DEFINITION SECTION -------------------------- //

  DATA: lt_wplist TYPE STANDARD TABLE OF wpinfos.
  DATA: lv_free   TYPE i.

* LOGIC SECTION ------------------------------- //

  CALL FUNCTION 'RZL_SYSTEMWIDE_WPINFO'
    TABLES
      wplist         = lt_wplist[]
    EXCEPTIONS
      argument_error = 1
      send_error     = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  DELETE lt_wplist[] WHERE wp_typ NE 'BTC'.
  DELETE lt_wplist[] WHERE wp_istatus NE '2'.

  DESCRIBE TABLE lt_wplist[] LINES lv_free.

  IF lv_free > 0.
    re_value = abap_true.
  ENDIF.

ENDMETHOD.


METHOD _get_task_name.

* DEFINITION SECTION -------------------------- //

  DATA: lv_guid   TYPE sysuuid_x16,
        lv_c_guid TYPE string.

* LOGIC SECTION ------------------------------- //

  lv_guid = cl_system_uuid=>create_uuid_x16_static( ).
  lv_c_guid = lv_guid.
  CONCATENATE im_id ':' lv_c_guid INTO re_name.

ENDMETHOD.


METHOD _get_wp_limit.

* DEFINITION SECTION -------------------------- //

  DATA: lv_quantity TYPE p DECIMALS 3.

* LOGIC SECTION ------------------------------- //

  IF v_wp_limit_defined = abap_true.

    IF v_wp_limit_quantity IS NOT INITIAL.
      re_wp_quantity = v_wp_limit_quantity.

    ELSEIF v_wp_limit_percent IS NOT INITIAL.
      lv_quantity = im_wp_available * ( v_wp_limit_percent / 100 ).
      re_wp_quantity = lv_quantity.

    ENDIF.

  ELSE.
    re_wp_quantity = im_wp_available.

  ENDIF.

  ASSERT re_wp_quantity IS NOT INITIAL.

ENDMETHOD.


METHOD _set_task_status.

* DEFINITION SECTION -------------------------- //

* LOGIC SECTION ------------------------------- //

  IF v_task_status <> im_task_status.
    v_task_status = im_task_status.
    RAISE EVENT status_change EXPORTING p_task_status = v_task_status.
  ENDIF.

ENDMETHOD.
ENDCLASS.
