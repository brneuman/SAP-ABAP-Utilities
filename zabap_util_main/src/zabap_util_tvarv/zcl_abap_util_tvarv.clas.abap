*----------------------------------------------------------------------*
*       CLASS ZCL_ABAP_UTIL_TVARV DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_ABAP_UTIL_TVARV definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_s_range_tvarvc,
          sign   TYPE tvarvc-sign,
          option TYPE tvarvc-opti,
          low    TYPE tvarvc-low,
          high   TYPE tvarvc-high,
        END OF ty_s_range_tvarvc .
  types:
    ty_t_range_tvarvc TYPE STANDARD TABLE OF ty_s_range_tvarvc .

  class-methods S_GET_PARAMETER
    importing
      !IM_NAME type TVARVC-NAME
    returning
      value(RE_VALUE) type TVARVC-LOW .
  class-methods S_GET_RANGE
    importing
      !IM_NAME type TVARVC-NAME
    exporting
      !EX_VALUE type TY_T_RANGE_TVARVC .
  class-methods S_IS_VALUE_EXISTS
    importing
      !IM_NAME type TVARVC-NAME
      !IM_VALUE type ANY
    returning
      value(RE_VALUE) type BOOLEAN .
  class-methods S_IS_VARIANT_EXISTS
    importing
      !IM_NAME type TVARVC-NAME
    returning
      value(RE_VALUE) type BOOLEAN .
  methods ADD_VARIANT
    importing
      !IM_NAME type TVARVC-NAME
      !IM_SEPARATOR type CHAR1 optional
    exceptions
      NOT_FOUND
      INCONSISTENT .
  methods GET_PARAMETER
    importing
      !IM_NAME type TVARVC-NAME
    returning
      value(RE_VALUE) type TVARVC-LOW .
  methods GET_RANGE
    importing
      !IM_NAME type TVARVC-NAME
    exporting
      !EX_VALUE type TY_T_RANGE_TVARVC .
  methods IS_VALUE_EXISTS
    importing
      !IM_NAME type TVARVC-NAME
      !IM_VALUE type ANY
    returning
      value(RE_VALUE) type BOOLEAN .
  methods FREE .
  PROTECTED SECTION.

private section.

  types:
    BEGIN OF ty_s_main,
          fullname TYPE tvarvc-name,
          prefix   TYPE tvarvc-name,
          sufix    TYPE tvarvc-name,
          type     TYPE tvarvc-type,
          data_ref TYPE REF TO data,
        END OF ty_s_main .
  types:
    ty_t_main TYPE STANDARD TABLE OF ty_s_main .
  types:
    TY_T_TVARVC TYPE STANDARD TABLE OF tvarvc .

  data T_MAIN type TY_T_MAIN .
  constants CO_VARI_TYPE_PARAM type TVARVC-TYPE value 'P'. "#EC NOTEXT                            "TVARVC Parameter type
  constants CO_VARI_TYPE_SELOPT type TVARVC-TYPE value 'S'. "#EC NOTEXT                           "TVARVC Select-Options

  methods _APPEND_VALUES
    importing
      !IM_VARIANT_DATA type TVARVC
      !IM_VARIANT_SEPARATOR type CHAR1 optional .
  methods _GET_CONDITION
    importing
      !IM_VARIANT_NAME type TVARVC-NAME
      !IM_VARIANT_SEPARATOR type CHAR1 optional
    returning
      value(RE_VALUE) type STRING .
  methods _GET_VARIANT_PREFIX
    importing
      !IM_VARIANT_NAME type TVARVC-NAME
      !IM_VARIANT_SEPARATOR type CHAR1
    returning
      value(RE_VALUE) type STRING .
  methods _GET_VARIANT_SUFIX
    importing
      !IM_VARIANT_NAME type TVARVC-NAME
      !IM_VARIANT_SEPARATOR type CHAR1
    returning
      value(RE_VALUE) type STRING .
  class-methods _S_FILL_RANGE_HELPER
    importing
      !IM_TVARVC type TY_T_TVARVC
    changing
      !CH_RANGE type TY_T_RANGE_TVARVC .
ENDCLASS.



CLASS ZCL_ABAP_UTIL_TVARV IMPLEMENTATION.


  METHOD add_variant.

* DEFINITION SECTION -------------------------- //

    DATA: lt_tvarvc TYPE STANDARD TABLE OF tvarvc,
          lt_where  TYPE STANDARD TABLE OF string.

    FIELD-SYMBOLS: <ls_main>   LIKE LINE OF t_main[],
                   <ls_where>  LIKE LINE OF lt_where[],
                   <ls_tvarvc> LIKE LINE OF lt_tvarvc[].

    DATA: lv_flag_variant_exists TYPE boolean.

* LOGIC SECTION ------------------------------- //

    "Preconditions
    IF im_name IS INITIAL.
      MESSAGE text-m01 TYPE 'E' RAISING inconsistent.
    ENDIF.

    IF t_main[] IS NOT INITIAL.
      LOOP AT t_main[] ASSIGNING <ls_main>.
        IF im_separator IS NOT INITIAL.
          IF <ls_main>-prefix = im_name.
            lv_flag_variant_exists = abap_true.
            EXIT.
          ENDIF.
        ELSE.
          IF <ls_main>-fullname = im_name.
            lv_flag_variant_exists = abap_true.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF lv_flag_variant_exists = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    " Prepare conditions
    APPEND INITIAL LINE TO lt_where[] ASSIGNING <ls_where>.
    <ls_where> = _get_condition( im_variant_name      = im_name
                                 im_variant_separator = im_separator ).

    " Select values
    SELECT *
      FROM tvarvc
      INTO TABLE lt_tvarvc[]
      BYPASSING BUFFER
      WHERE (lt_where).

    " Check results
    IF sy-subrc <> 0.
      MESSAGE text-m02 TYPE 'E' RAISING not_found.
    ENDIF.

    LOOP AT lt_tvarvc[] ASSIGNING <ls_tvarvc>.
      _append_values( im_variant_data      = <ls_tvarvc>
                      im_variant_separator = im_separator ).
    ENDLOOP.

  ENDMETHOD.                    "add_variant


METHOD free.

* DEFINITION SECTION -------------------------- //

* LOGIC SECTION ------------------------------- //

  FREE: t_main[].

ENDMETHOD.


  METHOD get_parameter.

* DEFINITION SECTION -------------------------- //

    FIELD-SYMBOLS: <t_tvarvc>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS: <ls_main>   LIKE LINE OF t_main[],
                   <ls_tvarvc> TYPE tvarvc.

* LOGIC SECTION ------------------------------- //

    READ TABLE t_main[] ASSIGNING <ls_main> WITH KEY fullname = im_name
                                                     type     = co_vari_type_param.
    IF sy-subrc <> 0.
      READ TABLE t_main[] ASSIGNING <ls_main> WITH KEY sufix = im_name
                                                       type  = co_vari_type_param.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

    ASSIGN <ls_main>-data_ref->* TO <t_tvarvc>.
    READ TABLE <t_tvarvc>[] ASSIGNING <ls_tvarvc> INDEX 1.
    re_value = <ls_tvarvc>-low.

  ENDMETHOD.                    "get_parameter


  METHOD get_range.

* DEFINITION SECTION -------------------------- //

    DATA: lt_range_tvarvc TYPE STANDARD TABLE OF ty_s_range_tvarvc.

    FIELD-SYMBOLS: <t_tvarvc>        TYPE STANDARD TABLE.
    FIELD-SYMBOLS: <ls_main>         LIKE LINE OF t_main[],
                   <ls_range_tvarvc> LIKE LINE OF lt_range_tvarvc[],
                   <ls_tvarvc>       TYPE tvarvc.

* LOGIC SECTION ------------------------------- //

    READ TABLE t_main[] ASSIGNING <ls_main> WITH KEY fullname = im_name
                                                     type     = co_vari_type_selopt.
    IF sy-subrc <> 0.
      READ TABLE t_main[] ASSIGNING <ls_main> WITH KEY sufix = im_name
                                                       type  = co_vari_type_selopt.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

    ASSIGN <ls_main>-data_ref->* TO <t_tvarvc>.

    CALL METHOD _s_fill_range_helper
      EXPORTING
        im_tvarvc = <t_tvarvc>[]
      CHANGING
        ch_range  = lt_range_tvarvc[].

    ex_value = lt_range_tvarvc[].

  ENDMETHOD.                    "get_range


  METHOD is_value_exists.

* DEFINITION SECTION -------------------------- //

    DATA: lt_range_tvarvc TYPE STANDARD TABLE OF ty_s_range_tvarvc.

    FIELD-SYMBOLS: <ls_main> LIKE LINE OF t_main[].

* LOGIC SECTION ------------------------------- //

    READ TABLE t_main[] ASSIGNING <ls_main> WITH KEY fullname = im_name.
    IF sy-subrc <> 0.
      READ TABLE t_main[] ASSIGNING <ls_main> WITH KEY sufix = im_name.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

    CASE <ls_main>-type.
      WHEN co_vari_type_param.
        "Get parameter and compare
        IF get_parameter( im_name ) = im_value.
          re_value = abap_true.
        ENDIF.

      WHEN co_vari_type_selopt.
        "Get range and compare
        CALL METHOD get_range
          EXPORTING
            im_name  = im_name
          IMPORTING
            ex_value = lt_range_tvarvc[].

        IF lt_range_tvarvc[] IS NOT INITIAL.
          IF im_value IN lt_range_tvarvc[].
            re_value = abap_true.
          ENDIF.
        ELSE.
          IF im_value IS INITIAL.
            re_value = abap_true.
          ENDIF.
        ENDIF.

      WHEN OTHERS.
        "Do nothing...

    ENDCASE.

  ENDMETHOD.                    "is_value_exists


  METHOD s_get_parameter.

* DEFINITION SECTION -------------------------- //

* LOGIC SECTION ------------------------------- //

    SELECT SINGLE low
      FROM tvarvc
      INTO re_value
      BYPASSING BUFFER
      WHERE name = im_name
        AND type = co_vari_type_param.

  ENDMETHOD.                    "S_GET_PARAMETER


  METHOD s_get_range.

* DEFINITION SECTION -------------------------- //

    DATA: lt_tvarvc       TYPE STANDARD TABLE OF tvarvc,
          lt_range_tvarvc TYPE ty_t_range_tvarvc.

* LOGIC SECTION ------------------------------- //

    SELECT *
      FROM tvarvc
      INTO TABLE lt_tvarvc[]
      BYPASSING BUFFER
      WHERE name = im_name
        AND type = co_vari_type_selopt.

    CALL METHOD _s_fill_range_helper
      EXPORTING
        im_tvarvc = lt_tvarvc[]
      CHANGING
        ch_range  = lt_range_tvarvc[].

    ex_value = lt_range_tvarvc[].

  ENDMETHOD.                    "S_GET_RANGE


  METHOD s_is_value_exists.

* DEFINITION SECTION -------------------------- //

* LOGIC SECTION ------------------------------- //

    SELECT COUNT( * ) UP TO 1 ROWS
      FROM tvarvc
      BYPASSING BUFFER
      WHERE name = im_name
        AND (  ( low  = im_value )
            OR ( opti =  'BT'      AND
                 low  <= im_value  AND
                 high >= im_value ) ).

    IF sy-subrc = 0.
      re_value = abap_true.
    ENDIF.

  ENDMETHOD.                    "S_IS_VALUE_EXISTS


  METHOD s_is_variant_exists.

* DEFINITION SECTION -------------------------- //

* LOGIC SECTION ------------------------------- //

    SELECT COUNT( * ) UP TO 1 ROWS
      FROM tvarvc
      BYPASSING BUFFER
      WHERE name = im_name.

    IF sy-subrc = 0.
      re_value = abap_true.
    ENDIF.

  ENDMETHOD.                    "S_IS_VARIANT_EXISTS


  METHOD _append_values.

* DEFINITION SECTION -------------------------- //

    DATA: lv_prefix TYPE tvarvc-name,
          lv_sufix  TYPE tvarvc-name.

    FIELD-SYMBOLS: <t_tvarvc> TYPE STANDARD TABLE.
    FIELD-SYMBOLS: <ls_main>  LIKE LINE OF t_main[].

* LOGIC SECTION ------------------------------- //

    READ TABLE t_main[] ASSIGNING <ls_main> WITH KEY fullname = im_variant_data-name
                                                     type     = im_variant_data-type.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO t_main[] ASSIGNING <ls_main>.
      <ls_main>-fullname = im_variant_data-name.

      <ls_main>-prefix = _get_variant_prefix( im_variant_name      = im_variant_data-name
                                              im_variant_separator = im_variant_separator ).

      <ls_main>-sufix  = _get_variant_sufix(  im_variant_name      = im_variant_data-name
                                              im_variant_separator = im_variant_separator ).

      <ls_main>-type   = im_variant_data-type.

      CREATE DATA <ls_main>-data_ref TYPE STANDARD TABLE OF tvarvc.
    ENDIF.

    ASSIGN <ls_main>-data_ref->* TO <t_tvarvc>.
    APPEND im_variant_data TO <t_tvarvc>[].

  ENDMETHOD.                    "_append_values


  METHOD _get_condition.

* DEFINITION SECTION -------------------------- //

    DATA: lv_search_val TYPE string.

* LOGIC SECTION ------------------------------- //

    IF  im_variant_name CA '*'.
      CONCATENATE `'` im_variant_name `'` INTO lv_search_val.
      TRANSLATE lv_search_val USING '*%'.
      CONCATENATE 'NAME LIKE' lv_search_val INTO re_value SEPARATED BY space.

    ELSEIF im_variant_separator IS NOT INITIAL.
      CONCATENATE `'` im_variant_name im_variant_separator '%' `'` INTO lv_search_val.
      CONCATENATE 'NAME LIKE' lv_search_val INTO re_value SEPARATED BY space.

    ELSE.
      CONCATENATE `'` im_variant_name `'` INTO lv_search_val.
      CONCATENATE 'NAME =' lv_search_val INTO re_value SEPARATED BY space.

    ENDIF.

  ENDMETHOD.                    "_get_condition


  METHOD _get_variant_prefix.

* DEFINITION SECTION -------------------------- //

    DATA: lt_split TYPE STANDARD TABLE OF string.

* LOGIC SECTION ------------------------------- //

    IF im_variant_separator IS INITIAL.
      RETURN.
    ENDIF.

    SPLIT im_variant_name AT im_variant_separator INTO TABLE lt_split[].
    READ TABLE lt_split[] INTO re_value INDEX 1.

  ENDMETHOD.                    "_get_variant_prefix


  METHOD _get_variant_sufix.

* DEFINITION SECTION -------------------------- //

    DATA: lt_split TYPE STANDARD TABLE OF string.

* LOGIC SECTION ------------------------------- //

    IF im_variant_separator IS INITIAL.
      RETURN.
    ENDIF.

    SPLIT im_variant_name AT im_variant_separator INTO TABLE lt_split[].
    READ TABLE lt_split[] INTO re_value INDEX 2.

  ENDMETHOD.                    "_get_variant_sufix


METHOD _s_fill_range_helper.

* DEFINITION SECTION -------------------------- //

  FIELD-SYMBOLS: <ls_tvarvc>       LIKE LINE OF im_tvarvc[],
                 <ls_range_tvarvc> LIKE LINE OF ch_range[].

* LOGIC SECTION ------------------------------- //

  LOOP AT im_tvarvc[] ASSIGNING <ls_tvarvc>.
    IF ( <ls_tvarvc>-low  IS INITIAL ) AND
       ( <ls_tvarvc>-high IS INITIAL ).
      CONTINUE.
    ENDIF.

    APPEND INITIAL LINE TO ch_range[] ASSIGNING <ls_range_tvarvc>.
    <ls_range_tvarvc>-sign   = <ls_tvarvc>-sign.
    <ls_range_tvarvc>-option = <ls_tvarvc>-opti.
    <ls_range_tvarvc>-low    = <ls_tvarvc>-low.
    <ls_range_tvarvc>-high   = <ls_tvarvc>-high.

    IF <ls_range_tvarvc>-sign IS INITIAL.
      <ls_range_tvarvc>-sign = 'I'.
    ENDIF.

    IF <ls_range_tvarvc>-option IS INITIAL.
      IF ( <ls_tvarvc>-low  IS NOT INITIAL ) AND
         ( <ls_tvarvc>-high IS NOT INITIAL ).
        <ls_range_tvarvc>-option = 'BT'.
      ELSE.
        <ls_range_tvarvc>-option = 'EQ'.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMETHOD.
ENDCLASS.
