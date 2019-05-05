*"* use this source file for your ABAP unit test classes

CLASS lcl_ut_abap_util_tvarv DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>lcl_Ut_Abap_Util_Tvarv
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_ABAP_UTIL_TVARV
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE>X
*?</GENERATE_CLASS_FIXTURE>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
* ================
    DATA:
      f_cut TYPE REF TO zcl_abap_util_tvarv.  "class under test

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: add_variant FOR TESTING.
    METHODS: get_parameter FOR TESTING.
    METHODS: get_range FOR TESTING.
    METHODS: is_value_exists FOR TESTING.
    METHODS: s_get_parameter FOR TESTING.
    METHODS: s_get_range FOR TESTING.
    METHODS: s_is_value_exists FOR TESTING.
    METHODS: s_is_variant_exists FOR TESTING.
ENDCLASS.       "lcl_Ut_Abap_Util_Tvarv


*----------------------------------------------------------------------*
*       CLASS lcl_Ut_Abap_Util_Tvarv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_ut_abap_util_tvarv IMPLEMENTATION.
* ============================================

  METHOD class_setup.
* ===================


  ENDMETHOD.       "class_Setup


  METHOD class_teardown.
* ======================


  ENDMETHOD.       "class_Teardown


  METHOD setup.
* =============

    CREATE OBJECT f_cut.

    f_cut->add_variant(
      EXPORTING
        im_name = 'ZUT_ABAP_UTIL_TVARV_PARAM1'
*       IM_SEPARATOR = im_Separator
      EXCEPTIONS
        not_found    = 1
        inconsistent = 2
    ).
    f_cut->add_variant(
      EXPORTING
        im_name = 'ZUT_ABAP_UTIL_TVARV_PARAM2'
*       IM_SEPARATOR = im_Separator
      EXCEPTIONS
        not_found    = 1
        inconsistent = 2
    ).

    f_cut->add_variant(
      EXPORTING
        im_name = 'ZUT_ABAP_UTIL_TVARV_SELOPT1'
*       IM_SEPARATOR = im_Separator
      EXCEPTIONS
        not_found    = 1
        inconsistent = 2
    ).

    f_cut->add_variant(
      EXPORTING
        im_name = 'ZUT_ABAP_UTIL_TVARV_SELOPT2'
*       IM_SEPARATOR = im_Separator
      EXCEPTIONS
        not_found    = 1
        inconsistent = 2
    ).

    f_cut->add_variant(
      EXPORTING
        im_name      = 'ZUT_ABAP_UTIL_TVARV'
        im_separator = '-'
      EXCEPTIONS
        not_found    = 1
        inconsistent = 2
    ).

  ENDMETHOD.       "setup


  METHOD teardown.
* ================


  ENDMETHOD.       "teardown


  METHOD add_variant.
* ===================
    DATA lv_subrc TYPE sy-subrc.

    f_cut->free( ).

    CLEAR lv_subrc.
    f_cut->add_variant(
      EXPORTING
        im_name = 'ZUT_ABAP_UTIL_TVARV_PARAM1'
*       IM_SEPARATOR = im_Separator
      EXCEPTIONS
        not_found    = 1
        inconsistent = 2
    ).

    lv_subrc = sy-subrc.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_subrc
      exp   = 0
      msg   = 'Testing ADD_VARIANT on SY-SUBRC result of ZUT_ABAP_UTIL_TVARV_PARAM1'
*     level =
    ).

    CLEAR lv_subrc.
    f_cut->add_variant(
      EXPORTING
        im_name = 'ZUT_ABAP_UTIL_TVARV_PARAM2'
*       IM_SEPARATOR = im_Separator
      EXCEPTIONS
        not_found    = 1
        inconsistent = 2
    ).

    lv_subrc = sy-subrc.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_subrc
      exp   = 0
      msg   = 'Testing ADD_VARIANT on SY-SUBRC result of ZUT_ABAP_UTIL_TVARV_PARAM2'
*     level =
    ).

    CLEAR lv_subrc.
    f_cut->add_variant(
      EXPORTING
        im_name = 'ZUT_ABAP_UTIL_TVARV_SELOPT1'
*       IM_SEPARATOR = im_Separator
      EXCEPTIONS
        not_found    = 1
        inconsistent = 2
    ).

    lv_subrc = sy-subrc.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_subrc
      exp   = 0
      msg   = 'Testing ADD_VARIANT on SY-SUBRC result of ZUT_ABAP_UTIL_TVARV_SELOPT1'
*     level =
    ).

    CLEAR lv_subrc.
    f_cut->add_variant(
      EXPORTING
        im_name = 'ZUT_ABAP_UTIL_TVARV_SELOPT2'
*       IM_SEPARATOR = im_Separator
      EXCEPTIONS
        not_found    = 1
        inconsistent = 2
    ).

    lv_subrc = sy-subrc.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_subrc
      exp   = 0
      msg   = 'Testing ADD_VARIANT on SY-SUBRC result of ZUT_ABAP_UTIL_TVARV_SELOPT2'
*     level =
    ).

    CLEAR lv_subrc.
    f_cut->add_variant(
      EXPORTING
        im_name      = 'ZUT_ABAP_UTIL_TVARV'
        im_separator = '-'
      EXCEPTIONS
        not_found    = 1
        inconsistent = 2
    ).

    lv_subrc = sy-subrc.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_subrc
      exp   = 0
      msg   = 'Testing ADD_VARIANT on SY-SUBRC result of ZUT_ABAP_UTIL_TVARV with Separator -'
*     level =
    ).

    CLEAR lv_subrc.
    f_cut->add_variant(
      EXPORTING
        im_name      = 'ZUT_ABAP_UTIL_TVARV_NOT_FOUND'
      EXCEPTIONS
        not_found    = 1
        inconsistent = 2
    ).

    lv_subrc = sy-subrc.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_subrc
      exp   = 1
      msg   = 'Testing ADD_VARIANT on SY-SUBRC result with a variant that not exists'
*     level =
    ).

    CLEAR lv_subrc.
    f_cut->add_variant(
      EXPORTING
        im_name      = 'ZUT_ABAP_UTIL_TVARV_NOT_FOUND'
        im_separator = '-'
      EXCEPTIONS
        not_found    = 1
        inconsistent = 2
    ).

    lv_subrc = sy-subrc.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_subrc
      exp   = 1
      msg   = 'Testing ADD_VARIANT on SY-SUBRC result with a variant that not exists with Separator -'
*     level =
    ).

    CLEAR lv_subrc.
    f_cut->add_variant(
      EXPORTING
        im_name      = ''
      EXCEPTIONS
        not_found    = 1
        inconsistent = 2
    ).

    lv_subrc = sy-subrc.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_subrc
      exp   = 2
      msg   = 'Testing ADD_VARIANT on SY-SUBRC result in inconsistent method parameters'
*     level =
    ).

    CLEAR lv_subrc.
    f_cut->add_variant(
      EXPORTING
        im_name      = ''
        im_separator = '-'
      EXCEPTIONS
        not_found    = 1
        inconsistent = 2
    ).

    lv_subrc = sy-subrc.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_subrc
      exp   = 2
      msg   = 'Testing ADD_VARIANT on SY-SUBRC result in inconsistent method parameters with Separator -'
*     level =
    ).

  ENDMETHOD.       "add_Variant


  METHOD get_parameter.
* =====================
    DATA re_value TYPE tvarv_val.

    CLEAR re_value.
    re_value = f_cut->get_parameter( 'ZUT_ABAP_UTIL_TVARV_PARAM1' ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = ''               "<--- please adapt expected value
      msg   = 'Testing GET_PARAMETER result of ZUT_ABAP_UTIL_TVARV_PARAM1'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->get_parameter( 'ZUT_ABAP_UTIL_TVARV_PARAM2' ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = 'X'         "<--- please adapt expected value
      msg   = 'Testing GET_PARAMETER result of ZUT_ABAP_UTIL_TVARV_PARAM2'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->get_parameter( 'PARAM3' ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = ''                "<--- please adapt expected value
      msg   = 'Testing GET_PARAMETER result of ZUT_ABAP_UTIL_TVARV-PARAM3'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->get_parameter( 'PARAM4' ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = 'X'         "<--- please adapt expected value
      msg   = 'Testing GET_PARAMETER result of ZUT_ABAP_UTIL_TVARV-PARAM4'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->get_parameter( 'PARAM5' ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = ''                "<--- please adapt expected value
      msg   = 'Testing GET_PARAMETER result with a variant that not loaded'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->get_parameter( 'ZUT_ABAP_UTIL_TVARV' ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = ''          "<--- please adapt expected value
      msg   = 'Testing GET_PARAMETER result with a variant prefix name that not exists'
*     level =
    ).

  ENDMETHOD.       "get_Parameter


  METHOD get_range.
* =================
    DATA ex_value_act TYPE zcl_abap_util_tvarv=>ty_t_range_tvarvc.
    DATA ex_value_exp TYPE zcl_abap_util_tvarv=>ty_t_range_tvarvc.

    FREE ex_value_act.
    f_cut->get_range(
      EXPORTING
        im_name  = 'ZUT_ABAP_UTIL_TVARV_SELOPT1'
      IMPORTING
        ex_value = ex_value_act
    ).

    FREE ex_value_exp.
    cl_abap_unit_assert=>assert_equals(
      act   = ex_value_act
      exp   = ex_value_exp      "<--- please adapt expected value
      msg   = 'Testing GET_RANGE result of ZUT_ABAP_UTIL_TVARV_SELOPT1'
*     level =
    ).

    FREE ex_value_act.
    f_cut->get_range(
      EXPORTING
        im_name  = 'ZUT_ABAP_UTIL_TVARV_SELOPT2'
      IMPORTING
        ex_value = ex_value_act
    ).

    FREE ex_value_exp.
    APPEND:
     'IEQX ' TO ex_value_exp,
     'IEQY ' TO ex_value_exp,
     'IEQZ ' TO ex_value_exp.
    cl_abap_unit_assert=>assert_equals(
      act   = ex_value_act
      exp   = ex_value_exp      "<--- please adapt expected value
      msg   = 'Testing GET_RANGE result of ZUT_ABAP_UTIL_TVARV_SELOPT2'
*     level =
    ).

    FREE ex_value_act.
    f_cut->get_range(
      EXPORTING
        im_name  = 'SELOPT3'
      IMPORTING
        ex_value = ex_value_act
    ).

    FREE ex_value_exp.
    cl_abap_unit_assert=>assert_equals(
      act   = ex_value_act
      exp   = ex_value_exp      "<--- please adapt expected value
      msg   = 'Testing GET_RANGE result of ZUT_ABAP_UTIL_TVARV-SELOPT3'
*     level =
    ).

    FREE ex_value_act.
    f_cut->get_range(
      EXPORTING
        im_name  = 'SELOPT4'
      IMPORTING
        ex_value = ex_value_act
    ).

    FREE ex_value_exp.
    APPEND:
     'IEQX ' TO ex_value_exp,
     'IEQY ' TO ex_value_exp,
     'IEQZ ' TO ex_value_exp.
    cl_abap_unit_assert=>assert_equals(
      act   = ex_value_act
      exp   = ex_value_exp      "<--- please adapt expected value
      msg   = 'Testing GET_RANGE result of ZUT_ABAP_UTIL_TVARV-SELOPT4'
*     level =
    ).

    FREE ex_value_act.
    f_cut->get_range(
      EXPORTING
        im_name  = 'SELOPT5'
      IMPORTING
        ex_value = ex_value_act
    ).

    FREE ex_value_exp.
    cl_abap_unit_assert=>assert_equals(
      act   = ex_value_act
      exp   = ex_value_exp      "<--- please adapt expected value
      msg   = 'Testing GET_RANGE result with a variant that not loaded'
*     level =
    ).

    FREE ex_value_act.
    f_cut->get_range(
      EXPORTING
        im_name  = 'ZUT_ABAP_UTIL_TVARV'
      IMPORTING
        ex_value = ex_value_act
    ).

    FREE ex_value_exp.
    cl_abap_unit_assert=>assert_equals(
      act   = ex_value_act
      exp   = ex_value_exp          "<--- please adapt expected value
      msg   = 'Testing GET_RANGE result with a variant prefix name that not exists'
*     level =
    ).

  ENDMETHOD.       "get_Range


  METHOD is_value_exists.
* =======================
    DATA re_value TYPE boolean.

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV_PARAM1'
        im_value = ''
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV_PARAM1 1/2'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV_PARAM1'
        im_value = 'X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV_PARAM1 2/2'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV_PARAM2'
        im_value = 'X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV_PARAM2 1/2'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV_PARAM2'
        im_value = ''
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV_PARAM2 2/2'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'PARAM3'
        im_value = ''
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV-PARAM3 1/2'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'PARAM3'
        im_value = 'X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV-PARAM3 2/2'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'PARAM4'
        im_value = 'X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV-PARAM4 1/2'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'PARAM4'
        im_value = ''
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV-PARAM4 2/2'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'PARAM5'
        im_value = ''
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result with a variant that not loaded 1/2'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'PARAM5'
        im_value = 'X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result with a variant that not loaded 2/2'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV_SELOPT1'
        im_value = '     '
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV_SELOPT1 1/2'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV_SELOPT1'
        im_value = 'X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV_SELOPT1 2/2'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV_SELOPT2'
        im_value = 'X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV_SELOPT2 1/4'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV_SELOPT2'
        im_value = 'Y'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV_SELOPT2 2/4'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV_SELOPT2'
        im_value = 'Z'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV_SELOPT2 3/4'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV_SELOPT2'
        im_value = 'XYZ'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV_SELOPT2 4/4'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'SELOPT3'
        im_value = ''
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV-SELOPT3 1/2'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'SELOPT3'
        im_value = 'X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV-SELOPT3 2/2'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'SELOPT4'
        im_value = 'X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV-SELOPT4 1/4'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'SELOPT4'
        im_value = 'Y'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV-SELOPT4 2/4'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'SELOPT4'
        im_value = 'Z'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV-SELOPT4 3/4'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'SELOPT4'
        im_value = 'XYZ'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV-SELOPT4 4/4'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV'
        im_value = ''
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result with a variant prefix name that not exists 1/2'
*     level =
    ).

    CLEAR re_value.
    re_value = f_cut->is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV'
        im_value = 'X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing IS_VALUE_EXISTS result with a variant prefix name that not exists 2/2'
*     level =
    ).

  ENDMETHOD.       "is_Value_Exists


  METHOD s_get_parameter.
* =======================
    DATA im_name TYPE rvari_vnam.
    DATA re_value TYPE tvarv_val.

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_get_parameter( 'ZUT_ABAP_UTIL_TVARV_PARAM1' ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = ''          "<--- please adapt expected value
      msg   = 'Testing S_GET_PARAMETER result of ZUT_ABAP_UTIL_TVARV_PARAM1'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_get_parameter( 'ZUT_ABAP_UTIL_TVARV_PARAM2' ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = 'X'          "<--- please adapt expected value
      msg   = 'Testing S_GET_PARAMETER result of ZUT_ABAP_UTIL_TVARV_PARAM2'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_get_parameter( 'ZUT_ABAP_UTIL_TVARV-PARAM3' ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = ''          "<--- please adapt expected value
      msg   = 'Testing S_GET_PARAMETER result of ZUT_ABAP_UTIL_TVARV-PARAM3'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_get_parameter( 'ZUT_ABAP_UTIL_TVARV-PARAM4' ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = 'X'          "<--- please adapt expected value
      msg   = 'Testing S_GET_PARAMETER result of ZUT_ABAP_UTIL_TVARV-PARAM4'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_get_parameter( 'ZUT_ABAP_UTIL_TVARV-PARAM5' ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = ''          "<--- please adapt expected value
      msg   = 'Testing S_GET_PARAMETER result with a variant that not exists'
*     level =
    ).

  ENDMETHOD.       "s_Get_Parameter


  METHOD s_get_range.
* ===================
    DATA ex_value_act TYPE zcl_abap_util_tvarv=>ty_t_range_tvarvc.
    DATA ex_value_exp TYPE zcl_abap_util_tvarv=>ty_t_range_tvarvc.

    FREE ex_value_act.
    zcl_abap_util_tvarv=>s_get_range(
      EXPORTING
        im_name = 'ZUT_ABAP_UTIL_TVARV_SELOPT1'
     IMPORTING
       ex_value = ex_value_act
    ).

    FREE ex_value_exp.
    cl_abap_unit_assert=>assert_equals(
      act   = ex_value_act
      exp   = ex_value_exp          "<--- please adapt expected value
      msg   = 'Testing S_GET_RANGE result of ZUT_ABAP_UTIL_TVARV_SELOPT1'
*     level =
    ).

    FREE ex_value_act.
    zcl_abap_util_tvarv=>s_get_range(
      EXPORTING
        im_name = 'ZUT_ABAP_UTIL_TVARV_SELOPT2'
     IMPORTING
       ex_value = ex_value_act
    ).

    FREE ex_value_exp.
    APPEND:
     'IEQX ' TO ex_value_exp,
     'IEQY ' TO ex_value_exp,
     'IEQZ ' TO ex_value_exp.
    cl_abap_unit_assert=>assert_equals(
      act   = ex_value_act
      exp   = ex_value_exp          "<--- please adapt expected value
      msg   = 'Testing S_GET_RANGE result of ZUT_ABAP_UTIL_TVARV_SELOPT2'
*     level =
    ).

    FREE ex_value_act.
    zcl_abap_util_tvarv=>s_get_range(
      EXPORTING
        im_name = 'ZUT_ABAP_UTIL_TVARV-SELOPT3'
     IMPORTING
       ex_value = ex_value_act
    ).

    FREE ex_value_exp.
    cl_abap_unit_assert=>assert_equals(
      act   = ex_value_act
      exp   = ex_value_exp          "<--- please adapt expected value
      msg   = 'Testing S_GET_RANGE result of ZUT_ABAP_UTIL_TVARV-SELOPT3'
*     level =
    ).

    FREE ex_value_act.
    zcl_abap_util_tvarv=>s_get_range(
      EXPORTING
        im_name = 'ZUT_ABAP_UTIL_TVARV-SELOPT4'
     IMPORTING
       ex_value = ex_value_act
    ).

    FREE ex_value_exp.
    APPEND:
     'IEQX ' TO ex_value_exp,
     'IEQY ' TO ex_value_exp,
     'IEQZ ' TO ex_value_exp.
    cl_abap_unit_assert=>assert_equals(
      act   = ex_value_act
      exp   = ex_value_exp          "<--- please adapt expected value
      msg   = 'Testing S_GET_RANGE result of ZUT_ABAP_UTIL_TVARV-SELOPT4'
*     level =
    ).

    FREE ex_value_act.
    zcl_abap_util_tvarv=>s_get_range(
      EXPORTING
        im_name = 'ZUT_ABAP_UTIL_TVARV-SELOPT5'
     IMPORTING
       ex_value = ex_value_act
    ).

    FREE ex_value_exp.
    cl_abap_unit_assert=>assert_equals(
      act   = ex_value_act
      exp   = ex_value_exp          "<--- please adapt expected value
      msg   = 'Testing S_GET_RANGE result with a variant that not exists'
*     level =
    ).

  ENDMETHOD.       "s_Get_Range


  METHOD s_is_value_exists.
* =========================
    DATA re_value TYPE boolean.

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV_PARAM1'
        im_value = ''
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV_PARAM1 1/2'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV_PARAM1'
        im_value = 'X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV_PARAM1 2/2'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV_PARAM2'
        im_value = 'X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV_PARAM2 1/2'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV_PARAM2'
        im_value = ''
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV_PARAM2 2/2'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV-PARAM3'
        im_value = ''
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV-PARAM3 1/2'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV-PARAM3'
        im_value = 'X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV-PARAM3 2/2'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV-PARAM4'
        im_value = 'X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV-PARAM4 1/2'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV-PARAM4'
        im_value = ''
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV-PARAM4 2/2'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV-PARAM5'
        im_value = ''
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result with a variant that not loaded 1/2'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV-PARAM5'
        im_value = 'X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result with a variant that not loaded 2/2'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV_SELOPT1'
        im_value = ''
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV_SELOPT1 1/2'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV_SELOPT1'
        im_value = 'X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV_SELOPT1 2/2'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV_SELOPT2'
        im_value = 'X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV_SELOPT2 1/4'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV_SELOPT2'
        im_value = 'Y'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV_SELOPT2 2/4'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV_SELOPT2'
        im_value = 'Z'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV_SELOPT2 3/4'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV_SELOPT2'
        im_value = 'XYZ'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV_SELOPT2 4/4'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV-SELOPT3'
        im_value = ''
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV-SELOPT3 1/2'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV-SELOPT3'
        im_value = 'X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV-SELOPT3 2/2'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV-SELOPT4'
        im_value = 'X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV-SELOPT4 1/4'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV-SELOPT4'
        im_value = 'Y'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV-SELOPT4 2/4'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV-SELOPT4'
        im_value = 'Z'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV-SELOPT4 3/4'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV-SELOPT4'
        im_value = 'XYZ'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result of ZUT_ABAP_UTIL_TVARV-SELOPT4 4/4'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV'
        im_value = ''
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result with a variant prefix name that not exists 1/2'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_value_exists(
        im_name = 'ZUT_ABAP_UTIL_TVARV'
        im_value = 'X'
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing S_IS_VALUE_EXISTS result with a variant prefix name that not exists 2/2'
*     level =
    ).

  ENDMETHOD.       "s_Is_Value_Exists


  METHOD s_is_variant_exists.
* ===========================
    DATA re_value TYPE boolean.

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_variant_exists( 'ZUT_ABAP_UTIL_TVARV_PARAM1' ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing S_IS_VARIANT_EXISTS 1/3'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_variant_exists( 'ZUT_ABAP_UTIL_TVARV_SELOPT1' ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_true          "<--- please adapt expected value
      msg   = 'Testing S_IS_VARIANT_EXISTS 2/3'
*     level =
    ).

    CLEAR re_value.
    re_value = zcl_abap_util_tvarv=>s_is_variant_exists( 'ZUT_ABAP_UTIL_TVARV-PARAM5' ).

    cl_abap_unit_assert=>assert_equals(
      act   = re_value
      exp   = abap_false          "<--- please adapt expected value
      msg   = 'Testing S_IS_VARIANT_EXISTS 3/3'
*     level =
    ).

  ENDMETHOD.       "s_Is_Variant_Exists




ENDCLASS.       "lcl_Ut_Abap_Util_Tvarv
