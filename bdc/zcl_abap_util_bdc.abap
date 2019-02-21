*&---------------------------------------------------------------------*
*& Class ZCL_ABAP_UTIL_BDC
*&
*&---------------------------------------------------------------------*
*& SAP-ABAP-Utilities/bdc
*& Batch Input helper class
*&
*& Bruno Neuman
*& http://www.linkedin.com/in/bruno-n-a51213a3/
*&
*&---------------------------------------------------------------------*

CLASS zcl_abap_util_bdc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA v_tcode TYPE tcode READ-ONLY .

    METHODS constructor
      IMPORTING
        !im_tcode    TYPE tcode
        !im_dismode  TYPE ctu_mode   DEFAULT 'N'
        !im_updmode  TYPE ctu_update OPTIONAL
        !im_cattmode TYPE ctu_catt   OPTIONAL
        !im_defsize  TYPE ctu_defsze OPTIONAL
        !im_racommit TYPE ctu_rafc   OPTIONAL
        !im_nobinpt  TYPE ctu_nobim  OPTIONAL
        !im_nobiend  TYPE ctu_noben  OPTIONAL
      EXCEPTIONS
        invalid_tcode .

    METHODS append
      IMPORTING
        !im_dbg TYPE bdc_start
        !im_nam TYPE any
        !im_val TYPE any .

    METHODS execute
      EXCEPTIONS
        failed .

    METHODS get_messages
      EXPORTING
        !ex_value TYPE STANDARD TABLE .

    METHODS free .

    METHODS is_message_exists
      IMPORTING
        !im_msgid       TYPE symsgid
        !im_msgno       TYPE symsgno
        !im_msgty       TYPE symsgty OPTIONAL
      RETURNING
        VALUE(re_value) TYPE boolean .

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      ty_t_bdc     TYPE STANDARD TABLE OF bdcdata .
    TYPES:
      ty_t_message TYPE STANDARD TABLE OF bdcmsgcoll .

    DATA t_bdc          TYPE ty_t_bdc .
    DATA t_messages     TYPE ty_t_message .
    DATA s_ctu_params   TYPE ctu_params .

    CONSTANTS co_msgid_bdc               TYPE symsgid VALUE 'ZABAP_UTIL_BDC' ##NO_TEXT. "@SE93 Message Class
    CONSTANTS co_msgty_on_exception      TYPE symsgty VALUE 'E'              ##NO_TEXT. "Message type on exceptions
    CONSTANTS co_msgno_tcode_not_exists  TYPE symsgno VALUE '000'            ##NO_TEXT. "@SE93-ZABAP_UTIL_BDC 000 = "Transação & não existente no sistema."
    CONSTANTS co_msgno_batchinput_failed TYPE symsgno VALUE '001'            ##NO_TEXT. "@SE93-ZABAP_UTIL_BDC 001 = "Erro encontrado no retorno do Batch Input."

* Mandatory macro to call method "APPEND" like old version with perform
*DEFINE append_bdc.
*
*  CALL METHOD <your_bdc_object_name->append
*    EXPORTING
*      im_dbg = &1
*      im_nam = &2
*      im_val = &3.
*
*END-OF-DEFINITION.

ENDCLASS.



CLASS ZCL_ABAP_UTIL_BDC IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAP_UTIL_BDC->APPEND
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_DBG                         TYPE        BDC_START
* | [--->] IM_NAM                         TYPE        ANY
* | [--->] IM_VAL                         TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD append.

* DEFINITION SECTION -------------------------- //

    FIELD-SYMBOLS: " "LOCAL STRUCTURES (<LS_...>)
      <ls_bdc> TYPE bdcdata.

* LOGIC SECTION ------------------------------- //

    APPEND INITIAL LINE TO me->t_bdc[] ASSIGNING <ls_bdc>.

    IF im_dbg = abap_true. " Tela
      <ls_bdc>-dynbegin = im_dbg.
      <ls_bdc>-program  = im_nam.
      <ls_bdc>-dynpro   = im_val.
    ELSE.                  " Campo
      <ls_bdc>-dynbegin = im_dbg.
      <ls_bdc>-fnam     = im_nam.
      <ls_bdc>-fval     = im_val.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAP_UTIL_BDC->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_TCODE                       TYPE        TCODE
* | [--->] IM_DISMODE                     TYPE        CTU_MODE (default ='N')
* | [--->] IM_UPDMODE                     TYPE        CTU_UPDATE(optional)
* | [--->] IM_CATTMODE                    TYPE        CTU_CATT(optional)
* | [--->] IM_DEFSIZE                     TYPE        CTU_DEFSZE(optional)
* | [--->] IM_RACOMMIT                    TYPE        CTU_RAFC(optional)
* | [--->] IM_NOBINPT                     TYPE        CTU_NOBIM(optional)
* | [--->] IM_NOBIEND                     TYPE        CTU_NOBEN(optional)
* | [EXC!] INVALID_TCODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

* DEFINITION SECTION -------------------------- //

* LOGIC SECTION ------------------------------- //

    "Verifica se transação existe
    SELECT COUNT( * ) UP TO 1 ROWS
      FROM tstc
      WHERE tcode = im_tcode.

    "Tratamento de erro
    IF sy-subrc <> 0.
      "Transação & não existente no sistema.
      MESSAGE ID co_msgid_bdc TYPE co_msgty_on_exception
      NUMBER co_msgno_tcode_not_exists WITH im_tcode RAISING invalid_tcode.
    ENDIF.

    "Configuração dos parâmetros
    me->v_tcode = im_tcode.
    me->s_ctu_params-dismode  = im_dismode.
    me->s_ctu_params-updmode  = im_updmode.
    me->s_ctu_params-cattmode = im_cattmode.
    me->s_ctu_params-defsize  = im_defsize.
    me->s_ctu_params-racommit = im_racommit.
    me->s_ctu_params-nobinpt  = im_nobinpt.
    me->s_ctu_params-nobiend  = im_nobiend.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAP_UTIL_BDC->EXECUTE
* +-------------------------------------------------------------------------------------------------+
* | [EXC!] FAILED
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD execute.

* DEFINITION SECTION -------------------------- //

* LOGIC SECTION ------------------------------- //

    CALL TRANSACTION me->v_tcode
               USING me->t_bdc[]
             OPTIONS FROM me->s_ctu_params
            MESSAGES INTO me->t_messages[].

    READ TABLE me->t_messages[] TRANSPORTING NO FIELDS WITH KEY msgtyp = 'E'.

    IF sy-subrc = 0.
      MESSAGE ID co_msgid_bdc TYPE co_msgty_on_exception
      NUMBER co_msgno_batchinput_failed RAISING failed.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAP_UTIL_BDC->FREE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD free.

* DEFINITION SECTION -------------------------- //

* LOGIC SECTION ------------------------------- //

    FREE: me->t_bdc[],
          me->t_messages[].

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAP_UTIL_BDC->GET_MESSAGES
* +-------------------------------------------------------------------------------------------------+
* | [<---] EX_VALUE                       TYPE        STANDARD TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_messages.

* DEFINITION SECTION -------------------------- //

* LOGIC SECTION ------------------------------- //

    ex_value = me->t_messages[].

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAP_UTIL_BDC->IS_MESSAGE_EXISTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_MSGID                       TYPE        SYMSGID
* | [--->] IM_MSGNO                       TYPE        SYMSGNO
* | [--->] IM_MSGTY                       TYPE        SYMSGTY(optional)
* | [<-()] RE_VALUE                       TYPE        BOOLEAN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD is_message_exists.

* DEFINITION SECTION -------------------------- //

* LOGIC SECTION ------------------------------- //

    IF im_msgty IS NOT INITIAL.
      READ TABLE me->t_messages[] TRANSPORTING NO FIELDS WITH KEY msgid  = im_msgid
                                                                  msgnr  = im_msgno
                                                                  msgtyp = im_msgty.
    ELSE.
      READ TABLE me->t_messages[] TRANSPORTING NO FIELDS WITH KEY msgid = im_msgid
                                                                  msgnr = im_msgno.
    ENDIF.

    IF sy-subrc = 0.
      re_value = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.