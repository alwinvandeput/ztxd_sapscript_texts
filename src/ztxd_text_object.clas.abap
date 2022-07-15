CLASS ztxd_text_object DEFINITION
  PUBLIC
  CREATE PUBLIC

  GLOBAL FRIENDS ztxd_text_object_ft .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF gts_key,
        object_name TYPE thead-tdobject,
        text_id     TYPE thead-tdid,
        text_name   TYPE thead-tdname,
      END OF gts_key.

    TYPES t_fall_back_language_codes TYPE STANDARD TABLE OF syst-langu WITH EMPTY KEY.

    CONSTANTS c_crlf TYPE string VALUE cl_abap_char_utilities=>cr_lf.

    METHODS get_text_tlines
      IMPORTING
        language_code            TYPE thead-tdspras DEFAULT sy-langu
        fall_back_language_codes TYPE t_fall_back_language_codes OPTIONAL
        not_found_error_ind      TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(text_tlines)       TYPE tlinet
      RAISING
        zcx_return3 .

    METHODS get_text_string
      IMPORTING
        language_code            TYPE thead-tdspras DEFAULT sy-langu
        fall_back_language_codes TYPE t_fall_back_language_codes OPTIONAL
        line_feed                TYPE string DEFAULT c_crlf
      RETURNING
        VALUE(text_string)       TYPE string
      RAISING
        zcx_return3 .

  PROTECTED SECTION.

    DATA gs_key TYPE gts_key .

  PRIVATE SECTION.

    METHODS _convert_tlines_to_string
      IMPORTING text_tlines        TYPE tlinet
      RETURNING VALUE(text_string) TYPE string.

    METHODS _get_text_tlines
      IMPORTING
        language_code            TYPE thead-tdspras DEFAULT sy-langu
        fall_back_language_codes TYPE t_fall_back_language_codes OPTIONAL
        not_found_error_ind      TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(text_tlines)       TYPE tlinet
      RAISING
        zcx_return3 .

ENDCLASS.



CLASS ztxd_text_object IMPLEMENTATION.

  METHOD get_text_tlines.

    text_tlines =
      _get_text_tlines(
        language_code             = language_code
        not_found_error_ind       = not_found_error_ind
      ).

    IF text_tlines[] IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF fall_back_language_codes[] IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT fall_back_language_codes
      ASSIGNING FIELD-SYMBOL(<language_code>).

      text_tlines =
        _get_text_tlines(
          language_code             = <language_code>
          not_found_error_ind       = not_found_error_ind
        ).

      IF text_tlines[] IS NOT INITIAL.
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD _get_text_tlines.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = me->gs_key-text_id
        language                = language_code
        name                    = me->gs_key-text_name
        object                  = me->gs_key-object_name
      TABLES
        lines                   = text_tlines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.

      IF not_found_error_ind = abap_true.

        DATA(lx_return) = NEW zcx_return3( ).

        lx_return->add_system_message( ).

        RAISE EXCEPTION lx_return.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD get_text_string.

    DATA(text_tlines) =
      get_text_tlines(
        language_code            = language_code
        fall_back_language_codes = fall_back_language_codes ).

    text_string = _convert_tlines_to_string( text_tlines ).

  ENDMETHOD.

  METHOD _convert_tlines_to_string.

    LOOP AT text_tlines
      ASSIGNING FIELD-SYMBOL(<line>).

      IF sy-tabix = 1.

        text_string = <line>-tdline.

      ELSE.

        CASE <line>-tdformat.

          WHEN ''.

            text_string = text_string && | | && <line>-tdline.

          WHEN '/'.

            text_string = text_string && | | && <line>-tdline.

          WHEN OTHERS.

            text_string = text_string && cl_abap_char_utilities=>cr_lf && <line>-tdline.

        ENDCASE.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
