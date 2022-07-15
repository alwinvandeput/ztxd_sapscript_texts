CLASS unit_test DEFINITION FOR TESTING
  DURATION LONG
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS test_language_en FOR TESTING.

    METHODS test_language_nl FOR TESTING.

    METHODS test_fall_back FOR TESTING.

    METHODS _get_en_tlines
      RETURNING VALUE(text_tlines) TYPE tlinet.

ENDCLASS.

CLASS unit_test IMPLEMENTATION.

  METHOD test_language_en.

    TRY.

        "Manually create test data:
        "Create in transaction SO10:
        "- Text name: ZTXT_ST_TEXT_UNIT_TEST'
        "- Text id  : ST
        "- Language : EN
        "*   This is line 1.
        "*   This is line 2.
        "*   This is line 3 is a very long long which covers more characters than 72
        "    so internally it has two lines. The TDFORMAT is empty.

        DATA(exp_text_tlines) = _get_en_tlines( ).

        DATA(text_object) =
          ztxd_text_object_ft=>get_factory(
            )->get_text_object_by_key(
              object_name  = 'TEXT'
              text_id      = 'ST'
              text_name    = 'ZTXT_ST_TEXT_UNIT_TEST' ).

        DATA(act_text_tlines) = text_object->get_text_tlines(
          language_code = 'E' ).

        cl_abap_unit_assert=>assert_equals(
          exp = exp_text_tlines
          act = act_text_tlines ).

      CATCH zcx_return3 INTO DATA(return3_exc).

        cl_abap_unit_assert=>fail( msg = return3_exc->get_text(  ) ).

    ENDTRY.

  ENDMETHOD.

  METHOD test_language_nl.

    TRY.

        "Manually create test data:
        "Create in transaction SO10:
        "- Text name: ZTXT_ST_TEXT_UNIT_TEST'
        "- Text id  : ST
        "- Language : NL
        "*   Een nederlandse regel

        DATA(exp_text_tlines) = VALUE tlinet(
          ( tdformat = '*'
            tdline   = 'Een nederlandse regel' )
        ).

        DATA(text_object) =
          ztxd_text_object_ft=>get_factory(
            )->get_text_object_by_key(
              object_name  = 'TEXT'
              text_id           = 'ST'
              text_name         = 'ZTXT_ST_TEXT_UNIT_TEST' ).

        DATA(act_text_tlines) = text_object->get_text_tlines(
          language_code = 'N' ).

        cl_abap_unit_assert=>assert_equals(
          exp = exp_text_tlines
          act = act_text_tlines ).

      CATCH zcx_return3 INTO DATA(return3_exc).

        cl_abap_unit_assert=>fail( msg = return3_exc->get_text(  ) ).

    ENDTRY.

  ENDMETHOD.

  METHOD test_fall_back.

    TRY.

        DATA(exp_text_tlines) = _get_en_tlines( ).

        DATA(text_object) =
          ztxd_text_object_ft=>get_factory(
            )->get_text_object_by_key(
              object_name  = 'TEXT'
              text_id           = 'ST'
              text_name         = 'ZTXT_ST_TEXT_UNIT_TEST' ).

        DATA(act_text_tlines) = text_object->get_text_tlines(
          language_code = 'S'
          fall_back_language_codes =
            value #(
              ( 'E' )
              ( 'N' )
            ) ).

        cl_abap_unit_assert=>assert_equals(
          exp = exp_text_tlines
          act = act_text_tlines ).

      CATCH zcx_return3 INTO DATA(return3_exc).

        cl_abap_unit_assert=>fail( msg = return3_exc->get_text(  ) ).

    ENDTRY.

  ENDMETHOD.

  METHOD _get_en_tlines.

    text_tlines = VALUE #(
      ( tdformat = '*'
        tdline   = 'This is line 1.' )
      ( tdformat = '*'
        tdline   = 'This is line 2.' )
      ( tdformat = '*'
        tdline   = 'This is line 3 is a very long long which covers more characters than 72' )
      ( tdformat = ''
        tdline   = 'so internally it has two lines. The TDFORMAT is empty.' )
    ).

  ENDMETHOD.

ENDCLASS.
