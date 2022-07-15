CLASS unit_test DEFINITION FOR TESTING
  DURATION LONG
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_sales_order_labels,
        sales_order_no TYPE string,
        date           TYPE string,
        payment_terms  TYPE string,
        item           TYPE string,
      END OF t_sales_order_labels.

    METHODS test_language_en FOR TESTING.

ENDCLASS.

CLASS unit_test IMPLEMENTATION.

  METHOD test_language_en.

    TRY.

        "Manually create test data:
        "Create in transaction SO10:
        "- Text name: ZTXT_ST_LABELS_UNIT_TEST
        "- Text id  : ST
        "- Language : EN
        "*   sales_order_no = Sales order number
        "*   date = Date
        "*   payment_terms = The order should be payed within <nn> days after
        "    receiving the invoice.
        "*   item = GET_SHORT_LABEL POSNR_VA

        "All Build-in functions are GET_SHORT_LABEL, GET_MEDIUM_LABEL, GET_LONG_LABEL, GET_REPORT_LABEL and GET_STANDARD_TEXT

        DATA(exp_labels) = VALUE t_sales_order_labels(
          sales_order_no   = |Sales order number|
          date             = |Date|
          payment_terms    = |The order should be payed within 30 days after receiving the invoice.|
          item             = |Item|  "is determined by function GET_SHORT_LABEL
        ).

        DATA act_labels TYPE t_sales_order_labels.

        DATA(text_object) =
          ztxd_text_labels_obj_ft=>get_factory(
            )->get_text_labels_obj(
              text_name         = 'ZTXT_ST_LABELS_UNIT_TEST' ).

        "---------------------------------------------------------------
        "Test 1. Labels static structure
        "---------------------------------------------------------------
        text_object->get_labels_static_struct(
          EXPORTING language_code    = 'E'
                    place_holders = VALUE #(
                      ( name  = '<nn>'
                        value = '30' ) )
          CHANGING  cs_labels_struct = act_labels ).

        cl_abap_unit_assert=>assert_equals(
          exp = exp_labels
          act = act_labels ).

        "---------------------------------------------------------------
        "Test 2. Labels Data object
        "---------------------------------------------------------------
        DATA(labels_data_obj) = text_object->get_labels_data_obj(
          language_code    = 'E'
          place_holders = VALUE #(
            ( name  = '<nn>'
              value = '30' ) ) ).

        CLEAR act_labels.

        ASSIGN labels_data_obj->* TO FIELD-SYMBOL(<labels>).

        act_labels = CORRESPONDING #( <labels> ).

        cl_abap_unit_assert=>assert_equals(
          exp = exp_labels
          act = act_labels ).

      CATCH zcx_return3 INTO DATA(return3_exc).

        cl_abap_unit_assert=>fail( msg = return3_exc->get_text(  ) ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
