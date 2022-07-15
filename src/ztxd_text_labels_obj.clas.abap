CLASS ztxd_text_labels_obj DEFINITION
  PUBLIC
  CREATE PUBLIC

  GLOBAL FRIENDS ztxd_text_labels_obj_ft .

  PUBLIC SECTION.
*"* public components of class ZEML_TEXT_LABELS_BO
*"* do not include other source files here!!!

    TYPES:
      BEGIN OF t_key,
        text_name TYPE thead-tdname,
      END OF t_key.

    TYPES:
      BEGIN OF gts_label,
        name  TYPE string,
        value TYPE string,
      END OF gts_label .
    TYPES:
      gtt_labels TYPE STANDARD TABLE OF gts_label WITH DEFAULT KEY .

    METHODS get_data
      RETURNING
        VALUE(rs_data) TYPE t_key .

    METHODS get_labels_data_obj
      IMPORTING language_code         TYPE syst-langu
                place_holders         TYPE gtt_labels OPTIONAL
      RETURNING VALUE(ro_labels_data) TYPE REF TO data
      RAISING   zcx_return3 .

    METHODS get_labels_table
      IMPORTING language_code    TYPE syst-langu
                place_holders    TYPE gtt_labels OPTIONAL
      RETURNING VALUE(rt_labels) TYPE gtt_labels
      RAISING   zcx_return3 .

    METHODS get_labels_static_struct
      IMPORTING language_code     TYPE syst-langu
                place_holders     TYPE gtt_labels OPTIONAL
      CHANGING  !cs_labels_struct TYPE any
      RAISING   zcx_return3 .

  PROTECTED SECTION.

    DATA gs_data TYPE t_key.

    METHODS get_short_label
      IMPORTING iv_data_element_name  TYPE rollname
                iv_language_code      TYPE syst-langu
      RETURNING VALUE(rv_description) TYPE string
      RAISING   zcx_return3.

    METHODS get_medium_label
      IMPORTING iv_data_element_name  TYPE rollname
                iv_language_code      TYPE syst-langu
      RETURNING VALUE(rv_description) TYPE string
      RAISING   zcx_return3.

    METHODS get_long_label
      IMPORTING iv_data_element_name  TYPE rollname
                iv_language_code      TYPE syst-langu
      RETURNING VALUE(rv_description) TYPE string
      RAISING   zcx_return3.

    METHODS get_report_label
      IMPORTING iv_data_element_name  TYPE rollname
                iv_language_code      TYPE syst-langu
      RETURNING VALUE(rv_description) TYPE string
      RAISING   zcx_return3.

    METHODS get_ddic_field
      IMPORTING iv_data_element_name TYPE rollname
                iv_language_code     TYPE syst-langu
      RETURNING VALUE(rs_ddic_field) TYPE dfies
      RAISING   zcx_return3.

  PRIVATE SECTION.

ENDCLASS.



CLASS ztxd_text_labels_obj IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZEML_TEXT_LABELS_BO->GET_DATA
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RS_DATA                        TYPE        GTS_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_data.

    rs_data = gs_data.

  ENDMETHOD.                    "get_data


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZEML_TEXT_LABELS_BO->GET_DDIC_FIELD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATA_ELEMENT_NAME           TYPE        ROLLNAME
* | [<-()] RS_DDIC_FIELD                  TYPE        DFIES
* | [!CX!] zcx_return3
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_ddic_field.

    DATA lx_move_cast_error TYPE REF TO cx_sy_move_cast_error.

    DATA lr_type_descr TYPE REF TO cl_abap_typedescr.

    CALL METHOD cl_abap_elemdescr=>describe_by_name
      EXPORTING
        p_name         = iv_data_element_name
      RECEIVING
        p_descr_ref    = lr_type_descr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.

      "DDic data type &1 not found.
      DATA lv_dummy TYPE string.
      MESSAGE e002
        WITH iv_data_element_name
        INTO lv_dummy.

      DATA lx_return3 TYPE REF TO zcx_return3.
      CREATE OBJECT lx_return3.
      lx_return3->add_system_message( ).
      RAISE EXCEPTION lx_return3.

    ENDIF.

    TRY.

        DATA lr_element_descr TYPE REF TO cl_abap_elemdescr.

        lr_element_descr ?= lr_type_descr.

      CATCH cx_sy_move_cast_error INTO lx_move_cast_error.

        CREATE OBJECT lx_return3.
        lx_return3->add_exception_object( lx_move_cast_error ).
        RAISE EXCEPTION lx_return3.

    ENDTRY.

    rs_ddic_field =
      lr_element_descr->get_ddic_field(
        p_langu = iv_language_code ).

  ENDMETHOD.                    "get_ddic_field



  METHOD get_labels_data_obj.

    DATA labels TYPE gtt_labels.

    labels = get_labels_table(
      language_code = language_code
      place_holders = place_holders ).

    "-----------------------------------------------
    "Fill components table
    "-----------------------------------------------
    DATA lt_component_table TYPE cl_abap_structdescr=>component_table.

    LOOP AT labels
      ASSIGNING FIELD-SYMBOL(<label>).

      APPEND INITIAL LINE TO lt_component_table
        ASSIGNING FIELD-SYMBOL(<ls_component>).

      <ls_component>-name = <label>-name.
      <ls_component>-type ?= cl_abap_datadescr=>describe_by_name( 'STRING' ).

    ENDLOOP.

    "-----------------------------------------------
    "Create structure
    "-----------------------------------------------
    DATA lo_struct_descr TYPE REF TO cl_abap_structdescr.

    lo_struct_descr  = cl_abap_structdescr=>create( lt_component_table ).

    DATA lo_struct_data TYPE REF TO data.

    CREATE DATA ro_labels_data TYPE HANDLE lo_struct_descr.

    FIELD-SYMBOLS <output_labels> TYPE any.
    ASSIGN ro_labels_data->* TO <output_labels>.

    "-----------------------------------------------
    "Fill structure
    "-----------------------------------------------
    LOOP AT labels
      ASSIGNING <label>.

      ASSIGN COMPONENT <label>-name
        OF STRUCTURE <output_labels>
        TO FIELD-SYMBOL(<output_label_value>).

      <output_label_value> = <label>-value.

    ENDLOOP.

  ENDMETHOD.                    "get_labels

  METHOD get_labels_static_struct.

    DATA lt_labels_table TYPE gtt_labels.

    lt_labels_table = get_labels_table(
      language_code = language_code
      place_holders = place_holders ).

    FIELD-SYMBOLS <ls_label> LIKE LINE OF lt_labels_table.

    LOOP AT lt_labels_table
      ASSIGNING <ls_label>.

      FIELD-SYMBOLS <lv_target_label_value> TYPE any.

      ASSIGN COMPONENT <ls_label>-name
        OF STRUCTURE cs_labels_struct
        TO <lv_target_label_value>.

      IF sy-subrc <> 0.

        DATA lv_dummy TYPE string.

        "Labelset &1: Field &2 is not found in target structure
        MESSAGE e003
          WITH me->gs_data-text_name
               <ls_label>-name
          INTO lv_dummy.

        DATA lx_return3 TYPE REF TO zcx_return3.
        CREATE OBJECT lx_return3.
        lx_return3->add_system_message( ).
        RAISE EXCEPTION lx_return3.

      ENDIF.

      <lv_target_label_value> = <ls_label>-value.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_labels_table.

    DATA lt_standard_text_lines TYPE STANDARD TABLE OF tline.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ST'
        language                = language_code
        name                    = me->gs_data-text_name
        object                  = 'TEXT'
      TABLES
        lines                   = lt_standard_text_lines
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
      DATA lx_return3 TYPE REF TO zcx_return3.
      CREATE OBJECT lx_return3.
      lx_return3->add_system_message( ).
      RAISE EXCEPTION lx_return3.
    ENDIF.

    "----------------------------------------------------
    "Convert SAPscript table to String table
    DATA lt_string_lines TYPE STANDARD TABLE OF string.

    FIELD-SYMBOLS <ls_standard_text_line> LIKE LINE OF lt_standard_text_lines.

    LOOP AT lt_standard_text_lines
      ASSIGNING <ls_standard_text_line>.

      CASE <ls_standard_text_line>-tdformat.

        WHEN ''.

          IF sy-tabix = 1.

            FIELD-SYMBOLS <ls_string_line> LIKE LINE OF lt_string_lines.

            APPEND INITIAL LINE TO lt_string_lines
              ASSIGNING <ls_string_line>.

            <ls_string_line> = <ls_standard_text_line>.

          ELSE.

            <ls_string_line> = <ls_string_line> && | | && <ls_standard_text_line>-tdline.

          ENDIF.

        WHEN OTHERS.

          APPEND INITIAL LINE TO lt_string_lines
            ASSIGNING <ls_string_line>.

          <ls_string_line> = <ls_standard_text_line>-tdline.

      ENDCASE.

    ENDLOOP.

    "----------------------------------------------------
    "Convert String table to Name / value table
    DATA lt_name_value_labels TYPE STANDARD TABLE OF gts_label WITH DEFAULT KEY.

    LOOP AT lt_string_lines
      ASSIGNING <ls_string_line>.

      DATA lv_name TYPE string.
      DATA lv_value TYPE string.

      CLEAR: lv_name, lv_value.

      SPLIT <ls_string_line>
        AT '='
        INTO
          lv_name
          lv_value.

      IF lv_name IS INITIAL.
        CONTINUE.
      ENDIF.

      FIELD-SYMBOLS <ls_name_value_label> LIKE LINE OF lt_name_value_labels.

      APPEND INITIAL LINE TO lt_name_value_labels
        ASSIGNING <ls_name_value_label>.

      <ls_name_value_label>-name  = lv_name.
      <ls_name_value_label>-value = lv_value.

      SHIFT <ls_name_value_label>-name LEFT DELETING LEADING space.
      <ls_name_value_label>-name = to_upper( <ls_name_value_label>-name ).
      CONDENSE <ls_name_value_label>-name.

      SHIFT <ls_name_value_label>-value LEFT DELETING LEADING space.
      CONDENSE <ls_name_value_label>-value.

    ENDLOOP.

    "----------------------------------------------------
    "Create structure
    DATA lt_component_table TYPE cl_abap_structdescr=>component_table.

    LOOP AT lt_name_value_labels
      ASSIGNING <ls_name_value_label>.

      FIELD-SYMBOLS <ls_component> LIKE LINE OF lt_component_table.

      READ TABLE lt_component_table
        ASSIGNING <ls_component>
        WITH KEY name = <ls_name_value_label>-name.

      IF sy-subrc = 0.

        "Standard text &1 &2 contains multiple variable name &3.
        DATA lv_dummy TYPE string.

        MESSAGE e001
        WITH
          language_code
          me->gs_data-text_name
          <ls_name_value_label>-name
        INTO lv_dummy.

        CREATE OBJECT lx_return3.
        lx_return3->add_system_message( ).
        RAISE EXCEPTION lx_return3.

      ENDIF.

      APPEND INITIAL LINE TO lt_component_table
        ASSIGNING <ls_component>.

      <ls_component>-name = <ls_name_value_label>-name.
      <ls_component>-type ?= cl_abap_datadescr=>describe_by_name( 'STRING' ).

    ENDLOOP.

    DATA lo_struct_descr TYPE REF TO cl_abap_structdescr.

    lo_struct_descr  = cl_abap_structdescr=>create( lt_component_table ).

    DATA lo_struct_data TYPE REF TO data.

*    CREATE DATA ro_labels_data TYPE HANDLE lo_struct_descr.

*    FIELD-SYMBOLS <ls_labels> TYPE any.
*    ASSIGN ro_labels_data->* TO <ls_labels>.

    "----------------------------------------------------
    "Fill structure
    LOOP AT lt_name_value_labels
      ASSIGNING <ls_name_value_label>.

      FIELD-SYMBOLS <ls_label> LIKE LINE OF rt_labels.

      APPEND INITIAL LINE TO rt_labels
        ASSIGNING <ls_label>.

      FIELD-SYMBOLS <lv_label_text> TYPE any.

      <ls_label>-name = <ls_name_value_label>-name.

      ASSIGN <ls_label>-value
        TO <lv_label_text>.

      DATA lv_first_value_part TYPE string.
      DATA lv_rest_value_part TYPE string.

      SPLIT <ls_name_value_label>-value
        AT space
        INTO
          lv_first_value_part
          lv_rest_value_part.

      lv_first_value_part = to_upper( lv_first_value_part ).

      DATA lv_rest_value_part_rollname TYPE rollname.

      lv_rest_value_part_rollname = lv_rest_value_part.

      CASE lv_first_value_part.

        WHEN 'GET_SHORT_LABEL'.

          <lv_label_text> = get_short_label(
            iv_data_element_name = lv_rest_value_part_rollname
            iv_language_code     = language_code ).

        WHEN 'GET_MEDIUM_LABEL'.

          <lv_label_text> = get_medium_label(
            iv_data_element_name = lv_rest_value_part_rollname
            iv_language_code     = language_code ).

        WHEN 'GET_LONG_LABEL'.

          <lv_label_text> = get_long_label(
            iv_data_element_name = lv_rest_value_part_rollname
            iv_language_code     = language_code ).

        WHEN 'GET_REPORT_LABEL'.

          <lv_label_text> = get_report_label(
            iv_data_element_name = lv_rest_value_part_rollname
            iv_language_code     = language_code  ).

        WHEN 'GET_STANDARD_TEXT'.

          DATA lo_standard_text_bo TYPE REF TO ztxd_text_object.

          DATA lv_rest_value_part_tdname TYPE thead-tdname.

          lv_rest_value_part_tdname = lv_rest_value_part.

          lo_standard_text_bo =
            ztxd_text_object_ft=>get_factory( )->get_text_object_by_key(
              text_name = lv_rest_value_part_tdname ).

          <lv_label_text> = lo_standard_text_bo->get_text_string(
            language_code = language_code ).

        WHEN OTHERS.

          <lv_label_text> = <ls_name_value_label>-value.

          LOOP AT place_holders
            ASSIGNING FIELD-SYMBOL(<place_holder>).

            REPLACE
              ALL OCCURRENCES
              OF <place_holder>-name
              IN <lv_label_text>
              WITH <place_holder>-value
              IN CHARACTER MODE.

          ENDLOOP.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_long_label.

    DATA ls_ddic_field TYPE dfies.

    ls_ddic_field = get_ddic_field(
      iv_data_element_name = iv_data_element_name
      iv_language_code     = iv_language_code ).

    rv_description = ls_ddic_field-scrtext_l.

  ENDMETHOD.                    "get_long_label

  METHOD get_medium_label.

    DATA ls_ddic_field TYPE dfies.

    ls_ddic_field = get_ddic_field(
      iv_data_element_name = iv_data_element_name
      iv_language_code     = iv_language_code ).

    rv_description = ls_ddic_field-scrtext_m.

  ENDMETHOD.                    "get_medium_label

  METHOD get_report_label.

    DATA ls_ddic_field TYPE dfies.

    ls_ddic_field = get_ddic_field(
      iv_data_element_name = iv_data_element_name
      iv_language_code     = iv_language_code ).

    rv_description = ls_ddic_field-reptext.

  ENDMETHOD.                    "get_report_label

  METHOD get_short_label.

    DATA ls_ddic_field TYPE dfies.

    ls_ddic_field = get_ddic_field(
      iv_data_element_name = iv_data_element_name
      iv_language_code     = iv_language_code ).

    rv_description = ls_ddic_field-scrtext_s.

  ENDMETHOD.                    "get_short_label
ENDCLASS.
