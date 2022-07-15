CLASS ztxd_text_object_ft DEFINITION
  PUBLIC
  FINAL
  CREATE PROTECTED.

  PUBLIC SECTION.

    CLASS-METHODS get_factory
      RETURNING
        VALUE(factory) TYPE REF TO ztxd_text_object_ft.

    CLASS-METHODS set_factory
      IMPORTING ir_factory TYPE REF TO ztxd_text_object_ft.

    METHODS get_text_object_by_key
      IMPORTING
        !object_name               TYPE thead-tdobject DEFAULT 'TEXT'
        !text_id                   TYPE thead-tdid DEFAULT 'ST'
        !text_name                 TYPE thead-tdname
      RETURNING
        VALUE(text_object) TYPE REF TO ztxd_text_object.

  PROTECTED SECTION.

    CLASS-DATA m_factory TYPE REF TO ztxd_text_object_ft .

  PRIVATE SECTION.

ENDCLASS.

CLASS ztxd_text_object_ft IMPLEMENTATION.


  METHOD get_factory.

    IF m_factory IS NOT INITIAL.

      factory = m_factory.

      RETURN.

    ENDIF.

    factory = NEW #( ).

  ENDMETHOD.


  METHOD get_text_object_by_key.

    text_object = NEW #( ).

    text_object->gs_key =
      VALUE #(
        object_name = object_name
        text_id     = text_id
        text_name   = text_name ).

  ENDMETHOD.


  METHOD set_factory.

    m_factory = ir_factory.

  ENDMETHOD.
ENDCLASS.
