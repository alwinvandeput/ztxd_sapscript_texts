CLASS ztxd_text_labels_obj_ft DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_factory
      RETURNING
        VALUE(factory) TYPE REF TO ztxd_text_labels_obj_ft .

    CLASS-METHODS set_factory
      IMPORTING
        !factory TYPE REF TO ztxd_text_labels_obj_ft .

    METHODS get_text_labels_obj
      IMPORTING
        text_name                 TYPE thead-tdname
      RETURNING
        VALUE(text_labels_obj) TYPE REF TO ztxd_text_labels_obj .

  PROTECTED SECTION.

    CLASS-DATA m_factory TYPE REF TO ztxd_text_labels_obj_ft.

ENDCLASS.

CLASS ZTXD_TEXT_LABELS_OBJ_FT IMPLEMENTATION.


  METHOD get_text_labels_obj.

    text_labels_obj = NEW #( ).

    text_labels_obj->gs_data-text_name = text_name.

  ENDMETHOD.


  METHOD get_factory.

    IF m_factory IS NOT INITIAL.

      factory = m_factory.

      RETURN.

    ENDIF.

    factory = NEW #( ).

  ENDMETHOD.


  METHOD set_factory.

    m_factory = factory.

  ENDMETHOD.
ENDCLASS.
