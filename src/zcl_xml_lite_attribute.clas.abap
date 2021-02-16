CLASS zcl_xml_lite_attribute DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

PUBLIC SECTION.

  METHODS constructor
    IMPORTING
      VALUE(i_name) TYPE string OPTIONAL
      !i_value TYPE string OPTIONAL
    PREFERRED PARAMETER i_name .
  METHODS set_name
    IMPORTING
      VALUE(i_name) TYPE string .
  METHODS get_name
    RETURNING
      VALUE(r_name) TYPE string .
  METHODS set_value
    IMPORTING
      VALUE(i_value) TYPE string .
  METHODS get_value
    RETURNING
      VALUE(r_value) TYPE string .
  METHODS clone
    IMPORTING
      !i_attribute TYPE REF TO zcl_xml_lite_attribute OPTIONAL
    RETURNING
      VALUE(r_attribute) TYPE REF TO zcl_xml_lite_attribute .
PROTECTED SECTION.
PRIVATE SECTION.

  DATA _name TYPE string .
  DATA _value TYPE string .
ENDCLASS.



CLASS ZCL_XML_LITE_ATTRIBUTE IMPLEMENTATION.


  METHOD clone.

    DATA: lr_src_att TYPE REF TO zcl_xml_lite_attribute ,
          lr_new_att TYPE REF TO zcl_xml_lite_attribute ,
          lv_att_nam TYPE        string                 ,
          lv_att_val TYPE        string                 .

    " Defining attribute to copy (if not set, copy current attribute)
    IF i_attribute IS SUPPLIED.
      lr_src_att = i_attribute.
    ELSE.
      lr_src_att = me.
    ENDIF.

    " Get attribute data
    lv_att_nam = lr_src_att->get_name( ).
    lv_att_val = lr_src_att->get_value( ).

    " Create new attribute
    lr_new_att = NEW zcl_xml_lite_attribute(
      i_name  = lv_att_nam
      i_value = lv_att_val
    ).

    r_attribute = lr_new_att.

  ENDMETHOD.


  METHOD constructor.

    DATA : lv_name TYPE string .

    IF i_name IS SUPPLIED.
      IF i_name IS INITIAL.
        lv_name = 'ATTR'.
      ELSE.
        lv_name = i_name.
      ENDIF.

       me->set_name( lv_name ).
    ENDIF.

    IF i_value IS SUPPLIED.
      me->set_value( i_value ).
    ENDIF.

  ENDMETHOD.


  METHOD get_name.

    r_name = me->_name.

  ENDMETHOD.


  METHOD get_value.

    r_value = me->_value.

  ENDMETHOD.


  METHOD set_name.

    me->_name = i_name.

  ENDMETHOD.


  METHOD set_value.

    me->_value = i_value.

  ENDMETHOD.
ENDCLASS.
