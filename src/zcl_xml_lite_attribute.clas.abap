class ZCL_XML_LITE_ATTRIBUTE definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      value(I_NAME) type STRING optional
      !I_VALUE type STRING optional
    preferred parameter I_NAME .
  methods SET_NAME
    importing
      value(I_NAME) type STRING .
  methods GET_NAME
    returning
      value(R_NAME) type STRING .
  methods SET_VALUE
    importing
      value(I_VALUE) type STRING .
  methods GET_VALUE
    returning
      value(R_VALUE) type STRING .
protected section.
private section.

  data _NAME type STRING .
  data _VALUE type STRING .
ENDCLASS.



CLASS ZCL_XML_LITE_ATTRIBUTE IMPLEMENTATION.


  method CONSTRUCTOR.

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

  endmethod.


  method GET_NAME.

    r_name = me->_name.

  endmethod.


  method GET_VALUE.

    r_value = me->_value.

  endmethod.


  method SET_NAME.

    me->_name = i_name.

  endmethod.


  method SET_VALUE.

    me->_value = i_value.

  endmethod.
ENDCLASS.
