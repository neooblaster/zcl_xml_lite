class ZCL_XML_LITE_NODE definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_node_list               ,
           name TYPE        string            ,
           node TYPE REF TO zcl_xml_lite_node ,
          END   OF ty_node_list .

  methods CONSTRUCTOR
    importing
      value(I_NODE_NAME) type STRING default 'NODE'
      !I_PARENT_NODE type ref to ZCL_XML_LITE_NODE optional
    preferred parameter I_NODE_NAME .
  methods SET_PARENT_NODE
    importing
      !I_PARENT_NODE type ref to ZCL_XML_LITE_NODE .
  methods GET_PARENT_NODE
    returning
      value(R_PARENT_NODE) type ref to ZCL_XML_LITE_NODE .
  methods SET_NODE_NAME
    importing
      value(I_NODE_NAME) type STRING .
  methods GET_NODE_NAME
    returning
      value(R_NODE_NAME) type STRING .
  methods SET_ATTRIBUTE
    importing
      !I_NAME type STRING
      !I_VALUE type STRING .
  methods GET_ATTRIBUTE
    importing
      !I_NAME type STRING
    returning
      value(R_ATTRIBUTE) type ref to ZCL_XML_LITE_ATTRIBUTE .
  methods GET_ATTRIBUTE_VALUE
    importing
      !I_NAME type STRING
    returning
      value(R_VALUE) type STRING .
  methods REMOVE_ATTRIBUTE .
  methods ATTRIBUTES
    returning
      value(R_ATTRIBUTES) type ZT_XML_LITE_ATTRIBUTE_LIST .
  methods SET_VALUE
    importing
      !I_VALUE type STRING .
  methods GET_VALUE
    returning
      value(R_VALUE) type STRING .
  methods APPEND_CHILD
    importing
      !I_CHILD_NODE type ref to ZCL_XML_LITE_NODE .
  methods CHILDREN
    returning
      value(R_CHILD_LIST) type ZT_XML_LITE_CHILD_LIST .
  methods LENGTH
    returning
      value(R_LENGTH) type I .
protected section.
private section.

  data _PARENT_NODE type ref to ZCL_XML_LITE_NODE .
  data _NODE_NAME type STRING .
  data _ATTRIBUTES type ZT_XML_LITE_ATTRIBUTE_LIST .
  data _VALUE type STRING .
  data _CHILDREN type ZT_XML_LITE_CHILD_LIST .
  data _CHILDREN_LEN type I .
ENDCLASS.



CLASS ZCL_XML_LITE_NODE IMPLEMENTATION.


  METHOD append_child.

    DATA : lv_node_name  TYPE string                  ,
           ls_child_node TYPE zst_xml_lite_child_node .

    lv_node_name = i_child_node->get_node_name( ).
    i_child_node->set_parent_node( me ).

    ls_child_node-name = lv_node_name.
    ls_child_node-node = i_child_node.

    APPEND ls_child_node TO me->_children.
    me->_children_len = me->_children_len + 1.

  ENDMETHOD.


  method ATTRIBUTES.

    r_attributes = me->_attributes.

  endmethod.


  method CHILDREN.

    r_child_list = me->_children.

  endmethod.


  METHOD constructor.

    IF i_node_name IS SUPPLIED.
      me->set_node_name( i_node_name ).
    ENDIF.

    IF i_parent_node IS SUPPLIED.
      me->set_parent_node( i_parent_node ).
    ENDIF.

  ENDMETHOD.


  method GET_ATTRIBUTE.
  endmethod.


  method GET_ATTRIBUTE_VALUE.
  endmethod.


  METHOD get_node_name.

    r_node_name = me->_node_name.

  ENDMETHOD.


  method GET_PARENT_NODE.

    r_parent_node = me->_parent_node.

  endmethod.


  method GET_VALUE.

    r_value = me->_value.

  endmethod.


  method LENGTH.

    r_length = me->_children_len.

  endmethod.


  method REMOVE_ATTRIBUTE.
  endmethod.


  method SET_ATTRIBUTE.

    DATA : ls_attribute  TYPE        zst_xml_lite_attribute  ,
           lr_attribute  TYPE REF TO zcl_xml_lite_attribute .


    lr_attribute = new zcl_xml_lite_attribute(
      i_name  = i_name
      i_value = i_value
    ).
    ls_attribute-name      = i_name.
    ls_attribute-attribute = lr_attribute .

    READ TABLE me->_attributes TRANSPORTING NO FIELDS WITH TABLE KEY name = i_name.

    IF sy-subrc EQ 0.
      MODIFY me->_attributes FROM ls_attribute INDEX sy-tabix.

    ELSE.
      APPEND ls_attribute TO me->_attributes.

    ENDIF.

  endmethod.


  METHOD set_node_name.

    me->_node_name = i_node_name.

  ENDMETHOD.


  method SET_PARENT_NODE.

    me->_parent_node = i_parent_node.

  endmethod.


  method SET_VALUE.

    me->_value = i_value.

  endmethod.
ENDCLASS.
