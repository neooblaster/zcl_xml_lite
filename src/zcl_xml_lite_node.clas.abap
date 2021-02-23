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
  methods SET_NAME
    importing
      value(I_NODE_NAME) type STRING .
  methods GET_NAME
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
  methods REMOVE_ATTRIBUTE
    importing
      !I_NAME type STRING .
  methods ATTRIBUTES
    returning
      value(R_ATTRIBUTES) type ZT_XML_LITE_ATTRIBUTE_LIST .
  methods SET_VALUE
    importing
      !I_VALUE type STRING optional .
  methods GET_VALUE
    returning
      value(R_VALUE) type STRING .
  methods APPEND_CHILD
    importing
      !I_CHILD_NODE type ref to ZCL_XML_LITE_NODE .
  methods INSERT_BEFORE
    importing
      !I_NEW_NODE type ref to ZCL_XML_LITE_NODE
      !I_REF_NODE type ref to ZCL_XML_LITE_NODE optional
      !I_INDEX_NODE type I optional .
  methods INSERT_AFTER
    importing
      !I_NEW_NODE type ref to ZCL_XML_LITE_NODE
      !I_REF_NODE type ref to ZCL_XML_LITE_NODE optional
      !I_INDEX_NODE type I optional .
  methods REMOVE_CHILD
    importing
      !I_CHILD_NODE type ref to ZCL_XML_LITE_NODE optional
      !I_CHILD_NODE_INDEX type I optional .
  methods REMOVE_BEFORE
    importing
      !I_REF_NODE type ref to ZCL_XML_LITE_NODE optional
      !I_INDEX_NODE type I optional .
  methods REMOVE_AFTER
    importing
      !I_REF_NODE type ref to ZCL_XML_LITE_NODE optional
      !I_INDEX_NODE type I optional .
  methods CHILDREN
    returning
      value(R_CHILD_LIST) type ZT_XML_LITE_CHILD_LIST .
  methods LENGTH
    returning
      value(R_LENGTH) type I .
  methods NEXT
    returning
      value(R_CHILD_NODE) type ref to ZCL_XML_LITE_NODE .
  methods NEXT_SIBLING
    importing
      !I_REF_NODE type ref to ZCL_XML_LITE_NODE optional
      !I_INDEX_NODE type I optional
    preferred parameter I_REF_NODE
    returning
      value(R_SIBLING_NODE) type ref to ZCL_XML_LITE_NODE .
  methods PREVIOUS
    returning
      value(R_CHILD_NODE) type ref to ZCL_XML_LITE_NODE .
  methods PREVIOUS_SIBLING
    importing
      !I_REF_NODE type ref to ZCL_XML_LITE_NODE optional
      !I_INDEX_NODE type I optional
    preferred parameter I_REF_NODE
    returning
      value(R_SIBLING_NODE) type ref to ZCL_XML_LITE_NODE .
  methods RESET
    importing
      !I_INDEX type I optional .
  methods CHILD
    returning
      value(R_CHILD_NODE) type ref to ZCL_XML_LITE_NODE .
  methods PARENT
    returning
      value(R_PARENT_NODE) type ref to ZCL_XML_LITE_NODE .
  methods CLONE
    importing
      !I_NODE type ref to ZCL_XML_LITE_NODE optional
    returning
      value(R_CLONE_NODE) type ref to ZCL_XML_LITE_NODE .
PROTECTED SECTION.
PRIVATE SECTION.

  DATA _parent_node TYPE REF TO zcl_xml_lite_node .
  DATA _node_name TYPE string .
  DATA _attributes TYPE zt_xml_lite_attribute_list .
  DATA _value TYPE string .
  DATA _children TYPE zt_xml_lite_child_list .
  DATA _children_len TYPE i .
  DATA _children_idx TYPE i VALUE 0 ##NO_TEXT.

  METHODS _get_index
    IMPORTING
      !i_ref_node TYPE REF TO zcl_xml_lite_node OPTIONAL
      !i_index_node TYPE i OPTIONAL
    RETURNING
      VALUE(r_index) TYPE i .
ENDCLASS.



CLASS ZCL_XML_LITE_NODE IMPLEMENTATION.


  METHOD append_child.

    DATA : lv_node_name  TYPE string                  ,
           ls_child_node TYPE zst_xml_lite_child_node .

    lv_node_name = i_child_node->get_name( ).
    i_child_node->set_parent_node( me ).

    ls_child_node-name = lv_node_name.
    ls_child_node-node = i_child_node.

    APPEND ls_child_node TO me->_children.
    me->_children_len = me->_children_len + 1.

  ENDMETHOD.


  METHOD attributes.

    r_attributes = me->_attributes.

  ENDMETHOD.


  METHOD child.

    DATA : lr_child_node TYPE REF TO zcl_xml_lite_node .

    IF me->length( ) > 0.
      lr_child_node = me->_children[ me->_children_idx ]-node.
    ENDIF.

    r_child_node = lr_child_node.

  ENDMETHOD.


  METHOD children.

    r_child_list = me->_children.

  ENDMETHOD.


  METHOD clone.

    DATA: lr_src_node   TYPE REF TO zcl_xml_lite_node       ,
          lr_new_node   TYPE REF TO zcl_xml_lite_node       ,
          lv_node_nam   TYPE        string                  ,
          ls_attribute  TYPE        zst_xml_lite_attribute  ,
          ls_child_node TYPE        zst_xml_lite_child_node .


    " Defining node to copy (if not set, copy current node)
    IF i_node IS SUPPLIED.
      lr_src_node = i_node.
    ELSE.
      lr_src_node = me.
    ENDIF.

    " Get node name
    lv_node_nam = lr_src_node->get_name( ).

    " Creation of node
    lr_new_node = NEW zcl_xml_lite_node( lv_node_nam ).

    " Copy Attributes
    LOOP AT lr_src_node->attributes( ) INTO ls_attribute.
      ls_attribute-attribute = ls_attribute-attribute->clone( ).
      APPEND ls_attribute TO lr_new_node->_attributes.
    ENDLOOP.

    " Copy Children
    LOOP AT lr_src_node->children( ) INTO ls_child_node.
      lr_new_node->append_child( ls_child_node-node->clone( ) ).
    ENDLOOP.

    r_clone_node = lr_new_node.

  ENDMETHOD.


  METHOD constructor.

    IF i_node_name IS SUPPLIED.
      me->set_name( i_node_name ).
    ENDIF.

    IF i_parent_node IS SUPPLIED.
      me->set_parent_node( i_parent_node ).
    ENDIF.

  ENDMETHOD.


  METHOD get_attribute.

    DATA : ls_attribute TYPE zst_xml_lite_attribute .

    READ TABLE me->_attributes INTO ls_attribute WITH TABLE KEY name = 'i_name'.

    IF sy-subrc EQ 0.
      r_attribute = ls_attribute-attribute.
    ENDIF.

  ENDMETHOD.


  METHOD get_attribute_value.

    DATA : ls_attribute TYPE zst_xml_lite_attribute .

    READ TABLE me->_attributes INTO ls_attribute WITH TABLE KEY name = 'i_name'.

    IF sy-subrc EQ 0.
      r_value = ls_attribute-attribute->get_value( ).
    ELSE.
      r_value = 'Attribute not found'.
    ENDIF.

  ENDMETHOD.


  METHOD GET_NAME.

    r_node_name = me->_node_name.

  ENDMETHOD.


  METHOD get_parent_node.

    r_parent_node = me->_parent_node.

  ENDMETHOD.


  METHOD get_value.

    r_value = me->_value.

  ENDMETHOD.


  METHOD insert_after.

    DATA : lv_index        TYPE i                       ,
           ls_child_node   TYPE zst_xml_lite_child_node ,
           lv_node_name    TYPE string                  .


    " Determining Index
    IF i_index_node IS SUPPLIED.
      lv_index = me->_get_index( i_index_node = i_index_node ).
    ELSEIF i_ref_node IS SUPPLIED.
      lv_index = me->_get_index( i_ref_node = i_ref_node ).
    ELSE.
      lv_index = me->_get_index(  ).
    ENDIF.


    " If index is equal to 0 => append
    IF lv_index EQ 0.
      me->append_child( i_new_node ).

    ELSE.
      " Insert after mean insert before next child
      lv_index = lv_index + 1.

      IF lv_index > me->length( ).
        me->append_child( i_new_node ).
      ELSE.
        lv_node_name = i_new_node->get_name( ).
        i_new_node->set_parent_node( me ).

        CLEAR ls_child_node.
        ls_child_node-name = lv_node_name.
        ls_child_node-node = i_new_node.

        INSERT ls_child_node INTO me->_children INDEX lv_index.
        me->_children_len = me->_children_len + 1.

        " Do not forget to update current handled child node index if it index is after
        " the insertion position
        IF me->_children_idx >= lv_index.
          me->_children_idx = me->_children_idx + 1.
        ENDIF.

      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD insert_before.

    DATA : lv_index        TYPE i                       ,
           ls_child_node   TYPE zst_xml_lite_child_node ,
           lv_node_name    TYPE string                  .


    " Determining Index
    IF i_index_node IS SUPPLIED.
      lv_index = me->_get_index( i_index_node = i_index_node ).
    ELSEIF i_ref_node IS SUPPLIED.
      lv_index = me->_get_index( i_ref_node = i_ref_node ).
    ELSE.
      lv_index = me->_get_index(  ).
    ENDIF.


    " If index is equal to 0 => append
    IF lv_index EQ 0.
      me->append_child( i_new_node ).

    ELSE.
      IF lv_index > me->length( ).
        me->append_child( i_new_node ).
      ELSE.
        lv_node_name = i_new_node->get_name( ).
        i_new_node->set_parent_node( me ).

        CLEAR ls_child_node.
        ls_child_node-name = lv_node_name.
        ls_child_node-node = i_new_node.

        INSERT ls_child_node INTO me->_children INDEX lv_index.
        me->_children_len = me->_children_len + 1.

        " Do not forget to update current handled child node index if it index is after
        " the insertion position
        IF me->_children_idx >= lv_index.
          me->_children_idx = me->_children_idx + 1.
        ENDIF.

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD length.

    r_length = me->_children_len.

  ENDMETHOD.


  METHOD next.

    DATA : lr_child_node TYPE REF TO zcl_xml_lite_node .

    IF me->length( ) > 0.
      me->_children_idx = me->_children_idx + 1.

      IF me->_children_idx <= me->length( ).
        lr_child_node = me->_children[ me->_children_idx ]-node.
      ELSE.
        me->_children_idx = me->_children_idx - 1.
      ENDIF.

    ENDIF.

    r_child_node = lr_child_node.

  ENDMETHOD.


  METHOD next_sibling.

    DATA : lv_index      TYPE        i                       ,
           ls_child_node TYPE        zst_xml_lite_child_node .


    " Determining index for 'ref', 'index', or 'current handling'
    IF i_index_node IS SUPPLIED.
      lv_index = me->_get_index( i_index_node = i_index_node ).
    ELSEIF i_ref_node IS SUPPLIED.
      lv_index = me->_get_index( i_ref_node = i_ref_node ).
    ELSE.
      lv_index = me->_get_index(  ).
    ENDIF.


    " If index is equal to 0 return first child
    IF lv_index EQ 0.
      lv_index = 1.
    ENDIF.


    " Be sure we have a next sibling
    IF lv_index < me->length( ).
      " As we want next sibiling :
      lv_index = lv_index + 1.

      READ TABLE me->_children INTO ls_child_node INDEX lv_index.

      IF sy-subrc EQ 0.
        r_sibling_node = ls_child_node-node.
      ENDIF.

    ELSE.
      " Create a new node to prevent dump
      " r_sibling_node = new zcl_xml_lite_node( 'NEW_NODE_NOT_SIBLING' ).

    ENDIF.

  ENDMETHOD.


  METHOD parent.

    r_parent_node = me->_parent_node .

  ENDMETHOD.


  METHOD previous.

    DATA : lr_child_node TYPE REF TO zcl_xml_lite_node .

    IF me->length( ) > 0.
      me->_children_idx = me->_children_idx - 1.

      IF me->_children_idx > 0.
        lr_child_node = me->_children[ me->_children_idx ]-node.
      ELSE.
        me->_children_idx = me->_children_idx + 1.
      ENDIF.

    ENDIF.

    r_child_node = lr_child_node.

  ENDMETHOD.


  METHOD previous_sibling.

    DATA : lv_index      TYPE i                       ,
           ls_child_node TYPE zst_xml_lite_child_node .


    " Determining index for 'ref', 'index', or 'current handling'
    IF i_index_node IS SUPPLIED.
      lv_index = me->_get_index( i_index_node = i_index_node ).
    ELSEIF i_ref_node IS SUPPLIED.
      lv_index = me->_get_index( i_ref_node = i_ref_node ).
    ELSE.
      lv_index = me->_get_index(  ).
    ENDIF.


    " If index is equal to 0 return first child
    IF lv_index EQ 0.
      lv_index = 1.
    ENDIF.

    " If it's the first child, it has no previous sibling
    IF lv_index > 1 AND lv_index <= me->length( ).
      " As we want previous sibiling :
      lv_index = lv_index - 1.

      READ TABLE me->_children INTO ls_child_node INDEX lv_index.

      IF sy-subrc EQ 0.
        r_sibling_node = ls_child_node-node.
      ENDIF.

    ELSE.
      " r_sibling_node = new zcl_xml_lite_node( 'NEW_NODE_NOT_SIBLNG' ).

    ENDIF.

  ENDMETHOD.


  METHOD remove_after.

    DATA : lv_index TYPE i .

    " Get current index
    IF i_index_node IS SUPPLIED.
      lv_index = me->_get_index( i_index_node = i_index_node ).
    ELSEIF i_ref_node IS SUPPLIED.
      lv_index = me->_get_index( i_ref_node = i_ref_node ).
    ELSE.
      lv_index = me->_get_index(  ).
    ENDIF.


    " Index can not be null, so remove last child
    IF lv_index EQ 0 OR lv_index > me->length( ).
      lv_index = me->length( ).
    ENDIF.

    " Remove child after index
    lv_index = lv_index + 1.

    me->remove_child( i_child_node_index = lv_index ).

  ENDMETHOD.


  METHOD remove_attribute.

    DELETE me->_attributes WHERE name = i_name.

  ENDMETHOD.


  METHOD remove_before.

    DATA : lv_index TYPE i .

    " Get current index
    IF i_index_node IS SUPPLIED.
      lv_index = me->_get_index( i_index_node = i_index_node ).
    ELSEIF i_ref_node IS SUPPLIED.
      lv_index = me->_get_index( i_ref_node = i_ref_node ).
    ELSE.
      lv_index = me->_get_index(  ).
    ENDIF.


    " Index can not be null, so remove last child
    IF lv_index EQ 0 OR lv_index > me->length( ).
      lv_index = me->length( ).
    ENDIF.

    " Remove child before index
    lv_index = lv_index - 1.

    me->remove_child( i_child_node_index = lv_index ).

  ENDMETHOD.


  METHOD remove_child.

    DATA : lv_index TYPE i .

    " Get current index
    IF i_child_node_index IS SUPPLIED.
      lv_index = me->_get_index( i_index_node = i_child_node_index ).
    ELSEIF i_child_node IS SUPPLIED.
      lv_index = me->_get_index( i_ref_node = i_child_node ).
    ELSE.
      lv_index = me->_get_index(  ).
    ENDIF.


    " Index can not be null, so remove last child
    IF lv_index EQ 0 OR lv_index > me->length( ).
      lv_index = me->length( ).
    ENDIF.


    " Remove child
    DELETE me->_children INDEX lv_index.

    IF sy-subrc EQ 0.
      " If current handled child has been removed
      " Handle previous one for 'next()' method
      " (most common method used vs 'previous()'
      " Or if current handled child was next one
      " We have to update the current handle
      IF lv_index <= me->_children_idx.
        me->_children_idx = me->_children_idx - 1.
      ENDIF.
      me->_children_len = _children_len - 1.
    ENDIF.

  ENDMETHOD.


  METHOD reset.

    IF i_index IS SUPPLIED.
      IF i_index > me->length( ) OR i_index < 0 .
        me->_children_idx = me->length( ).

      ELSE.
        me->_children_idx = i_index.

      ENDIF.
    ELSE.
      me->_children_idx = 0.
    ENDIF.

  ENDMETHOD.


  METHOD set_attribute.

    DATA : ls_attribute  TYPE        zst_xml_lite_attribute  ,
           lr_attribute  TYPE REF TO zcl_xml_lite_attribute .


    lr_attribute = NEW zcl_xml_lite_attribute(
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

  ENDMETHOD.


  METHOD SET_NAME.

    me->_node_name = i_node_name.

  ENDMETHOD.


  METHOD set_parent_node.

    me->_parent_node = i_parent_node.

  ENDMETHOD.


  METHOD set_value.

    IF i_value IS SUPPLIED.
     me->_value = i_value.
    ELSE.
      CLEAR me->_value.
    ENDIF.


  ENDMETHOD.


  METHOD _get_index.

    DATA : lv_index        TYPE i                       , "
           ls_child_node   TYPE zst_xml_lite_child_node , "
           lv_node_name    TYPE string                  . "


    " Determining Index
    IF     i_index_node IS SUPPLIED.
      " Index can not be null (or negative)
      IF i_index_node < 0.
        lv_index = 1.
      ELSE.
        lv_index = i_index_node.
      ENDIF.

    ELSEIF i_ref_node   IS SUPPLIED.
      " Search for corresponding node
      LOOP AT me->_children INTO ls_child_node.
        IF i_ref_node = ls_child_node-node.
          lv_index = sy-tabix.
        ENDIF.

        IF lv_index IS INITIAL.
          lv_index = me->length( ).
        ENDIF.

      ENDLOOP.

    ELSE.
      lv_index = me->_children_idx.

    ENDIF.

    r_index = lv_index.

  ENDMETHOD.
ENDCLASS.
