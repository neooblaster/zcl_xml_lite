CLASS zcl_xml_lite_node DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

PUBLIC SECTION.

  TYPES:
    BEGIN OF ty_node_list               ,
           name TYPE        string            ,
           node TYPE REF TO zcl_xml_lite_node ,
          END   OF ty_node_list .

  METHODS constructor
    IMPORTING
      VALUE(i_node_name) TYPE string DEFAULT 'NODE'
      !i_parent_node TYPE REF TO zcl_xml_lite_node OPTIONAL
    PREFERRED PARAMETER i_node_name .
  METHODS set_parent_node
    IMPORTING
      !i_parent_node TYPE REF TO zcl_xml_lite_node .
  METHODS get_parent_node
    RETURNING
      VALUE(r_parent_node) TYPE REF TO zcl_xml_lite_node .
  METHODS set_node_name
    IMPORTING
      VALUE(i_node_name) TYPE string .
  METHODS get_node_name
    RETURNING
      VALUE(r_node_name) TYPE string .
  METHODS set_attribute
    IMPORTING
      !i_name TYPE string
      !i_value TYPE string .
  METHODS get_attribute
    IMPORTING
      !i_name TYPE string
    RETURNING
      VALUE(r_attribute) TYPE REF TO zcl_xml_lite_attribute .
  METHODS get_attribute_value
    IMPORTING
      !i_name TYPE string
    RETURNING
      VALUE(r_value) TYPE string .
  METHODS remove_attribute .
  METHODS attributes
    RETURNING
      VALUE(r_attributes) TYPE zt_xml_lite_attribute_list .
  METHODS set_value
    IMPORTING
      !i_value TYPE string .
  METHODS get_value
    RETURNING
      VALUE(r_value) TYPE string .
  METHODS append_child
    IMPORTING
      !i_child_node TYPE REF TO zcl_xml_lite_node .
  METHODS insert_before
    IMPORTING
      !i_new_node TYPE REF TO zcl_xml_lite_node
      !i_ref_node TYPE REF TO zcl_xml_lite_node OPTIONAL
      !i_index_node TYPE i OPTIONAL .
  METHODS insert_after
    IMPORTING
      !i_new_node TYPE REF TO zcl_xml_lite_node
      !i_ref_node TYPE REF TO zcl_xml_lite_node OPTIONAL
      !i_index_node TYPE i OPTIONAL .
  METHODS remove_child
    IMPORTING
      !i_child_node TYPE REF TO zcl_xml_lite_node OPTIONAL
      !i_child_node_index TYPE i OPTIONAL .
  METHODS remove_before
    IMPORTING
      !i_ref_node TYPE REF TO zcl_xml_lite_node OPTIONAL
      !i_index_node TYPE i OPTIONAL .
  METHODS remove_after
    IMPORTING
      !i_ref_node TYPE REF TO zcl_xml_lite_node OPTIONAL
      !i_index_node TYPE i OPTIONAL .
  METHODS children
    RETURNING
      VALUE(r_child_list) TYPE zt_xml_lite_child_list .
  METHODS length
    RETURNING
      VALUE(r_length) TYPE i .
  METHODS next
    RETURNING
      VALUE(r_child_node) TYPE REF TO zcl_xml_lite_node .
  METHODS next_sibling
    IMPORTING
      !i_ref_node TYPE REF TO zcl_xml_lite_node OPTIONAL
      !i_index_node TYPE i OPTIONAL
    PREFERRED PARAMETER i_ref_node
    RETURNING
      VALUE(r_sibling_node) TYPE REF TO zcl_xml_lite_node .
  METHODS previous
    RETURNING
      VALUE(r_child_node) TYPE REF TO zcl_xml_lite_node .
  METHODS previous_sibling
    IMPORTING
      !i_ref_node TYPE REF TO zcl_xml_lite_node OPTIONAL
      !i_index_node TYPE i OPTIONAL
    PREFERRED PARAMETER i_ref_node
    RETURNING
      VALUE(r_sibling_node) TYPE REF TO zcl_xml_lite_node .
  METHODS reset
    IMPORTING
      !i_index TYPE i OPTIONAL .
  METHODS child
    RETURNING
      VALUE(r_child_node) TYPE REF TO zcl_xml_lite_node .
  METHODS parent
    RETURNING
      VALUE(r_parent_node) TYPE REF TO zcl_xml_lite_node .
  METHODS clone .
  METHODS clone_child .
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

    lv_node_name = i_child_node->get_node_name( ).
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
  ENDMETHOD.


  METHOD clone_child.
  ENDMETHOD.


  METHOD constructor.

    IF i_node_name IS SUPPLIED.
      me->set_node_name( i_node_name ).
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


  METHOD get_node_name.

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
        lv_node_name = i_new_node->get_node_name( ).
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
        lv_node_name = i_new_node->get_node_name( ).
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


  METHOD set_node_name.

    me->_node_name = i_node_name.

  ENDMETHOD.


  METHOD set_parent_node.

    me->_parent_node = i_parent_node.

  ENDMETHOD.


  METHOD set_value.

    me->_value = i_value.

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
