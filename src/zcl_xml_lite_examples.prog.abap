*&---------------------------------------------------------------------*
*& Report ZCL_XML_LITE_EXAMPLES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcl_xml_lite_examples.


DATA: lo_xml   TYPE REF TO zcl_xml_lite ,
      lv_xml   TYPE        string       .

lv_xml = |<?xml version="1.0" encoding="utf-8" ?>|      &&
         |<NODE_1 lvl="1.0">|                           &&
*lv_xml = |<NODE_1 lvl="1.0">|                           &&
         |  <NODE_2 lvl="2.0">|                         &&
         |    <NODE_3 lvl="3.1">value_3.1</NODE_3>|     &&
         |    <NODE_4 lvl="3.2">|                       &&
         |      <NODE_5 lvl="4.1">value_4.1</NODE5>|    &&
         |      <NODE_6 lvl="4.2">|                     &&
         |        <NODE_7 lvl="5.0">value_5.0</NODE_7>| &&
         |      </NODE_6>|                              &&
         |      <NODE_8 lvl="4.3">value_4.3</NODE8>|    &&
         |    </NODE_4>|                                &&
         |  </NODE_2>|                                  &&
         |  <NODE_2>A</NODE_2>|                         &&
         |  <NODE_2>B</NODE_2>|                         &&
         |  <NODE_2>C</NODE_2>|                         &&
         |  <NODE_9>I</NODE_9>|                         &&
         |  <NODE_10>|                                  &&
         |    <NODE_11>eleven</NODE_11>|                &&
         |    <NODE_12 attr="at12"/>|                   &&
         |    <NODE_13>thirteen</NODE_13>|              &&
         |  </NODE_10>|                                 &&
         |</NODE_1>| .

*lv_xml = |<?xml version="1.0" encoding="utf-8" ?>|      &&
*         |<NODE_1 lvl="1.0">|                           &&
*         |  <NODE_12 attr="at12"/>|                   &&
*         |</NODE_1>| .

lo_xml = NEW zcl_xml_lite( lv_xml ).


" Retrieving child nodes filtering using tag name
" --------------------------------------------------
DATA : lt_node_2 TYPE zt_xml_lite_child_list ,
       lt_node_9 TYPE zt_xml_lite_child_list .

lt_node_2 = lo_xml->nodes( 'NODE_2' ).
lt_node_9 = lo_xml->nodes( 'NODE_9' ).



" Retrieving a child node filetring using tag name
" --------------------------------------------------
DATA : lr_node_9 TYPE REF TO zcl_xml_lite_node ,
       lr_root   TYPE REF TO zcl_xml_lite_node .

lr_node_9 = lo_xml->node( 'NODE_9' ). " << First occurence found
lr_root   = lo_xml->root_node( ).     " << XML Root node



" Creating Node
" -------------------------
*DATA(new_node) = new zcl_xml_lite_node( 'MY_NEW_NODE' ).
*lr_root->append_child( new_node ).



" Looping on node list
" ------------------------
*LOOP AT lo_xml->root_node( )->children( ) INTO DATA(children_l1).
*  DATA(child_l1) = children_l1-node.
*
*  WRITE : / '- ' && child_l1->get_node_name( ).
*
*  LOOP AT child_l1->children( ) INTO DATA(children_l2).
*    DATA(child_l2) = children_l2-node.
*    WRITE : / '  - ' && child_l2->get_node_name( ).
*  ENDLOOP.
*ENDLOOP.

*lo_xml->prettify( 'X' ).
*lo_xml->use_space( 4 ).
*lo_xml->use_tab( ).

write : / lo_xml->stringify( ).



IF 1 = 2. ENDIF.
