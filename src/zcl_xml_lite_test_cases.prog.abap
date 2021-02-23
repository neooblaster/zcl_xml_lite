*&---------------------------------------------------------------------*
*& Report ZCL_XML_LITE_TEST_CASES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZCL_XML_LITE_TEST_CASES.

DATA: lo_xml   TYPE REF TO zcl_xml_lite ,
      lv_xml   TYPE        string       .

lv_xml = |<?xml version="1.0" encoding="utf-8" ?>|      &&
         |<NODE_1 lvl="1.0">|                           &&
**lv_xml = |<NODE_1 lvl="1.0">|                           &&
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

*lv_xml = |<?xml version="1.0" encoding="utf-8" ?>|         &&
*         |<NODE_1 lvl="1.0">|                              &&
*         |  <NODE_12 attr="at12"/>|                        &&
*         |  <NODE_13 attr="at12">value_node_13</NODE_13>|  &&
*         |  <NODE_13>value_node_13</NODE_13>|              &&
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
DATA(new_node) = new zcl_xml_lite_node( 'MY_NEW_NODE' ).
new_node->set_value( 'My Node Value' ).
new_node->set_attribute(
  i_name  = 'MyNodeAttr'
  i_value = 'AttrValue'
).
lr_root->append_child( new_node ).



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


" Looping without structure
WHILE lo_xml->root_node( )->next( ) IS NOT INITIAL.

  WRITE : / lo_xml->root_node( )->child( )->get_node_name( ).

ENDWHILE.

DATA(lr_child1) = lo_xml->root_node( )->next( ).
DATA(lr_child2) = lo_xml->root_node( )->next( ).
DATA(lr_child3) = lo_xml->root_node( )->next( ).
DATA(lr_child22) = lo_xml->root_node( )->previous( ).
DATA(lr_child211) = lo_xml->root_node( )->previous( ).


DATA(lv_pname) = lo_xml->root_node( )->child( )->parent( )->get_node_name( ). " NODE_1



*lo_xml->prettify( 'X' ).
*lo_xml->set_eol( i_ux_eol = 'X' ).
*lo_xml->set_eol( i_nt_eol = 'X' ).
*lo_xml->set_eol( i_nt_eol = 'X' i_ux_eol = 'X' ).
*lo_xml->prettify( ' ' ).
*lo_xml->use_space( 4 ).
*lo_xml->use_tab( ).

*write : / lo_xml->stringify( ).

*cl_abap_browser=>show_xml(
*  lo_xml->stringify( )
*"  lv_xml
*).



" Create from scratch
" --- Creation
DATA(lr_xml2)  = new zcl_xml_lite( ).
DATA(lr_root1) = new zcl_xml_lite_node( 'ROOT' ).
DATA(lr_chld1) = new zcl_xml_lite_node( 'CHILD_1_1' ).
DATA(lr_chld2) = new zcl_xml_lite_node( 'CHILD_1_2' ).
DATA(lr_chld3) = new zcl_xml_lite_node( 'CHILD_1_3' ).
DATA(lr_chld4) = new zcl_xml_lite_node( 'CHILD_1_4' ).
DATA(lr_chld5) = new zcl_xml_lite_node( 'CHILD_1_5' ).
DATA(lr_chld6) = new zcl_xml_lite_node( 'CHILD_1_6' ).
DATA(lr_chld7) = new zcl_xml_lite_node( 'CHILD_1_7' ).

" --- Appends
lr_xml2->set_root_node( lr_root1 ).   " Append Root node to XML document
lr_root1->append_child( lr_chld1 ).   " Append the 1st child                          " 1 /                             " OK
lr_root1->append_child( lr_chld2 ).   " Append the 2nd child                          " 1 / 2                           " OK
lr_root1->append_child( lr_chld3 ).   " Append the 3rd child                          " 1 / 2 / 3                       " OK

"<<< Set handler on 2nd
lr_root1->next( ). " idx 0 --> 1 (CHILD_1_1)
lr_root1->next( ). " idx 1 --> 2 (CHILD_1_2)
DATA(hdl_name1) = lr_root1->child( )->get_node_name( ).                               " CHILD_1_2                       " OK
DATA(hdl_psib1) = lr_root1->previous_sibling( lr_root1->child( ) )->get_node_name( ). " CHILD_1_1                       " OK
DATA(hdl_nsib1) = lr_root1->next_sibling( lr_root1->child( ) )->get_node_name( ).     " CHILD_1_3                       " OK
">>> Set handler on 2nd

lr_root1->insert_before(              " Insert 4th child before 2nd child             " 1 / 4 / [2] / 3                 " OK
  i_new_node = lr_chld4
  i_ref_node = lr_chld2
).                 " idx 2 --> 3

"<<< Check handle
DATA(hdl_name2) = lr_root1->child( )->get_node_name( ).                               " CHILD_1_2                       " OK
DATA(hdl_psib2) = lr_root1->previous_sibling( lr_root1->child( ) )->get_node_name( ). " CHILD_1_4                       " OK
DATA(hdl_nsib2) = lr_root1->next_sibling( lr_root1->child( ) )->get_node_name( ).     " CHILD_1_3                       " OK
">>> Check handle

lr_root1->insert_after(               " Insert 5th child after                        " 1 / 4 / 5 / [2] / 3             " OK
  i_new_node = lr_chld5                                                          " idx: 1   2   3    4    5
  i_ref_node = lr_chld4
).                 " idx 3 --> 4

"<<< Check handle
DATA(hdl_name3) = lr_root1->child( )->get_node_name( ).                               " CHILD_1_2                       " OK
DATA(hdl_psib3) = lr_root1->previous_sibling( lr_root1->child( ) )->get_node_name( ). " CHILD_1_5                       " OK
DATA(hdl_nsib3) = lr_root1->next_sibling( lr_root1->child( ) )->get_node_name( ).     " CHILD_1_3                       " OK
">>> Check handle

lr_root1->insert_before(              " Insert 6th child before 5th                   " 1 / 4 / 6 / 5 / [2] / 3         " OK
  i_new_node   = lr_chld6                                                        " idx: 1   2   3   4    5    6
  i_index_node = 3                    " ATM index 3 = 5th
).                 " idx 4 --> 5

"<<< Check handle
DATA(hdl_name4) = lr_root1->child( )->get_node_name( ).                               " CHILD_1_2                       " OK
DATA(hdl_psib4) = lr_root1->previous_sibling( lr_root1->child( ) )->get_node_name( ). " CHILD_1_5                       " OK
DATA(hdl_nsib4) = lr_root1->next_sibling( lr_root1->child( ) )->get_node_name( ).     " CHILD_1_3                       " OK
">>> Check handle

lr_root1->insert_after(               " Insert 7th child after 6th                    " 1 / 4 / 6 / 7 / 5 / [2] / 3     " OK
  i_new_node   = lr_chld7                                                        " idx: 1   2   3   4   5    6    7
  i_index_node = 3
).                 " idx 5 --> 6

"<<< Check handle
DATA(hdl_name5) = lr_root1->child( )->get_node_name( ).                               " CHILD_1_2                       " OK
DATA(hdl_psib5) = lr_root1->previous_sibling( lr_root1->child( ) )->get_node_name( ). " CHILD_1_5                       " OK
DATA(hdl_nsib5) = lr_root1->next_sibling( lr_root1->child( ) )->get_node_name( ).     " CHILD_1_3                       " OK
">>> Check handle


" --- Display
cl_abap_browser=>show_xml(
  lr_xml2->stringify( )
).


" Removing
lr_root1->remove_child( ).                                                            " 1 / 4 / 6 / 7 / [5] / 3         " OK

"<<< Check handle
DATA(hdl_name6) = lr_root1->child( )->get_node_name( ).                               " CHILD_1_5                       " OK
DATA(hdl_psib6) = lr_root1->previous_sibling( lr_root1->child( ) )->get_node_name( ). " CHILD_1_7                       " OK
DATA(hdl_nsib6) = lr_root1->next_sibling( lr_root1->child( ) )->get_node_name( ).     " CHILD_1_3                       " OK
">>> Check handle

lr_root1->remove_child( i_child_node_index = 4 ).                                     " 1 / 4 / 6 / [5] / 3            " OK

"<<< Check handle
DATA(hdl_name7) = lr_root1->child( )->get_node_name( ).                               " CHILD_1_5                       " OK
DATA(hdl_psib7) = lr_root1->previous_sibling( lr_root1->child( ) )->get_node_name( ). " CHILD_1_6                       " OK
DATA(hdl_nsib7) = lr_root1->next_sibling( lr_root1->child( ) )->get_node_name( ).     " CHILD_1_3                       " OK
">>> Check handle

lr_root1->remove_child( i_child_node = lr_chld3 ).                                    " 1 / 4 / 6 / [5]                 " OK

"<<< Check handle
DATA(hdl_name8) = lr_root1->child( )->get_node_name( ).                               " CHILD_1_5                       " OK
DATA(hdl_psib8) = lr_root1->previous_sibling( lr_root1->child( ) )->get_node_name( ). " CHILD_1_6                       " OK

DATA: lr_nsib8 TYPE REF TO zcl_xml_lite_node.
lr_nsib8 = lr_root1->next_sibling( lr_root1->child( ) ).
IF lr_nsib8 IS NOT INITIAL.
DATA(hdl_nsib8) = lr_nsib8->get_node_name( ).                                         " INITIAL                         " OK
ENDIF.
">>> Check handle

lr_root1->previous( ).
lr_root1->remove_before( i_ref_node = lr_chld4 ).                                     " 4 / [6] / 5

"<<< Check handle
DATA(hdl_name9) = lr_root1->child( )->get_node_name( ).                               " CHILD_1_6                       " OK
DATA(hdl_psib9) = lr_root1->previous_sibling( lr_root1->child( ) )->get_node_name( ). " CHILD_1_4                       " OK
DATA(hdl_nsib9) = lr_root1->next_sibling( lr_root1->child( ) )->get_node_name( ).     " CHILD_1_5                       " OK
">>> Check handle

lr_root1->remove_after( i_index_node = 1 ).                                           " [4] / 5

"<<< Check handle
DATA(hdl_name10) = lr_root1->child( )->get_node_name( ).                              " CHILD_1_6                       " OK
DATA: lr_psib10 TYPE REF TO zcl_xml_lite_node.
lr_psib10 = lr_root1->previous_sibling( lr_root1->child( ) ).
IF lr_psib10 IS NOT INITIAL.
DATA(hdl_psib10) = lr_psib10->get_node_name( ).
ENDIF.
DATA(hdl_nsib10) = lr_root1->next_sibling( lr_root1->child( ) )->get_node_name( ).    " CHILD_1_5                       " OK
">>> Check handle

" --- Display
cl_abap_browser=>show_xml(
  lr_xml2->stringify( )
).






DATA(lr_xml3) = new zcl_xml_lite( ).
DATA(lr_View)  = new zcl_xml_lite_node( 'mvc:View' ).
lr_View->set_attribute(
  i_name  = 'controllerName'
  i_value = 'sap.m.sample.ActionSheet.controller.ActionSheet'
).
lr_View->set_attribute(
  i_name  = 'xmlns:mvc'
  i_value = 'sap.ui.core.mvc'
).
lr_View->set_attribute(
  i_name  = 'xmlns:core'
  i_value = 'sap.ui.core'
).
lr_View->set_attribute(
  i_name  = 'xmlns'
  i_value = 'sap.m'
).
DATA(lr_Button)  = new zcl_xml_lite_node( 'Button' ).
lr_Button->set_attribute(
  i_name  = 'text'
  i_value = 'Open Action Sheet'
).
lr_Button->set_attribute(
  i_name  = 'class'
  i_value = 'sapUiSmallMargin'
).
lr_Button->set_attribute(
  i_name  = 'press'
  i_value = '.onButtonPress'
).
lr_Button->set_attribute(
  i_name  = 'ariaHasPopup'
  i_value = 'Menu'
).
lr_Button->remove_attribute( 'ariaHasPopup' ).
DATA(lr_dependents) = new zcl_xml_lite_node( 'dependents' ).
DATA(lr_Fragment)   = new zcl_xml_lite_node( 'core:Fragment' ).
lr_Fragment->set_attribute(
  i_name  = 'fragmentName'
  i_value = 'sap.m.sample.ActionSheet.view.ActionSheet'
).
lr_Fragment->set_attribute(
  i_name  = 'type'
  i_value = 'XML'
).

lr_xml3->set_root_node( lr_View ).
lr_View->append_child( lr_Button ).
lr_Button->append_child( lr_dependents ).
lr_dependents->append_child( lr_Fragment ).


" --- Display
DATA(lv_xml_view) = lr_xml3->stringify( ).
cl_abap_browser=>show_xml(
  lr_xml3->stringify( )
).


DATA(lr_clone) = lr_View->clone( ).






IF 1 = 2. ENDIF.



" Usage list :

" --- zcl_xml_lite_node
"
" new zcl_xml_lite_node( ).
" new zcl_xml_lite_node( 'node_name' ).
" new zcl_xml_lite_node( i_parent_node = lr_node )
" new zcl_xml_lite_node( i_node_name = 'node_name' i_parent_node = lr_node )
"
" lr_node TYPE REF TO zcl_xml_lite_node
" lr_node->set_parent_node( lr_parent_node ).
"
" lr_node->get_parent_node( ).
"
" lr_node->set_node_name( 'node_name' ).
"
" lr_node->get_node_name( ).
"
" lr_node->set_attribute
"   i_name  = 'attribute_name'
"   i_value = 'attribute_value'
" ).
"
" lr_node->get_attribute( 'attribut_name' ).
"
" lr_node->get_attribut_value( 'attribute_name' ).
"
" lr_node->attributes( ).
"
" lr_node->set_value( 'node_value' ).
"
" lr_node->get_value( ).
"
" lr_node->append_child( node ).
"
" lr_node->insert_before( new_node).            " Insert new node before current handled child node
" lr_node->insert_before( new_node, ref_node ). " Insert new node before provided node (by reference)
" lr_node->insert_before( new_node, 1 ).        " Insert new node before node corresponding to provided index
"
" lr_node->insert_after( new_node ).            " Insert new node after current handled child node
" lr_node->insert_after( new_node, ref_node ).  " Insert new node after provided node (by reference)
" lr_node->insert_after( new_node, 1 ).         " Insert new node after node corresponding to provided index
"
" lr_node->remove_child( ).      " Remove current handled child node
" lr_node->remove_child( node ). " Remove provided children node (by reference)
" lr_node->remove_child( 1 ).    " Remove child with provided index
"
" lr_node->children( ).
"
" lr_node->length( ).
"
" lr_node->next( ).
"
" lr_node->previous( ).
"
" lr_node->reset( )      " Set index to 0
" lr_node->reset( 0 )    " Set index to 0
" lr_node->reset( 5 )    " Set index to 5 , if there is no 5 children, set index to the last children
" lr_node->reset( -1 )   " Set index to the last children
"
" lr_node->child( )      " Get handled children using index
"
" lr_node->parent( )     " Returning parent node (shorten version of get_parent_node)
