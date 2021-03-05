DATA : lv_xml_string TYPE string .

" TEST :
" - [X] APPEND
" - [X] INSERT_BEFORE
" - [X] INSERT_AFTER
" - [X] REMOVE_CHILD
" - [X] REMOVE_ME
" - [X] REMOVE_BEFORE
" - [X] REMOVE_AFTER
" - [X] REMOVE_PREVIOUS_SIBLING
" - [X] REMOVE_NEXT_SIBLING
" - [X] NEXT_CHILD
" - [X] NEXT_SIBLING
" - [X] PREVIOUS_CHILD
" - [X] PREVIOUS_SIBLING

lv_xml_string = |<ROOT>|  &&
                |   <CHILD_1 />| &&
                |   <CHILD_2 />| &&
                |   <CHILD_3 />| &&
                |   <CHILD_4 />| &&
                |   <CHILD_5 />| &&
                |</ROOT>| .

DATA(lr_xmlx)  = new zcl_xml_lite( lv_xml_string ).
DATA(lr_xmlz)  = new zcl_xml_lite( ).
DATA(lr_xmlrt) = lr_xmlx->root_node( ).
lr_xmlrt->next( ). " => C1
lr_xmlrt->next( ). " => C2
lr_xmlrt->next( ). " => C3
DATA(lr_nc3)   = lr_xmlrt->child( ).
DATA(lr_nn) = new zcl_xml_lite_node( 'CHILD_X' ).
DATA(lr_rt) = new zcl_xml_lite_node( 'ROOT' ).

" lr_xmlrt->append_child( lr_nn ). " OK
"lr_xmlrt->insert_before( lr_nn ).
"lr_xmlrt->insert_before( i_new_node = lr_nn i_ref_node = lr_nc3 ).
"lr_xmlrt->insert_before( i_new_node = lr_nn i_index_node = 3 ).
"lr_xmlrt->insert_after( lr_nn ).
"lr_xmlrt->insert_after( i_new_node = lr_nn i_ref_node = lr_nc3 ).
"lr_xmlrt->insert_after( i_new_node = lr_nn i_index_node = 3 ).
"lr_xmlrt->remove_child( ).
"lr_xmlrt->remove_child( lr_nc3 ).
"lr_xmlrt->remove_child( i_child_node_index = 3 ).
"lr_nc3->remove_me( ).
"lr_xmlrt->remove_before( ).
"lr_xmlrt->remove_before( lr_nc3 ).
"lr_xmlrt->remove_before( i_index_node = 3 ).
"lr_xmlrt->remove_after( ).
"lr_xmlrt->remove_after( lr_nc3 ).
"lr_xmlrt->remove_after( i_index_node = 3 ).
"lr_nc3->remove_previous_sibling( ).
"lr_nc3->remove_next_sibling( ).

"DATA(lr_getn) = lr_nc3->previous_sibling( ).
"DATA(lr_getn) = lr_nc3->next_sibling( ).
"DATA(lr_getn) = lr_xmlrt->next_child( ).
"DATA(lr_getn) = lr_xmlrt->next_child( lr_nc3 ).
"DATA(lr_getn) = lr_xmlrt->next_child( i_index_node = 3 ).
"DATA(lr_getn) = lr_xmlrt->previous_child( ).
"DATA(lr_getn) = lr_xmlrt->previous_child( lr_nc3 ).
"DATA(lr_getn) = lr_xmlrt->previous_child( i_index_node = 3 ).

lr_xmlz->set_root_node( lr_rt ).
lr_rt->append_child( lr_getn ).

"cl_abap_browser=>show_xml( lr_xmlx->stringify( ) ).
cl_abap_browser=>show_xml( lr_xmlz->stringify( ) ).

