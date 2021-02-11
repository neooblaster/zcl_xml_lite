class ZCL_XML_LITE definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      value(I_XML_STRING) type STRING .
  methods ATTRIBUTE
    importing
      !I_ATTRIBUTE_NAME type ZDT_XML_LITE_ATTRIBUTE_NAME
    returning
      value(R_ATTRIBUTE) type ref to ZCL_XML_LITE_ATTRIBUTE .
  methods ATTRIBUTES
    returning
      value(R_ATTRIBUTES) type ZT_XML_LITE_ATTRIBUTE_LIST .
  methods ROOT_NODE
    returning
      value(R_ROOT_NODE) type ref to ZCL_XML_LITE_NODE .
  methods NODE
    importing
      !I_NODE_NAME type ZDT_XML_LITE_NODE_NAME
    returning
      value(R_NODE) type ref to ZCL_XML_LITE_NODE .
  methods NODES
    importing
      value(I_NODE_NAME) type ZDT_XML_LITE_NODE_NAME optional
    returning
      value(R_NODES) type ZT_XML_LITE_CHILD_LIST .
  methods VERSION
    returning
      value(R_VERSION) type STRING .
protected section.
private section.

  types ABAP_BOOL type CHAR1 .

  data _XML_STRING type STRING .
  data _LT_RESULT type MATCH_RESULT_TAB .
  data _LT_RESULT_LEN type I value 0 ##NO_TEXT.
  data _LT_TABIX type I value 1 ##NO_TEXT.
  data _LT_TABIX_PREV type I value 0 ##NO_TEXT.
  data _GT_RESULT type MATCH_RESULT_TAB .
  data _GT_TABIX type I value 1 ##NO_TEXT.
  data _GT_TABIX_PREV type I value 0 ##NO_TEXT.
  data _GT_RESULT_LEN type I value 0 ##NO_TEXT.
  data _PARSING_NODE_LEVEL type I value 0 ##NO_TEXT.
  data _ROOT_NODE type ref to ZCL_XML_LITE_NODE .
  data _ATTRIBUTES type ZT_XML_LITE_ATTRIBUTE_LIST .
  class-data _ABAP_TRUE type ZCL_XML_LITE=>ABAP_BOOL value 'X' ##NO_TEXT.
  class-data _ABAP_FALSE type ZCL_XML_LITE=>ABAP_BOOL value ' ' ##NO_TEXT.
  class-data _VERSION type STRING value 'V0.1.0' ##NO_TEXT.

  methods _GET_TAG_NAME
    importing
      value(I_TAG_NODE) type STRING
    returning
      value(R_TAG_NAME) type STRING .
  methods _PARSE_NODE
    returning
      value(R_NODE) type ref to ZCL_XML_LITE_NODE .
  methods _PARSE_PROCESS_INSTRUCTION .
ENDCLASS.



CLASS ZCL_XML_LITE IMPLEMENTATION.


  method ATTRIBUTE.
  endmethod.


  method ATTRIBUTES.

    r_attributes = me->_attributes.

  endmethod.


  METHOD constructor.

    " #------------------------------------------------------------#
    " | #--------------------------------------------------------# |
    " | |                                                        | |
    " | |                     ZCL_XML_LITE                       | |
    " | |                                                        | |
    " | |                 v0.1.0 -- 2021.02.11                   | |
    " | |                                                        | |
    " | #--------------------------------------------------------# |
    " #------------------------------------------------------------#

    " #------------------------------------------------------------#
    " #---[ Source ]-----------------------------------------------#
    " #------------------------------------------------------------#
    " |                                                            |
    " |      https://github.com/neooblaster/zcl_xml_lite           |
    " |                                                            |
    " #------------------------------------------------------------#

    " #------------------------------------------------------------#
    " #---[ Description ]------------------------------------------#
    " #------------------------------------------------------------#
    " |                                                            |
    " | Please Confer to report (SE38) ZCL_XML_LITE_EXAMPLES       |
    " | to find all existing example to works with ZCL_XML_LITE    |
    " | and subclasses.                                            |
    " |                                                            |
    " -------------------------------------------------------------#
    " -------------------------------------------------------------#

    " Storing provided XML in attribute for sharing handling
    me->_xml_string = i_xml_string.

    " Remove New Line char (Unix & NT)
    REPLACE ALL OCCURRENCES OF |\r| IN i_xml_string WITH space.
    REPLACE ALL OCCURRENCES OF |\n| IN i_xml_string WITH space.

    " Working with LT & GT sign
    FIND ALL OCCURRENCES OF '<' IN i_xml_string RESULTS me->_lt_result.
    FIND ALL OCCURRENCES OF '>' IN i_xml_string RESULTS me->_gt_result.

    " Initialization of processing tabix
    DESCRIBE TABLE me->_lt_result LINES me->_lt_result_len.
    DESCRIBE TABLE me->_gt_result LINES me->_gt_result_len.

    " Checking for "Processing Instructions" (src: https://www.w3.org/TR/xml/#sec-pi)
    me->_parse_process_instruction( ).

    " Start parsing XML to build all node
    me->_root_node = me->_parse_node( ).

    " Once instanciated, removing XML String to free memory
    CLEAR   : me->_xml_string .
    REFRESH : me->_lt_result  ,
              me->_gt_result  .

  ENDMETHOD.


  method NODE.

    DATA : ls_node TYPE zst_xml_lite_child_node .

    READ TABLE me->_root_node->children( ) INTO ls_node WITH TABLE KEY name = i_node_name.

    r_node = ls_node-node.

  endmethod.


  method NODES.

    DATA : lt_nodes TYPE zt_xml_lite_child_list  ,
           ls_node  TYPE zst_xml_lite_child_node .

    IF i_node_name IS SUPPLIED.
      LOOP AT me->_root_node->children( ) INTO ls_node WHERE name = i_node_name.
        APPEND ls_node TO lt_nodes.
      ENDLOOP.

      r_nodes = lt_nodes.
    ELSE.
      r_nodes = me->_root_node->children( ).
    ENDIF.

  endmethod.


  method ROOT_NODE.

    r_root_node = me->_root_node.

  endmethod.


  method VERSION.

    r_version = zcl_xml_lite=>_version.

  endmethod.


  method _GET_TAG_NAME.
    DATA : lv_tag_name     TYPE        string                  ,
           lr_regex        TYPE REF TO cl_abap_regex           ,
           lr_matcher      TYPE REF TO cl_abap_matcher         ,
           lv_matched      TYPE        zcl_xml_lite=>abap_bool ,
           lt_match_result TYPE        match_result_tab        ,
           lv_beg_offset   TYPE        i                       ,
           lv_match_len    TYPE        i                       .


   CREATE OBJECT lr_regex   EXPORTING pattern = '^<\/?\s*(\S+)(.*)>$'.
   CREATE OBJECT lr_matcher EXPORTING regex   = lr_regex
                                      text    = i_tag_node .

   CALL METHOD lr_matcher->match RECEIVING success = lv_matched.

   IF lv_matched EQ zcl_xml_lite=>_abap_true.
     lt_match_result = lr_matcher->find_all( ).

     lv_beg_offset = lt_match_result[ 1 ]-submatches[ 1 ]-offset.
     lv_match_len  = lt_match_result[ 1 ]-submatches[ 1 ]-length.

     lv_tag_name = i_tag_node+lv_beg_offset(lv_match_len).

   ELSE.
   ENDIF.

    r_tag_name = lv_tag_name.

  endmethod.


  method _PARSE_NODE.

    DATA : lr_xml_node          TYPE REF TO zcl_xml_lite_node       ,
           lr_xml_child_node    TYPE REF TO zcl_xml_lite_node       ,
           ls_lt_result         TYPE        match_result            ,
           ls_gt_result         TYPE        match_result            ,
           lv_lt_offset         TYPE        i                       ,
           lv_gt_offset         TYPE        i                       ,
           lv_tag_candidat_len  TYPE        i                       ,
           lv_tag_candidat      TYPE        string                  ,
           lr_regexp_tag        TYPE REF TO cl_abap_regex           ,
           lr_matcher           TYPE REF TO cl_abap_matcher         ,
           lv_matched           TYPE        zcl_xml_lite=>abap_bool ,
           lv_tag_name          TYPE        string                  ,
           lv_has_child         TYPE        zcl_xml_lite=>abap_bool ,
           lv_opn_tag_gt_offset TYPE        i                       ,
           lv_clo_tag_lt_offset TYPE        i                       ,
           lv_value_len         TYPE        i                       ,
           lv_value             TYPE        string                  ,
           lv_prev_lt_offset    TYPE        i                       ,
           lv_prev_gt_offset    TYPE        i                       .



    " Create a XML_NODE instance
    lr_xml_node = new zcl_xml_lite_node( ).


    " Searching for node  :
    "   - The number of < and > can not be contain somes of value field.
    "   - So we have to "try" to indentify a tag candidat
    DO.
      " Loop Control
      IF me->_lt_tabix > me->_lt_result_len.
        EXIT.
      ENDIF.

      " Get offset for both < and >
      READ TABLE me->_lt_result INTO ls_lt_result INDEX me->_lt_tabix.
      READ TABLE me->_gt_result INTO ls_gt_result INDEX me->_lt_tabix.

      " Making tag candidat
      lv_lt_offset = ls_lt_result-offset.
      lv_gt_offset = ls_gt_result-offset.

      lv_tag_candidat_len = lv_gt_offset - lv_lt_offset + 1. " +1 to include > char

      " Get string as tag candidat
      lv_tag_candidat = me->_xml_string+lv_lt_offset(lv_tag_candidat_len).

      " Checking if tag candidat represente XML Tag (node)
      CREATE OBJECT lr_regexp_tag EXPORTING pattern = '^<\S+(\s+\S+=["''].*["''])?>$'.
      CREATE OBJECT lr_matcher    EXPORTING regex   = lr_regexp_tag
                                            text    = lv_tag_candidat.

      CALL METHOD lr_matcher->match RECEIVING success = lv_matched.

      " tag_candidate is well formed string
      IF lv_matched EQ zcl_xml_lite=>_abap_true.

        " -----------------------------------------------------------------------------------
        " Is a closing tag ?
        CREATE OBJECT lr_regexp_tag EXPORTING pattern = '^<\/\S+(\s+\S+=["''].*["''])?>$'.
        CREATE OBJECT lr_matcher    EXPORTING regex   = lr_regexp_tag
                                              text    = lv_tag_candidat.

        CALL METHOD lr_matcher->match RECEIVING success = lv_matched.

        IF lv_matched EQ zcl_xml_lite=>_abap_true.

          " When it's a closing tag
          " If the current node has children, it can not have a value
          IF lv_has_child NE zcl_xml_lite=>_abap_true.

            " Find the > of the opening char and the < of the closing char.
            " Value is between these two offet
            lv_opn_tag_gt_offset = lv_prev_gt_offset + 1.
            lv_clo_tag_lt_offset = lv_lt_offset.

            lv_value_len = lv_clo_tag_lt_offset - lv_opn_tag_gt_offset.
            lv_value = me->_xml_string+lv_opn_tag_gt_offset(lv_value_len).

            lr_xml_node->set_value( lv_value ).

*         "
          ELSE.

          ENDIF.

          EXIT.

        ENDIF.


        " -----------------------------------------------------------------------------------
        " Else it's an openning tag ?
        IF lv_matched EQ zcl_xml_lite=>_abap_false.

          " Is a another opening tag ?
          IF lv_tag_name IS NOT INITIAL.
            " Get Child Node
            lr_xml_child_node = me->_parse_node( ).
            lr_xml_child_node->set_parent_node( lr_xml_node ).
            lr_xml_node->append_child( lr_xml_child_node ).

            IF lr_xml_node->children( ) IS NOT INITIAL.
              lv_has_child = zcl_xml_lite=>_abap_true.
            ENDIF.

*         " Any tag name found yet.
          ELSE.
            lv_tag_name = me->_get_tag_name( lv_tag_candidat ).
            lr_xml_node->set_node_name( lv_tag_name ).

          ENDIF.

          " Now, we have to check the next tag candidat :
          "  - Is the closing tag ?
          "  - Is a new one ? -> new level
          lv_prev_lt_offset =  lv_lt_offset.
          lv_prev_gt_offset =  lv_gt_offset.

          me->_lt_tabix = _lt_tabix + 1.
          me->_gt_tabix = _gt_tabix + 1.

          CONTINUE.

        ENDIF.

*     " Is not a valide XML tag (@TODO)
      ELSE.
        " tmp debug
        me->_lt_tabix = _lt_tabix + 1.

      ENDIF.

    ENDDO.

    " Return build XML_NODE instance
    r_node = lr_xml_node.

  endmethod.


  method _PARSE_PROCESS_INSTRUCTION.

    DATA :
*           lr_xml_node          TYPE REF TO zcl_xml_lite_node       ,
*           lr_xml_child_node    TYPE REF TO zcl_xml_lite_node       ,
           ls_lt_result              TYPE        match_result            ,
           ls_gt_result              TYPE        match_result            ,
           lv_lt_offset              TYPE        i                       ,
           lv_gt_offset              TYPE        i                       ,
           lv_proc_ins_candidat_len  TYPE        i                       ,
           lv_proc_ins_candidat      TYPE        string                  ,
           lr_regexp_tag             TYPE REF TO cl_abap_regex           ,
           lr_matcher                TYPE REF TO cl_abap_matcher         ,
           lv_matched                TYPE        zcl_xml_lite=>abap_bool
*           lv_tag_name          TYPE        string                  ,
*           lv_has_child         TYPE        zcl_xml_lite=>abap_bool ,
*           lv_opn_tag_gt_offset TYPE        i                       ,
*           lv_clo_tag_lt_offset TYPE        i                       ,
*           lv_value_len         TYPE        i                       ,
*           lv_value             TYPE        string                  ,
*           lv_prev_lt_offset    TYPE        i                       ,
*           lv_prev_gt_offset    TYPE        i
           .


    "
*    DO.
*      " Get offset for both < and >
*      READ TABLE me->_lt_result INTO ls_lt_result INDEX me->_lt_tabix.
*      READ TABLE me->_gt_result INTO ls_gt_result INDEX me->_lt_tabix.
*
*      " Making Proc Instruction candidat
*      lv_lt_offset = ls_lt_result-offset.
*      lv_gt_offset = ls_gt_result-offset.
*
*      lv_proc_ins_candidat_len = lv_gt_offset - lv_lt_offset + 1. " +1 to include > char
*
*      " Get string as Process Instruction candidat
*      lv_proc_ins_candidat = me->_xml_string+lv_lt_offset(lv_proc_ins_candidat_len).
*
*      " Checking if tag candidat represente XML Tag (node)
*      CREATE OBJECT lr_regexp_tag EXPORTING pattern = '^<?(.*)?>'.
*      CREATE OBJECT lr_matcher    EXPORTING regex   = lr_regexp_tag
*                                            text    = lv_proc_ins_candidat.
*
*      CALL METHOD lr_matcher->match RECEIVING success = lv_matched.
*
*
*      IF lv_matched EQ zcl_xml_lite=>_abap_true.
*
*
*      ELSE.
*        " We check if we get something which looks like to a tag
*        CREATE OBJECT lr_regexp_tag EXPORTING pattern = '^<\S+(\s+\S+=["''].*["''])?>$'.
*        CREATE OBJECT lr_matcher    EXPORTING regex   = lr_regexp_tag
*                                              text    = lv_proc_ins_candidat.
*
*        CALL METHOD lr_matcher->match RECEIVING success = lv_matched.
*
*        IF lv_matched EQ zcl_xml_lite=>_abap_true.
*          EXIT.
*
*        ELSE.
*        ENDIF.
*
*      ENDIF.
*
*    ENDDO.

  endmethod.
ENDCLASS.
