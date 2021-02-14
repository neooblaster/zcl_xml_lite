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
  methods PRETTIFY
    importing
      !I_PRETTIFY type CHAR1 optional .
  methods SET_EOL
    importing
      !I_NT_EOL type CHAR1 optional
      !I_UX_EOL type CHAR1 optional .
  methods USE_SPACE
    importing
      !I_SIZE type I optional .
  methods USE_TAB .
  methods STRINGIFY
    returning
      value(R_XML_STR) type STRING .
  methods VERSION
    returning
      value(R_VERSION) type STRING .
protected section.
private section.

  types ABAP_BOOL type CHAR1 .

  data _XML_STRING type STRING .
  data _XML_PRETTIFY type ZCL_XML_LITE=>ABAP_BOOL value 'X' ##NO_TEXT.
  data _XML_EOL type ZCL_XML_LITE=>ABAP_BOOL .
  data _XML_EOL_SET type STRING value 'AUTO' ##NO_TEXT.
  data _XML_USE_SPACE type ZCL_XML_LITE=>ABAP_BOOL value 'X' ##NO_TEXT.
  data _XML_TAB_SIZE type I value 4 ##NO_TEXT.
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
  methods _PARSE_ATTRIBUTES
    importing
      !I_TAG type STRING
    returning
      value(R_ATTRIBUTES) type ZT_XML_LITE_ATTRIBUTE_LIST_PAR .
  methods _RENDER_NODE
    importing
      !I_XML_NODE type ref to ZCL_XML_LITE_NODE
      !I_LEVEL type I default 0
    preferred parameter I_XML_NODE .
  methods _RENDER_ATTRIBUTE
    importing
      !I_ATTRIBUTE type ref to ZCL_XML_LITE_ATTRIBUTE
    returning
      value(R_ATTRIBUTE_STR) type STRING .
  methods _RENDER_PROCESS_INSTRUCTION .
  methods _EOL
    returning
      value(R_EOL) type STRING .
  methods _INDENT
    importing
      !I_LEVEL type I
    returning
      value(R_INDENT) type STRING .
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

    DATA : lv_eol_count TYPE i        ,
           ls_opsystem  TYPE opsystem ,
           lv_filsys    TYPE FILESYS  .


    " Storing provided XML in attribute for sharing handling
    me->_xml_string = i_xml_string.


    " Search current EOL
    FIND ALL OCCURRENCES OF |\r\n| IN i_xml_string MATCH COUNT lv_eol_count.

    IF lv_eol_count > 0.
      me->_XML_EOL = 'NT'.
    ELSE.
      FIND ALL OCCURRENCES OF |\n| IN i_xml_string MATCH COUNT lv_eol_count.

      IF lv_eol_count > 0.
        me->_XML_EOL = 'UX'.
      ELSE.
        SELECT SINGLE filesys FROM opsystem INTO lv_filsys WHERE opsys = sy-opsys.

        CASE lv_filsys .
          WHEN 'UNIX'.
            me->_XML_EOL = 'UX'.
          WHEN 'WINDOWS NT'.
            me->_XML_EOL = 'NT'.
          WHEN OTHERS.
            me->_XML_EOL = 'NT'.
        ENDCASE.

      ENDIF.
    ENDIF.


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


  METHOD prettify.
    "
    " If parameter is not provided, calling this method
    " said "use prettify mode"
    "
    " To disable prettify, call this method with import
    " parameter equal to ' ' blank char
    "
    "
    IF i_prettify IS SUPPLIED.
      me->_xml_prettify = i_prettify.
    ELSE.
      me->_xml_prettify = 'X'.
    ENDIF.

  ENDMETHOD.


  method ROOT_NODE.

    r_root_node = me->_root_node.

  endmethod.


  METHOD set_eol.

    IF i_nt_eol IS SUPPLIED AND i_ux_eol IS SUPPLIED.
      me->_xml_eol_set = 'AUTO'.
    ELSEIF i_nt_eol IS SUPPLIED.
      me->_xml_eol_set = 'NT'.
    ELSEIF i_ux_eol IS SUPPLIED.
      me->_xml_eol_set = 'UX'.
    ELSE.
      me->_xml_eol_set = 'AUTO'.
    ENDIF.

  ENDMETHOD.


  METHOD stringify.

    me->_render_process_instruction( ).
    me->_render_node( me->_root_node ).

    r_xml_str = me->_xml_string.

  ENDMETHOD.


  METHOD use_space.

    me->_xml_use_space = 'X'.

    IF i_size IS SUPPLIED.
      me->_xml_tab_size = i_size.
    ENDIF.

  ENDMETHOD.


  method USE_TAB.

    me->_xml_use_space = ' '.

  endmethod.


  method VERSION.

    r_version = zcl_xml_lite=>_version.

  endmethod.


  METHOD _eol.

    DATA : lv_eol_type TYPE string ,
           lv_eol      TYPE string .

    IF me->_xml_prettify EQ 'X'.
      IF me->_xml_eol_set IS NOT INITIAL AND me->_xml_eol_set NE 'AUTO'.
        lv_eol_type = me->_xml_eol_set.
      ELSE.
        lv_eol_type = me->_xml_eol.
      ENDIF.

      CASE lv_eol_type.
        WHEN 'UX' .
          lv_eol = |\n|.
        WHEN 'NT' .
          lv_eol = |\r\n|.
        WHEN OTHERS.
          lv_eol = |\n|.
      ENDCASE.

    ENDIF.

    r_eol = lv_eol.

  ENDMETHOD.


  method _GET_TAG_NAME.
    DATA : lv_tag_name     TYPE        string                  ,
           lr_regex        TYPE REF TO cl_abap_regex           ,
           lr_matcher      TYPE REF TO cl_abap_matcher         ,
           lv_matched      TYPE        zcl_xml_lite=>abap_bool ,
           lt_match_result TYPE        match_result_tab        ,
           lv_beg_offset   TYPE        i                       ,
           lv_match_len    TYPE        i                       .


   " [https://regex101.com/r/qDnROF/latest]
   CREATE OBJECT lr_regex   EXPORTING pattern = '^<\/?\s*(\S+[^\/\s])(.*)>$'.
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


  METHOD _indent.

    DATA : lv_indent      TYPE string ,
           lv_indent_char TYPE string.

    IF me->_xml_prettify EQ 'X'.

      IF me->_xml_use_space EQ 'X'.
        lv_indent_char = repeat( val = | | occ = me->_xml_tab_size ). " ALT+255
      ELSE.
        lv_indent_char = cl_abap_char_utilities=>horizontal_tab.
      ENDIF.

    ENDIF.

    r_indent = repeat( val = lv_indent_char occ = i_level ).

  ENDMETHOD.


  METHOD _parse_attributes.

    DATA : lt_attributes      TYPE          zt_xml_lite_attribute_list_par ,
           ls_attribute       TYPE          zst_xml_lite_attribute_parsed  ,
           lr_regex           TYPE REF TO   cl_abap_regex                  ,
           lr_matcher         TYPE REF TO   cl_abap_matcher                ,
           lv_matched         TYPE          zcl_xml_lite=>abap_bool        ,
           lt_results         TYPE          match_result_tab               ,
           ls_result          TYPE          match_result                   ,
           lv_att_nam_offset  TYPE          i                              ,
           lv_att_nam_length  TYPE          i                              ,
           lv_att_val_offset  TYPE          i                              ,
           lv_att_val_length  TYPE          i                              ,
           lv_att_nam         TYPE          string                         ,
           lv_att_val         TYPE          string                         .

    " Match all couple of "attr=value" with : [https://regex101.com/r/iair6w/latest]
    " $1 = attr=value
    " $2 = attr
    " $3 = value
    "
    CREATE OBJECT lr_regex   EXPORTING pattern = '((\S+)\s*=["'']\s*(\S+)["''])' .
    CREATE OBJECT lr_matcher EXPORTING regex   = lr_regex
                                       text    = i_tag    .

    lt_results = lr_matcher->find_all( ).

    LOOP AT lt_results INTO ls_result.

      " Match $2 = attribute
      lv_att_nam_offset = ls_result-submatches[ 2 ]-offset.
      lv_att_nam_length = ls_result-submatches[ 2 ]-length.

      " Match $3 = value
      lv_att_val_offset = ls_result-submatches[ 3 ]-offset.
      lv_att_val_length = ls_result-submatches[ 3 ]-length.

      lv_att_nam = i_tag+lv_att_nam_offset(lv_att_nam_length).
      lv_att_val = i_tag+lv_att_val_offset(lv_att_val_length).

      ls_attribute-name  = lv_att_nam.
      ls_attribute-value = lv_att_val.

      APPEND ls_attribute TO lt_attributes.

    ENDLOOP.

    r_attributes = lt_attributes.

  ENDMETHOD.


  method _PARSE_NODE.

    DATA : lr_xml_node          TYPE REF TO zcl_xml_lite_node              ,
           lr_xml_child_node    TYPE REF TO zcl_xml_lite_node              ,
           ls_lt_result         TYPE        match_result                   ,
           ls_gt_result         TYPE        match_result                   ,
           lv_lt_offset         TYPE        i                              ,
           lv_gt_offset         TYPE        i                              ,
           lv_tag_candidat_len  TYPE        i                              ,
           lv_tag_candidat      TYPE        string                         ,
           lr_regexp_tag        TYPE REF TO cl_abap_regex                  ,
           lr_matcher           TYPE REF TO cl_abap_matcher                ,
           lv_matched           TYPE        zcl_xml_lite=>abap_bool        ,
           lv_tag_name          TYPE        string                         ,
           lv_child_tag_name    TYPE        string                         ,
           lv_has_child         TYPE        zcl_xml_lite=>abap_bool        ,
           lv_opn_tag_gt_offset TYPE        i                              ,
           lv_clo_tag_lt_offset TYPE        i                              ,
           lv_value_len         TYPE        i                              ,
           lv_value             TYPE        string                         ,
           lv_prev_lt_offset    TYPE        i                              ,
           lv_prev_gt_offset    TYPE        i                              ,
           lt_attributes        TYPE        zt_xml_lite_attribute_list_par ,
           ls_attribute         TYPE        zst_xml_lite_attribute_parsed  .



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

      " Checking if tag candidat represente XML Tag (node) [https://regex101.com/r/POUvnH/latest]
      CREATE OBJECT lr_regexp_tag EXPORTING pattern = '^<\S+(\s+\S+=["''].*["''])?(?:\s*\/)?>$'.
      CREATE OBJECT lr_matcher    EXPORTING regex   = lr_regexp_tag
                                            text    = lv_tag_candidat.

      CALL METHOD lr_matcher->match RECEIVING success = lv_matched.

      " tag_candidate is well formed string
      IF lv_matched EQ zcl_xml_lite=>_abap_true.

        " -----------------------------------------------------------------------------------
        " Is an empty tag ? [https://regex101.com/r/Ccp82r/latest]
        CREATE OBJECT lr_regexp_tag EXPORTING pattern = '^<\S+(\s+\S+=["''].*["''])?\s*\/>$'.
        CREATE OBJECT lr_matcher    EXPORTING regex   = lr_regexp_tag
                                              text    = lv_tag_candidat.

        CALL METHOD lr_matcher->match RECEIVING success = lv_matched.

        IF lv_matched EQ zcl_xml_lite=>_abap_true.

          " We know this node has no child neither value.
          " So we have simply to create node with it attribute
          " and append it
          lv_child_tag_name = me->_get_tag_name( lv_tag_candidat ).
          lr_xml_child_node = new zcl_xml_lite_node( lv_child_tag_name ).
          lr_xml_node->append_child( lr_xml_child_node ).

          " Get parsed attribute (name = value)
          lt_attributes = me->_parse_attributes( lv_tag_candidat ).

          " Create Attribute instance and append it
          LOOP AT lt_attributes INTO ls_attribute.

            lr_xml_child_node->set_attribute(
              i_name  = ls_attribute-name
              i_value = ls_attribute-value
            ).

          ENDLOOP.

          " Now, we have to check the next tag candidat :
          "  - Is the closing tag ?
          "  - Is a new one ? -> new level
          lv_prev_lt_offset =  lv_lt_offset.
          lv_prev_gt_offset =  lv_gt_offset.

          me->_lt_tabix = _lt_tabix + 1.
          me->_gt_tabix = _gt_tabix + 1.

          CONTINUE.

        ENDIF.


        " -----------------------------------------------------------------------------------
        " Is a closing tag ? [https://regex101.com/r/9QflIk/latest]
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
        " Else it's an openning tag (last possibility) ?
        IF lv_matched EQ zcl_xml_lite=>_abap_false.

          " Is a another opening tag ?
          IF lv_tag_name IS NOT INITIAL.
            " Get Child Node
            lr_xml_child_node = me->_parse_node( ).
            lr_xml_node->append_child( lr_xml_child_node ).

            IF lr_xml_node->children( ) IS NOT INITIAL.
              lv_has_child = zcl_xml_lite=>_abap_true.
            ENDIF.

*         " Any tag name found yet.
          ELSE.
            lv_tag_name = me->_get_tag_name( lv_tag_candidat ).
            lr_xml_node->set_node_name( lv_tag_name ).

            " Get parsed attribute (name = value)
            lt_attributes = me->_parse_attributes( lv_tag_candidat ).

            " Create Attribute instance and append it
            LOOP AT lt_attributes INTO ls_attribute.

              lr_xml_node->set_attribute(
                i_name  = ls_attribute-name
                i_value = ls_attribute-value
              ).

            ENDLOOP.

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

    DATA : ls_lt_result              TYPE        match_result                   ,
           ls_gt_result              TYPE        match_result                   ,
           lv_lt_offset              TYPE        i                              ,
           lv_gt_offset              TYPE        i                              ,
           lv_proc_ins_candidat_len  TYPE        i                              ,
           lv_proc_ins_candidat      TYPE        string                         ,
           lr_regexp_tag             TYPE REF TO cl_abap_regex                  ,
           lr_matcher                TYPE REF TO cl_abap_matcher                ,
           lv_matched                TYPE        zcl_xml_lite=>abap_bool        ,
           lt_attributes             TYPE        zt_xml_lite_attribute_list_par ,
           ls_attribute              TYPE        zst_xml_lite_attribute_parsed  ,
           lr_attribute              TYPE REF TO zcl_xml_lite_attribute         ,
           ls_attribute_entry        TYPE        zst_xml_lite_attribute         .


    DO.
      " Get offset for both < and >
      READ TABLE me->_lt_result INTO ls_lt_result INDEX me->_lt_tabix.
      READ TABLE me->_gt_result INTO ls_gt_result INDEX me->_lt_tabix.

      " Making Proc Instruction candidat
      lv_lt_offset = ls_lt_result-offset.
      lv_gt_offset = ls_gt_result-offset.

      lv_proc_ins_candidat_len = lv_gt_offset - lv_lt_offset + 1. " +1 to include > char

      " Get string as Process Instruction candidat
      lv_proc_ins_candidat = me->_xml_string+lv_lt_offset(lv_proc_ins_candidat_len).

      " Checking if tag candidat represente XML Tag (node) [https://regex101.com/r/Zyh27B/latest]
      CREATE OBJECT lr_regexp_tag EXPORTING pattern = '^<\?\s*(?:xml)?(.*)\?>'.
      CREATE OBJECT lr_matcher    EXPORTING regex   = lr_regexp_tag
                                            text    = lv_proc_ins_candidat.

      CALL METHOD lr_matcher->match RECEIVING success = lv_matched.


      IF lv_matched EQ zcl_xml_lite=>_abap_true.

        " Get parsed attribute (name = value)
        lt_attributes = me->_parse_attributes( lv_proc_ins_candidat ).

        " Create Attribute instance and append it
        LOOP AT lt_attributes INTO ls_attribute.

          CLEAR ls_attribute_entry.

          lr_attribute = new zcl_xml_lite_attribute(
            i_name  = ls_attribute-name
            i_value = ls_attribute-value
          ).

          ls_attribute_entry-name      = ls_attribute-name.
          ls_attribute_entry-attribute = lr_attribute.

          APPEND ls_attribute_entry TO me->_attributes.

        ENDLOOP.

        me->_lt_tabix = _lt_tabix + 1.
        me->_gt_tabix = _gt_tabix + 1.

        EXIT.

      ELSE.

        EXIT.

      ENDIF.

    ENDDO.

  endmethod.


  method _RENDER_ATTRIBUTE.

    DATA : lv_attribute TYPE string ,
           lv_att_nam   TYPE string ,
           lv_att_val   TYPE string .

    lv_att_nam = i_attribute->get_name( ).
    lv_att_val = i_attribute->get_value( ).

    CONCATENATE lv_att_nam '="' lv_att_val '"' INTO lv_attribute.

    r_attribute_str = lv_attribute.

  endmethod.


  METHOD _render_node.

    DATA : lv_node_opening TYPE string                  ,
           lv_node_closing TYPE string                  ,
           lv_node_odd     TYPE string                  ,
           lv_node_name    TYPE string                  ,
           lv_attribute    TYPE string                  ,
           lv_attributes   TYPE string                  ,
           ls_attribute    TYPE zst_xml_lite_attribute  ,
           lv_eol          TYPE string                  ,
           lv_indent       TYPE string                  ,
           ls_child_node   TYPE zst_xml_lite_child_node ,
           lv_level        TYPE i                       ,
           lv_node_val     TYPE string                  .


    lv_eol    = me->_eol( ).
    lv_indent = me->_indent( i_level ).

    lv_node_name = i_xml_node->get_node_name( ).
    lv_node_val  = i_xml_node->get_value( ).

    LOOP AT i_xml_node->attributes( ) INTO ls_attribute .

      lv_attribute = me->_render_attribute( ls_attribute-attribute ).

      CONCATENATE lv_attributes lv_attribute INTO lv_attributes SEPARATED BY space.

    ENDLOOP.

    " Empty tag (<node attr="" />)
    IF (
         i_xml_node->get_value( ) IS INITIAL
      OR i_xml_node->get_value( ) EQ '' )
      AND i_xml_node->length( ) EQ 0.
      " Render Odd tag
      CONCATENATE lv_node_name lv_attribute INTO lv_node_opening SEPARATED BY space.
      CONCATENATE me->_xml_string lv_indent '<' lv_node_opening ' />' lv_eol INTO me->_xml_string.


*   " Openning & Closing Tags :
    ELSE.
      " With children
      "   <NODE>
      "     <CHILD></CHILD>
      "   </NODE>
      IF i_xml_node->length( ) > 0.
        " Render Opening tag
        CONCATENATE lv_node_name lv_attribute INTO lv_node_opening SEPARATED BY space.
        CONCATENATE me->_xml_string lv_indent '<' lv_node_opening '>' lv_eol INTO me->_xml_string.

        " Render Children
        LOOP AT i_xml_node->children( ) INTO ls_child_node.
          lv_level = i_level + 1.

          me->_render_node(
            i_xml_node = ls_child_node-node
            i_level    = lv_level
          ).
        ENDLOOP.

        " Render Closing Tag
        CONCATENATE '/' lv_node_name INTO lv_node_closing.
        CONCATENATE me->_xml_string lv_indent '<' lv_node_closing '>' lv_eol INTO me->_xml_string.


*     " Only value :
*     "   <NODE>value</NODE>
      ELSE.
        CONCATENATE lv_node_name lv_attribute INTO lv_node_odd SEPARATED BY space.
        CONCATENATE '<' lv_node_odd '>' lv_node_val '</' lv_node_name '>' INTO lv_node_odd.
        "lv_node_odd = |<| && lv_node_odd && |>| && lv_node_val && |'</| && lv_node_name && |>|.
        CONCATENATE me->_xml_string lv_indent lv_node_odd lv_eol INTO me->_xml_string.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  method _RENDER_PROCESS_INSTRUCTION.

    DATA : lv_eol        TYPE string                 ,
           lv_attribute  TYPE string                 ,
           lv_attributes TYPE string                 ,
           ls_attribute  TYPE zst_xml_lite_attribute .

    LOOP AT me->_attributes INTO ls_attribute .

      lv_attribute = me->_render_attribute( ls_attribute-attribute ).

      CONCATENATE lv_attributes lv_attribute INTO lv_attributes SEPARATED BY space.

    ENDLOOP.

    lv_eol = me->_eol( ).
    CONCATENATE '<?xml' lv_attributes ' ?>' lv_eol INTO me->_xml_string.

  endmethod.
ENDCLASS.
