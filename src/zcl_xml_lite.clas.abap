CLASS zcl_xml_lite DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

PUBLIC SECTION.

  METHODS constructor
    IMPORTING
      VALUE(i_xml_string) TYPE string OPTIONAL .
  METHODS attribute
    IMPORTING
      !i_attribute_name TYPE zdt_xml_lite_attribute_name
    RETURNING
      VALUE(r_attribute) TYPE REF TO zcl_xml_lite_attribute .
  METHODS attributes
    RETURNING
      VALUE(r_attributes) TYPE zt_xml_lite_attribute_list .
  METHODS set_attribute
    IMPORTING
      !i_name TYPE string
      !i_value TYPE string .
  METHODS root_node
    RETURNING
      VALUE(r_root_node) TYPE REF TO zcl_xml_lite_node .
  METHODS node
    IMPORTING
      !i_node_name TYPE zdt_xml_lite_node_name
    RETURNING
      VALUE(r_node) TYPE REF TO zcl_xml_lite_node .
  METHODS nodes
    IMPORTING
      VALUE(i_node_name) TYPE zdt_xml_lite_node_name OPTIONAL
    RETURNING
      VALUE(r_nodes) TYPE zt_xml_lite_child_list .
  METHODS set_root_node
    IMPORTING
      !i_root_node TYPE REF TO zcl_xml_lite_node .
  METHODS prettify
    IMPORTING
      !i_prettify TYPE char1 OPTIONAL .
  METHODS set_eol
    IMPORTING
      !i_nt_eol TYPE char1 OPTIONAL
      !i_ux_eol TYPE char1 OPTIONAL .
  METHODS use_space
    IMPORTING
      !i_size TYPE i OPTIONAL .
  METHODS use_tab .
  METHODS stringify
    RETURNING
      VALUE(r_xml_str) TYPE string .
  METHODS version
    RETURNING
      VALUE(r_version) TYPE string .
PROTECTED SECTION.
PRIVATE SECTION.

  TYPES abap_bool TYPE char1 .

  DATA _xml_string TYPE string .
  DATA _xml_prettify TYPE zcl_xml_lite=>abap_bool VALUE 'X' ##NO_TEXT.
  DATA _xml_eol TYPE zcl_xml_lite=>abap_bool .
  DATA _xml_eol_set TYPE string VALUE 'AUTO' ##NO_TEXT.
  DATA _xml_use_space TYPE zcl_xml_lite=>abap_bool VALUE 'X' ##NO_TEXT.
  DATA _xml_tab_size TYPE i VALUE 4 ##NO_TEXT.
  DATA _lt_result TYPE match_result_tab .
  DATA _lt_result_len TYPE i VALUE 0 ##NO_TEXT.
  DATA _lt_tabix TYPE i VALUE 1 ##NO_TEXT.
  DATA _lt_tabix_prev TYPE i VALUE 0 ##NO_TEXT.
  DATA _gt_result TYPE match_result_tab .
  DATA _gt_tabix TYPE i VALUE 1 ##NO_TEXT.
  DATA _gt_tabix_prev TYPE i VALUE 0 ##NO_TEXT.
  DATA _gt_result_len TYPE i VALUE 0 ##NO_TEXT.
  DATA _parsing_node_level TYPE i VALUE 0 ##NO_TEXT.
  DATA _root_node TYPE REF TO zcl_xml_lite_node .
  DATA _attributes TYPE zt_xml_lite_attribute_list .
  CLASS-DATA _abap_true TYPE zcl_xml_lite=>abap_bool VALUE 'X' ##NO_TEXT.
  CLASS-DATA _abap_false TYPE zcl_xml_lite=>abap_bool VALUE ' ' ##NO_TEXT.
  CLASS-DATA _version TYPE string VALUE 'V0.1.0' ##NO_TEXT.

  METHODS _get_tag_name
    IMPORTING
      VALUE(i_tag_node) TYPE string
    RETURNING
      VALUE(r_tag_name) TYPE string .
  METHODS _parse_node
    RETURNING
      VALUE(r_node) TYPE REF TO zcl_xml_lite_node .
  METHODS _parse_process_instruction .
  METHODS _parse_attributes
    IMPORTING
      !i_tag TYPE string
    RETURNING
      VALUE(r_attributes) TYPE zt_xml_lite_attribute_list_par .
  METHODS _render_node
    IMPORTING
      !i_xml_node TYPE REF TO zcl_xml_lite_node
      !i_level TYPE i DEFAULT 0
    PREFERRED PARAMETER i_xml_node .
  METHODS _render_attribute
    IMPORTING
      !i_attribute TYPE REF TO zcl_xml_lite_attribute
    RETURNING
      VALUE(r_attribute_str) TYPE string .
  METHODS _render_process_instruction .
  METHODS _eol
    RETURNING
      VALUE(r_eol) TYPE string .
  METHODS _indent
    IMPORTING
      !i_level TYPE i
    RETURNING
      VALUE(r_indent) TYPE string .
ENDCLASS.



CLASS ZCL_XML_LITE IMPLEMENTATION.


  METHOD attribute.
  ENDMETHOD.


  METHOD attributes.

    r_attributes = me->_attributes.

  ENDMETHOD.


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
           lv_filsys    TYPE filesys  .


    IF i_xml_string IS SUPPLIED.
      " Storing provided XML in attribute for sharing handling
      me->_xml_string = i_xml_string.


      " Search current EOL
      FIND ALL OCCURRENCES OF |\r\n| IN i_xml_string MATCH COUNT lv_eol_count.

      IF lv_eol_count > 0.
        me->_xml_eol = 'NT'.
      ELSE.
        FIND ALL OCCURRENCES OF |\n| IN i_xml_string MATCH COUNT lv_eol_count.

        IF lv_eol_count > 0.
          me->_xml_eol = 'UX'.
        ELSE.
          SELECT SINGLE filesys FROM opsystem INTO lv_filsys WHERE opsys = sy-opsys.

          CASE lv_filsys .
            WHEN 'UNIX'.
              me->_xml_eol = 'UX'.
            WHEN 'WINDOWS NT'.
              me->_xml_eol = 'NT'.
            WHEN OTHERS.
              me->_xml_eol = 'NT'.
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


    ELSE.
      me->set_attribute(
        i_name  = 'version'
        i_value = '1.0'
      ).

    ENDIF.

    " Once instanciated, removing XML String to free memory
    CLEAR   : me->_xml_string .
    REFRESH : me->_lt_result  ,
              me->_gt_result  .

  ENDMETHOD.


  METHOD node.

    DATA : ls_node TYPE zst_xml_lite_child_node .

    READ TABLE me->_root_node->children( ) INTO ls_node WITH TABLE KEY name = i_node_name.

    r_node = ls_node-node.

  ENDMETHOD.


  METHOD nodes.

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

  ENDMETHOD.


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


  METHOD root_node.

    r_root_node = me->_root_node.

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


  METHOD set_root_node.

    me->_root_node = i_root_node.

  ENDMETHOD.


  METHOD stringify.

    me->_render_process_instruction( ).

    IF me->_root_node IS NOT INITIAL.
      me->_render_node( me->_root_node ).
    ENDIF.

    r_xml_str = me->_xml_string.

  ENDMETHOD.


  METHOD use_space.

    me->_xml_use_space = 'X'.

    IF i_size IS SUPPLIED.
      me->_xml_tab_size = i_size.
    ENDIF.

  ENDMETHOD.


  METHOD use_tab.

    me->_xml_use_space = ' '.

  ENDMETHOD.


  METHOD version.

    r_version = zcl_xml_lite=>_version.

  ENDMETHOD.


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


  METHOD _get_tag_name.
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

  ENDMETHOD.


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


  METHOD _parse_node.

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
    lr_xml_node = NEW zcl_xml_lite_node( ).


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
          lr_xml_child_node = NEW zcl_xml_lite_node( lv_child_tag_name ).
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

  ENDMETHOD.


  METHOD _parse_process_instruction.

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

          lr_attribute = NEW zcl_xml_lite_attribute(
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

  ENDMETHOD.


  METHOD _render_attribute.

    DATA : lv_attribute TYPE string ,
           lv_att_nam   TYPE string ,
           lv_att_val   TYPE string .

    lv_att_nam = i_attribute->get_name( ).
    lv_att_val = i_attribute->get_value( ).

    CONCATENATE lv_att_nam '="' lv_att_val '"' INTO lv_attribute.

    r_attribute_str = lv_attribute.

  ENDMETHOD.


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


  METHOD _render_process_instruction.

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

  ENDMETHOD.
ENDCLASS.
