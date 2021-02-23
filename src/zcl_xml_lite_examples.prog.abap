*&---------------------------------------------------------------------*
*& Report ZCL_XML_LITE_EXAMPLES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcl_xml_lite_examples.


"SELECTION-SCREEN BEGIN OF BLOCK blkdemo WITH FRAME TITLE text-t01.
  PARAMETERS: p_dem000 RADIOBUTTON GROUP seld MODIF ID rad USER-COMMAND fm ,
              p_dem010 RADIOBUTTON GROUP seld MODIF ID rad                 ,
              p_dem020 RADIOBUTTON GROUP seld MODIF ID rad                 ,
              p_dem030 RADIOBUTTON GROUP seld MODIF ID rad                 ,
              p_dem040 RADIOBUTTON GROUP seld MODIF ID rad                 .
*              p_dem999 RADIOBUTTON GROUP seld MODIF ID rad                 .
"SELECTION-SCREEN END   OF BLOCK blkdemo.



*&----------------------------------------------------------------------*
*&-------------------------[  XML SAMPLE  ]-----------------------------*
*&----------------------------------------------------------------------*
DATA : lv_xml_string TYPE string .

lv_xml_string = |<?xml version="1.0" encoding="utf-8" ?>|                            &&
                |<IDOC idocno="242824057" message_type="MATMAS" partner_type="LS">|  &&
                |    <E1MAKTM segment="000001">|                                     &&
                |        <MAKTX>Fil pes 20/3 V1005-70518</MAKTX>|                    &&
                |        <SPRAS_ISO>EN</SPRAS_ISO>|                                  &&
                |    </E1MAKTM>|                                                     &&
                |    <E1MAKTM segment="000002">|                                     &&
                |        <MAKTX>Fil pes 20/3 V1005-70518</MAKTX>|                    &&
                |        <SPRAS_ISO>FR</SPRAS_ISO>|                                  &&
                |    </E1MAKTM>|                                                     &&
                |    <E1MARCM segment="000003">|                                     &&
                |        <E1MARDM segment="000004">|                                 &&
                |            <LGORT>1100</LGORT>|                                    &&
                |        </E1MARDM>|                                                 &&
                |        <E1MARDM segment="000005">|                                 &&
                |            <LGORT>1200</LGORT>|                                    &&
                |        </E1MARDM>|                                                 &&
                |    </E1MARCM>|                                                     &&
                |</IDOC>|                                                             .
*&----------------------------------------------------------------------*



*&----------------------------------------------------------------------*
*&---[ 000 ]----------[  PURPOSE OF THE EXAMPLE  ]----------------------*
*&----------------------------------------------------------------------*
*&
*&  Introduction example : Update node to "upperize" value
*&
*&----------------------------------------------------------------------*
IF p_dem000 EQ 'X'.
  BREAK-POINT.

  DATA xml TYPE string.
  xml = |<text>|               &&
        |  <line>aaaa</line>|  &&
        |  <line>bbbb</line>|  &&
        |  <line>cccc</line>|  &&
        |</text>|              .

  cl_abap_browser=>show_xml( xml ).

  DATA lr_xml TYPE REF TO zcl_xml_lite.
  lr_xml = NEW zcl_xml_lite( xml ).

  DATA lr_root_node TYPE REF TO zcl_xml_lite_node .
  lr_root_node = lr_xml->root_node( ).

  WHILE lr_root_node->next( ) IS NOT INITIAL.
    lr_root_node->child( )->set_value(
      to_upper( lr_root_node->child( )->get_value( ) )
    ).
  ENDWHILE.

  cl_abap_browser=>show_xml( lr_xml->stringify( ) ).

  BREAK-POINT.
ENDIF.



*&----------------------------------------------------------------------*
*&---[ 010 ]----------[  PURPOSE OF THE EXAMPLE  ]----------------------*
*&----------------------------------------------------------------------*
*&
*&  How to parse your XML String into ZCL_XML_LITE
*&
*&----------------------------------------------------------------------*
IF p_dem010 EQ 'X'.
  BREAK-POINT.

  DATA: lr_xml010 TYPE REF TO zcl_xml_lite.

  " Instanciating class with provided XML will try to parse your XML
  lr_xml010 = NEW zcl_xml_lite( lv_xml_string ).

  " Checking XML from ZCL_XML_LITE
  cl_abap_browser=>show_xml( lr_xml010->stringify( ) ).

  BREAK-POINT.
ENDIF.



*&----------------------------------------------------------------------*
*&---[ 020 ]----------[  PURPOSE OF THE EXAMPLE  ]----------------------*
*&----------------------------------------------------------------------*
*&
*&  How to create new XML with ZCL_XML_CLASS
*&
*&----------------------------------------------------------------------*
IF p_dem020 EQ 'X'.
  BREAK-POINT.

  DATA: lr_xml020 TYPE REF TO zcl_xml_lite.

  " Instanciating class without provided XML will create an bare XML document
  " It's not really bare. By default Process Instruction has the '"version="1.0"'
  " attribute defined
  lr_xml010 = NEW zcl_xml_lite( ).

  " Can not display XML due to missing root node

  BREAK-POINT.
ENDIF.



*&----------------------------------------------------------------------*
*&---[ 030 ]----------[  PURPOSE OF THE EXAMPLE  ]----------------------*
*&----------------------------------------------------------------------*
*&
*&  Creating & setting the root node
*&
*&----------------------------------------------------------------------*
IF p_dem030 EQ 'X'.
  BREAK-POINT.

  DATA: lr_xml030         TYPE REF TO zcl_xml_lite      ,
        lr_root_node030_n TYPE REF TO zcl_xml_lite_node , " New       node
        lr_root_node030_r TYPE REF TO zcl_xml_lite_node . " Retrieved node

  " Instanciating class with provided XML will try to parse your XML
  lr_xml030 = NEW zcl_xml_lite(  ).

  " Creating a node
  lr_root_node030_n = new zcl_xml_lite_node( 'ROOT_NODE' ).

  " Setting Up the root node
  lr_xml030->set_root_node( lr_root_node030_n ).

  " Retrieving root node
  lr_root_node030_r = lr_xml030->root_node( ).

  " Checking XML from ZCL_XML_LITE
  cl_abap_browser=>show_xml( lr_xml030->stringify( ) ).

  BREAK-POINT.
ENDIF.



*&----------------------------------------------------------------------*
*&---[ 040 ]----------[  PURPOSE OF THE EXAMPLE  ]----------------------*
*&----------------------------------------------------------------------*
*&
*&  How to get the XML Document Root Node
*&
*&----------------------------------------------------------------------*
IF p_dem040 EQ 'X'.
  BREAK-POINT.

  DATA: lr_xml040       TYPE REF TO zcl_xml_lite      ,
        lr_root_node040 TYPE REF TO zcl_xml_lite_node .

  " Instanciating class with provided XML will try to parse your XML
  lr_xml040 = NEW zcl_xml_lite( lv_xml_string ).

  " Retrieving root node
  lr_root_node040 = lr_xml040->get_root_node( ).

  BREAK-POINT.
ENDIF.



*&----------------------------------------------------------------------*
*&---[ 999 ]-------------[  USAGE REFERENCE  ]--------------------------*
*&----------------------------------------------------------------------*
*&
*&  List of all possible method call
*&
*&----------------------------------------------------------------------*
*&---[ ZCL_XML_LITE ]---------------------------------------------------*
*&----------------------------------------------------------------------*
*&
*&  NEW zcl_xml_lite( 'yourXmlString')                                  : Parse XML string to Structured object for manipulation
*&  NEW zcl_xml_lite( )                                                 : Create a new XML "Document"
*&
*&  zcl_xml_lite->root_node( )                                          : Retrieve Root Node
*&
*&
*&----------------------------------------------------------------------*
*&---[ ZCL_XML_LITE_NODE ]----------------------------------------------*
*&----------------------------------------------------------------------*
*&
*&
*&
*&
*&----------------------------------------------------------------------*
*&---[ ZCL_XML_LITE_ATTRIBUTE ------------------------------------------*
*&----------------------------------------------------------------------*
