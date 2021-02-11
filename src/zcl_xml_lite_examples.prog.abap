*&---------------------------------------------------------------------*
*& Report ZCL_XML_LITE_EXAMPLES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZCL_XML_LITE_EXAMPLES.


DATA: lo_xml   TYPE REF TO zcl_xml_lite ,
      lv_xml   TYPE        string       .

*lv_xml = |<?xml version="1.0" encoding="utf-8" ?>|              &&
*         |<NODE>|                &&
lv_xml = |<NODE_1 lvl="1.0">|                           &&          " <<< OK
         |  <NODE_2 lvl="2.0">|                         &&          " <<< OK
         |    <NODE_3 lvl="3.1">value_3.1</NODE_3>|     &&          "
         |    <NODE_4 lvl="3.2">|                       &&
         |      <NODE_5 lvl="4.1">value_4.1</NODE5>|    &&
         |      <NODE_6 lvl="4.2">|                     &&
         |        <NODE_7 lvl="5.0">value_5.0</NODE_7>| &&
         |      </NODE_6>|                              &&
         |      <NODE_8 lvl="4.3">value_4.3</NODE8>|    &&
         |    </NODE_4>|                                &&
         |  </NODE_2>|                                  &&
         |</NODE_1>| .

lo_xml = new zcl_xml_lite( lv_xml ).

IF 1 = 2. ENDIF.
