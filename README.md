# zcl_xml_lite

> Documentation is still in WIP status.

A very basic class to handle XML nodes &amp;
attributes in a better way than **ixml** &amp; **sxml**.

**Important** : ``ZCL_XML_LITE`` do not handle nodes which have a
``text node`` and also `children nodes` at the same time :

````xml
<NODE>
    text
    <CHILDREN />
</NODE>
````



## Summary

[](BeginSummary)
* [Summary](#summary)
* [Purpose of the class](#purpose-of-the-class)
* [Getting Started](#getting-started)
    * [Handling an existing XML document](#handling-an-existing-xml-document)
    * [Creating a new XML document](#creating-a-new-xml-document)
    * [Create a new node](#create-a-new-node)
    * [Setting up the root node](#setting-up-the-root-node)
    * [Handling a node (name and value)](#handling-a-node-name-and-value)
* [Detailed Documentation](#detailed-documentation)
    * [Class ``ZCL_XML_LITE``](#class-zcl_xml_lite)
        * [``CONSTRUCTOR``](#constructor)
            * [Usage 1 - Parsing an XML](#usage-1---parsing-an-xml)
            * [Usage 2 - Create new XML](#usage-2---create-new-xml)
        * [``ATTRIBUTE``](#attribute)
        * [``ATTRIBUTES``](#attributes)
        * [``NODE``](#node)
        * [``NODES``](#nodes)
        * [``PRETTIFY``](#prettify)
        * [``ROOT_NODE``](#root_node)
            * [Get Root Node](#get-root-node)
        * [``SET_ATTRIBUTE``](#set_attribute)
        * [``SET_EOL``](#set_eol)
        * [``SET_ROOT_NODE``](#set_root_node)
        * [``STRINGIFY``](#stringify)
        * [``USE_SPACE``](#use_space)
        * [``USE_TAB``](#use_tab)
        * [``VERSION``](#version)
    * [Class ``ZCL_XML_LITE_NODE``](#class-zcl_xml_lite_node)
        * [``CONSTRUCTOR``](#constructor)
        * [``APPEND_CHILD``](#append_child)
        * [``ATTRIBUTES``](#attributes)
        * [``CHILD``](#child)
        * [``CHILDREN``](#children)
        * [``CLONE``](#clone)
        * [``GET_ATTRIBUTE``](#get_attribute)
        * [``GET_ATTRIBUTE_VALUE``](#get_attribute_value)
        * [``GET_NAME``](#get_name)
        * [``GET_PARENT_NODE``](#get_parent_node)
        * [``GET_VALUE``](#get_value)
        * [``INSERT_AFTER``](#insert_after)
        * [``INSERT_BEFORE``](#insert_before)
        * [``LENGTH``](#length)
        * [``NEXT``](#next)
        * [``NEXT_SIBLING``](#next_sibling)
        * [``PARENT``](#parent)
        * [``PREVIOUS``](#previous)
        * [``PREVIOUS_SIBLING``](#previous_sibling)
        * [``REMOVE_AFTER``](#remove_after)
        * [``REMOVE_ATTRIBUTE``](#remove_attribute)
        * [``REMOVE_BEFORE``](#remove_before)
        * [``REMOVE_CHILD``](#remove_child)
        * [``RESET``](#reset)
        * [``SET_ATTRIBUTE``](#set_attribute)
        * [``SET_NAME``](#set_name)
        * [``SET_PARENT_NODE``](#set_parent_node)
        * [``SET_VALUE``](#set_value)
    * [Class ``ZCL_XML_LITE_ATTRIBUTE``](#class-zcl_xml_lite_attribute)
        * [``CONSTRUCTOR``](#constructor)
        * [``SET_NAME``](#set_name)
        * [``GET_NAME``](#get_name)
        * [``SET_VALUE``](#set_value)
        * [``GET_VALUE``](#get_value)
        * [``CLONE``](#clone)
[](EndSummary)



## Purpose of the class

For the most basic developments on XML strings, using standard
classes ``cl_ixml`` and `cl_sxml` are difficult  to use and
debug is not possible due to how classes works using only handlers numbers.

For instance, to process a XML string with ``cl_ixml``,
you have to follow these steps :

* Create an new instance of ``cl_ixml``
* Create an new document using ``cl_ixml``
* Create a stream factory
* Parse the XML using instance ``cl_ixml`` with :
    * The document
    * The stream factory
    * And the stream, using stream factory with convertion of the XML in hexadecimal
* Create an interator
* Read node from iterator

With ``zcl_xml_lite``, you have only to instantiate with your XML string,
then loop on children nodes.

This is the goal of this class : allowing most common and basic manipulation
of XML nodes : read, update, create, remove nodes and their attributes, no more, no less.

Below, an example with ``cl_ixml``:

````abap
" Source : https://blogs.sap.com/2013/04/11/abap-and-xml-wrapping-it-up/
DATA xml TYPE xstring.
xml =
cl_abap_codepage=>convert_to(
  `<text>` &&
  `<line>aaaa</line>` &&
  `<line>bbbb</line>` &&
  `<line>cccc</line>` &&
  `</text>` ).

DATA ixml TYPE REF TO if_ixml.
DATA stream_factory TYPE REF TO if_ixml_stream_factory.
DATA document TYPE REF TO if_ixml_document.

ixml = cl_ixml=>create( ).
stream_factory = ixml->create_stream_factory( ).
document = ixml->create_document( ).

IF ixml->create_parser(
  document = document
  stream_factory = stream_factory
  istream = stream_factory->create_istream_xstring( string = xml )
  )->parse( ) <> 0.
  RETURN.
ENDIF.

" Iterate DOM and modify text elements
DATA iterator TYPE REF TO if_ixml_node_iterator.
iterator = document->create_iterator( ).
DATA node TYPE REF TO if_ixml_node.
DO.
  node = iterator->get_next( ).
  IF node IS INITIAL.
    EXIT.
  ENDIF.
  IF node->get_type( ) = if_ixml_node=>co_node_text.
    node->set_value( to_upper( node->get_value( ) ) ).
  ENDIF.
ENDDO.

" Render DOM into xstring
CLEAR xml.
document->render(
  ostream = ixml->create_stream_factory(
  )->create_ostream_xstring(
  string = xml ) ).

cl_abap_browser=>show_xml( xml_xstring = xml ).
````

With ``zcl_xml_lite`` that becomes :

````abap
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
````

In debug, it's very easy to browse between nodes :

![](lib/img/debug_01.png)

![](lib/img/debug_02.png)

![](lib/img/debug_03.png)


**If you think ``cl_ixml`` and `cl_sxml` are too difficult to
use and you have only basics modifications to do,
``zcl_xml_lite`` is made for you !**



## Getting Started

I recommend to follow the chapter ``Getting Started``, step by step, 
because each sub-chapter implies previous code has been executed.

Also, please considering our existing XML is the following one :

**Note**  : This XML do not represent a real IDOC.

````xml
<?xml version="1.0" encoding="utf-8" ?>
<IDOC idocno="242824057" message_type="MATMAS" partner_type="LS">
    <E1MAKTM segment="000001">
        <MAKTX>Fil pes 20/3 V1005-70518</MAKTX>
        <SPRAS_ISO>EN</SPRAS_ISO>
    </E1MAKTM>
    <E1MAKTM segment="000002">
        <MAKTX>Fil pes 20/3 V1005-70518</MAKTX>
        <SPRAS_ISO>FR</SPRAS_ISO>
    </E1MAKTM>
    <E1MARCM segment="000003">
        <E1MARDM segment="000004">
            <LGORT>1100</LGORT>
        </E1MARDM>
        <E1MARDM segment="000005">
            <LGORT>1200</LGORT>
        </E1MARDM>
    </E1MARCM>
</IDOC>
````

````abap
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
````


### Handling an existing XML document

``ZCL_XML_LITE`` represent the XML Document.
Please do as follow to parse your XML Document as string :

````abap
DATA : lr_xml TYPE REF TO zcl_xml_lite .
lr_xml = NEW zcl_xml_lite( lv_xml_string ).
````

From this point, you can manipulate the **Process Instruction**
( [W3 - XML - Processing Instructions](https://www.w3.org/TR/xml/#sec-pi) )
or the **Root Node** (`<IDOC></IDOC>`).



### Creating a new XML document

To create a new XML Document, simply instantiate a ``zcl_xml_lite`` instance :

````abap
DATA : lr_xml TYPE REF TO zcl_xml_lite .
lr_xml = NEW zcl_xml_lite( ).
````

The default document have the following **Process Instruction** :

````xml
<?xml version="1.0" ?>
````

You will have to create a root node to get a valid rendered XML.



### Create a new node

All node of the XML are an instance of class ``zcl_xml_lite_node``.

````abap
DATA : lr_node TYPE REF TO zcl_xml_lite_node .
lr_node = NEW zcl_xml_lite_node( 'IDOC' ).
````

At this moment, ``lr_node`` is equal to :

````xml
<IDOC />
````

You can instantiate ``zcl_xml_lite_node`` without import parameter.
If no import parameter is passed, the default name of the node is ``NODE`` :

````xml
<NODE />
````



### Setting up the root node

Once you have created your new node, simple use method ``set_root_node`` :

````abap
lr_xml->set_root_node( lr_node ).
````



### Handling a node (name and value)

You can change at any time the root name :

````abap
lr_node->set_name( 'IDOC' ).
````

To set a value, please do as follow :

````abap
lr_node->set_value( 'MATMAS IDOC' ).
````

To remove the value, simply call the method without import parameter :

````abap
lr_node->set_value(  ).
````

**Important** : ``ZCL_XML_LITE`` handle a **value** or a
**children node list** at once. If a node has **children**, the value
is ignored in rendering.



### Handling node attributes



### Browsing between node



### Adding new node (append and insert)



### Removing node






## Detailed Documentation

Please find in this chapter, the list of **public** method
with the different way to use them and the associated behavior.


### Class ``ZCL_XML_LITE``


#### ``CONSTRUCTOR``

* Import parameter :
    * ``i_xml_string``, **optional**, type `string` - _XML Text_.
* Returning parameter :
    * The created instance of ``zcl_xml_lite``.
    
##### Usage 1 - Parsing an XML

> Parse the XML from variable ``lv_xml_string``.

````abap
DATA(lr_xml) = NEW zcl_xml_lite( lv_xml_string ) .
````    
    
##### Usage 2 - Create new XML

> Create a bare XML document.

````abap
DATA(lr_xml) = NEW zcl_xml_lite(  ) .
````    



#### ``ATTRIBUTE``



#### ``ATTRIBUTES``



#### ``NODE``



#### ``NODES``



#### ``PRETTIFY``



#### ``ROOT_NODE``

* Returning parameter :
    * ``r_root_node``, type `zcl_xml_lite_node` - _Represents the root XML node_.
    
##### Get Root Node

> Retrieve the root node of the XML Document

````abap
DATA(lr_root_node) = lr_xml->root_node( ).
````



#### ``SET_ATTRIBUTE``



#### ``SET_EOL``



#### ``SET_ROOT_NODE``



#### ``STRINGIFY``



#### ``USE_SPACE``



#### ``USE_TAB``



#### ``VERSION``





 


### Class ``ZCL_XML_LITE_NODE``

#### ``CONSTRUCTOR``



#### ``APPEND_CHILD``



#### ``ATTRIBUTES``



#### ``CHILD``



#### ``CHILDREN``



#### ``CLONE``



#### ``GET_ATTRIBUTE``



#### ``GET_ATTRIBUTE_VALUE``



#### ``GET_NAME``



#### ``GET_PARENT_NODE``



#### ``GET_VALUE``



#### ``INSERT_AFTER``



#### ``INSERT_BEFORE``



#### ``LENGTH``



#### ``NEXT``



#### ``NEXT_SIBLING``



#### ``PARENT``



#### ``PREVIOUS``



#### ``PREVIOUS_SIBLING``



#### ``REMOVE_AFTER``



#### ``REMOVE_ATTRIBUTE``



#### ``REMOVE_BEFORE``



#### ``REMOVE_CHILD``



#### ``RESET``



#### ``SET_ATTRIBUTE``



#### ``SET_NAME``



#### ``SET_PARENT_NODE``



#### ``SET_VALUE``






### Class ``ZCL_XML_LITE_ATTRIBUTE``

#### ``CONSTRUCTOR``



#### ``SET_NAME``



#### ``GET_NAME``



#### ``SET_VALUE``



#### ``GET_VALUE``



#### ``CLONE``
