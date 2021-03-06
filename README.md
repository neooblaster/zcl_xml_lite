# zcl_xml_lite - Version 0.1.0

> Documentation is still in WIP status.
>
> English mistakes will be fixed at the end (as possible)

A **very basic** class to handle XML nodes &amp;
attributes in a better way than **ixml** &amp; **sxml**.

**Important** : ``ZCL_XML_LITE`` do not handle nodes which have a
``text node`` and also `children nodes` at the same time :

````xml
<NODE>
    text
    <CHILDREN />
</NODE>
````

**Important 2** : ``CDATA`` is not implemented yet.



## Summary

[](BeginSummary)
* [Summary](#summary)
* [Purpose of the class](#purpose-of-the-class)
* [Getting Started](#getting-started)
    * [Handling an existing XML document](#handling-an-existing-xml-document)
    * [Creating a new XML document](#creating-a-new-xml-document)
    * [Create a new node](#create-a-new-node)
    * [Setting up the root node](#setting-up-the-root-node)
    * [Getting the root node](#getting-the-root-node)
    * [Handling a node (name and value)](#handling-a-node-name-and-value)
    * [Browsing between nodes](#browsing-between-nodes)
        * [Get node siblings](#get-node-siblings)
    * [Adding new node (append and insert)](#adding-new-node-append-and-insert)
        * [Appending a new child node](#appending-a-new-child-node)
        * [Inserting before an another child node](#inserting-before-an-another-child-node)
        * [Inserting after an another child node.](#inserting-after-an-another-child-node.)
    * [Removing a node](#removing-a-node)
        * [Remove a child node](#remove-a-child-node)
            * [Remove the child directly](#remove-the-child-directly)
            * [Remove the child indirectly](#remove-the-child-indirectly)
        * [Remove a sibling node](#remove-a-sibling-node)
    * [Handling node attributes](#handling-node-attributes)
* [Advanced functionality](#advanced-functionality)
* [Detailed Documentation](#detailed-documentation)
    * [Class ``ZCL_XML_LITE``](#class-zcl_xml_lite)
        * [``CONSTRUCTOR``](#constructor)
            * [Usage 1 - Parsing an XML](#usage-1---parsing-an-xml)
            * [Usage 2 - Create new XML](#usage-2---create-new-xml)
        * [``ATTRIBUTE``](#attribute)
        * [``ATTRIBUTES``](#attributes)
        * [``GET_ROOT_NODE``](#get_root_node)
            * [Get Root Node](#get-root-node)
        * [``NODE``](#node)
        * [``NODES``](#nodes)
        * [``PRETTIFY``](#prettify)
        * [``ROOT_NODE``](#root_node)
            * [Usage 1 - Get Root Node](#usage-1---get-root-node)
            * [Usage 2 - Set Root Node](#usage-2---set-root-node)
        * [``SET_ATTRIBUTE``](#set_attribute)
        * [``SET_EOL``](#set_eol)
        * [``SET_ROOT_NODE``](#set_root_node)
            * [Set Root Node](#set-root-node)
        * [``STRINGIFY``](#stringify)
        * [``USE_SPACE``](#use_space)
        * [``USE_TAB``](#use_tab)
        * [``VERSION``](#version)
    * [Class ``ZCL_XML_LITE_NODE``](#class-zcl_xml_lite_node)
        * [``CONSTRUCTOR``](#constructor)
        * [``APPEND_CHILD``](#append_child)
            * [Add a new child node](#add-a-new-child-node)
        * [``ATTRIBUTES``](#attributes)
        * [``CHILD``](#child)
            * [Get current handled child node](#get-current-handled-child-node)
        * [``CHILDREN``](#children)
        * [``CLONE``](#clone)
        * [``GET_ATTRIBUTE``](#get_attribute)
        * [``GET_ATTRIBUTE_VALUE``](#get_attribute_value)
        * [``GET_NAME``](#get_name)
        * [``GET_PARENT_NODE``](#get_parent_node)
        * [``GET_VALUE``](#get_value)
        * [``INSERT_AFTER``](#insert_after)
            * [Usage 1 - Insert after current handled child node](#usage-1---insert-after-current-handled-child-node)
            * [Usage 2 - Insert after specified child node](#usage-2---insert-after-specified-child-node)
            * [Usage 3 - Insert after specified child node using it index](#usage-3---insert-after-specified-child-node-using-it-index)
        * [``INSERT_BEFORE``](#insert_before)
            * [Usage 1 - Insert before current handled child node](#usage-1---insert-before-current-handled-child-node)
            * [Usage 2 - Insert before specified child node](#usage-2---insert-before-specified-child-node)
            * [Usage 3 - Insert before specified child node using it index](#usage-3---insert-before-specified-child-node-using-it-index)
        * [``LENGTH``](#length)
        * [``NEXT``](#next)
            * [Move handling index to the next child](#move-handling-index-to-the-next-child)
        * [``NEXT_CHILD``](#next_child)
            * [Usage 1 - Get next sibling of the current handled child node](#usage-1---get-next-sibling-of-the-current-handled-child-node)
            * [Usage 2 - Get next sibling using reference node](#usage-2---get-next-sibling-using-reference-node)
            * [Usage 3 - Get next sibling using index node](#usage-3---get-next-sibling-using-index-node)
        * [``NEXT_SIBLING``](#next_sibling)
            * [Get next sibling of the node](#get-next-sibling-of-the-node)
        * [``PARENT``](#parent)
        * [``PREVIOUS``](#previous)
            * [Move handling index to the previous child](#move-handling-index-to-the-previous-child)
        * [``PREVIOUS_CHILD``](#previous_child)
            * [Usage 1 - Get previous sibiling of the current handled child node](#usage-1---get-previous-sibiling-of-the-current-handled-child-node)
            * [Usage 2 - Get previous sibling using reference node](#usage-2---get-previous-sibling-using-reference-node)
            * [Usage 3 - Get previous sibling using index node](#usage-3---get-previous-sibling-using-index-node)
        * [``PREVIOUS_SIBLING``](#previous_sibling)
            * [Get previous sibling of the node](#get-previous-sibling-of-the-node)
        * [``REMOVE_AFTER``](#remove_after)
            * [Usage 1 - Remove child node after current handled child node](#usage-1---remove-child-node-after-current-handled-child-node)
            * [Usage 2 - Remove child node after the provided node reference](#usage-2---remove-child-node-after-the-provided-node-reference)
            * [Usage 3 - Remove child node after the child with provided index](#usage-3---remove-child-node-after-the-child-with-provided-index)
        * [``REMOVE_ATTRIBUTE``](#remove_attribute)
        * [``REMOVE_BEFORE``](#remove_before)
            * [Usage 1 - Remove child node before current handled child node](#usage-1---remove-child-node-before-current-handled-child-node)
            * [Usage 2 - Remove child node before the provided node reference](#usage-2---remove-child-node-before-the-provided-node-reference)
            * [Usage 3 - Remove child node before the child with provided index](#usage-3---remove-child-node-before-the-child-with-provided-index)
        * [``REMOVE_CHILD``](#remove_child)
            * [Usage 1 - Remove the current handled child node](#usage-1---remove-the-current-handled-child-node)
            * [Usage 2 - Remove a child node using a reference](#usage-2---remove-a-child-node-using-a-reference)
            * [Usage 3 - Remove a child node using its index](#usage-3---remove-a-child-node-using-its-index)
        * [``REMOVE_ME``](#remove_me)
            * [Self removing from its own parent node](#self-removing-from-its-own-parent-node)
        * [``REMOVE_NEXT_SIBLING``](#remove_next_sibling)
            * [Remove its own next sibling](#remove-its-own-next-sibling)
        * [``REMOVE_PREVIOUS_SIBLING``](#remove_previous_sibling)
            * [Remove its own previous sibling](#remove-its-own-previous-sibling)
        * [``RESET``](#reset)
            * [Reset the index to the beginning](#reset-the-index-to-the-beginning)
            * [Set index to the end](#set-index-to-the-end)
            * [Set index to specified child node index](#set-index-to-specified-child-node-index)
        * [``SET_ATTRIBUTE``](#set_attribute)
        * [``SET_NAME``](#set_name)
            * [Set Node Name](#set-node-name)
        * [``SET_PARENT_NODE``](#set_parent_node)
        * [``SET_VALUE``](#set_value)
            * [Usage 1 - Set Node Value](#usage-1---set-node-value)
            * [Usage 2 - Remove Node Value](#usage-2---remove-node-value)
    * [Class ``ZCL_XML_LITE_ATTRIBUTE``](#class-zcl_xml_lite_attribute)
        * [``CONSTRUCTOR``](#constructor)
        * [``SET_NAME``](#set_name)
        * [``GET_NAME``](#get_name)
        * [``SET_VALUE``](#set_value)
        * [``GET_VALUE``](#get_value)
        * [``CLONE``](#clone)
* [WIP Status](#wip-status)
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

Once you have created your new node, simply use method ``set_root_node`` :

````abap
lr_xml->set_root_node( lr_node ).
````



### Getting the root node

The first node of the XML represents the **root node**,
the one which contains all the others.

To retrieve the **root node** of the parsed XML or of your new XML
simply use method ``get_root_node`` :

````abap
DATA : lr_root_node TYPE zcl_xml_lite_node .
lr_root_node = lr_xml->get_root_node( ).
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



### Browsing between nodes

Understanding the navigation within child nodes early in the documentation
is important to understand the effect of adding and removing nodes for
navigation.

Please considering the following node :

````xml
<ROOT_NODE>
    <CHILD_1 />
    <CHILD_2 />
    <CHILD_3 />
</ROOT_NODE>
````

In ``ZCL_XML_LITE_NODE``, we use a virtual handle which is simply an index
refering to the entry of the internal table containing children.

By default (after parsing or node creation),
the handle index value is ``0``, that means currently we pointing any child node.

I choose to set ``0`` to allow browsing child nodes with `WHILE` loop statement.
If that was ``1``, that will implies using `DO..WHILE` loop statement, but
you will have to make check to prevent dump.

So to browse between child, there is two methods :
* ``next( )`` to move handle index to the next child (`0` -> `1`).
* ``previous( )`` to move handle index to the previous child (`1` -> `0`).

``next( )`` and `previous( )` automatically return the handled child node,
returning a reference of the node which is an instance of ``zcl_xml_lite_node``.

You can use these method to move handle without catching returned node.
If you need to retrieve handled child node, simple use method ``child( )``
which return handled child node without updating handle index.

To read all children, simply make a ``WHILE`` loop :

````abap
DATA : lr_child_node TYPE REF TO zcl_xml_lite_node .

WHILE lr_node->next( ) IS NOT INITIAL.
    lr_child_node = lr_node->child( ).
ENDWHILE.
````

While loop will procede as follow :

````xml
<ROOT_NODE>
    <CHILD_1 />   <<< lr_child_node in loop 1
    <CHILD_2 />   <<< lr_child_node in loop 2
    <CHILD_3 />   <<< lr_child_node in loop 3
</ROOT_NODE>
````

Once loop is done, the handle index is now ``4`` explaining why
``lr_node->next( )`` did not return nothing and fill condition `IS INITIAL`.
So, using ``child( )`` will also returning nothing (**INITIAL**).

You can make a "reverse" loop using ``previous( )`` or reset the handle
using ``reset( )``.

**reset( )** can take an import parameter which is the child node index :
* ``reset( )`` will automatically set the handle index to `0`
* ``reset( -1 )`` will automatically set the handle index to the end
* ``reset( 2 )`` will set the handle index to `2`, that mean method
``child( )`` will return the second child node.



#### Get node siblings

From a **parent node** you can easily get the **previous** or the **next** sibiling
of a child node with methods ``previous_child( )`` and `next_child( )`.

From a **child node** you can directly get the **previous** or the **next** sibiling
with methods ``previous_sibling( )`` and `next_sibling( )`.

All return the sibiling which is an instance of ``zcl_xml_lite_node``.

``previous_child( )`` and `next_child( )` called without parameter
use the handled child node.

````xml
<PARENT_NODE>
    <CHILD_1 />
    <CHILD_2 />
    <CHILD_3 />
    <CHILD_4 />
    <CHILD_5 />
</PARENT_NODE>
````

````abap
DATA : lr_parent_node TYPE REF TO zcl_xml_lite_node ,
       lr_child_node  TYPE REF TO zcl_xml_lite_node ,
       lr_sibling_1   TYPE REF TO zcl_xml_lite_node ,
       lr_sibling_2   TYPE REF TO zcl_xml_lite_node .

lr_parent_node = lr_xml->get_root_node( ).

" Set handled index to 2 - pointing on child <CHILD_2 />
lr_parent_node->reset( 2 ).

lr_sibling_1 =  lr_parent_node->previous_child( ). " <CHILD_1 />
lr_sibling_2 =  lr_parent_node->next_child( ).     " <CHILD_2 />
````

You can get siblings of the reference child node.
You have to use method of the parent of the reference node.
You can not use the method of a random node to get sibling of the node :

````abap
" Considering lr_child_4 is a reference of node <CHILD_4 />
lr_sibling_1 =  lr_parent_node->previous_child( lr_child_4 ). " <CHILD_3 />
lr_sibling_2 =  lr_parent_node->next_child( lr_child_4 ).     " <CHILD_5 />
````

If we want to work from node ``<CHILD_3 />`` using it index : 

````abap
lr_sibling_1 =  lr_parent_node->previous_child( 3 ). " <CHILD_2 />
lr_sibling_2 =  lr_parent_node->next_child( 3 ).     " <CHILD_4 />
````

Now, considering ``lr_node`` is the node reference `<CHILD_3 />`, we can
get siblings without using the **parent node**

````abap
lr_prev_node = lr_node->previous_sibling( ). " <CHILD_2 />
lr_next_node = lr_node->next_sibling( ).     " <CHILD_4 />
````

Do not forget they return an instance of ``zcl_xml_lite_node``, so you
can repeat methods like this :

````abanp
lr_x_node = lr_node->previous_sibling( )->previous_sibling( ). " <CHILD_1 />
````






### Adding new node (append and insert)

A node can have none to many children which are also
instances of ``zcl_xml_lite_node``.

We saw how to create new node. Now we will see how to 
add a new node to another one.

There is 3 way to add a new child node :
* by **appending** the new child node.
* insert the new child node **before** an another child node.
* insert the new child node **after**  an another child node.


#### Appending a new child node

Please considering the following node :

````xml
<ROOT_NODE>
    <CHILD_1 />
    <CHILD_2 />
    <CHILD_3 />
                 <<<<<< APPEND NODE
</ROOT_NODE>
````

**Append** means insert at the end.


````abap
DATA(lr_root_node) = lr_xml->get_root_node( ).
DATA(lr_new_node) = NEW zcl_xml_lite_node( 'CHILD_4' ).
lr_root_node->append_child( lr_new_node ).
````

The herebefore code will produce this result :

````xml
<ROOT_NODE>
    <CHILD_1 />
    <CHILD_2 />
    <CHILD_3 />
    <CHILD_4 />
</ROOT_NODE>
````



#### Inserting before an another child node

In the most of case, node order do not matter but ``zcl_xml_lite_node``
offer the possibility to add a new child node before an another child node,
or directly using an index.

Please considering the following node with 3 children.
``<CHILD_2 />`` is currently handled (`child( )`).

````xml
<ROOT_NODE>
    <CHILD_1 />     " Index 1
    <CHILD_2 />     " Index 2 - Current Handled Child
    <CHILD_3 />     " Index 3
</ROOT_NODE>
````

The method ``insert_before( )`` let you to insert the new node before another
one using :
* The **current handled node** if no parameter is supplied
* A child node reference using parameter ``i_ref_node``
* The index of the child node

The three **ABAP** code examples will produce the same result :

````xml
<ROOT_NODE>
    <CHILD_1 />     " Index 1
    <CHILD_4 />     " Index 2 
    <CHILD_2 />     " Index 2->3 - Current Handled Child
    <CHILD_3 />     " Index 3->4
</ROOT_NODE>
````

**Using handled node** :

````abap
lr_root_node->insert_before( lr_new_node ).
````

**Using a reference node** :

````abap
" lr_ref_node is <CHILD_2 />
lr_root_node->insert_before( 
    i_new_node = lr_new_node 
    i_ref_node = lr_ref_node
).
````

**Using handled node** :

````abap
lr_root_node->insert_before( 
    i_new_node   = lr_new_node 
    i_index_node = 2
).
````

* **Important** : Handled node index will be updated to prevent bugs in read loop.
Here, handled index was ``2`` and becomes `3` next to the insert. If index was not
updated (left to `2`), the next iteration (index `3`) will repeat the same child node.

* **Note** : If ``i_ref_node`` and `i_index_node` are supplied at the same time,
``i_index_node`` has the priority over `i_ref_node`.



#### Inserting after an another child node.

It's also possible to add a new node behind the specified **node**, **index** or
if not specified, current handled child node.

The method ``insert_after( )`` has the same **import parameters** that
``insert_before( )``.

Inserting node after is not common and have an important aspect to keep in mind,
especially when it's done without import parameter :

If you insert a new node after the current handled node, as expected, the next
sibling is the new node. In a ``WHILE`` loop, the next children will be the 
new node. **This method can easily creates infinite loop if you do not make
checks.**

Please see below to illustrate.

Considering the current XML with node ``CHILD_2`` which is currently handled :

````xml
<ROOT_NODE>
    <CHILD_1 />     " Index 1
    <CHILD_2 />     " Index 2 - Current Handled Child
    <CHILD_3 />     " Index 3
</ROOT_NODE>
````

After ``insert_before( lr_new_node )`` (using current handled node), the result
will become :

````xml
<ROOT_NODE>
    <CHILD_1 />     " Index 1
    <CHILD_2 />     " Index 2 - Current Handled Child
    <CHILD_4 />     " Index 3
    <CHILD_3 />     " Index 3 => 4
</ROOT_NODE>
````

* Before ``insert_before``, the sibling is `CHILD_3` with index `3`.
* After ``insert_before``, the sibling with index `3` is `CHILD_4`.

**Note**: For both methods ``insert_before`` and `insert_after`,
if there is no current handled child node, the new node will be appended.






### Removing a node

The ``zcl_xml_lite_node`` offers many way to remove a child node :

* From itself (`me`).
* From a parent node.
* Remove a node sibling.

The easiest way to remove a node is to call the method ``remove_me( )``
from the node we want to remove.

This method requires that its **parent node** is defined.

The parent node is defined by default, during XML parsing or when a node is 
added as children.

**Important** : Removing a node only delete the link between the parent node and the
child node. The object reference is not cleared letting you to continue to handle
it to append it in another node or to reuse it for later use (or clone).
So keep in mind to not use ``IS INITIAL`` on your reference to check if remove 
operation has done successfully.

**Note** : All remove methods return ``0`` if action is done successfully and
``1`` if something happens wrong letting you to make controls :

````abap
IF lr_parent_node->remove_child( ) NE 0.
    MESSAGE e123(zclass).
ENDIF.
````




#### Remove a child node

There is two ways to remove a child node from a parent node :
* Remove the current handled child or a specified child using a reference or an index.
* Remove the child before or after the current handed node of from the specified one.



##### Remove the child directly

To remove the desired node from the parent, simply call the method as follows :

````abap
" Your node refernce
lr_parent_node->remove_child( lr_child_node ).

" Current Handled Node
lr_parent_node->remove_child( ).
````

* **If method called without a import parameter,
that will remove the current handled node.**
* **If there is no current handled node,
nothing will be removed.**
* **If your reference node is invalid, nothing will be removed.**

You can also ask to remove your node by specifying its index :

````abap
lr_parent_node->remove_child( i_child_node_index = 3 ).
````

* **If you provide an invalid child index, nothing will be removed.**



##### Remove the child indirectly

If you want to remove a sibling node of your reference (or index),
you can use ``remove_before( )`` or `remove_after( )`.

As ``remove_child( )``, calling these methods without import parameter,
the current handled child node is used :

````xml
<ROOT_NODE>
    <CHILD_1 />     " Index 1
    <CHILD_2 />     " Index 2 
    <CHILD_3 />     " Index 3 - Current Handled Child
    <CHILD_4 />     " Index 4
    <CHILD_4 />     " Index 5
</ROOT_NODE>
````

These three statements will remove ``CHILD_2`` (if called independently) :

````abap
" Current Handled Node used
lr_root_node->remove_before( ).

" Reference node
lr_root_node->remove_before( lr_child_3 ).

" Index Node
lr_root_node->remove_before ( i_index_node = 3 ).
````

These three statements will remove ``CHILD_4`` (if called independently) :

````abap
" Current Handled Node used
lr_root_node->remove_after( ).

" Reference node
lr_root_node->remove_after( lr_child_3 ).

" Index Node
lr_root_node->remove_after ( i_index_node = 3 ).
````

**Important** : If there is no child node **before** or **after**,
nothing is removed.






#### Remove a sibling node

Removing the sibling node is only available if the node has its parent node
defined, which must be the case by default. The parent node is set when
you append/insert the new node.

You can easily remove a directly sibling from a child node without using the 
parent node thanks to methods ``remove_previous_sibling( )`` and
``remove_next_sibling( )``.

````xml
<ROOT_NODE>
    <CHILD_1 />     " Index 1
    <CHILD_2 />     " Index 2 
    <CHILD_3 />     " Index 3 - Current Handled Child
    <CHILD_4 />     " Index 4
    <CHILD_4 />     " Index 5
</ROOT_NODE>
````

This statement will remove ``CHILD_2`` : 

````abap
lr_child_3->remove_previous_sibling( ).
````

This statement will remove ``CHILD_4`` : 

````abap
lr_child_3->remove_next_sibling( ).
````

* **Important** : If your reference node do not have the requested sibling
(**previous** or **next**), nothing is removed.






### Handling node attributes






## Advanced functionality



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



#### ``GET_ROOT_NODE``

* Returning parameter :
    * ``r_root_node``, type `zcl_xml_lite_node` - _Represents the root XML node_.
    

##### Get Root Node

> Retrieve the root node of the XML Document

````abap
DATA(lr_root_node) = lr_xml->get_root_node( ).
````






#### ``NODE``



#### ``NODES``



#### ``PRETTIFY``



#### ``ROOT_NODE``

* Import parameter :
    * ``i_root_node``, **optional**, type `zcl_xml_lite_node` - _XML Node to set as root node_.
* Returning parameter :
    * ``r_root_node``, type `zcl_xml_lite_node` - _Represents the root XML node_.
    
    
##### Usage 1 - Get Root Node

> Retrieve the root node of the XML Document

````abap
DATA(lr_root_node) = lr_xml->root_node( ).
````
    
    
##### Usage 2 - Set Root Node

> Retrieve the root node of the XML Document

````abap
DATA(lr_root_node) = NEW zcl_xml_lite_node( 'ROOT_NODE' ).
lr_xml->root_node( lr_root_node ).
````






#### ``SET_ATTRIBUTE``



#### ``SET_EOL``



#### ``SET_ROOT_NODE``

* Import parameter :
    * ``i_root_node``, type `zcl_xml_lite_node` - _XML Node to set as root node_.


##### Set Root Node

> Set the root node of the XML document.

````abap
DATA(lr_root_node) = NEW zcl_xml_lite_node( 'ROOT_NODE' ).
lr_xml->set_root_node( lr_root_node ).
````






#### ``STRINGIFY``



#### ``USE_SPACE``



#### ``USE_TAB``



#### ``VERSION``





 


### Class ``ZCL_XML_LITE_NODE``

#### ``CONSTRUCTOR``

* Import parameter :
    * ``i_node_name``, **optional**, type `string`, default `NODE` - _The node name_.
    * ``i_parent_node``, **optional**, type `zcl_xml_lite_node` - _The parent node_.
* Returning parameter :
    * The created instance of ``zcl_xml_lite``.



#### ``APPEND_CHILD``

* Import parameter :
    * ``i_child_node``, type `zcl_xml_lite_node` - _The new child node to append_.


##### Add a new child node

> Insert at the end of the children list the new child node.

````abap
DATA(lr_root_node) = lr_xml->get_root_node( ).
DATA(lr_new_node) = NEW zcl_xml_lite_node( 'CHILD_4' ).
lr_root_node->append_child( lr_new_node ).
````






#### ``ATTRIBUTES``



#### ``CHILD``

* Returning parameter :
    * ``r_child_node``, type `zcl_xml_lite_node` - _Current child node_.


##### Get current handled child node

> Return the current handled child node.

````abap
DATA(lr_child_node) = lr_parent_node->child( ).
````






#### ``CHILDREN``



#### ``CLONE``



#### ``GET_ATTRIBUTE``



#### ``GET_ATTRIBUTE_VALUE``



#### ``GET_NAME``



#### ``GET_PARENT_NODE``



#### ``GET_VALUE``



#### ``INSERT_AFTER``

* Import parameter :
    * ``i_new_node``, **preferred**, type `zcl_xml_lite_node` - _New child node to insert_.
    * ``i_ref_node``, **optional**, type `zcl_xml_lite_node` - _Sibling child to insert after_.
    * ``i_index_node``, **optional**, type `I` - _Sibling index child node_.


##### Usage 1 - Insert after current handled child node

> Insert the provided new node after the current handled child node
>
> If the is not current handled node, the new node will be appended.

````abap
lr_parent_node->insert_after( lr_new_child_node ) .
````



##### Usage 2 - Insert after specified child node

> Insert the provided new node after the child node passed in parameter.
> The reference node must be a child of the parent node.
>
> If the reference node is not a child of the parent node, the new node
> will be appended.

````abap
lr_parent_node->insert_after( 
    i_new_node = lr_new_child_node
    i_ref_node = lr_ref_child_node
) .
````



##### Usage 3 - Insert after specified child node using it index

> Insert the provided new node after the child node with specified index.
>
> If the index is inconsistent (index which do not represents a child),
> the new node will be appended.

````abap
lr_parent_node->insert_after( 
    i_new_node   = lr_new_child_node
    i_index_node = 3
) .
````






#### ``INSERT_BEFORE``

* Import parameter :
    * ``i_new_node``, **preferred**, type `zcl_xml_lite_node` - _New child node to insert_.
    * ``i_ref_node``, **optional**, type `zcl_xml_lite_node` - _Sibling child to insert before_.
    * ``i_index_node``, **optional**, type `I` - _Sibling index child node_.


##### Usage 1 - Insert before current handled child node

> Insert the provided new node before the current handled child node
>
> If the is not current handled node, the new node will be appended.

````abap
lr_parent_node->insert_before( lr_new_child_node ) .
````



##### Usage 2 - Insert before specified child node

> Insert the provided new node before the child node passed in parameter.
> The reference node must be a child of the parent node.
>
> If the reference node is not a child of the parent node, the new node
> will be appended.

````abap
lr_parent_node->insert_before( 
    i_new_node = lr_new_child_node
    i_ref_node = lr_ref_child_node
) .
````



##### Usage 3 - Insert before specified child node using it index

> Insert the provided new node before the child node with specified index.
>
> If the index is inconsistent (index which do not represent a child),
> the new node will be appended.

````abap
lr_parent_node->insert_before( 
    i_new_node   = lr_new_child_node
    i_index_node = 3
) .
````






#### ``LENGTH``



#### ``NEXT``

* Returning parameter :
    * ``r_child_node``, type `zcl_xml_lite_node` - _Current handled child node_.

##### Move handling index to the next child

> Move handling index to the next child node and return it.

````abap
DATA(lr_child_node= lr_parent_node->next( ).
````






#### ``NEXT_CHILD``

* Import parameter :
    * ``i_ref_node``, **optional**, type `zcl_xml_lite_node` -
     _Reference of child node we want to get the next sibling_.
    * ``i_index_node``, **optional**, type `I` -
     _Index of the child node we want to get the nexy sibling_.
* Returning parameter :
    * ``r_sibling_node``, type `zcl_xml_lite_node` - _Return the next child node_.


##### Usage 1 - Get next sibling of the current handled child node

> Return the child node which is after the node which is currently
> handled (cf `child( )`).
> If there is no next sibling, that will return ``INITIAL``.

````abap
DATA(lr_sibling) = lr_parent_node->next_child( ).
````



##### Usage 2 - Get next sibling using reference node

> Return the child node which is after the provided reference child node.
> If there is no next sibling, that will return ``INITIAL``.

````abap
DATA(lr_sibling) = lr_parent_node->next_child( lr_ref_node ).
````



##### Usage 3 - Get next sibling using index node

> Return the child node which is after the child with provided index.
> If there is no next sibling, that will return ``INITIAL``.

````abap
DATA(lr_sibling) = lr_parent_node->next_child( 2 ).
````






#### ``NEXT_SIBLING``

* Returning parameter :
    * ``r_sibling_node``, type `zcl_xml_lite_node` - _Return the next sibling node_.


##### Get next sibling of the node

> Return the sibling node which is after.
> If there is no next sibling, that will return ``INITIAL``.

````abap
DATA(lr_sibling) = lr_node->next_sibling( ).
````






#### ``PARENT``



#### ``PREVIOUS``

* Returning parameter :
    * ``r_child_node``, type `zcl_xml_lite_node` - _Current handled child node_.

##### Move handling index to the previous child

> Move handling index to the previous child node and return it.

````abap
DATA(lr_child_node= lr_parent_node->previous( ).
````



#### ``PREVIOUS_CHILD``

* Import parameter :
    * ``i_ref_node``, **optional**, type `zcl_xml_lite_node` -
     _Reference of child node we want to get the previous sibling_.
    * ``i_index_node``, **optional**, type `I` -
     _Index of the child node we want to get the previous sibling_.
* Returning parameter :
    * ``r_sibling_node``, type `zcl_xml_lite_node` - _Return the previous child node_.


##### Usage 1 - Get previous sibiling of the current handled child node

> Return the child node which is before the node which is currently
> handled (cf `child( )`).
> If there is no previous sibling, that will return ``INITIAL``.

````abap
DATA(lr_sibling) = lr_parent_node->previous_child( ).
````



##### Usage 2 - Get previous sibling using reference node

> Return the child node which is before the provided reference child node.
> If there is no previous sibling, that will return ``INITIAL``.

````abap
DATA(lr_sibling) = lr_parent_node->previous_child( lr_ref_node ).
````



##### Usage 3 - Get previous sibling using index node

> Return the child node which is before the child with provided index.
> If there is no previous sibling, that will return ``INITIAL``.

````abap
DATA(lr_sibling) = lr_parent_node->previous_child( 2 ).
````






#### ``PREVIOUS_SIBLING``

* Returning parameter :
    * ``r_sibling_node``, type `zcl_xml_lite_node` - _Return the previous sibling node_.


##### Get previous sibling of the node

> Return the sibling node which is before.
> If there is no previous sibling, that will return ``INITIAL``.

````abap
DATA(lr_sibling) = lr_node->previous_sibling( ).
````






#### ``REMOVE_AFTER``

* Import parameter :
    * ``i_ref_node``, **optional**, **preferred**, type `zcl_xml_lite_node` - _Reference node_.
    * ``i_index_node``, **optional**, type `I` - _Child node index_.
* Returning parameter :
    * ``r_ret_code``, type `I` - _Indicates if removing has been done successfully_:
        * ``0``, default value, if operation is done successfully.
        * ``1`` if something happens wrong.


##### Usage 1 - Remove child node after current handled child node

> From a parent node, remove the child node which is after the current 
> handled node.
>
> **If there is no node after your reference node, nothing is removed.**

````abap
lr_parent_node->remove_after( ).
````



##### Usage 2 - Remove child node after the provided node reference

> From a parent node, remove the child node which is after the provided
> reference node.
>
> **If there is no node after your reference node, nothing is removed.**

````abap
lr_parent_node->remove_after( lr_child_node ).
````



##### Usage 3 - Remove child node after the child with provided index

> From a parent node, remove the child node which is after the provided
> reference node using it index.
>
> **If there is no node after your reference node, nothing is removed.**

````abap
lr_parent_node->remove_after( i_index_node = 3 ).
````






#### ``REMOVE_ATTRIBUTE``



#### ``REMOVE_BEFORE``

* Import parameter :
    * ``i_ref_node``, **optional**, **preferred**, type `zcl_xml_lite_node` - _Reference node_.
    * ``i_index_node``, **optional**, type `I` - _Child node index_.
* Returning parameter :
    * ``r_ret_code``, type `I` - _Indicates if removing has been done successfully_ :
        * ``0``, default value, if operation is done successfully.
        * ``1`` if something happens wrong.


##### Usage 1 - Remove child node before current handled child node

> From a parent node, remove the child node which is before the current 
> handled node.
>
> **If there is no node before your reference node, nothing is removed.**

````abap
lr_parent_node->remove_before( ).
````



##### Usage 2 - Remove child node before the provided node reference

> From a parent node, remove the child node which is before the provided
> reference node.
>
> **If there is no node before your reference node, nothing is removed.**

````abap
lr_parent_node->remove_before( lr_child_node ).
````



##### Usage 3 - Remove child node before the child with provided index

> From a parent node, remove the child node which is before the provided
> reference node using it index.
>
> **If there is no node before your reference node, nothing is removed.**

````abap
lr_parent_node->remove_before( i_index_node = 3 ).
````



#### ``REMOVE_CHILD``

* Import parameter :
    * ``i_child_node``, **optional**, **preferred**, type `zcl_xml_lite_node` - _Reference node to remove_.
    * ``i_index_node``, **optional**, type `I` - _Child node index in children list_.
* Returning parameter :
    * ``r_ret_code``, type `I` - _Indicates if removing has been done successfully_:
        * ``0``, default value, if operation is done successfully. 
        * ``1`` if something happens wrong.



##### Usage 1 - Remove the current handled child node

> From a parent node, remove the child node which is currently handed
> (cf `child( )`).
>
> **If there is no node currently handled, nothing is removed, returning `1`.**

````abap
lr_parent_node->remove_child( ).
````



##### Usage 2 - Remove a child node using a reference

> From a parent node, remove the child node which is passed using a reference.
>
> **If the reference node is not a child of the parent, nothing is removed,
>returning `1`.**

````abap
lr_parent_node->remove_child( lr_child_node ).
````



##### Usage 3 - Remove a child node using its index

> From a parent node, remove the child node which corresponding to the provided
> index.
>
> **If the index is invalid, nothing is removed, returning `1`.**

````abap
lr_parent_node->remove_child( i_index_node = 3 ).
````






#### ``REMOVE_ME``

* Returning parameter :
    * ``r_ret_code``, type `I` - _Indicates if removing has been done successfully_:
        * ``0``, default value, if operation is done successfully. 
        * ``1`` if something happens wrong.
        

##### Self removing from its own parent node

> From a child node, remove the link between the node and its own parent node.
>
> The parent node must be defined to perform the action.
> The definition of the parent node is made during XML parsing or when you
> add a new node.
>
> If the parent node is not set, nothing is done and returns ``1``.

````abap
lr_node->remove_me( ).

" Which is equal to
lr_parent_node->remove_child( lr_node ).
" or
lr_node->parent( )->remove_child( lr_node ).
````






#### ``REMOVE_NEXT_SIBLING``

* Returning parameter :
    * ``r_ret_code``, type `I` - _Indicates if removing has been done successfully_:
        * ``0``, default value, if operation is done successfully. 
        * ``1`` if something happens wrong.
        

##### Remove its own next sibling

> From a child node, remove the link between the its own next sibling node and
> its own parent node.
>
> The parent node must be defined to perform the action.
> The definition of the parent node is made during XML parsing or when you
> add a new node.
>
> If the parent node is not set, nothing is done and returns ``1``.

````abap
lr_node->remove_next_sibling( ).

" Which is equal to
lr_parent_node->remove_after( lr_node ).
````



#### ``REMOVE_PREVIOUS_SIBLING``

* Returning parameter :
    * ``r_ret_code``, type `I` - _Indicates if removing has been done successfully_:
        * ``0``, default value, if operation is done successfully. 
        * ``1`` if something happens wrong.
        

##### Remove its own previous sibling

> From a child node, remove the link between the its own previous sibling node and
> its own parent node.
>
> The parent node must be defined to perform the action.
> The definition of the parent node is made during XML parsing or when you
> add a new node.
>
> If the parent node is not set, nothing is done and returns ``1``.

````abap
lr_node->remove_previous_sibling( ).

" Which is equal to
lr_parent_node->remove_before( lr_node ).
````







#### ``RESET``

* Import parameter :
    * ``i_index``, **optional**, type `I` - _Child node index_.


##### Reset the index to the beginning

> Reset the current handling index to ``0``. That will allows
> to perform a new ``WHILE`` loop on children.

````abap
lr_parent_node->reset( ).
````



##### Set index to the end

> Set the current handling index to the end (``children + 1``).
> That will allows you to make a reverse ``WHILE`` loop in
> combination of ``previous( )`` method.

````abap
lr_parent_node->reset( -1 ).
````

**Note** : All other negative values set the index to the end.



##### Set index to specified child node index

> Set the current handling index to specified index.
> That will allows you to retrieved child node with method
> ``child( )``. Keep in mind making loop using `previous( )`
> and ``next( )`` will update handling index before returning
> handled child node.

````abap
lr_parent_node->reset( 2 ). " Set handling index to the second child node.
````

**Note** : If the index does not exist, that will produce the
same behavior of ``reset( -1 )``.






#### ``SET_ATTRIBUTE``



#### ``SET_NAME``

* Import parameter :
    * ``i_node_name``, type `string` - _XML tag name_.
    

##### Set Node Name

> Set the XML tag name.

````abap
lr_node->set_name( 'NODE_TAG_NAME' ).
````






#### ``SET_PARENT_NODE``



#### ``SET_VALUE``

* Import parameter :
    * ``i_value``, **optional**, type `string` - _Value of node_.
    

##### Usage 1 - Set Node Value

> Set the value between XML tag.

````abap
lr_node->set_value( 'My node value here' ).
````

````xml
<NODE>My node value here</NODE>
````



##### Usage 2 - Remove Node Value

> Remove the node value to get empty tag (if node has no children).

````abap
lr_node->set_value( ).
````

````xml
<NODE />
````






### Class ``ZCL_XML_LITE_ATTRIBUTE``

#### ``CONSTRUCTOR``



#### ``SET_NAME``



#### ``GET_NAME``



#### ``SET_VALUE``



#### ``GET_VALUE``



#### ``CLONE``





## WIP Status

* [ ] ZCL_XML_LITE
    * [X] CONSTRUCTOR
    * [ ] ATTRIBUTE
    * [ ] ATTRIBUTES
    * [ ] SET_ATTRIBUTE
    * [X] ROOT_NODE
    * [ ] NODE
    * [ ] NODES
    * [X] SET_ROOT_NODE
    * [X] GET_ROOT_NODE
    * [ ] PRETTIFY
    * [ ] SET_EOL
    * [ ] USE_SPACE
    * [ ] USE_TAB
    * [ ] STRINGIFY
    * [ ] VERSION
* [ ] ZCL_XML_LITE_NODE
    * [ ] CONSTRUCTOR
    * [ ] SET_PARENT_NODE
    * [ ] GET_PARENT_NODE
    * [X] SET_NAME
    * [ ] GET_NAME
    * [ ] SET_ATTRIBUTE
    * [ ] GET_ATTRIBUTE
    * [ ] GET_ATTRIBUTE_VALUE
    * [ ] REMOVE_ATTRIBUTE
    * [ ] ATTRIBUTES
    * [X] SET_VALUE
    * [ ] GET_VALUE
    * [X] APPEND_CHILD
    * [X] INSERT_BEFORE
    * [X] INSERT_AFTER
    * [X] REMOVE_CHILD
    * [X] REMOVE_BEFORE
    * [X] REMOVE_AFTER
    * [X] REMOVE_ME
    * [X] REMOVE_PREVIOUS_SIBLING
    * [X] REMOVE_NEXT_SIBLING
    * [ ] CHILDREN
    * [ ] LENGTH
    * [X] NEXT
    * [X] NEXT_CHILD
    * [X] NEXT_SIBLING
    * [X] PREVIOUS
    * [X] PREVIOUS_CHILD
    * [X] PREVIOUS_SIBLING
    * [X] RESET
    * [X] CHILD
    * [ ] PARENT
    * [ ] CLONE
* [ ] ZCL_XML_LITE_ATTRIBUTE
    * [ ] CONSTRUCTOR
    * [ ] SET_NAME
    * [ ] GET_NAME
    * [ ] SET_VALUE
    * [ ] GET_VALUE
    * [ ] CLONE
    

Documentation sample :

* Import parameter :
    * ``i_``, **optional**, **preferred**, type ` `, default `NODE` - _desc_.
* Exporting parameter :
    * ``e_``, type ` ` - _desc_.
* Returning parameter :
    * ``r_``, type ` ` - _desc_.
