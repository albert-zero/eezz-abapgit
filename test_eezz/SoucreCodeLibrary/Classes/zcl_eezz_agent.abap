* -----------------------------------------------------------
* EezzServer: 
*    High speed application development and 
*    high speed execution based on HTML5
*    
*    Copyright (C) 2015  Albert Zedlitz
*
*    This program is free software: you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation, either version 3 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
* Description:
*    The agent interacts with the push channel to handle requests.    
*
class zcl_eezz_agent definition
  public
  final
  create public

  global friends zif_eezz_agent .

  public section.

    interfaces zif_eezz_agent .

    data mt_messages type ztty_messages .

    methods parse_eezz_dom
      importing
        !iv_document   type ref to if_ixml_document
        !iv_ostream    type ref to if_ixml_ostream
      returning
        value(rv_json) type string .
  protected section.
  private section.

    data m_message_db type zstr_symbols .
    class-data m_tbl_global type ztty_global_symbols .
    class-data m_tbl_update type ztty_update .

    methods create_navigation_update
      importing
        !iv_symbol type zstr_symbols .
    methods create_table
      importing
        !is_entry          type zstr_symbols
        !is_eezz_table     type ref to zif_eezz_table optional
      returning
        value(ro_new_node) type ref to if_ixml_node .
    methods remove_templates
      importing
        !iv_document type ref to if_ixml_document .
    methods show_document
      importing
        !iv_document type ref to if_ixml_document
        !iv_ostream  type ref to if_ixml_ostream .
    methods xml_node2string
      importing
        !io_node         type ref to if_ixml_node
      returning
        value(rv_string) type string .
    class-methods create_xml
      importing
        !iv_html_doc type string .
    class-methods create_json_response
      importing
        !iv_cmd        type string optional
        !iv_object     type string optional
        !iv_value      type string optional
      returning
        value(rv_json) type string .
    methods handle_request
      importing
        !iv_ostream  type ref to if_ixml_ostream
        !iv_str_json type string .
    methods render_node
      importing
        !iv_node type ref to if_ixml_node optional .
ENDCLASS.



CLASS ZCL_EEZZ_AGENT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_EEZZ_AGENT=>CREATE_JSON_RESPONSE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CMD                         TYPE        STRING(optional)
* | [--->] IV_OBJECT                      TYPE        STRING(optional)
* | [--->] IV_VALUE                       TYPE        STRING(optional)
* | [<-()] RV_JSON                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method create_json_response.

    data: x_update   type zstr_update,
          x_response type string,
          x_tmp      type string.

    data(x_result_stream) = new cl_abap_string_c_writer(  ).

    sort m_tbl_update by priority.

    x_result_stream->write( '{ "update":{' ).

    loop at m_tbl_update into x_update.
      if sy-tabix <> 1.
        x_result_stream->write( |,| ).
      endif.
      x_result_stream->write( |"{ x_update-c_key }":"{ cl_http_utility=>escape_url( x_update-c_value ) }"| ).
    endloop.
    x_result_stream->write( '}}' ).
    rv_json = x_result_stream->get_result_string( ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_EEZZ_AGENT->CREATE_NAVIGATION_UPDATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SYMBOL                      TYPE        ZSTR_SYMBOLS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method create_navigation_update.

    data: x_dictionary type ref to ztty_dictionary,
          x_update     type zstr_update.

    data(x_attributes) = iv_symbol-c_ref_node->get_attributes( ).
    data(x_attr_iterator) = x_attributes->create_iterator( ).
    data(x_attr_element) = x_attr_iterator->get_next( ).
    call method iv_symbol-c_object->('GET_DICTIONARY') receiving rt_dictionary = x_dictionary.
    while x_attr_element is bound.
      data(x_attr_value) = x_attr_element->get_value( ).
      if x_attr_value+0(1) eq '{'.
        data(x_length) = strlen( x_attr_value ).
        x_length = x_length - 2.
        data(x_key) = x_attr_value+1(x_length).
        try.
            data(x_new_value) = x_dictionary->*[ c_key = x_key ].
            if x_new_value is not initial.
              clear x_update.
              data(x_attr_name) = x_attr_element->get_name( ).
              x_update-c_key = iv_symbol-c_name && '.' && x_attr_name.
              x_update-c_value = x_new_value-c_value.
              x_update-priority = 10.
              append x_update to m_tbl_update.
            endif.
          catch cx_sy_itab_line_not_found.
        endtry.
      endif.
      x_attr_element = x_attr_iterator->get_next( ).
    endwhile.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_EEZZ_AGENT->CREATE_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_ENTRY                       TYPE        ZSTR_SYMBOLS
* | [--->] IS_EEZZ_TABLE                  TYPE REF TO ZIF_EEZZ_TABLE(optional)
* | [<-()] RO_NEW_NODE                    TYPE REF TO IF_IXML_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method create_table.

    data: x_node_table type ref to zcl_eezz_table_node,
          x_globals    type ref to ztty_global_symbols.
    data  x_eezz_table  type ref to zif_eezz_table.

    if is_eezz_table is bound.
      x_eezz_table ?= is_eezz_table.
    else.
      x_eezz_table ?= is_entry-c_object.
    endif.


    get reference of m_tbl_global into x_globals.
    create object x_node_table
      exporting
        io_node     = is_entry-c_templ_node
        it_globals  = x_globals
        io_eezz_tbl = x_eezz_table.

    ro_new_node = x_node_table->get( ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_EEZZ_AGENT=>CREATE_XML
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_HTML_DOC                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method create_xml.

    data: lo_xml                type ref to if_ixml,
          lo_xml_stream_factory type ref to if_ixml_stream_factory,
          lo_xml_parser         type ref to if_ixml_parser,
          lo_xml_stream         type ref to if_ixml_istream,
          lo_xml_document       type ref to if_ixml_document,
          lo_xml_iterator       type ref to if_ixml_node_iterator,
          lo_xml_node           type ref to if_ixml_node,
          lo_attr_map           type ref to if_ixml_named_node_map,
          lv_debug              type abap_bool value abap_false,
          lv_debug_xml_string   type xstring,
          lv_rc                 type i,
          lv_html_doc           type string,
          "ls_eezz_action        TYPE t_eezz_action,
          lo_attr_node          type ref to if_ixml_node.

    "FIELD-SYMBOLS: <fs_eezz_action> TYPE t_eezz_action.

    lv_html_doc = iv_html_doc.

* Add <HTML> tags: needed for parser - else RC <> 0
    concatenate '<HTML>' lv_html_doc '</HTML>' into lv_html_doc.

    lo_xml = cl_ixml=>create( ).
    lo_xml_stream_factory = lo_xml->create_stream_factory( ).
    lo_xml_stream = lo_xml_stream_factory->create_istream_string( string = lv_html_doc ).
    lo_xml_document = lo_xml->create_document( ).
    lo_xml_parser = lo_xml->create_parser(
                      document       = lo_xml_document
                      stream_factory = lo_xml_stream_factory
                      istream        = lo_xml_stream ).
* Check if document can be parsed
    lv_rc = lo_xml_parser->parse( ).

* Iterate nodes
    lo_xml_iterator = lo_xml_document->create_iterator( ).
    lo_xml_node = lo_xml_iterator->get_next( ).
    "WHILE NOT lo_xml_node IS INITIAL.
    "  lo_xml_node = lo_xml_iterator->get_next( ).
    "  IF lo_xml_node IS NOT INITIAL.
    "    DATA(lv_html_tag) = lo_xml_node->get_name( ).

* Handle <TABLE> tag
    "   TRANSLATE lv_html_tag TO UPPER CASE.
    "   IF lv_html_tag EQ 'TABLE' OR lv_html_tag EQ 'BODY'.
    "CLEAR ls_eezz_action.

    "lo_attr_map  = lo_xml_node->get_attributes( ).
    " IF lo_attr_map IS BOUND.
    "  lo_attr_node = lo_attr_map->get_named_item( name = 'name' ).
    "  IF lo_attr_node IS BOUND.
    "   ls_eezz_action-node_name = lo_attr_node->get_value( ).
    "  ENDIF.
    "  lo_attr_node = lo_attr_map->get_named_item( name = zif_eezz_constants=>con_data_eezz_action ).
    "  IF lo_attr_node IS BOUND.
    "    ls_eezz_action-html_tag = lv_html_tag.
    "    ls_eezz_action-eezz_cmd = zif_eezz_constants=>con_data_eezz_action.
    "    ls_eezz_action-eezz_action = lo_attr_node->get_value( ).
    "    APPEND ls_eezz_action TO mt_eezz_actions.
    " ENDIF.
    "ENDIF.
    "  ELSEIF lv_html_tag EQ 'TR' OR lv_html_tag EQ 'TH'.
    " CLEAR ls_eezz_action.

    " lo_attr_map = lo_xml_node->get_attributes( ).
    " IF lo_attr_map IS BOUND.
    "  lo_attr_node = lo_attr_map->get_named_item( name = zif_eezz_constants=>con_data_eezz_template ).
    "   IF lo_attr_node IS BOUND.
    "     ls_eezz_action-html_tag = lv_html_tag.
    "     ls_eezz_action-eezz_cmd = zif_eezz_constants=>con_data_eezz_template.
    "     ls_eezz_action-eezz_action = lo_attr_node->get_value( ).
    "     APPEND ls_eezz_action TO mt_eezz_actions.
    "   ENDIF.
    " ENDIF.
    "ENDIF.

    "ENDIF.
    "ENDWHILE.

    " IF lv_debug EQ 'X'.
* Display XML document in debugger
    "  CALL FUNCTION 'SDIXML_DOM_TO_XML'
    "    EXPORTING
    "      document      = lo_xml_document
    "      pretty_print  = 'X'
    "    IMPORTING
    "     xml_as_string = lv_debug_xml_string.

* Not working 'No dynpro to assign'
*      CALL FUNCTION 'SDIXML_DOM_TO_SCREEN'
*        EXPORTING
*          document = lo_xml_document
*        EXCEPTIONS
*          OTHERS   = 1.
    "ENDIF.

* Convert ' -> " in JSON string
    "LOOP AT mt_eezz_actions ASSIGNING <fs_eezz_action>.
    "  REPLACE ALL OCCURRENCES OF '''' IN <fs_eezz_action>-eezz_action WITH '"'.
    "ENDLOOP.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_EEZZ_AGENT->HANDLE_REQUEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OSTREAM                     TYPE REF TO IF_IXML_OSTREAM
* | [--->] IV_STR_JSON                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method handle_request.

    data: x_instance      type string,
          x_method        type string,
          x_key           type string,
          x_obj           type zstr_eezz_json,
          x_nav_obj       type ref to ztty_eezz_json,
          x_entry         type zstr_symbols,
          x_where_value   type i,
          x_index_value   type i,
          x_new_table     type ref to if_ixml_node,
          x_json_response type string,
          x_timestamp     type timestampl,
          x_timestop      type timestampl,
          x_seconds       type tzntstmpl,
          x_msg           type zstr_message,
          x_ref_obj       type ref to zif_eezz_table,
          x_data          type ref to data.

    data(x_json) = new zcl_eezz_json( iv_json = iv_str_json ).
    data(x_json_update)   = x_json->get( iv_path = 'update' ).
    data(x_json_callback) = x_json->get( iv_path = 'callback' ).
    data x_json_object_name  type string.
    data x_json_method_name type string.

    if x_json_callback is not bound.
      x_json_callback = x_json->get( iv_path = 'eezzAgent.assign' ).
    endif.

    clear mt_messages.
    get time stamp field x_timestamp.

    " execute callback event
    get reference of m_tbl_global into data(lo_symbols).
    data(x_eezz_table) = x_json->callback( lo_symbols ).
    field-symbols <fs_http_value> type string.


    " get table name in case the callback has generated a temp object
    if x_eezz_table is bound.
      if x_json_callback is bound.
        split x_json_callback->*[ 1 ]-c_key at '.' into x_json_object_name x_json_method_name.
      endif.
    endif.

    " Enter the result the callback into the update
    loop at x_json_update->* into data(x_update).
      " The update has the format <tag-name>.[(style.)]<attribute> = (*) | <tag-name>.<attribute>
      clear x_ref_obj.
      data(x_style) = ''.

      if x_update-c_value cs '*'.
        split x_update-c_key at '.' into table data(x_key_struct).
        data(x_http_dictionary_key)  = x_update-c_key.
        data(x_http_element_by_name) = x_key_struct[ 1 ].
        data(x_http_element_attr)    = x_key_struct[ 2 ].
      else.
        split x_update-c_value at '.' into table x_key_struct.
        x_http_dictionary_key        = x_update-c_value.
        x_http_element_by_name       = x_key_struct[ 1 ].
        x_http_element_attr          = x_key_struct[ 2 ].
      endif.

      try.
          data(x_global_symbol) = m_tbl_global[ c_name = x_http_element_by_name ].

          if x_eezz_table is bound.
            x_ref_obj = x_eezz_table.
          else.
            x_ref_obj = x_global_symbol-c_object.
          endif.

          if x_http_element_attr cs 'innerHTML'.
            " For innerHTML we have to create the node tree
            x_new_table = create_table( is_entry = x_global_symbol is_eezz_table = x_ref_obj ).
            render_node( iv_node = x_new_table ).
            create_navigation_update( x_global_symbol ).
          else.
            " For any attribute we use the dictionary of the eezz_table
            data x_http_update type zstr_update.
            data(x_dictionary)    = x_ref_obj->get_dictionary( ).

            if line_exists( x_dictionary->*[ c_key = to_upper( x_http_dictionary_key ) ] ).
              x_http_update-c_key   = x_http_dictionary_key.
              x_http_update-c_value = x_dictionary->*[ c_key = x_http_dictionary_key ]-c_value.
              append x_http_update to m_tbl_update.
            elseif line_exists( x_dictionary->*[ c_key = to_upper( x_http_element_by_name ) ] ).
              x_http_update-c_key   = x_http_dictionary_key.
              x_http_update-c_value = x_dictionary->*[ c_key = to_upper( x_http_element_by_name ) ]-c_value.
              append x_http_update to m_tbl_update.
            endif.
          endif.
        catch cx_root into data(cx_error_83).
      endtry.
    endloop.

    get time stamp field x_timestop.
    x_seconds = cl_abap_tstmp=>subtract( tstmp1 = x_timestop tstmp2 = x_timestamp ).
    message s000(zcl_eezz_message) with x_seconds into data(x_msg_text).
    x_msg     = value zstr_message( c_msgtext = x_msg_text c_msgcls  = 'zcl_eezz_message' c_msgnum  = 0 ).
    append x_msg to mt_messages.
    render_node( ).

    x_json_response = create_json_response( ).
    iv_ostream->write_string( string = x_json_response ).

    return.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_AGENT->PARSE_EEZZ_DOM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DOCUMENT                    TYPE REF TO IF_IXML_DOCUMENT
* | [--->] IV_OSTREAM                     TYPE REF TO IF_IXML_OSTREAM
* | [<-()] RV_JSON                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method parse_eezz_dom.

    data: x_dictionary type ref to ztty_dictionary.
    data: x_update     type zstr_update.
    data: x_params     type ref to ztty_eezz_json.
    data: x_index_int      type integer,
          x_visible_items  type integer,
          x_visible_blocks type integer.
    data: x_wa_parameter type abap_parmbind,
          x_parameters   type abap_parmbind_tab.

    " Create a click event for eezz-event attributes
    data(x_processor) = new cl_xslt_processor( ).
    x_processor->set_source_node( iv_document ).
    x_processor->set_expression( '//*[@data-eezz-event]' ).
    x_processor->run( progname = space ).
    data(x_nodelist) = x_processor->get_nodes( ).

    if x_nodelist is bound.
      data(x_iterator)  = x_nodelist->create_iterator( ).
      data(x_next)      = x_iterator->get_next( ).
      do 100 times.
        if x_next is not bound.
          exit.
        endif.
        cast if_ixml_element( x_next )->set_attribute( name = 'onclick' value  = 'easyClick(event,this)' ).
        data(x_name) = x_next->get_name( ).
        x_next       = x_iterator->get_next( ).
      enddo.
    endif.

    " Get all elements with the attribute "name" and store in global table
    x_processor = new cl_xslt_processor( ).
    x_processor->set_source_node( iv_document ).
    x_processor->set_expression( '//*[@name]' ).
    x_processor->run( progname = space ).
    x_nodelist = x_processor->get_nodes( ).

    if x_nodelist is bound.
      x_iterator  = x_nodelist->create_iterator( ).
      x_next      = x_iterator->get_next( ).
    endif.

    while x_next is bound.
      data(x_name_attr) = x_next->get_attributes( )->get_named_item_ns( 'name' )->get_value( ).
      data(x_tmpl_node) = x_next.
      data x_wa_named_node type ref to zstr_symbols.
      x_name = x_next->get_name( ).

      if x_name co 'body'.
        " we will prepare the body to return
        x_wa_named_node = new zstr_symbols( c_name = x_name_attr c_tag = x_name c_ref_node = x_next c_templ_node = x_tmpl_node ).
      else.
        " make a copy of regular named nodes
        x_tmpl_node     = x_next->clone( ).
        x_wa_named_node = new zstr_symbols( c_name = x_name_attr c_tag = x_name c_ref_node = x_next c_templ_node = x_tmpl_node ). "? x_tmpl_node->clone( )
      endif.

      " check if we have also an data-eezz-action
      data(x_eezz_action) = x_next->get_attributes( )->get_named_item_ns( 'data-eezz-action' ).
      if x_eezz_action is bound.
        data(x_json) = new zcl_eezz_json( x_eezz_action->get_value( ) ).
        x_wa_named_node->c_eezz_json = x_json.
        data x_ref_global type ref to ztty_global_symbols.
        get reference of m_tbl_global into x_ref_global.
        x_wa_named_node->c_object    = x_json->callback( x_ref_global ).
      endif.

      data(x_eezz_attr) = x_next->get_attributes( )->get_named_item_ns( 'data-eezz-attributes' ).
      if x_eezz_attr is bound.
        x_wa_named_node->c_eezz_json = new zcl_eezz_json( x_eezz_attr->get_value( ) ).
      endif.

      " store message output area
      data(x_eezz_template) = x_next->get_attributes( )->get_named_item_ns( 'data-eezz-template' ).
      if x_eezz_template is bound.
        if x_eezz_template->get_value( ) co 'database' and x_name co 'table'.
          m_message_db-c_name       = x_name_attr.
          m_message_db-c_templ_node = x_next->clone( ).
          " m_message_db-c_object = new zcl_eezz_intertable
        endif.
      endif.

      if  x_wa_named_node is bound.
        append x_wa_named_node->* to m_tbl_global.
      endif.

      x_next = x_iterator->get_next( ).
    endwhile.

    " remove all templates from body:
*    remove_templates( iv_document ). -> moved to xml_node2string()

    " create table
    loop at m_tbl_global assigning field-symbol(<x_entry>).
      data x_tbl_class type string.

      "IF <x_entry>-c_tag CO 'body'.
      "  DATA(x_tbl_class) = <x_entry>-c_eezz_json->get_value( 'eezzAgent.assign' ).
      "  CONTINUE.
      "ELSEIF <x_entry>-c_tag CN 'table'.
      "  CONTINUE.
      "ENDIF.

      if <x_entry>-c_eezz_json is not bound.
        " Hide empty tables
        if <x_entry>-c_tag eq 'table'.
          clear x_update.
          x_update-c_key    = m_message_db-c_name && '.innerHTML'.
          x_update-c_value  = ''.
          x_update-priority = 10.
          append x_update to m_tbl_update.
        endif.
        continue.
      endif.

      try.
          data(x_assign) = <x_entry>-c_eezz_json->get( 'eezzAgent.assign' ).
          if x_assign is not bound.
            continue.
          endif.
          data(x_call)   = x_assign->*[ 1 ].
          x_params       = cast #( x_call-c_ref ).
          x_tbl_class    = x_call-c_key.
        catch cx_sy_itab_line_not_found.
          continue.
      endtry.

      if x_tbl_class is initial.
        continue.
      endif.

      if <x_entry>-c_ref_node->get_name( ) cs 'TABLE'.
        data(xx_new_table)   = create_table( is_entry = <x_entry> ).
        data(xx_ref_table)   = <x_entry>-c_ref_node.
        xx_ref_table->get_parent( )->replace_child( new_child = xx_new_table old_child = xx_ref_table ).
        <x_entry>-c_ref_node = xx_new_table.
        " create navigation updates
        create_navigation_update( <x_entry> ).
      endif.
    endloop.



    " get body node
    read table m_tbl_global with key c_tag = 'body' into data(x_table).
    " create json string
    render_node( iv_node = x_table-c_ref_node ).
    rv_json = create_json_response( ).

    if 1 = 2.
      show_document( iv_document = iv_document iv_ostream = iv_ostream ).
      data(c_out) = cl_demo_output=>new( )->begin_section( `agent result` ).
      c_out->write( rv_json ).
      c_out->display( ).
    endif.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_EEZZ_AGENT->REMOVE_TEMPLATES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DOCUMENT                    TYPE REF TO IF_IXML_DOCUMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method remove_templates.

    data(x_processor) = new cl_xslt_processor( ).
    x_processor->set_source_node( iv_document ).
    x_processor->set_expression( '//*[@data-eezz-template]' ).
    x_processor->run( progname = space ).

    data(x_iterator) = x_processor->get_nodes( )->create_iterator( ).
    data(x_node) = x_iterator->get_next( ).
    while x_node is bound.
      x_node->get_attributes( )->remove_named_item_ns( 'data-eezz-template' ).
      x_node = x_iterator->get_next( ).
    endwhile.

    x_processor->set_expression( '//*[@data-eezz-action]' ).
    x_processor->run( progname = space ).

    x_iterator = x_processor->get_nodes( )->create_iterator( ).
    x_node = x_iterator->get_next( ).
    while x_node is bound.
      x_node->get_attributes( )->remove_named_item_ns( 'data-eezz-action' ).
      x_node = x_iterator->get_next( ).
    endwhile.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_EEZZ_AGENT->RENDER_NODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE                        TYPE REF TO IF_IXML_NODE(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method render_node.

    data: x_update     type zstr_update.
    " data x_cls_tblnode type ref to zcl_eezz_table_node.

    " create html string
    if iv_node is bound.
      data(x_tbl_string) = xml_node2string( iv_node ).
      data(x_attributes) = iv_node->get_attributes( ).
      data(x_iterator)   = x_attributes->create_iterator( ).
      data(x_element)    = x_iterator->get_next( ).

      while x_element is bound.
        data(x_name) = x_element->get_name( ).
        if x_name eq 'name'.
          data(x_value) = x_element->get_value( ).
          exit.
        endif.
        x_element = x_iterator->get_next( ).
      endwhile.

      x_update-c_key    = x_value && '.innerHTML'.
      x_update-c_value  = x_tbl_string.
      x_update-priority = 1.
      append x_update to m_tbl_update.
    elseif lines( mt_messages ) gt 0.
      if m_message_db-c_name is initial.
        x_update-c_key   = 'eezz_errors.innerHTML'.
        x_update-c_value = '<tr>'.
        loop at mt_messages into data(wa_msg).
          x_update-c_value = x_update-c_value &&
              |<td>{ wa_msg-c_msgtext }</td|  &&
              |<td>{ wa_msg-c_msgcls } </td|  &&
              |<td>{ wa_msg-c_msgnum } </td|.
        endloop.
        concatenate x_update-c_value '</tr>' into x_update-c_value.
        append x_update to m_tbl_update.
      else.
        get reference of mt_messages   into data(x_ref_msg).
        get reference of m_tbl_global  into data(x_ref_global).

        data(x_cls_message) = new zcl_eezz_table( iv_table = x_ref_msg ).
        data(x_cls_tblnode) = new zcl_eezz_table_node(
          io_eezz_tbl = x_cls_message
          io_node     = m_message_db-c_templ_node
          it_globals  = x_ref_global ).

        x_update-c_key    = m_message_db-c_name && '.innerHTML'.
        x_update-c_value  = xml_node2string( x_cls_tblnode->get( ) ).
        append x_update to m_tbl_update.
      endif.
      clear mt_messages.
    endif.

    " create json string
    " rv_json = create_json_response( ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_EEZZ_AGENT->SHOW_DOCUMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DOCUMENT                    TYPE REF TO IF_IXML_DOCUMENT
* | [--->] IV_OSTREAM                     TYPE REF TO IF_IXML_OSTREAM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method show_document.

    data(x_ixml) = cl_ixml=>create( ).

    x_ixml->create_renderer(
      document = iv_document
      ostream  = iv_ostream )->render( ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_EEZZ_AGENT->XML_NODE2STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_NODE                        TYPE REF TO IF_IXML_NODE
* | [<-()] RV_STRING                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method xml_node2string.
    data x_string type string.
    data(x_ixml)     = cl_ixml=>create( ).
    data(x_oostream) = x_ixml->create_stream_factory( )->create_ostream_cstring( string = rv_string ).

    "remove_templates( io_node ).
    data(x_filter1)  = io_node->create_filter_attribute_ns( name = |data-eezz-template| ).
    data(x_filter2)  = io_node->create_filter_attribute_ns( name = |data-eezz-action| ).
    data(x_filter)   = io_node->create_filter_or( filter1 = x_filter1 filter2 = x_filter2 ).
    data(x_iterator) = io_node->create_iterator_filtered( filter = x_filter ).

    do 100 times.
      data(x_next) = x_iterator->get_next( ).
      if x_next is not bound.
        exit.
      endif.
      x_next->get_attributes( )->remove_named_item_ns( |data-eezz-template| ).
      x_next->get_attributes( )->remove_named_item_ns( |data-eezz-action| ).

      " delete class eezz_template
      data(x_class_attr_map) = cast if_ixml_named_node_map( x_next->get_attributes( )->get_named_item_ns( |class| ) ).
      if x_class_attr_map is bound.
        data(x_class_attr) = x_class_attr_map->get_item( 1 )->get_value( ).

        "SPLIT lv_string AT space INTO TABLE lt_split.
      endif.
    enddo.

    " Render inner HTML
    x_iterator = io_node->create_iterator( 1 ).
    x_next     = x_iterator->get_next( ).

    while x_next is bound.
      x_next = x_iterator->get_next( ).
      if x_next is not bound.
        exit.
      endif.
      x_next->render( ostream = x_oostream recursive = abap_true ).
      data(x_name) = x_next->get_name( ).
    endwhile.
    return.

*    DATA(x_new_xml) = cl_ixml=>create( ).
*    DATA(x_new_doc) = x_new_xml->create_document( ).
*    DATA(x_ostream) = x_new_xml->create_stream_factory( )->create_ostream_cstring( string = rv_string ).
*    x_new_doc->append_child( io_node ).
*    remove_templates( x_new_doc ).*
*    x_new_xml->create_renderer(
*      document = x_new_doc
*      ostream  = x_ostream )->render( ).

*-- Get rid of the <?xml encoding=??? > tag.
*    DATA: defidx    TYPE sy-fdpos,
*          clstagidx TYPE sy-fdpos.

*    defidx = -1. clstagidx = 10000.
*    IF rv_string CS '<?xml'.                                "#EC NOTEXT
*      defidx = sy-fdpos.
*      IF rv_string CS '>'.
*        clstagidx = sy-fdpos + 1.
*        IF clstagidx > defidx.
*          rv_string = rv_string+clstagidx.
*        ENDIF.
*      ENDIF.
*    ENDIF.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_EEZZ_AGENT=>ZIF_EEZZ_AGENT~ON_WEBSOCKET
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_MESSAGE                      TYPE REF TO IF_APC_WSP_MESSAGE
* | [--->] I_MESSAGE_MANAGER              TYPE REF TO IF_APC_WSP_MESSAGE_MANAGER
* | [--->] I_CONTEXT                      TYPE REF TO IF_APC_WSP_SERVER_CONTEXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_eezz_agent~on_websocket.

**************************************************************
* Create XML for sent HTML
**************************************************************
    " --> all this has to be excuted once!
    " --> check if an agent exists and else execute:
    data:
      x_xml_cl         type ref to if_ixml,
      x_stream_factory type ref to if_ixml_stream_factory,
      x_xml_parser     type ref to if_ixml_parser,
      x_in_stream      type ref to if_ixml_istream,
      x_out_stream     type ref to if_ixml_ostream,
      x_document       type ref to if_ixml_document,
      x_apc_error      type ref to cx_apc_error,
      x_agent          type ref to zcl_eezz_agent,
      x_json           type ref to ztty_eezz_json,
      x_response       type string,
      x_message        type ref to if_apc_wsp_message,
      x_offset         type i.


    " Clear previous updates
    clear m_tbl_update.

    if zif_eezz_agent~m_eezz_agent is not bound.
      data(x_cl_json)  = new zcl_eezz_json( iv_json = i_message->get_text( ) iv_conv_qmark = space ).

      data(x_str_doc)  = x_cl_json->get_value( 'document' ).

      " More efficient using stream and regex, processing the document once.

      "--DATA(x_str_path)  = 'http://ldcixp1.mo.sap.corp:44311/' && x_cl_json->get_value( 'path' ).
      "--data x_lo_client type ref to if_http_client.
      "--cl_http_client=>create_by_url( EXPORTING url = x_str_path IMPORTING client = x_lo_client ).

      data(x_result_stream) = new cl_abap_string_c_writer(  ).
      data(x_regex_pattern) = '<(img|area|br|link|input|hr)[a-zA-Z0-9=",.:#*\-_''{}[:space:]]*>'.

      data(x_regex_parser)  = new cl_abap_regex( pattern = x_regex_pattern  ).
      data(x_regex_matcher) = x_regex_parser->create_matcher( text = x_str_doc ).
      data x_map_offset type i value 0.
      data x_map_length type i value 0.

      data(x_match_result)  = x_regex_matcher->find_all( ).

      x_result_stream->write( |<html>| ).
      loop at x_match_result into data(wa_regex).
        x_map_length = wa_regex-offset - x_map_offset.
        x_result_stream->write( substring( val = x_str_doc off = x_map_offset len = x_map_length ) ).
        x_map_offset = wa_regex-offset + wa_regex-length.

        x_result_stream->write( substring( val = x_str_doc off = wa_regex-offset len = wa_regex-length - 1 ) ).
        x_result_stream->write( |/>| ).
      endloop.
      x_result_stream->write( substring( val = x_str_doc off = x_map_offset ) ).
      x_result_stream->write( |</html>| ).
      x_str_doc = x_result_stream->get_result_string( ).

      try.
          data(x_xml_data) = i_message->get_text( ).
          x_message = i_message_manager->create_message( ).

          x_xml_cl         = cl_ixml=>create( ).
          x_stream_factory = x_xml_cl->create_stream_factory( ).
          x_in_stream      = x_stream_factory->create_istream_string( string = x_str_doc ).
          x_out_stream     = x_stream_factory->create_ostream_cstring( x_response ).

          x_document   = x_xml_cl->create_document( ).
          x_xml_parser = x_xml_cl->create_parser(
                        document       = x_document
                        stream_factory = x_stream_factory
                        istream        = x_in_stream ).

          if x_xml_parser->parse( ) = 0.
            " we would need to save this instance for a session
            zif_eezz_agent~m_eezz_agent = new zcl_eezz_agent( ).
            x_agent ?= zif_eezz_agent~m_eezz_agent.
            data(x_response2) = x_agent->parse_eezz_dom( iv_document = x_document iv_ostream = x_out_stream ).
            x_message->set_text( x_response2 ).
            i_message_manager->send( x_message ).
          else.
            data(x_tmp_agent) = new zcl_eezz_agent( ).
            data(x_num_err)   = x_xml_parser->num_errors( ).
            data: x_cnt type i value 0.
            do x_num_err times.
              data(x_error) = x_xml_parser->get_error( x_cnt ).
              data(x_reason) = x_error->get_reason( ).
              data(x_lineno) = x_error->get_line( ).
              data(x_column) = x_error->get_column( ).

              add 1 to x_cnt.

              message s001(zcl_eezz_message) with x_reason x_lineno x_column into data(x_msg_text).
              data(x_msg)  = value zstr_message( c_msgtext = x_msg_text c_msgcls  = 'zcl_eezz_message' c_msgnum  = 1 ).
              append x_msg to x_tmp_agent->mt_messages.

              x_tmp_agent->render_node( ).
              x_response       = x_tmp_agent->create_json_response( ).
              x_xml_cl         = cl_ixml=>create( ).
              x_stream_factory = x_xml_cl->create_stream_factory( ).
              x_out_stream     = x_stream_factory->create_ostream_cstring( x_response ).
              x_message        = i_message_manager->create_message( ).
              x_message->set_text( x_response ).
              i_message_manager->send( x_message ).
            enddo.
          endif.
        catch cx_apc_error into x_apc_error.
      endtry.
    else.
      if zif_eezz_agent~m_eezz_agent is bound.
        try.
            x_xml_cl         = cl_ixml=>create( ).
            x_stream_factory = x_xml_cl->create_stream_factory( ).
            x_out_stream     = x_stream_factory->create_ostream_cstring( x_response ).
            x_agent     ?= zif_eezz_agent~m_eezz_agent.
            x_agent->handle_request( iv_str_json = i_message->get_text( ) iv_ostream = x_out_stream ).
            if x_response is not initial.
              x_message = i_message_manager->create_message( ).
              x_message->set_text( x_response ).
              i_message_manager->send( x_message ).
            endif.
          catch cx_apc_error into x_apc_error.
        endtry.
      endif.
    endif.
    " --- finish ---
  endmethod.
ENDCLASS.