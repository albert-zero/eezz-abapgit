class ZCL_EEZZ_AGENT definition
  public
  final
  create public

  global friends ZIF_EEZZ_AGENT .

public section.

  interfaces ZIF_EEZZ_AGENT .
  interfaces IF_AMC_MESSAGE_RECEIVER .
  interfaces IF_AMC_MESSAGE_RECEIVER_PCP .

  data MT_MESSAGES type ZTTY_MESSAGES .

  methods PARSE_EEZZ_DOM
    importing
      !IV_DOCUMENT type ref to IF_IXML_DOCUMENT
      !IV_OSTREAM type ref to IF_IXML_OSTREAM
    returning
      value(RV_JSON) type STRING .
  methods CONSTRUCTOR .
  protected section.
private section.

  class-data M_MESSAGE_MGR type ref to IF_APC_WSP_MESSAGE_MANAGER .
  class-data M_TBL_EVENT type ZTTY_EVENTS .
  data M_MESSAGE_DB type ZSTR_SYMBOLS .
  class-data M_TBL_GLOBAL type ZTTY_SYMBOLS .
  class-data M_TBL_UPDATE type ZTTY_UPDATE .

  methods CREATE_NAVIGATION_UPDATE
    importing
      !IV_SYMBOL type ZSTR_SYMBOLS .
  methods CREATE_TABLE
    importing
      !IS_ENTRY type ZSTR_SYMBOLS
      !IS_EEZZ_TABLE type ref to ZIF_EEZZ_TABLE optional
    returning
      value(RO_NEW_NODE) type ref to IF_IXML_NODE .
  methods SHOW_DOCUMENT
    importing
      !IV_DOCUMENT type ref to IF_IXML_DOCUMENT
      !IV_OSTREAM type ref to IF_IXML_OSTREAM .
  methods XML_NODE2STRING
    importing
      !IO_NODE type ref to IF_IXML_NODE
    returning
      value(RV_STRING) type STRING .
  methods HANDLE_REQUEST
    importing
      !IV_OSTREAM type ref to IF_IXML_OSTREAM
      !IV_STR_JSON type STRING .
  methods RENDER_NODE
    importing
      !IT_UPDATE type ref to ZTTY_UPDATE
      !IV_NODE type ref to IF_IXML_NODE optional
      !IV_PATH type STRING optional
      !IV_JSON type ref to ZCL_EEZZ_JSON optional .
ENDCLASS.



CLASS ZCL_EEZZ_AGENT IMPLEMENTATION.


  method constructor.
    try.
        data(x_msg_cons) = cl_amc_channel_manager=>create_message_consumer(
                              i_application_id = 'Z_EEZZ_WS_MSG_CHANNEL'
                              i_channel_id     = '/eezz/web').
        x_msg_cons->start_message_delivery( i_receiver = me ).
      catch cx_amc_error.
    endtry.
  endmethod.


  method create_navigation_update.

    data: x_dictionary type ref to ztty_dictionary,
          x_update     type zstr_update,
          x_value      type string.

    data(x_attributes)    = iv_symbol-c_ref_node->get_attributes( ).
    "data(x_action_attr)   = x_next->get_attributes( )->get_named_item_ns( 'data-eezz-action' )->get_value( ).
    data(x_attr_iterator) = x_attributes->create_iterator( ).
    data(x_attr_element)  = x_attr_iterator->get_next( ).

    FIELD-SYMBOLS: <fs> type zstr_update.

    call method iv_symbol-c_object->('GET_DICTIONARY') receiving rt_dictionary = x_dictionary.

    data(x_regex_pattern) = '\{([a-zA-Z0-9.:*\-_]*)\}'.
    data(x_regex_parser)  = new cl_abap_regex( pattern = x_regex_pattern  ).

    while x_attr_element is bound.
      try.
          data(x_attr_name)   = x_attr_element->get_name( ).
          data(x_attr_value)  = x_attr_element->get_value( ).
          data(x_matcher)     = x_regex_parser->create_matcher( text = x_attr_value ).

          if x_matcher->match( ).
            data(x_key)       = x_matcher->get_submatch( 1 ).
            data(x_new_value) = x_dictionary->*[ c_key = x_key ].

            append value #(
                c_key   = |{ iv_symbol-c_name }.{ x_attr_name }|
                c_value = x_new_value-c_value
                c_prio  = 10 ) to m_tbl_update.

          elseif x_attr_name cs |data-eezz-action|.
            data(x_json) = new zcl_eezz_json( iv_json = x_attr_value ).
            data(x_updt) = x_json->get( |update| ).

            if x_updt is bound.
              loop at x_updt->* into data(x_upd_wa).
                if not line_exists( m_tbl_update[ c_key = x_upd_wa-c_key ] ).
                  if x_upd_wa-c_type cs |object|.
                    x_value = x_json->dump( |update/{ x_upd_wa-c_key }| )->get_result_string( ).
                  else.
                    x_value = x_upd_wa-c_value.
                  endif.
                  append value #( c_key = x_upd_wa-c_key c_value = x_value c_prio = 10 ) to m_tbl_update.
                endif.
              endloop.
            endif.
          endif.

        catch cx_sy_no_current_match.
        catch cx_sy_itab_line_not_found.
        catch cx_sy_matcher.
      endtry.

      x_attr_element = x_attr_iterator->get_next( ).
    endwhile.

  endmethod.


  method create_table.

    data: x_node_table type ref to zcl_eezz_table_node,
          x_globals    type ref to ztty_symbols.
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
    data(x_json_update)    = x_json->get( iv_path = 'update' ).
    data(x_json_callback)  = x_json->get( iv_path = 'callback' ).

    data x_json_object_name  type string.
    data x_json_method_name  type string.

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
    " if x_eezz_table is bound.
    "   if x_json_callback is bound.
    "     split x_json_callback->*[ 1 ]-c_key at '.' into x_json_object_name x_json_method_name.
    "   endif.
    " endif.

    " Enter the result of the callback into the update
    if x_json_update is bound.
      loop at x_json_update->* into data(x_update).
        " The update has the format <tag-name>.[(style.)]<attribute> = (*) | <tag-name>.<attribute>
        clear x_ref_obj.
        data(x_style) = ''.

        try.
            if x_update-c_value cs '*'.
              split x_update-c_key at '.' into table data(x_key_struct).
              data(x_http_dictionary_key)  = x_update-c_key.
              data(x_http_element_by_name) = x_key_struct[ 1 ].
              data(x_http_element_attr)    = x_key_struct[ 2 ].
              data(x_http_element_path)    = x_key_struct[ 3 ].
            else.
              split x_update-c_value at '.' into table x_key_struct.
              x_http_dictionary_key        = x_update-c_value.
              x_http_element_by_name       = x_key_struct[ 1 ].
              x_http_element_attr          = x_key_struct[ 2 ].
              x_http_element_path          = x_key_struct[ 3 ].
            endif.
          catch cx_sy_itab_line_not_found.
        endtry.

        try.
            data(x_global_symbol) = m_tbl_global[ c_name = x_http_element_by_name ].

            if x_eezz_table is bound.
              x_ref_obj = x_eezz_table.
            else.
              x_ref_obj = x_global_symbol-c_object.
            endif.

            if x_http_element_attr cs |style|.
              x_http_element_attr = x_http_element_path.
            endif.

            if x_http_element_attr cs 'innerHTML'.
              " For innerHTML we have to create the node tree
              x_new_table = create_table( is_entry = x_global_symbol is_eezz_table = x_ref_obj ).
              render_node( it_update = ref #( m_tbl_update ) iv_node = x_new_table iv_path = x_http_dictionary_key ).
              create_navigation_update( x_global_symbol ).
            else.
              " For any attribute we use the dictionary of the eezz_table
              data x_http_update type zstr_update.
              data(x_dictionary)    = x_ref_obj->get_dictionary( ).

              if line_exists( x_dictionary->*[ c_key = to_upper( x_http_dictionary_key ) ] ).
                x_http_update-c_key   = x_http_dictionary_key.
                x_http_update-c_value = x_dictionary->*[ c_key = x_http_dictionary_key ]-c_value.
                append x_http_update to m_tbl_update.
              elseif line_exists( x_dictionary->*[ c_key = to_upper( x_http_element_attr ) ] ).
                x_http_update-c_key   = x_http_dictionary_key.
                x_http_update-c_value = x_dictionary->*[ c_key = to_upper( x_http_element_attr ) ]-c_value.
                append x_http_update to m_tbl_update.
              endif.
            endif.
          catch cx_root into data(cx_error_83).
        endtry.
      endloop.
    endif.

    get time stamp field x_timestop.
    x_seconds = cl_abap_tstmp=>subtract( tstmp1 = x_timestop tstmp2 = x_timestamp ).
    "message s000(zcl_eezz_message) with x_seconds into data(x_msg_text).
    message id 'ZCL_EEZZ_MESSAGE' type 'S' number 0  with x_seconds into data(x_msg_text).
    x_msg     = value zstr_message( c_msgtext = x_msg_text c_msgcls  = 'zcl_eezz_message' c_msgnum  = 0 ).
    append x_msg to mt_messages.
    render_node( ref #( m_tbl_update ) ).

    x_json_response = zcl_eezz_json=>gen_response( ref #( m_tbl_update ) ).
    iv_ostream->write_string( string = x_json_response ).

  endmethod.


  method if_amc_message_receiver_pcp~receive.

    data: x_pcp_fields type pcp_fields.
    data: xt_update    type ztty_update.

    try.
        i_message->get_fields( changing c_fields = x_pcp_fields ).
        data(x_event_key)   = i_message->get_field( |eezz_event_key| ).
        data(x_target)      = i_message->get_field( |eezz_target| ).
        data(x_agent)       = zcl_eezz_agent=>zif_eezz_agent~m_eezz_agent .

        data(x_event_entry) = m_tbl_event[ c_hash = x_event_key ].

        loop at x_pcp_fields into data(x_pcp).
          data(x_translate) = x_event_entry-c_json->get( |/{ x_event_key }/{ x_pcp-name }.{ x_pcp-value }| ).

          if x_translate is bound.
            data(x_named_target) = x_translate->*[ 1 ]-c_key.

            replace 'this' with x_target into x_named_target.
            append value #( c_key = x_named_target c_value = x_translate->*[ 1 ]-c_value ) to xt_update.
          endif.
        endloop.

        data(x_response)    = zcl_eezz_json=>gen_response( ref #( xt_update ) ).

        cast if_amc_message_producer_text(
             cl_amc_channel_manager=>create_message_producer(
               i_application_id = 'Z_EEZZ_WS_MSG_CHANNEL'
               i_channel_id     = '/eezz' )
          )->send( i_message = x_response ).
      catch cx_amc_error cx_root into data(x_exc).
        data(x_text) = x_exc->get_text( ).
    endtry.
  endmethod.


  method parse_eezz_dom.

    data: x_dictionary     type ref to ztty_dictionary.
    data: x_update         type zstr_update.
    data: x_params         type ref to ztty_eezz_json.
    data: x_index_int      type integer,
          x_visible_items  type integer,
          x_visible_blocks type integer.

    data: x_wa_parameter type abap_parmbind,
          x_parameters   type abap_parmbind_tab.
    data: x_jsn_animation  type ref to zcl_eezz_json.

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
        x_next = x_iterator->get_next( ).
      enddo.
    endif.

    " Get all elements with the attribute "name" and store in global table
    "---x_processor = new cl_xslt_processor( ).
    "---x_processor->set_source_node( iv_document ).
    x_processor->set_expression( '//*[@name]' ).
    x_processor->run( progname = space ).
    x_nodelist = x_processor->get_nodes( ).

    if x_nodelist is bound.
      x_iterator  = x_nodelist->create_iterator( ).
    endif.

    do 1000 times.
      x_next = x_iterator->get_next( ).
      if x_next is not bound.
        exit.
      endif.

      data(x_name_attr) = x_next->get_attributes( )->get_named_item_ns( 'name' )->get_value( ).
      data(x_tmpl_node) = x_next.
      data x_wa_named_node type ref to zstr_symbols.
      data(x_name)      = x_next->get_name( ).

      if x_name_attr cs 'eezz-i18n-'.
        data x_number type int4.
        x_number  = x_name_attr+10.
        x_update-c_key  = |{ x_name_attr }.innerHTML|.
        message id 'ZCL_EEZZ_MESSAGE' type 'S' number x_number into data(x_translated).
        if strlen( x_translated ) > 0.
          x_update-c_value = x_translated.
          append x_update to m_tbl_update.
        endif.
      endif.

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
        data(x_json)      = new zcl_eezz_json( iv_json = x_eezz_action->get_value( ) ).
        x_wa_named_node->c_eezz_json = x_json.

        data(x_sym_local) = value ztty_eezz_json( ( c_key = 'table_name' c_value = x_name_attr ) ).
        x_wa_named_node->c_object = x_json->callback(
            iv_symbols   = ref #( m_tbl_global )
            iv_parameter = ref #( x_sym_local  ) ).
      endif.

      data(x_eezz_attr) = x_next->get_attributes( )->get_named_item_ns( 'data-eezz-attributes' ).
      if x_eezz_attr is bound.
        x_wa_named_node->c_eezz_json = new zcl_eezz_json( iv_json = x_eezz_attr->get_value( ) ).
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

      data(x_eezz_script) = x_next->get_attributes( )->get_named_item_ns( 'data-eezz-async' ).
      if x_eezz_script is bound.
        x_json = new zcl_eezz_json( iv_json = x_eezz_script->get_value( ) ).
        data(x_tbl_script) = x_json->get( ).
        data(x_key_script) = x_tbl_script->*[ 1 ].
        append value #( c_name = x_name_attr c_hash = x_key_script-c_key c_json = x_json ) to m_tbl_event.
      endif.

      if  x_wa_named_node is bound.
        append x_wa_named_node->* to m_tbl_global.
      endif.
    enddo.

    " remove all templates from body:
    " remove_templates( iv_document ). -> moved to xml_node2string()

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
          x_update-c_prio   = 10.
          append x_update to m_tbl_update.
        endif.
        continue.
      endif.

      try.
          x_json ?= <x_entry>-c_eezz_json.

          data(x_assign) = x_json->get( 'eezzAgent.assign' ).
          if x_assign is not bound.
            continue.
          endif.

          data(x_call)   = x_assign->*[ 1 ].
          x_params       = cast #( x_call-c_ref ).
          x_tbl_class    = x_call-c_key.
        catch cx_sy_itab_line_not_found.
          " CONTINUE.
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

        data(x_tree_class) = cast if_ixml_element( xx_new_table )->get_attribute_ns( 'class' ).
        data(x_tree_id)    = cast if_ixml_element( xx_new_table )->get_attribute_ns( 'id' ).

        if x_tree_class cs |eezzTree|.
          render_node( it_update = ref #( m_tbl_update ) iv_node = <x_entry>-c_ref_node iv_path = |{ <x_entry>-c_name }.innerHTML.{ x_tree_id }| ).
        else.
          render_node( it_update = ref #( m_tbl_update ) iv_node = <x_entry>-c_ref_node iv_path = |{ <x_entry>-c_name }.innerHTML| ).
        endif.
      endif.
    endloop.

    rv_json = zcl_eezz_json=>gen_response( it_update = ref #( m_tbl_update ) ).

    " get body node
    "az----read table m_tbl_global with key c_tag = 'body' into data(x_table).
    " create json string
    "az----render_node( iv_node = x_table-c_ref_node ).
    "az----rv_json = create_json_response( ).

    if 1 = 2.
      show_document( iv_document = iv_document iv_ostream = iv_ostream ).
      data(c_out) = cl_demo_output=>new( )->begin_section( `agent result` ).
      c_out->write( rv_json ).
      c_out->display( ).
    endif.
  endmethod.


  method render_node.

    data: x_update      type zstr_update.
    FIELD-SYMBOLS: <fs> type zstr_update.
    " data x_cls_tblnode type ref to zcl_eezz_table_node.

    " create html string
    if iv_node is bound.
      data(x_tbl_string) = xml_node2string( iv_node ).
      if line_exists( it_update->*[ c_key = iv_path ] ).
        assign it_update->*[ c_key = iv_path ] to <fs>.
        <fs>-c_value = x_tbl_string.
      else.
        x_update-c_key    = iv_path.
        x_update-c_value  = x_tbl_string.
        x_update-c_prio   = 1.
        append x_update to it_update->*.
      endif.
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
        append x_update to it_update->*.
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
        append x_update to it_update->*.
      endif.
      clear mt_messages.
    endif.

    " create json string
    " rv_json = create_json_response( ).

  endmethod.


  method show_document.

    data(x_ixml) = cl_ixml=>create( ).

    x_ixml->create_renderer(
      document = iv_document
      ostream  = iv_ostream )->render( ).

  endmethod.


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
      "---x_next->get_attributes( )->remove_named_item_ns( |data-eezz-template| ).
      "---x_next->get_attributes( )->remove_named_item_ns( |data-eezz-action| ).

      " delete class eezz_template
      "---data(x_class_attr_map) = cast if_ixml_named_node_map( x_next->get_attributes( )->get_named_item_ns( |class| ) ).
      "---if x_class_attr_map is bound.
      "---  data(x_class_attr) = x_class_attr_map->get_item( 1 )->get_value( ).

        "SPLIT lv_string AT space INTO TABLE lt_split.
      "---endif.
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

  endmethod.


  method ZIF_EEZZ_AGENT~ON_BINARY_TRANSFER.
  endmethod.


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
      x_offset         type i,
      x_query          type tihttpnvp.

    m_message_mgr = i_message_manager.

    " Clear previous updates
    clear m_tbl_update.

    try.
      data(x_request) = i_context->get_initial_request( ).
      x_request->get_header_fields( changing c_fields = x_query ).
      x_request->get_form_fields( changing c_fields = x_query ).

      data(xxd_value)  = x_request->get_form_field( 'document' ).
    catch cx_apc_error.
      return.
    endtry.

    if zif_eezz_agent~m_eezz_agent is not bound.
      try.
          data(x_cl_json)  = new zcl_eezz_json( iv_json = i_message->get_text( ) iv_conv_qmark = space ).
        catch cx_apc_error.
          return.
      endtry.

      data(x_str_doc)  = x_cl_json->get_value( 'document' ).
      find '<body' in x_str_doc match offset data(x_body_off).
      " More efficient using stream and regex, processing the document once.

      "--DATA(x_str_path)  = 'http://ldcixp1.mo.sap.corp:44311/' && x_cl_json->get_value( 'path' ).
      "--data x_lo_client type ref to if_http_client.
      "--cl_http_client=>create_by_url( EXPORTING url = x_str_path IMPORTING client = x_lo_client ).

      data(x_result_stream) = new cl_abap_string_c_writer(  ).
      data(x_regex_pattern) = '<(img|area|br|link|input|hr)[a-zA-Z0-9/%=",.:#*\-_'';{}[:space:]]*>'.

      data(x_regex_parser)  = new cl_abap_regex( pattern = x_regex_pattern  ).
      data(x_regex_matcher) = x_regex_parser->create_matcher( text = x_str_doc ).
      data x_map_offset type i value 0.
      data x_map_length type i value 0.

      x_map_offset = x_body_off.
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

              x_tmp_agent->render_node( it_update = ref #( m_tbl_update ) ).
              x_response       = zcl_eezz_json=>gen_response( ref #( m_tbl_update ) ). " x_tmp_agent->create_json_response( ).
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
            x_agent         ?= zif_eezz_agent~m_eezz_agent.

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
