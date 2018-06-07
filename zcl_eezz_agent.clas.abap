class ZCL_EEZZ_AGENT definition
  public
  final
  create public

  global friends ZIF_EEZZ_AGENT .

public section.

  interfaces ZIF_EEZZ_AGENT .

  data MT_MESSAGES type ZTTY_MESSAGES .

  methods PARSE_EEZZ_DOM
    importing
      !IV_DOCUMENT type ref to IF_IXML_DOCUMENT
      !IV_OSTREAM type ref to IF_IXML_OSTREAM
    returning
      value(RV_JSON) type STRING .
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
      !IV_NODE type ref to IF_IXML_NODE optional
      !IV_PATH type STRING optional .
ENDCLASS.



CLASS ZCL_EEZZ_AGENT IMPLEMENTATION.


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
              x_update-c_prio = 10.
              append x_update to m_tbl_update.
            endif.
          catch cx_sy_itab_line_not_found.
        endtry.
      endif.
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

          if x_http_element_attr cs 'innerHTML'.
            " For innerHTML we have to create the node tree
            x_new_table = create_table( is_entry = x_global_symbol is_eezz_table = x_ref_obj ).
            render_node( iv_node = x_new_table iv_path = x_http_dictionary_key ).
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
    "message s000(zcl_eezz_message) with x_seconds into data(x_msg_text).
    message id 'ZCL_EEZZ_MESSAGE' type 'S' number 0  with x_seconds into data(x_msg_text).
    x_msg     = value zstr_message( c_msgtext = x_msg_text c_msgcls  = 'zcl_eezz_message' c_msgnum  = 0 ).
    append x_msg to mt_messages.
    render_node( ).

    get reference of m_tbl_update into data(x_tbl_update).
    x_json_response = zcl_eezz_json=>gen_response( x_tbl_update ).
    iv_ostream->write_string( string = x_json_response ).

  endmethod.


  METHOD parse_eezz_dom.

    DATA: x_dictionary TYPE REF TO ztty_dictionary.
    DATA: x_update     TYPE zstr_update.
    DATA: x_params     TYPE REF TO ztty_eezz_json.
    DATA: x_index_int      TYPE integer,
          x_visible_items  TYPE integer,
          x_visible_blocks TYPE integer.
    DATA: x_wa_parameter TYPE abap_parmbind,
          x_parameters   TYPE abap_parmbind_tab.

    " Create a click event for eezz-event attributes
    DATA(x_processor) = NEW cl_xslt_processor( ).
    x_processor->set_source_node( iv_document ).
    x_processor->set_expression( '//*[@data-eezz-event]' ).
    x_processor->run( progname = space ).
    DATA(x_nodelist) = x_processor->get_nodes( ).

    IF x_nodelist IS BOUND.
      DATA(x_iterator)  = x_nodelist->create_iterator( ).
      DATA(x_next)      = x_iterator->get_next( ).
      DO 100 TIMES.
        IF x_next IS NOT BOUND.
          EXIT.
        ENDIF.
        CAST if_ixml_element( x_next )->set_attribute( name = 'onclick' value  = 'easyClick(event,this)' ).
        x_next = x_iterator->get_next( ).
      ENDDO.
    ENDIF.

    " Get all elements with the attribute "name" and store in global table
    "---x_processor = new cl_xslt_processor( ).
    "---x_processor->set_source_node( iv_document ).
    x_processor->set_expression( '//*[@name]' ).
    x_processor->run( progname = space ).
    x_nodelist = x_processor->get_nodes( ).

    IF x_nodelist IS BOUND.
      x_iterator  = x_nodelist->create_iterator( ).
    ENDIF.

    DO 1000 TIMES.
      x_next = x_iterator->get_next( ).
      IF x_next IS NOT BOUND.
        EXIT.
      ENDIF.

      DATA(x_name_attr) = x_next->get_attributes( )->get_named_item_ns( 'name' )->get_value( ).
      DATA(x_tmpl_node) = x_next.
      DATA x_wa_named_node TYPE REF TO zstr_symbols.
      DATA(x_name)      = x_next->get_name( ).

      IF x_name_attr CS 'eezz-i18n-'.
        DATA x_number TYPE int4.
        x_number  = x_name_attr+10.
        x_update-c_key  = |{ x_name_attr }.innerHTML|.
        MESSAGE ID 'ZCL_EEZZ_MESSAGE' TYPE 'S' NUMBER x_number INTO DATA(x_translated).
        IF strlen( x_translated ) > 0.
          x_update-c_value = x_translated.
          APPEND x_update TO m_tbl_update.
        ENDIF.
      ENDIF.

      IF x_name CO 'body'.
        " we will prepare the body to return
        x_wa_named_node = NEW zstr_symbols( c_name = x_name_attr c_tag = x_name c_ref_node = x_next c_templ_node = x_tmpl_node ).
      ELSE.
        " make a copy of regular named nodes
        x_tmpl_node     = x_next->clone( ).
        x_wa_named_node = NEW zstr_symbols( c_name = x_name_attr c_tag = x_name c_ref_node = x_next c_templ_node = x_tmpl_node ). "? x_tmpl_node->clone( )
      ENDIF.

      " check if we have also an data-eezz-action
      DATA(x_eezz_action) = x_next->get_attributes( )->get_named_item_ns( 'data-eezz-action' ).
      IF x_eezz_action IS BOUND.
        DATA(x_json) = NEW zcl_eezz_json( x_eezz_action->get_value( ) ).
        x_wa_named_node->c_eezz_json = x_json.
        DATA x_ref_global TYPE REF TO ztty_symbols.
        "data x_sym_local  type ztty_eezz_json.

        data(x_sym_local) = value ztty_eezz_json( ( c_key = 'table_name' c_value = x_name_attr ) ).

        GET REFERENCE OF m_tbl_global INTO x_ref_global.
        get reference of x_sym_local  into data(x_ref_local).
        x_wa_named_node->c_object    = x_json->callback( iv_symbols = x_ref_global iv_parameter = x_ref_local ).
      ENDIF.

      DATA(x_eezz_attr) = x_next->get_attributes( )->get_named_item_ns( 'data-eezz-attributes' ).
      IF x_eezz_attr IS BOUND.
        x_wa_named_node->c_eezz_json = NEW zcl_eezz_json( x_eezz_attr->get_value( ) ).
      ENDIF.

      " store message output area
      DATA(x_eezz_template) = x_next->get_attributes( )->get_named_item_ns( 'data-eezz-template' ).
      IF x_eezz_template IS BOUND.
        IF x_eezz_template->get_value( ) CO 'database' AND x_name CO 'table'.
          m_message_db-c_name       = x_name_attr.
          m_message_db-c_templ_node = x_next->clone( ).
          " m_message_db-c_object = new zcl_eezz_intertable
        ENDIF.
      ENDIF.

      IF  x_wa_named_node IS BOUND.
        APPEND x_wa_named_node->* TO m_tbl_global.
      ENDIF.
    ENDDO.

    " remove all templates from body:
*    remove_templates( iv_document ). -> moved to xml_node2string()

    " create table
    LOOP AT m_tbl_global ASSIGNING FIELD-SYMBOL(<x_entry>).
      DATA x_tbl_class TYPE string.

      "IF <x_entry>-c_tag CO 'body'.
      "  DATA(x_tbl_class) = <x_entry>-c_eezz_json->get_value( 'eezzAgent.assign' ).
      "  CONTINUE.
      "ELSEIF <x_entry>-c_tag CN 'table'.
      "  CONTINUE.
      "ENDIF.

      IF <x_entry>-c_eezz_json IS NOT BOUND.
        " Hide empty tables
        IF <x_entry>-c_tag EQ 'table'.
          CLEAR x_update.
          x_update-c_key    = m_message_db-c_name && '.innerHTML'.
          x_update-c_value  = ''.
          x_update-c_prio   = 10.
          APPEND x_update TO m_tbl_update.
        ENDIF.
        CONTINUE.
      ENDIF.

      TRY.
          x_json ?= <x_entry>-c_eezz_json.

          DATA(x_assign) = x_json->get( 'eezzAgent.assign' ).
          data(x_async)  = x_json->get( 'eezzAgent.assign/async' ).

          IF x_assign IS not bound.
            continue.
          ENDIF.

          DATA(x_call)   = x_assign->*[ 1 ].
          x_params       = CAST #( x_call-c_ref ).
          x_tbl_class    = x_call-c_key.

* Register on AMC events
          IF x_async IS BOUND.
            DATA: x_event TYPE zstr_event.
            LOOP AT x_async->* INTO DATA(x_wa).
              IF x_wa-c_key EQ 'timeout'.
                x_event-timeout = x_wa-c_value.
              ELSEIF x_wa-c_key EQ 'event'.
                x_event-c_event = x_wa-c_value.
              ENDIF.
            ENDLOOP.

            x_event-c_callback = x_json.

            APPEND x_event TO m_tbl_event.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
          " CONTINUE.
      ENDTRY.

      IF x_tbl_class IS INITIAL.
        CONTINUE.
      ENDIF.

      IF <x_entry>-c_ref_node->get_name( ) CS 'TABLE'.
        DATA(xx_new_table)   = create_table( is_entry = <x_entry> ).
        DATA(xx_ref_table)   = <x_entry>-c_ref_node.
        xx_ref_table->get_parent( )->replace_child( new_child = xx_new_table old_child = xx_ref_table ).
        <x_entry>-c_ref_node = xx_new_table.
        " create navigation updates
        create_navigation_update( <x_entry> ).

        DATA(x_tree_class) = CAST if_ixml_element( xx_new_table )->get_attribute_ns( 'class' ).
        DATA(x_tree_id)    = CAST if_ixml_element( xx_new_table )->get_attribute_ns( 'id' ).

        IF x_tree_class CS |eezzTree|.
          render_node( iv_node = <x_entry>-c_ref_node iv_path = |{ <x_entry>-c_name }.innerHTML.{ x_tree_id }| ).
        ELSE.
          render_node( iv_node = <x_entry>-c_ref_node iv_path = |{ <x_entry>-c_name }.innerHTML| ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    DATA x_ref_update TYPE REF TO ztty_update.
    "x_ref_update ?= m_tbl_global.
    GET REFERENCE OF m_tbl_update INTO x_ref_update.
    rv_json = zcl_eezz_json=>gen_response( it_update = x_ref_update  ).

    " get body node
    "az----read table m_tbl_global with key c_tag = 'body' into data(x_table).
    " create json string
    "az----render_node( iv_node = x_table-c_ref_node ).
    "az----rv_json = create_json_response( ).

    IF 1 = 2.
      show_document( iv_document = iv_document iv_ostream = iv_ostream ).
      DATA(c_out) = cl_demo_output=>new( )->begin_section( `agent result` ).
      c_out->write( rv_json ).
      c_out->display( ).
    ENDIF.
  ENDMETHOD.


  method render_node.

    data: x_update     type zstr_update.
    " data x_cls_tblnode type ref to zcl_eezz_table_node.

    " create html string
    if iv_node is bound.
      data(x_tbl_string) = xml_node2string( iv_node ).

      x_update-c_key    = iv_path.
      x_update-c_value  = x_tbl_string.
      x_update-c_prio   = 1.
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


  METHOD zif_eezz_agent~on_event.

    DATA: x_parameter  TYPE ztty_eezz_json,
          x_agent      TYPE REF TO zcl_eezz_agent,
          x_update     TYPE ztty_update,
          x_update_tmp TYPE REF TO ztty_update.

    CHECK m_tbl_event IS NOT INITIAL.

    CLEAR m_tbl_update.
    x_agent ?= zif_eezz_agent~m_eezz_agent.

    IF x_agent IS not BOUND.
      return.
    endif.

    LOOP AT m_tbl_event INTO DATA(x_event) WHERE c_event EQ '*'.
      GET REFERENCE OF i_message INTO DATA(x_ref_data).
      INSERT VALUE #( c_key = 'message' c_ref = x_ref_data ) INTO TABLE x_parameter.
      GET REFERENCE OF m_tbl_global INTO DATA(x_symbols).
      GET REFERENCE OF x_parameter  INTO DATA(x_params).

*     Excute callback
      data(x_asyncupd) = x_event-c_callback->get( iv_path = 'eezzAgent.assign/async/update' ).
      data(x_table)    = x_event-c_callback->callback( iv_symbols = x_symbols iv_path = 'eezzAgent.assign/async/callback' iv_parameter = x_params ).
      IF x_table is not BOUND.
        continue.
      endif.

      data(x_table_xml_node) = x_table->create_node( iv_symbols = x_symbols ).

      " --> get tbody elements for update rows
      data(x_processor) = new cl_xslt_processor( ).
      x_processor->set_source_node( x_table_xml_node ).
      x_processor->set_expression( |./tbody/*[@name]| ).
      x_processor->run( progname = space ).

      " --> render all elements
      data(x_nodelist) = x_processor->get_nodes( ).
      data(x_iterator) = x_nodelist->create_iterator( ).


      do 100 times.
        data(x_next) = x_iterator->get_next( ).
        if x_next is not bound.
          exit.
        endif.

        data(x_element_name)  = cast if_ixml_element( x_next )->get_attribute_ns( 'name' ).

        " Loop on "this" elements in async update record for elements created by callback
        loop at x_asyncupd->* into data(wa_upd).
          split wa_upd-c_key at '.' into table data(x_upd_dest_tbl).
          data(x_nr_entries) = lines( x_upd_dest_tbl ).
          data(x_upd_dest)   = wa_upd-c_key.
          data(x_upd_attr)   = x_upd_dest_tbl[ x_nr_entries ].
          data(x_upd_source) = wa_upd-c_value.

          if not x_upd_dest cs 'this.'.
            continue.
          endif.

          " Calculate destination for this placeholder:
          replace first OCCURRENCE OF |this| in x_upd_dest with x_element_name.

          " Calculate source for star paceholder:
          if x_upd_source eq '*'.
            if x_upd_attr eq 'innerHTML'.
              x_upd_source = x_agent->xml_node2string( x_next ).
            else.
              x_upd_source = cast if_ixml_element( x_next )->get_attribute_ns( x_upd_attr ).
            endif.
          endif.

          insert value #( c_key = x_upd_dest c_value = x_upd_source ) into table x_update.
        endloop.
        "data(x_inner_html)    = x_agent->xml_node2string( x_next ).
        "insert value #( c_key = |{ x_element_name }.innerHTML| c_value = x_inner_html ) into table x_update.
      enddo.

      continue.

        " data(x_row) = X_table->get_row( iv_index = 1 ).
        " data(x_symbol) = x_table->

      " ENDIF.
* Get update requests
      IF zif_eezz_agent~m_eezz_agent IS BOUND.
        DATA(x_json) = x_event-c_callback->get( 'eezzAgent.async/callback' ).
        if x_json is not bound.
          continue.
        endif.

        SPLIT x_json->*[ 1 ]-c_key AT '.' INTO DATA(x_name) DATA(x_method).

        x_agent ?= zif_eezz_agent~m_eezz_agent.
        data(x_node) = x_agent->create_table(
          EXPORTING
            is_entry      = m_tbl_global[ c_name = 'aStatusMonitorList' ]
*            is_eezz_table =
*          RECEIVING
*            ro_new_node   =
        ).
        x_agent->render_node( iv_node = x_node iv_path = 'aStatusMonitorList.innerHTML' ).
*        x_agent->create_navigation_update( m_tbl_global[ c_name = 'aStatusMonitorList' ] ).



        DATA(x_object) = x_agent->m_tbl_global[ c_name = x_name ]-c_object.
        IF x_object IS BOUND.
          CALL METHOD x_object->('GET_UPDATE')
            RECEIVING
              rt_update = x_update_tmp.
          APPEND LINES OF x_update_tmp->* TO x_update.
        ENDIF.

*        x_object = x_agent->m_tbl_global[ c_name = 'aStatusMonitorList' ]-c_object.
*        IF x_object IS BOUND.
*          CALL METHOD x_object->('GET_UPDATE')
*            RECEIVING
*              rt_update = x_update_tmp.
          APPEND LINES OF x_agent->m_tbl_update TO x_update.
*        ENDIF.
      ENDIF.
    ENDLOOP.

    IF x_update IS NOT INITIAL.
      GET REFERENCE OF x_update INTO DATA(x_tbl_update).
      DATA(x_json_response) = zcl_eezz_json=>gen_response( x_tbl_update ).
      DATA(x_message) = m_message_mgr->create_message( ).
      x_message->set_text( x_json_response ).
      m_message_mgr->send( i_message = x_message ).
    ENDIF.

  ENDMETHOD.


  method ZIF_EEZZ_AGENT~ON_WEBSOCKET.

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

    data(x_request) = I_CONTEXT->get_initial_request( ).
    x_request->get_header_fields( CHANGING C_FIELDS = x_query ).
    x_request->get_form_fields( CHANGING c_fields = x_query ).
    data(xxd_value)  = x_request->get_form_field( 'document' ).

    if zif_eezz_agent~m_eezz_agent is not bound.
      data(x_cl_json)  = new zcl_eezz_json( iv_json = i_message->get_text( ) iv_conv_qmark = space ).

      data(x_str_doc)  = x_cl_json->get_value( 'document' ).
      find '<body' in x_str_doc match Offset data(x_body_off).
      " More efficient using stream and regex, processing the document once.

      "--DATA(x_str_path)  = 'http://ldcixp1.mo.sap.corp:44311/' && x_cl_json->get_value( 'path' ).
      "--data x_lo_client type ref to if_http_client.
      "--cl_http_client=>create_by_url( EXPORTING url = x_str_path IMPORTING client = x_lo_client ).

      data(x_result_stream) = new cl_abap_string_c_writer(  ).
      data(x_regex_pattern) = '<(img|area|br|link|input|hr)[a-zA-Z0-9/%=",.:#*\-_''{}[:space:]]*>'.

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

              x_tmp_agent->render_node( ).
              get reference of m_tbl_update into data(x_tbl_update).
              x_response       = zcl_eezz_json=>gen_response( x_tbl_update )." x_tmp_agent->create_json_response( ).
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
