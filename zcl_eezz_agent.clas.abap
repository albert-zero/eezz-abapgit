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

  data M_FILE_LOADER type ref to ZIF_EEZZ_TABLE .
  class-data M_TBL_EVENT type ZTTY_EVENTS .
  data M_MESSAGE_DB type ZSTR_SYMBOLS .
  class-data M_TBL_GLOBAL type ZTTY_SYMBOLS .
  class-data M_TBL_UPDATE type ZTTY_UPDATE .

  methods HANDLE_WEBSERVICE
    importing
      !IV_MANAGER type ref to IF_APC_WSP_MESSAGE_MANAGER
      !IV_MESSAGE type ref to IF_APC_WSP_MESSAGE
      !IV_OSTREAM type ref to IF_IXML_OSTREAM .
  methods CREATE_NAVIGATION_UPDATE
    importing
      !IV_SYMBOL type ZSTR_SYMBOLS
      !IV_ALIAS type STRING optional .
  methods CREATE_TABLE
    importing
      !IV_TABLE_NAME type STRING optional
      !IV_DESTINATION type STRING optional
      !IS_ENTRY type ZSTR_SYMBOLS
      !IS_EEZZ_TABLE type ref to ZIF_EEZZ_TABLE optional
      !IS_TEMPLATE type ref to IF_IXML_NODE
    returning
      value(RO_NEW_NODE) type ref to IF_IXML_NODE .
  methods SHOW_DOCUMENT
    importing
      !IV_DOCUMENT type ref to IF_IXML_DOCUMENT
      !IV_OSTREAM type ref to IF_IXML_OSTREAM .
ENDCLASS.



CLASS ZCL_EEZZ_AGENT IMPLEMENTATION.


  method constructor.
    try.
        data x_msg_cons type ref to IF_AMC_MESSAGE_CONSUMER.
        x_msg_cons =
         cl_amc_channel_manager=>create_message_consumer( i_application_id = 'Z_EEZZ_WS_MSG_CHANNEL' i_channel_id = '/eezz/web').
        x_msg_cons->start_message_delivery( i_receiver = me ).

        x_msg_cons = cl_amc_channel_manager=>create_message_consumer( i_application_id = 'Z_EEZZ_WS_MSG_CHANNEL' i_channel_id = '/eezz/auth').
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
    data(x_name)          = iv_symbol-c_name.

    FIELD-SYMBOLS: <fs> type zstr_update.

    if iv_alias is not initial.
      x_name = iv_alias.
    endif.
    if iv_symbol-c_object is not bound.
      return.
    endif.

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
                c_key   = |{ x_name }.{ x_attr_name }|
                c_value = x_new_value-c_value
                c_prio  = 10 ) to m_tbl_update.

          elseif x_attr_name cs |data-eezz-action|.
            data(x_json) = new zcl_eezz_json( iv_json = x_attr_value ).
            data(x_updt) = x_json->get( iv_path = |update| ).

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
    data  x_eezz_table type ref to zif_eezz_table.
    data  x_template   type ref to if_ixml_node.

    if is_eezz_table is bound.
      x_eezz_table ?= is_eezz_table.
    elseif is_entry-c_object is bound.
      x_eezz_table ?= is_entry-c_object.
    else.
      x_eezz_table  = new zcl_eezz_table( ).
    endif.

    get reference of m_tbl_global into x_globals.
    " x_template = is_entry-c_ref_node->clone( ).

    create object x_node_table
        exporting
          iv_table_name  = iv_table_name
          iv_destination = iv_destination
          io_node        = is_template
          it_globals     = x_globals
          io_eezz_tbl    = x_eezz_table.

      ro_new_node = x_node_table->get( ).
    endmethod.


  method handle_webservice.
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
          x_json          type ref to zcl_eezz_json,
          x_current_path  type string,
          x_data          type ref to data.
    field-symbols <fs_symbol>     type zstr_symbols.
    data          x_global_symbol type zstr_symbols.

    try.
        if iv_message->get_message_type( ) = iv_message->co_message_type_binary.
          if m_file_loader is bound.
            data(x_eezz_message) = m_file_loader->on_download( iv_message = iv_message ).
            if x_eezz_message->get_status( ) eq 201.
              x_json = new zcl_eezz_json( iv_json = x_eezz_message->get_request( ) ).
            elseif x_eezz_message->get_status( ) lt 201.
              x_json_response = x_eezz_message->render( iv_symbols = ref #( m_tbl_global ) ).
              iv_ostream->write_string( x_json_response ).
              return.
            endif.
          endif.
        else.
          data(x_str_json) = iv_message->get_text( ).
          x_json = new zcl_eezz_json( iv_json = x_str_json ).

          data(x_prep_files)    = x_json->get( iv_path = |files| ).
          data(x_load_files)    = x_json->get( iv_path = |file|  ).


          if x_prep_files is bound.
            data(x_reader_name) = x_json->get_value( |reader| ).
            data(x_reader_sym)  = m_tbl_global[ c_name = x_reader_name ].
            m_file_loader       = x_reader_sym-c_object.
            x_str_json          = m_file_loader->prepare_download( iv_message = iv_message ).
            iv_ostream->write_string( x_str_json ).
            return.
          endif.

          if x_load_files is bound.
            if m_file_loader is bound.
              x_eezz_message = m_file_loader->on_download( iv_message = iv_message ).
            endif.
            return.
          endif.
        endif.

        clear m_file_loader.
        data(x_json_callback) = x_json->get( iv_path = 'callback' ).
        data(x_json_update)   = x_json->get( iv_path = 'update' ).

        data x_json_object_name  type string.
        data x_json_method_name  type string.
        data(x_parameters) = new ztty_eezz_json( ).
        x_parameters->* = value #(
          ( c_type = |IF_APC_WSP_MESSAGE_MANAGER| c_key = |manager| c_object = iv_manager )
          ( c_type = |IF_APC_WSP_MESSAGE|         c_key = |message| c_object = cast if_apc_wsp_message( iv_message ) )
        ).

        if x_json_callback is not bound.
          x_json_callback = x_json->get( iv_path = 'eezzAgent.assign' ).
        endif.

        clear mt_messages.
        get time stamp field x_timestamp.

        " execute callback event
        data(x_eezz_table) = x_json->callback( iv_symbols = ref #( m_tbl_global ) iv_parameter = x_parameters  ).
        field-symbols <fs_http_value> type string.
      catch cx_root into data(x_root_exception).
        zcl_eezz_message=>add( iv_key = |WebService|  iv_exception = x_root_exception ).
        x_json_response = zcl_eezz_message=>render( iv_symbols = ref #( m_tbl_global ) ).
        iv_ostream->write_string( string = x_json_response ).
        return.
    endtry.

    " Enter the result of the callback into the update
    if x_json_update is bound.
      loop at x_json_update->* into data(x_update).
        " The update has the format <tag-name>.[(style.)]<attribute> = (*) | <tag-name>.<attribute>
        clear x_ref_obj.
        data  x_reset type abap_bool value abap_false.
        clear x_reset.

        try.
            data x_source      type string.
            data x_destination type string.

            if x_update-c_value cs '*'.
              if x_update-c_value cs '.*'.
                x_reset = abap_true.
              endif.
              x_source = x_update-c_key.
            else.
              if x_update-c_type cp 'object'.
                data(x_writer_value) = x_json->dump( iv_path = |update/{ x_update-c_key }| ).
                x_update-c_value = x_writer_value->get_result_string( ).
              else.
                x_source = x_update-c_value.
              endif.
            endif.

            split x_update-c_key at '.' into table data(x_key_struct).
            split x_source       at '.' into table data(x_val_struct).

            x_destination = x_key_struct[ 1 ].

            data(x_http_dictionary_key)  = condense( x_source ).
            data(x_http_element_by_name) = condense( x_val_struct[ 1 ] ).
            data(x_http_element_attr)    = condense( x_val_struct[ 2 ] ).
            data(x_http_element_path)    = condense( x_key_struct[ 3 ] ).
          catch cx_sy_itab_line_not_found.
        endtry.

        try.
            if x_update-c_key cs '.script'.
              " line exists m_tbl_global[ c_name = X_KEY_STRUCT[ 1 ] ].
              " render_node x_global_symbol-c_object
              zcl_eezz_message=>add( iv_key = |{ x_key_struct[ 1 ] }.script|  iv_value = x_update-c_value ).
              continue.
            elseif x_http_element_by_name cp 'this'.
              x_http_element_by_name    = x_key_struct[ 1 ].
              read table m_tbl_global with table key c_name = x_http_element_by_name assigning <fs_symbol>.
              <fs_symbol>-c_ref_obj = x_eezz_table.
            else.
              read table m_tbl_global with table key c_name = x_http_element_by_name assigning <fs_symbol>.
            endif.
            x_global_symbol = m_tbl_global[ c_name = x_http_element_by_name ].

            if x_reset = abap_true.
              clear x_global_symbol-c_object.
              x_global_symbol-c_templ_node = x_global_symbol-c_ref_node->clone( ).
            elseif x_eezz_table is not bound and x_http_element_attr cs 'innerHTML'.
              if x_global_symbol-c_eezz_json is bound.
                x_json      ?= x_global_symbol-c_eezz_json.
                x_eezz_table = x_json->callback( ref #( m_tbl_global ) ).
                x_ref_obj    = x_eezz_table.
                <fs_symbol>-c_object = x_eezz_table.
              endif.
            endif.

            if x_eezz_table is bound.
              x_ref_obj = x_eezz_table.
            elseif x_global_symbol-c_object is bound.
              x_ref_obj = <fs_symbol>-c_object.
            else.
              x_ref_obj = new zcl_eezz_table( ).
              x_global_symbol-c_object = x_ref_obj.
            endif.

            data(x_dictionary) = x_ref_obj->get_dictionary( ).

            if x_http_element_attr cs |style|.
              x_http_element_attr = x_http_element_path.
              x_http_element_path = x_key_struct[ 1 ].
            elseif x_http_element_path is not initial.
              x_update-c_key       = |{ x_key_struct[ 1 ] }.{   x_key_struct[ 2 ] }|.
              x_http_element_path  = |{ x_http_element_path }/{ x_val_struct[ 1 ] }|.
            else.
              x_http_element_path  = x_dictionary->*[ c_key = 'tree_path' ]-c_value.
              if x_http_element_path is initial.
                x_http_element_path = x_key_struct[ 1 ].
              endif.
            endif.

            if x_http_element_attr cs 'innerHTML'.
              " store data for navigation
              x_global_symbol-c_name   = x_key_struct[ 1 ].
              x_global_symbol-c_object = x_ref_obj.
              modify table m_tbl_global from x_global_symbol transporting c_object c_templ_node c_ref_node c_eezz_json c_ref_obj.
              if sy-subrc ne 0.
                insert x_global_symbol into table m_tbl_global.
              endif.

              " insert new tree reference
              x_global_symbol-c_name   = x_http_element_path.
              modify table m_tbl_global from x_global_symbol transporting c_object c_templ_node c_ref_node c_eezz_json c_ref_obj.
              if sy-subrc ne 0.
                insert x_global_symbol into table m_tbl_global.
              endif.
              "---->x_dictionary->*[ c_key = |tree_path| ]-c_value = x_http_element_path.

              data(x_ixml_template) = x_global_symbol-c_templ_node->clone( ).
              x_new_table           = create_table( iv_table_name = x_global_symbol-c_name iv_destination = x_destination is_entry = x_global_symbol is_template = x_ixml_template is_eezz_table = x_ref_obj  ).
              zcl_eezz_message=>add( iv_key = x_update-c_key  iv_node = x_new_table  iv_symbol = ref #( x_global_symbol ) ).

            else.
              " For any attribute we use the dictionary of the eezz_table
              data x_http_update type zstr_update.

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
            zcl_eezz_message=>add( iv_key = |WebService|  iv_exception = cx_error_83 ).
        endtry.
      endloop.
    endif.

    " timer
    get time stamp field x_timestop.
    x_seconds = cl_abap_tstmp=>subtract( tstmp1 = x_timestop tstmp2 = x_timestamp ).
    message id 'ZCL_EEZZ_MESSAGE' type 'S' number 0  with x_seconds into data(x_msg_text).
    x_msg     = value zstr_message( c_msgtext = x_msg_text c_msgcls  = 'zcl_eezz_message' c_msgnum  = 0 ).

    zcl_eezz_message=>add( iv_key = 'timer' iv_message = value #( c_msgtext = x_msg_text c_msgcls  = 'zcl_eezz_message'  c_msgnum = 0 ) ).
    x_json_response = zcl_eezz_message=>render( iv_symbols = ref #( m_tbl_global ) ).
    iv_ostream->write_string( string = x_json_response ).

  endmethod.


  method if_amc_message_receiver_pcp~receive.

    data: x_pcp_fields type pcp_fields.
    data: xt_update    type ztty_update.

    try.
        i_message->get_fields( changing c_fields = x_pcp_fields ).
        data(x_agent) = zcl_eezz_agent=>zif_eezz_agent~m_eezz_agent .

        if line_exists( x_pcp_fields[ name = |OneDrive.Authentication| ] ).
          return.
        endif.

        loop at x_pcp_fields into data(x_pcp).
          append value #( c_key = x_pcp-name c_value = x_pcp-value ) to xt_update.
        endloop.

        data(x_response) = zcl_eezz_json=>gen_response( it_update = ref #( xt_update ) iv_command = |async| ).

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

    data: x_dictionary     type ztty_dictionary.
    data: x_update         type zstr_update.
    data: x_params         type ref to ztty_eezz_json.
    data: x_index_int      type integer,
          x_visible_items  type integer,
          x_visible_blocks type integer.

    data: x_wa_parameter type abap_parmbind,
          x_parameters   type abap_parmbind_tab.
    data: x_jsn_animation  type ref to zcl_eezz_json.

    " Add default behavior onclick
    data(x_agent_message) = new zcl_eezz_message( ).
    data(x_processor)     = new cl_xslt_processor( ).
    x_processor->set_source_node( iv_document ).
    x_processor->set_expression( |//*[@data-eezz-event]| ).
    x_processor->run( progname = space ).
    data(x_nodelist)  = x_processor->get_nodes( ).
    if x_nodelist is bound.
      data(x_iterator) = x_nodelist->create_iterator( ).
      do 10000 times.
        data(x_next) = x_iterator->get_next( ).
        if x_next is not bound.
          exit.
        endif.
        cast if_ixml_element( x_next )->set_attribute_ns( name = |onclick| value = |easyClick(event, this)| ).
      enddo.
    endif.

    " Process named elememts
    x_processor->set_expression( |//*[@name]| ).
    x_processor->run( progname = space ).
    x_nodelist  = x_processor->get_nodes( ).

    if x_nodelist is not bound.
      return.
    endif.
    x_iterator = x_nodelist->create_iterator( ).

    try.
        do 10000 times.
          x_next = x_iterator->get_next( ).
          if x_next is not bound.
            exit.
          endif.

          if x_next->get_type( ) <> if_ixml_node=>co_node_element.
            continue.
          endif.

          data(x_name_attr) = cast if_ixml_element( x_next )->get_attribute_ns( 'name' ).
          if x_name_attr is initial.
            continue.
          endif.

          data(x_tmpl_node) = x_next.
          data(x_name)      = x_next->get_name( ).
          data x_wa_named_node type ref to zstr_symbols.

          if x_name np 'body'.
            x_tmpl_node = x_next->clone( ).
          endif.
          x_wa_named_node = new zstr_symbols( c_name = x_name_attr c_tag = x_name c_ref_node = x_next c_templ_node = x_tmpl_node ).

          " store message output area
          data(x_eezz_template) = cast if_ixml_element( x_next )->get_attribute_ns( 'data-eezz-template' ).
          if x_eezz_template is not initial.
            if x_name co |table|.
              if x_eezz_template co |database|.
                m_message_db-c_name       = x_name_attr.
                m_message_db-c_templ_node = x_next->clone( ).
                x_agent_message->add( iv_key = |{ x_name_attr }.innerHTML| ).
                "----->append value #( c_key = |{ x_name_attr }.innerHTML|  c_value = '' c_prio = 10 ) to m_tbl_update.
              endif.
            endif.
          endif.

          if x_name_attr cs 'eezz-i18n-'.
            data x_number type int4.
            x_number  = x_name_attr+10.
            x_update-c_key  = |{ x_name_attr }.innerHTML|.
            message id 'ZCL_EEZZ_MESSAGE' type 'S' number x_number into data(x_translated).
            if strlen( x_translated ) > 0.
              "------>x_update-c_value = x_translated.
              "------>append x_update to m_tbl_update.
              x_agent_message->add( iv_key = |{ x_name_attr }.innerHTML| iv_value = x_translated ).
            endif.
          endif.

          " check if we have also an data-eezz-action
          data(x_eezz_class)  = cast if_ixml_element( x_next )->get_attribute_ns( 'class' ).
          data(x_eezz_action) = cast if_ixml_element( x_next )->get_attribute_ns( 'data-eezz-action' ).
          if x_eezz_action is not initial.
            data(x_json)      = new zcl_eezz_json( iv_json = x_eezz_action ).
            x_wa_named_node->c_eezz_json = x_json.

            if x_eezz_template is initial and x_eezz_class np |eezzTreeTemplate|.
              x_wa_named_node->c_object = x_json->callback( iv_symbols = ref #( m_tbl_global ) ).
            endif.

            data(x_action_upd) = cast ztty_eezz_json( x_json->get( iv_path = |/update| ) ).
            if x_action_upd is not initial.
              loop at x_action_upd->* into data(x_wa_actupd).
                data(x_json_value) = new zcl_eezz_json( it_json = cast ztty_eezz_json( x_wa_actupd-c_ref ) ).
                data(x_json_dump)  = x_json_value->dump( )->get_result_string( ).
                x_agent_message->add( iv_key = x_wa_actupd-c_key iv_value = x_json_dump ).
              endloop.
            endif.

            data(x_assign) = cast ztty_eezz_json( x_json->get( iv_path = 'eezzAgent.assign' ) ).
            if x_assign is not initial.
              "---->data(xxx_new_table) = create_table( is_entry = x_wa_named_node->* ).
            endif.

          endif.

          data(x_eezz_attr) = cast if_ixml_element( x_next )->get_attribute_ns( 'data-eezz-attributes' ).
          if x_eezz_attr is not initial.
            x_wa_named_node->c_eezz_json = new zcl_eezz_json( iv_json = x_eezz_attr ).
          endif.


          data(x_eezz_script) = cast if_ixml_element( x_next )->get_attribute_ns( 'data-eezz-async' ).
          if x_eezz_script is not initial.
            x_json = new zcl_eezz_json( iv_json = x_eezz_script ).
            data(x_tbl_script) = x_json->get( ).
            data(x_key_script) = x_tbl_script->*[ 1 ].
            x_agent_message->add( iv_key = x_name_attr iv_value = x_json_dump ).
          endif.

          if not line_exists( m_tbl_global[ c_name = x_name_attr ] ).
            append x_wa_named_node->* to m_tbl_global.
          endif.
        enddo.
      catch cx_root into data(x_ex_loop).
        x_agent_message->add( iv_key = 'Parse' iv_exception = x_ex_loop ).
    endtry.

    " To create initial tables, loop over global symbols and find table templates
    loop at m_tbl_global assigning field-symbol(<x_entry>).
      data x_tbl_class type string.

      try.
          if <x_entry>-c_eezz_json is not bound or <x_entry>-c_object is not bound.
            continue.
          endif.

          x_json  ?= <x_entry>-c_eezz_json.
          x_assign = x_json->get( iv_path = 'eezzAgent.assign' ).
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
        data(xx_new_table) = create_table( is_entry = <x_entry> is_template = <x_entry>-c_ref_node->clone( ) ).
        data(xx_ref_table) = <x_entry>-c_ref_node.
        xx_ref_table->get_parent( )->replace_child( new_child = xx_new_table old_child = xx_ref_table ).
        <x_entry>-c_ref_node = xx_new_table.

        x_agent_message->add( iv_key = |{ <x_entry>-c_name }.innerHTML| iv_node = <x_entry>-c_ref_node iv_symbol = ref #( <x_entry> ) ).
      endif.
    endloop.

    "----->rv_json = zcl_eezz_json=>gen_response( it_update = ref #( m_tbl_update ) ).
    rv_json = x_agent_message->render( ).
  endmethod.


  method show_document.

    data(x_ixml) = cl_ixml=>create( ).

    x_ixml->create_renderer(
      document = iv_document
      ostream  = iv_ostream )->render( ).

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
      x_agent_message  type ref to zcl_eezz_message,
      x_response       type string,
      x_message        type ref to if_apc_wsp_message,
      x_offset         type i,
      x_query          type tihttpnvp.

    "---m_message_mgr = i_message_manager.

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

      try.
          data(x_message_obj)  = x_cl_json->get_value( 'msgobj' ).
          data(x_message_name) = x_cl_json->get_value( 'msgname' ).

          x_xml_cl             = cl_ixml=>create( ).
          x_stream_factory     = x_xml_cl->create_stream_factory( ).
          x_in_stream          = x_stream_factory->create_istream_string( string = x_message_obj ).
          x_out_stream         = x_stream_factory->create_ostream_cstring( x_response ).

          x_document           = x_xml_cl->create_document( ).
          x_xml_parser         = x_xml_cl->create_parser(
                                   document       = x_document
                                   stream_factory = x_stream_factory
                                   istream        = x_in_stream ).

          if x_xml_parser->parse( ) = 0.
            zcl_eezz_message=>set_node( iv_name = x_message_name  iv_message_obj = x_document->get_root_element( )  ).
          endif.
        catch cx_root.

      endtry.

      data(x_str_doc)  = x_cl_json->get_value( 'document' ).
      replace all occurrences of |&nbsp;| in x_str_doc with | |.
      find '<body' in x_str_doc match offset data(x_body_off).
      " More efficient using stream and regex, processing the document once.

      "--DATA(x_str_path)  = 'http://ldcixp1.mo.sap.corp:44311/' && x_cl_json->get_value( 'path' ).
      "--data x_lo_client type ref to if_http_client.
      "--cl_http_client=>create_by_url( EXPORTING url = x_str_path IMPORTING client = x_lo_client ).

      data(x_result_stream) = new cl_abap_string_c_writer(  ).
      data(x_regex_pattern) = '<(img|area|br|link|input|hr)[a-zA-Z0-9/%=",.:#*\-_'';{}[:space:]\[\]]*>'.

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
              data(x_error)  = x_xml_parser->get_error( x_cnt ).
              data(x_reason) = x_error->get_reason( ).
              data(x_lineno) = x_error->get_line( ).
              data(x_column) = x_error->get_column( ).
              add 1 to x_cnt.

              message s001(zcl_eezz_message) with x_reason x_lineno x_column into data(x_msg_text).
              x_agent_message->add(
                 iv_status  = 500
                 iv_key     = 'ParserError'
                 iv_message = value #(
                    c_msgtext = |on_websocket({ x_lineno }:{ x_column }): { x_msg_text }|
                    c_msgcls  = 'zcl_eezz_message' c_msgnum  = 1  ) ).
            enddo.
            x_response = x_agent_message->render( ).
            x_message  = i_message_manager->create_message( ).
            x_message->set_text( x_response ).
            i_message_manager->send( x_message ).

          endif.
        catch cx_apc_error into x_apc_error.
          x_agent_message->add( iv_key = |ParserError| iv_status  = 500 iv_exception = x_apc_error ).
          x_response = x_agent_message->render( ).
      endtry.
    else.
      if zif_eezz_agent~m_eezz_agent is bound.
        try.
            x_xml_cl         = cl_ixml=>create( ).
            x_stream_factory = x_xml_cl->create_stream_factory( ).
            x_out_stream     = x_stream_factory->create_ostream_cstring( x_response ).
            x_agent         ?= zif_eezz_agent~m_eezz_agent.

            x_agent->handle_webservice( iv_manager = i_message_manager iv_message = i_message iv_ostream = x_out_stream ).

            if x_response is not initial.
              x_message = i_message_manager->create_message( ).
              x_message->set_text( x_response ).
              i_message_manager->send( x_message ).
            endif.
          catch cx_apc_error into x_apc_error.
            x_agent_message->add( iv_key = |ParserError| iv_status  = 500 iv_exception = x_apc_error ).
            x_response = x_agent_message->render( ).
        endtry.
      endif.
    endif.
    " --- finish ---
  endmethod.
ENDCLASS.
