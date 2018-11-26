class ZCL_EEZZ_MESSAGE definition
  public
  final
  create public .

public section.

  class-methods GET_STATUS
    returning
      value(RV_STATUS) type I .
  class-methods SET_STATUS
    importing
      !IV_STATUS type I .
  class-methods GET_REQUEST
    returning
      value(RV_REQUEST) type STRING .
  class-methods RENDER_NODE
    importing
      !IV_IXML_NODE type ref to IF_IXML_NODE
    returning
      value(RT_IXML_SERIALIZED) type STRING .
  class-methods SET_NODE
    importing
      !IV_NAME type STRING
      !IV_MESSAGE_OBJ type ref to IF_IXML_ELEMENT .
  class-methods SET_REQUEST
    importing
      !IV_REQUEST type STRING .
  methods CONSTRUCTOR
    importing
      !IV_REQUEST type STRING optional .
  class-methods ADD
    importing
      !IV_KEY type STRING
      !IV_MESSAGE type ZSTR_MESSAGE optional
      !IV_EXCEPTION type ref to CX_ROOT optional
      !IV_NODE type ref to IF_IXML_NODE optional
      !IV_STATUS type I default 0
      !IV_VALUE type STRING optional
      !IV_SYMBOL type ref to ZSTR_SYMBOLS optional .
  class-methods RENDER
    importing
      !IV_SYMBOLS type ref to ZTTY_SYMBOLS optional
    returning
      value(RT_JSON) type STRING .
  protected section.
private section.

  class-data M_IXML_NAME type STRING .
  class-data M_IXML_NODE type ref to IF_IXML_NODE .
  class-data MT_MESSAGES type ZTTY_MESSAGES .
  class-data MT_UPDATE type ZTTY_UPDATE .
  class-data M_REQUEST type STRING .
  class-data M_STATUS type I .

  class-methods SET_NAVIGATION
    importing
      !IV_KEY type STRING
      !IV_SYMBOL type ref to ZSTR_SYMBOLS .
ENDCLASS.



CLASS ZCL_EEZZ_MESSAGE IMPLEMENTATION.


  method add.
    m_status = nmax( val1 = m_status val2 = iv_status ).

    if iv_message is not initial.
      append iv_message to mt_messages.
      return.
    endif.

    if iv_exception is bound.
      iv_exception->get_source_position( importing program_name = data(x_prog) include_name = data(x_include) source_line = data(x_line) ).
      data(x_text) = |{ x_include }/{ iv_key }:{ x_line }:: { iv_exception->get_text( ) }|.
      append value #( c_msgtext = x_text c_msgcls = 'exception' c_msgnum = 0 ) to mt_messages.
      " append value #( c_key = m_ixml_name c_value = x_text ) to mt_update.
      " append value #( c_msgtext = x_text ) to mt_messages.
      return.
    endif.

    if iv_node is not initial.
      data(x_ixml_serialized) = render_node( iv_ixml_node = iv_node ).
      if line_exists( mt_update[ c_key = iv_key ] ).
        assign mt_update[ c_key = iv_key ] to field-symbol(<fs>).
        <fs>-c_value = x_ixml_serialized.
      else.
        append value #( c_key = iv_key c_value = x_ixml_serialized ) to mt_update.
      endif.

      if iv_symbol is not initial and iv_node->get_name( ) cp 'TABLE'.
        data x_name type string.
        data x_attr type string.
        split iv_key at '.' into x_name x_attr.
        set_navigation( iv_key = x_name iv_symbol = iv_symbol ).
      endif.

      return.
    endif.

    append value #( c_key = iv_key c_value = iv_value ) to mt_update.
  endmethod.


  method constructor.

  endmethod.


  method GET_REQUEST.
    rv_request = m_request.
  endmethod.


  method GET_STATUS.
    rv_status = m_status.
  endmethod.


  method render.

    if mt_messages is not initial.
      loop at mt_messages assigning field-symbol(<fs_msg>).
        if <fs_msg>-c_msgnum ge 0.
          <fs_msg>-_eezz_row_cell_ = |{ <fs_msg>-c_msgcls }.{ <fs_msg>-c_msgnum }|.
        endif.
      endloop.

      if m_ixml_node is bound.
        data(x_eezz_table) = new zcl_eezz_table( iv_table = ref #( mt_messages ) ).
        data(x_eezz_node)  = new zcl_eezz_table_node( io_eezz_tbl = x_eezz_table io_node = m_ixml_node it_globals = iv_symbols ).
        append value #( c_key = |{ m_ixml_name }.innerHTML| c_value = render_node( x_eezz_node->get( ) ) ) to mt_update.
      else.
        cl_demo_output=>new( )->write_data( mt_messages )->display( ).
      endif.
      clear mt_messages.
    endif.

    rt_json = zcl_eezz_json=>gen_response( ref #( mt_update ) ).
    clear mt_update.
    clear mt_messages.

  endmethod.


  method render_node.
    data x_string type string.
    data(x_ixml)     = cl_ixml=>create( ).
    data(x_oostream) = x_ixml->create_stream_factory( )->create_ostream_cstring( string = rt_ixml_serialized ).

    " remove_templates from node.
    data(x_filter1)  = iv_ixml_node->create_filter_attribute_ns( name = |data-eezz-template| ).
    data(x_filter2)  = iv_ixml_node->create_filter_attribute_ns( name = |data-eezz-action| ).
    data(x_filter)   = iv_ixml_node->create_filter_or( filter1 = x_filter1 filter2 = x_filter2 ).
    data(x_iterator) = iv_ixml_node->create_iterator_filtered( filter = x_filter ).

    do 1 times.
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
      "---endif.
    enddo.

    data(x_recurs) = abap_true.
    x_iterator     = iv_ixml_node->create_iterator( 1 ).

    " Render inner HTML: step over the root element
    x_next         = x_iterator->get_next( ).

    while x_next is bound.
      x_next = x_iterator->get_next( ).
      if x_next is not bound.
        exit.
      endif.

      x_next->render( ostream = x_oostream recursive = abap_true ).
    endwhile.

  endmethod.


  method SET_NAVIGATION.
        data: x_dictionary type ref to ztty_dictionary,
          x_update     type zstr_update,
          x_value      type string.

    data(x_attributes)    = iv_symbol->*-c_ref_node->get_attributes( ).
    "data(x_action_attr)   = x_next->get_attributes( )->get_named_item_ns( 'data-eezz-action' )->get_value( ).
    data(x_attr_iterator) = x_attributes->create_iterator( ).
    data(x_attr_element)  = x_attr_iterator->get_next( ).
    data(x_name)          = iv_symbol->*-c_name.

    if iv_key is not initial.
       x_name = iv_key.
    endif.
    FIELD-SYMBOLS: <fs> type zstr_update.

    if iv_symbol->*-c_object is not bound.
      return.
    endif.

    x_dictionary = iv_symbol->*-c_object->GET_DICTIONARY( ).

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
                c_prio  = 10 ) to mt_update.

          elseif x_attr_name cs |data-eezz-action|.
            data(x_json) = new zcl_eezz_json( iv_json = x_attr_value ).
            data(x_updt) = x_json->get( iv_path = |update| ).

            if x_updt is bound.
              loop at x_updt->* into data(x_upd_wa).
                if not line_exists( mt_update[ c_key = x_upd_wa-c_key ] ).
                  if x_upd_wa-c_type cs |object|.
                    x_value = x_json->dump( |update/{ x_upd_wa-c_key }| )->get_result_string( ).
                  else.
                    x_value = x_upd_wa-c_value.
                  endif.
                  append value #( c_key = x_upd_wa-c_key c_value = x_value c_prio = 10 ) to mt_update.
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


  method set_node.
    m_ixml_node = iv_message_obj.
    m_ixml_name = iv_name.
  endmethod.


  method set_request.
    m_request = iv_request.
  endmethod.


  method SET_STATUS.
    m_status = iv_status.
  endmethod.
ENDCLASS.
