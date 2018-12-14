class ZCL_EEZZ_JSON definition
  public
  final
  create public .

public section.

  data M_PARENT type ZSTR_EEZZ_JSON .
  data M_STATUS type STRING .
  data M_PATH type STRING .

  methods GET_CONTROL
    importing
      !IV_PATH type STRING
    returning
      value(RV_CONTROL) type ref to OBJECT .
  methods GET_STATUS
    importing
      !IV_PATH type STRING
      !IV_KEY type STRING
    returning
      value(RV_STATUS) type STRING .
  methods DUMP
    importing
      !IV_PATH type STRING optional
    returning
      value(RV_JSN_WRT) type ref to CL_ABAP_STRING_C_WRITER .
  methods JOIN
    importing
      !IV_PATH type STRING optional
      !IT_JSON type ref to ZTTY_EEZZ_JSON optional
      !IV_KEY type STRING optional
      !IV_STATUS type STRING optional
      !IV_VALUE type STRING optional
      !IV_TYPE type STRING optional
      !IV_OBJECT type ref to OBJECT optional
      !IV_CONTROL type ref to OBJECT optional
      !IV_CREATE type ABAP_BOOL default ABAP_FALSE .
  class-methods GEN_RESPONSE
    importing
      !IT_UPDATE type ref to ZTTY_UPDATE
      !IV_COMMAND type STRING default 'update'
    returning
      value(RV_JSON) type STRING .
  class-methods GEN_HEADER_EVENT
    importing
      !IV_NAME type STRING
      !IV_INDEX type INT4
    returning
      value(EV_UPDATE) type STRING .
  methods GET
    importing
      !IV_PATH type STRING optional
      !IV_MATCH type I default -1
    returning
      value(RT_OBJ) type ref to ZTTY_EEZZ_JSON .
  methods GET_RANGE
    importing
      !IV_NAMES type ref to ZTTY_EEZZ_ROW
    returning
      value(RT_RANGE) type ref to ZTTY_EEZZ_ROW .
  methods CONSTRUCTOR
    importing
      !IV_JSON type STRING default '{}'
      !IV_CONV_QMARK type BOOLEAN default 'X'
      !IT_JSON type ref to ZTTY_EEZZ_JSON optional
      !IS_JSON type ZSTR_EEZZ_JSON optional .
  methods GET_UPDATE
    returning
      value(ET_EEZZ_JSON) type ref to ZTTY_EEZZ_JSON .
  methods CALLBACK
    importing
      !IV_SYMBOLS type ref to ZTTY_SYMBOLS
      !IV_PARAMETER type ref to ZTTY_EEZZ_JSON optional
      !IV_PATH type STRING optional
    returning
      value(RT_TABLE) type ref to ZIF_EEZZ_TABLE .
  class-methods SET_UPDATE
    importing
      !IV_UPDATE type ZTTY_EEZZ_JSON
    returning
      value(EV_UPDATE) type STRING .
  methods GET_VALUE
    importing
      !IV_PATH type STRING optional
      !IV_KEY type STRING
      !IV_ENTRY type STRING default 'VALUE'
    returning
      value(RV_VALUE) type STRING .
  methods GEN_ROW_EVENT
    importing
      !IV_NAME type STRING
      !IV_THIS type STRING optional
      !IV_INDEX type INT4
      !IV_DICTIONARY type ref to ZTTY_DICTIONARY optional
    returning
      value(EV_RESULT) type STRING .
  protected section.
private section.

  data M_JSON type STRING .
  data M_TBL_JSON type ref to ZTTY_EEZZ_JSON .
  data M_WRITER type ref to CL_ABAP_STRING_C_WRITER .

  class-methods WALK
    importing
      !IV_WRITER type ref to CL_ABAP_STRING_C_WRITER
      !IT_JSON type ref to ZTTY_EEZZ_JSON .
ENDCLASS.



CLASS ZCL_EEZZ_JSON IMPLEMENTATION.


  method callback.
    data:
      x_class_name     type string,
      x_method_name    type string,
      x_target_name    type string,
      x_path_extension type string,
      x_tbl_callback   type ref to ztty_eezz_json,
      x_tbl_parameter  type ref to ztty_eezz_json,
      x_str_call       type zstr_eezz_json,
      x_dyn_param_wa   type abap_parmbind,
      x_dyn_param      type abap_parmbind_tab.

    field-symbols <fs_data> type any.
    data x_ref_data type ref to data.
    data x_ref_oref type ref to object.
    data x_ref_obj  type ref to zif_eezz_table.
    data x_tbl_obj  type ref to zif_eezz_table.

    if iv_path is not initial.
      x_tbl_callback = me->get( iv_path = iv_path ).
    endif.

    if x_tbl_callback is initial.
      x_tbl_callback = me->get( iv_path = 'callback' ).
    endif.

    if x_tbl_callback is initial.
      x_tbl_callback = me->get( iv_path = 'eezzAgent.assign' ).
    endif.

    if x_tbl_callback is initial.
      return.
    endif.

    split x_tbl_callback->*[ 1 ]-c_key at '.' into table data(x_callback_cmd).
    try.
        x_class_name     = x_callback_cmd[ 1 ].
        x_method_name    = x_callback_cmd[ 2 ].
        x_target_name    = x_callback_cmd[ 3 ].
      catch cx_sy_itab_line_not_found.
    endtry.

    try.
        x_tbl_parameter   = cast #( x_tbl_callback->*[ 1 ]-c_ref ).

        if strlen( x_method_name ) = 0.
          x_method_name = 'CONSTRUCTOR'.
          data(lo_cref) = cast cl_abap_classdescr( cl_abap_classdescr=>describe_by_name( x_class_name ) ).
        else.
          translate x_method_name to upper case.
          x_ref_obj    ?= iv_symbols->*[ c_name = x_class_name ]-c_object.
          if x_ref_obj is not bound.
            raise exception type cx_sy_ref_is_initial.
          endif.
          lo_cref       = cast cl_abap_classdescr( cl_abap_classdescr=>describe_by_object_ref( x_ref_obj ) ).
        endif.

        data(x_class_type) = cl_abap_classdescr=>get_class_name( x_ref_obj ).
        data(lo_params)    = lo_cref->methods[ name = to_upper( x_method_name ) ]-parameters.

        loop at lo_params into data(x_class_params).
          data  x_params_js type zstr_eezz_json.
          clear x_params_js.

          if x_class_params-parm_kind   = cl_abap_objectdescr=>returning.
            if x_class_params-type_kind = cl_abap_typedescr=>typekind_oref.
              x_dyn_param_wa-kind       = cl_abap_objectdescr=>returning.
              x_dyn_param_wa-name       = x_class_params-name.
              get reference of x_tbl_obj into x_dyn_param_wa-value.
              insert x_dyn_param_wa into table x_dyn_param.
            endif.
            continue.
          endif.

          if x_params_js is initial.
            if line_exists(  x_tbl_parameter->*[ c_key = to_lower( x_class_params-name ) ] ).
              x_params_js = x_tbl_parameter->*[ c_key = to_lower( x_class_params-name ) ].
            endif.
          endif.

          if x_params_js is initial and iv_parameter is not initial.
            if line_exists(  iv_parameter->*[ c_key = to_lower( x_class_params-name ) ] ).
              x_params_js = iv_parameter->*[ c_key = to_lower( x_class_params-name ) ].
            endif.
          endif.

          if x_params_js is initial.
            continue.
          endif.

          split x_params_js-c_value at '.' into data(x_ref_this) data(x_ref_value).

          if x_class_params-parm_kind   = cl_abap_objectdescr=>importing.
            if x_class_params-type_kind = cl_abap_typedescr=>typekind_int.
              create data x_ref_data type i.
              assign x_ref_data->* to <fs_data>.
              <fs_data> = x_params_js-c_value.
            elseif x_class_params-type_kind = cl_abap_typedescr=>typekind_string.
              create data x_ref_data type string.
              assign x_ref_data->* to <fs_data>.
              <fs_data> = x_params_js-c_value.
            elseif x_class_params-type_kind = cl_abap_typedescr=>typekind_oref.
              data(x_type) = to_upper( x_params_js-c_type ).
              create data x_ref_data type ref to (x_type).
              assign x_ref_data->* to <fs_data>.
              <fs_data> ?= x_params_js-c_object.

              x_dyn_param_wa-kind  = cl_abap_objectdescr=>exporting.
              x_dyn_param_wa-name  = x_class_params-name.
              x_dyn_param_wa-value = x_ref_data.
              insert x_dyn_param_wa into table x_dyn_param.
              continue.
            elseif x_class_params-type_kind = cl_abap_typedescr=>typekind_data.
              create data x_ref_data type ref to data.
              assign x_ref_data to <fs_data>.
              <fs_data> = x_params_js-c_value.
            else.
              continue.
            endif.

            "if x_ref_this eq 'this'.
            "  <fs_data> = iv_parameter->*[ c_key = x_ref_value ]-c_ref.
            "else.
            "  <fs_data> = x_params_js-c_value.
            "endif.

            x_dyn_param_wa-kind  = cl_abap_objectdescr=>exporting.
            x_dyn_param_wa-name  = x_class_params-name.
            x_dyn_param_wa-value = x_ref_data.
            insert x_dyn_param_wa into table x_dyn_param.
            continue.
          endif.
        endloop.

        if x_method_name eq 'CONSTRUCTOR'.
          create object x_tbl_obj type (x_class_name) parameter-table x_dyn_param.
        else.
          data: x_cast_obj type ref to object.
          create object x_cast_obj type (x_class_type).
          x_cast_obj ?= x_ref_obj.
          call method x_cast_obj->(x_method_name) parameter-table x_dyn_param.
        endif.

        if x_tbl_obj is bound.
          x_ref_obj ?= x_tbl_obj.
        endif.
      catch cx_dynamic_check into data(x_exception).
        zcl_eezz_message=>add( iv_key = |callback { x_class_name }::{ x_method_name }| iv_exception = x_exception ).
        return.
    endtry.


    rt_table ?= x_ref_obj.

    if x_target_name is not initial and rt_table is not initial.
      modify table iv_symbols->* from value #( c_name = x_target_name c_object = rt_table ) transporting c_object.
      if sy-subrc ne 0.
        insert value #( c_name = x_target_name c_object = rt_table ) into table iv_symbols->*.
      endif.
      data(x_dictionary) = rt_table->get_dictionary( ).
      x_dictionary->*[ c_key = 'tree_path' ]-c_value = x_target_name.
    endif.
  endmethod.


  method constructor.
*   for develop and maintenance output:
    data(c_out) = cl_demo_output=>new( )->begin_section( `PARSE_EEZZ_JSON` ).
    data(x_dbg) = abap_false.
    m_json      = iv_json.

    if it_json is bound.
      m_tbl_json = new ztty_eezz_json( ).
      append value #( c_ref = cast #( it_json ) ) to m_tbl_json->*.
      return.
    endif.

    if is_json is not initial.
      m_tbl_json       = new ztty_eezz_json( ).
      data(x_jsn_root) = new ztty_eezz_json( ).
      append value #( c_ref = x_jsn_root ) to m_tbl_json->*.

      append is_json to x_jsn_root->*.
      return.
    endif.

    if strlen( m_json ) = 0.
      return.
    endif.

    if iv_conv_qmark eq 'X'.
      replace all occurrences of '''' in m_json with '"'.
    endif.

*   Temporary structure and table
*   It would be possible to parse all in one pass
*   Choosing two pass makes things easier to develop
    data: begin of x_wa_node,
            c_type  type string,
            c_name  type string,
            c_value type string,
            c_raw   type xstring,
          end of x_wa_node,
          x_tbl_nodes like table of x_wa_node.

    data(x_json)        = cl_abap_codepage=>convert_to( m_json ).
    data(x_json_reader) = cl_sxml_string_reader=>create( x_json ).

*   Standard parser to ship the json into a table structure
    try.
        do.
          clear x_wa_node.
          data(x_sxml_node) = x_json_reader->read_next_node( ).
          if x_sxml_node is initial.
            exit.
          endif.

          case x_sxml_node->type.
            when if_sxml_node=>co_nt_element_open.
              data(x_open_element)   = cast if_sxml_open_element( x_sxml_node ).
              x_wa_node-c_type       = `open element`.
              x_wa_node-c_name       = x_open_element->qname-name.
              data(x_tbl_attributes) = x_open_element->get_attributes( ).

              append x_wa_node to x_tbl_nodes.

              loop at x_tbl_attributes into data(x_wa_attribute).
                x_wa_node-c_type     = `attribute`.
                x_wa_node-c_name     = x_wa_attribute->qname-name.

                if x_wa_attribute->value_type = if_sxml_value=>co_vt_text.
                  x_wa_node-c_value  = x_wa_attribute->get_value( ).
                elseif x_wa_attribute->value_type = if_sxml_value=>co_vt_raw.
                  x_wa_node-c_raw    = x_wa_attribute->get_value_raw( ).
                endif.

                append x_wa_node to x_tbl_nodes.
              endloop.
              continue.
            when if_sxml_node=>co_nt_element_close.
              data(x_close_element) = cast if_sxml_close_element( x_sxml_node ).
              x_wa_node-c_type      = `close element`.
              x_wa_node-c_name      = x_close_element->qname-name.

              append x_wa_node to x_tbl_nodes.
              continue.
            when if_sxml_node=>co_nt_value.
              data(x_value_node)    = cast if_sxml_value_node( x_sxml_node ).
              x_wa_node-c_type      = `value`.

              if x_value_node->value_type   = if_sxml_value=>co_vt_text.
                x_wa_node-c_value   = x_value_node->get_value( ).
              elseif x_value_node->value_type = if_sxml_value=>co_vt_raw.
                x_wa_node-c_raw     = x_value_node->get_value_raw( ).
              endif.

              append x_wa_node to x_tbl_nodes.
              continue.
          endcase.
        enddo.
      catch cx_sxml_parse_error into data(x_parse_error).
        c_out->write_text( x_parse_error->get_text( ) ).
    endtry.
    c_out->write_data( x_tbl_nodes ).
    c_out->next_section( | Generate JSON tree | ).

*   From the temp table create a recursive structure
*   Define the root element and add elements
    data(x_tbl_root)   = new ztty_eezz_json( ).
    data(x_wajs_param) = new zstr_eezz_json( c_ref = cast #( x_tbl_root ) ).
    data x_wajs_parent  type ref to zstr_eezz_json.

    data x_tbl_stack    type table of ref to zstr_eezz_json.
    data x_tbl_json     type ref to ztty_eezz_json.

    data x_wa_node_key  like x_wa_node.
    data x_wa_node_val  like x_wa_node.

    insert x_wajs_param into x_tbl_stack index 1.
    data(x_cursor) = 1.

*   Process the generated table. A fixed number prevents us from endless loops
    do 100 times.
      try.
          x_wa_node = x_tbl_nodes[ x_cursor ].

          if x_wa_node-c_type co 'open element'.
            " Create a node with a table for attributes as c_ref
            x_tbl_json    = new ztty_eezz_json( ).
            x_wajs_param  = new zstr_eezz_json( c_type = x_wa_node-c_name c_ref = cast #( x_tbl_json ) ).

            insert x_wajs_param into x_tbl_stack index 1.

            if ( x_wa_node-c_name co 'object' ) or ( x_wa_node-c_name co 'array' ).
              x_wa_node_key = x_tbl_nodes[ x_cursor + 1 ].

              if x_wa_node_key-c_type co 'attribute'.
                x_wajs_param->*-c_key = x_wa_node_key-c_value.
                x_cursor              = x_cursor + 1.
              endif.
            elseif ( x_wa_node-c_name co 'str' ) or ( x_wa_node-c_name co 'num' ).
              " Add an entry into the parents table:
              x_wajs_param  = x_tbl_stack[ 1 ].
              x_wajs_parent = x_tbl_stack[ 2 ].

              x_wa_node_key = x_tbl_nodes[ x_cursor + 1 ].
              x_wa_node_val = x_tbl_nodes[ x_cursor + 2 ].

              x_tbl_json    = cast #( x_wajs_parent->*-c_ref ).

              if x_wa_node_key-c_type co 'value'.
                x_wajs_param   = new zstr_eezz_json( c_type = x_wa_node-c_name c_value = x_wa_node_key-c_value ).
                x_cursor       = x_cursor + 1.
              else.
                x_wajs_param   = new zstr_eezz_json( c_type = x_wa_node-c_name c_value = x_wa_node_val-c_value c_key = x_wa_node_key-c_value ).
                x_cursor       = x_cursor + 2.
              endif.

              append x_wajs_param->* to x_tbl_json->*.

            endif.
          elseif x_wa_node-c_type co 'close element'.
            "  At close event join the nodes recursively
            x_wajs_param  = x_tbl_stack[ 1 ].
            x_wajs_parent = x_tbl_stack[ 2 ].

            if ( x_wa_node-c_name co 'object' ) or ( x_wa_node-c_name co 'array' ).
              x_tbl_json = cast #( x_wajs_parent->*-c_ref ).
              append x_wajs_param->* to x_tbl_json->*.
            endif.

            delete x_tbl_stack index: 1.
          endif.

          x_cursor = x_cursor + 1.

        catch cx_sy_itab_line_not_found.
          exit.
      endtry.
    enddo.

    " Collect the result and return, omitting the
    " first entry (root), which is an artificial fetch hook
    try.
        m_tbl_json = cast #( x_tbl_root ).
      catch cx_sy_itab_line_not_found.
        exit.
    endtry.

  endmethod.


  method dump.
    data(x_writer) = new cl_abap_string_c_writer(  ).

    me->walk( iv_writer = x_writer it_json = me->get( iv_path = iv_path ) ).
    rv_jsn_wrt = x_writer.

  endmethod.


  method gen_header_event.
    ev_update =
       '{"callback":{"' && iv_name && '.do_sort":{"index":' && |"{ iv_index }"| && '}},' &&
       ' "update":{"'   && iv_name && '.innerHTML":"*"}}'.
  endmethod.


  method gen_response.
    data: x_update   type zstr_update,
          x_response type string,
          x_tmp      type string.

    data(x_result_stream) = new cl_abap_string_c_writer(  ).
    data(x_command)       = to_lower( iv_command ).

    sort it_update->* by c_prio.

    x_result_stream->write( |\{ "{ x_command }":\{| ).

    loop at it_update->* into x_update.
      if sy-tabix <> 1.
        x_result_stream->write( |,| ).
      endif.
      x_result_stream->write( |"{ condense( x_update-c_key ) }":"{ cl_abap_dyn_prg=>escape_xss_url( x_update-c_value ) }"| ).
    endloop.
    x_result_stream->write( '}' ).
    x_result_stream->write( '}' ).
    rv_json = x_result_stream->get_result_string( ).

  endmethod.


  method gen_row_event.
    data x_attributes type ref to ztty_eezz_json.
    data(x_comma)         = ||.
    data(x_json_update)   = me->get( iv_path = 'update' ).
    data(x_json_callback) = me->get( iv_path = 'callback' ).
    data(x_json_stream)   = new cl_abap_string_c_writer(  ).

    data(x_name) = iv_name.

    if x_json_callback is bound.
      x_json_stream->write( |\{"callback":\{"{ x_json_callback->*[ 1 ]-c_key }":\{| ).

      x_attributes = cast #( x_json_callback->*[ 1 ]-c_ref ).
      if x_attributes is bound.
        loop at x_attributes->* into data(xwa_attr).
          x_json_stream->write( |{ x_comma }"{ xwa_attr-c_key }":"{ xwa_attr-c_value }"| ).
          x_comma = ','.
        endloop.
      endif.

      x_json_stream->write( |\}\}| ).
      x_json_stream->write( | ,"update"  :\{| ).
    else.
      x_json_stream->write( |\{"callback":\{"{ x_name }.do_select"  :\{"index":"{ iv_index }"\}\}| ).
      x_json_stream->write( | ,"update"  :\{| ).
    endif.

    x_comma = ||.
    if x_json_update is bound.
      loop at x_json_update->* into data(xwa_update).

        try.
            split xwa_update-c_key at '.' into table data(x_key_element).
            if iv_dictionary is bound and x_key_element[ 1 ] eq 'this'.
              data(x_this_update_key) = iv_dictionary->*[ c_key = x_key_element[ 2 ] ]-c_value.
              " data(x_this_update_att) = x_key_element[ 3 ].
              x_json_stream->write( |{ x_comma }"{ iv_this }.{ x_this_update_key }":"{ xwa_update-c_value }"| ).
              x_comma = ','.
              continue.
            endif.
          catch cx_sy_itab_line_not_found.
        endtry.

        x_json_stream->write( |{ x_comma }"{ xwa_update-c_key }":"{ xwa_update-c_value }"| ).
        x_comma = ','.
      endloop.
    endif.
    x_json_stream->write( '}}' ).

    ev_result = x_json_stream->get_result_string( ).
  endmethod.


  method get.
    data:
      x_tbl_parameter type ref to ztty_eezz_json,
      x_ref_str       type zstr_eezz_json,
      x_tbl_json      type ref to ztty_eezz_json.

    data(x_ref_tbl) = m_tbl_json.
    data(x_match)   = iv_match.
    data(x_matches) = new cl_abap_string_c_writer(  ).

    clear rt_obj.
    if m_tbl_json is initial.
      return.
    endif.

    if iv_path is initial.
      rt_obj = cast #( x_ref_tbl->*[ 1 ]-c_ref ).
      return.
    endif.
    m_parent      = x_ref_tbl->*[ 1 ].

    split iv_path at '/' into table data(x_tbl_path).
    x_ref_tbl   = cast #( x_ref_tbl->*[ 1 ]-c_ref ).

    if x_ref_tbl is not bound.
      return.
    endif.

    loop at x_tbl_path into data(x_path).
      try.
          if x_path is initial.
            continue.
          endif.
          x_ref_str = x_ref_tbl->*[ c_key = x_path ].
          m_parent  = x_ref_str.

          if x_ref_str-c_ref is not bound.
            modify table x_ref_tbl->* from value #( c_key = x_path c_ref = new ztty_eezz_json( ) ) TRANSPORTING c_ref.
            x_ref_str = x_ref_tbl->*[ c_key = x_path ].
          endif.

          if x_ref_str-c_ref is bound.
            m_status  = x_ref_str-c_status.
            x_ref_tbl = cast #( x_ref_str-c_ref ).
          endif.

          x_match = x_match - 1.
          x_matches->write( |/{ x_path }| ).
          if x_match = 0.
            exit.
          endif.
        catch cx_sy_itab_line_not_found into data(x_exception).
          if x_match <= 0.
            clear rt_obj.
            return.
          endif.
      endtry.
    endloop.

    data(x_match_path) = x_matches->get_result_string( ).

    m_path = x_match_path.
    rt_obj = cast #( x_ref_tbl ).

  endmethod.


  method get_control.
    data(x_element) = me->get( iv_path = iv_path ).
    rv_control      = me->m_parent-c_control.
  endmethod.


  method get_range.

    data:
      x_range         type ref to ztty_eezz_row,
      x_tbl_parameter type ref to ztty_eezz_json,
      x_str_call      type zstr_eezz_json,
      x_entry         type zstr_eezz_json,
      x_ref_tbl       type ref to ztty_eezz_json.

    try.
        create data x_range.
        data(x_length) = lines( iv_names->* ).
        x_ref_tbl      = cast #( m_tbl_json->*[ 1 ]-c_ref ).

        if     line_exists( x_ref_tbl->*[ c_key = 'table-rows' ] ).
          x_entry         = x_ref_tbl->*[ c_key = 'table-rows' ].
          x_tbl_parameter = cast #( x_entry-c_ref ).
        elseif line_exists( x_ref_tbl->*[ c_key = 'table-columns' ] ).
          x_entry         = x_ref_tbl->*[ c_key = 'table-columns' ].
          x_tbl_parameter = cast #( x_entry-c_ref ).
        endif.

        if x_tbl_parameter is not bound.
          return.
        endif.

        " check first entry:
        "  if x_tbl_parameter->*[ 1 ]-c_value co ':'.
        "    rt_range = new #( for i = 1 until i > x_length ( i ) ).
        "  endif.
      catch cx_sy_itab_line_not_found.
    endtry.

    data(xlow)  = 1.
    data(xhigh) = lines( iv_names->* ).

    if x_tbl_parameter->*[ 1 ]-c_value cs ':'.
      append lines of iv_names->* to x_range->*.
    elseif x_tbl_parameter->*[ 1 ]-c_value co ':'.
      " get range
      split x_tbl_parameter->*[ 1 ]-c_value at ':' into data(nxlow) data(nxhigh).
      do nxhigh - nxlow + 1 times.
        append iv_names->*[ sy-index + nxlow ] to x_range->*.
      enddo.
    else.
      loop at x_tbl_parameter->* into data(xwa).
        if xwa-c_type = 'str'.
          if line_exists( iv_names->*[ c_field_name = to_upper( xwa-c_value ) ] ).
            append iv_names->*[ c_field_name = to_upper( xwa-c_value ) ] to x_range->*.
          endif.
        elseif xwa-c_type = 'num'.
          if line_exists( iv_names->*[ xwa-c_value ] ).
            append iv_names->*[ xwa-c_value ] to x_range->*.
          endif.
        endif.
      endloop.
    endif.

    rt_range = x_range.
  endmethod.


  method get_status.
    rv_status = me->get_value( iv_path = iv_path iv_key = iv_key iv_entry = 'status' ).
  endmethod.


  method get_update.
    "et_eezz_json = cast #( m_tbl_json->*[ c_key = 'update' ]->c_ref ).
  endmethod.


  method get_value.
    data:
      x_tbl_parameter type ref to ztty_eezz_json,
      x_str_call      type zstr_eezz_json.

    data(x_ref_tbl) = me->get( iv_path = iv_path ).
    if x_ref_tbl is initial.
      return.
    endif.

    try.
        data(x_entry)   = x_ref_tbl->*[ c_key = iv_key ].
        if iv_entry cs 'VALUE'.
          rv_value = x_entry-c_value.
        elseif iv_entry cs 'status'.
          rv_value = x_entry-c_status.
        endif.
      catch cx_root.
    endtry.

  endmethod.


  method join.
    data x_ref_json   type ref to data.
    data x_value      type string.
    data x_wajs_param type ref to zstr_eezz_json.
    data x_parent     type ref to ztty_eezz_json.

    " Get a referenece to the json table
    data(x_path)    = iv_path.
    data(x_key)     = iv_key.
    data(x_ins_key) = iv_key.


    if x_path is initial.
      x_ins_key = x_key.
      if x_key cs |/|.
        split x_key at '/' into table data(x_wa_path).
        x_key     = x_wa_path[ lines( x_wa_path ) ].
        x_ins_key = x_key.
        delete x_wa_path index lines( x_wa_path ).
        concatenate lines of x_wa_path into x_path separated by '/'.
      endif.
    else.
      x_ins_key = x_path.
      if x_path cs |/|.
        split x_path at '/' into table x_wa_path.
        x_ins_key = x_wa_path[ lines( x_wa_path ) ].
      endif.
    endif.

    if x_ins_key is initial.
      return.
    endif.

    try.
        data(x_ref_tbl) = get( iv_path = x_path ).
      catch cx_root into data(x_exception).

    endtry.

    if x_ref_tbl is not bound.
      if iv_create = abap_false.
        return.
      endif.

      clear x_wa_path.
      split x_path at '/' into table x_wa_path.

      x_ref_tbl = get( ).
      x_parent  = x_ref_tbl.

      loop at x_wa_path into data(x_wa_path_elem).
        if not line_exists( x_ref_tbl->*[ c_key = x_wa_path_elem ] ).
          insert value #( c_key = x_wa_path_elem c_type = |object| c_ref = new ztty_eezz_json( ) ) into table x_ref_tbl->*.
        endif.

        if x_ref_tbl->*[ c_key = x_wa_path_elem ]-c_ref is initial.
          modify table x_ref_tbl->* from value #( c_key = x_wa_path_elem c_ref = new ztty_eezz_json( ) ) transporting c_ref.
        endif.
        x_parent  = x_ref_tbl.
        x_ref_tbl = cast #( x_ref_tbl->*[ c_key = x_wa_path_elem ]-c_ref ).
      endloop.
    endif.

    data(x_type) = iv_type.

    if it_json is bound and x_type is initial.
      x_type = |object|.
    endif.

    if x_type is initial.
      x_type = |str|.
    endif.

    if line_exists( x_ref_tbl->*[ c_key = x_key ] ).
      modify table x_ref_tbl->* from value #( c_key = x_key c_type = x_type c_value = iv_value c_status = iv_status c_object = iv_object c_control = iv_control c_ref = it_json ).
    else.
      append value #( c_key = x_key c_type = x_type c_value = iv_value c_status = iv_status c_object = iv_object c_control = iv_control c_ref = it_json ) to x_ref_tbl->*.
    endif.

  endmethod.


  method set_update.
    data x_json_writer type ref to if_sxml_writer.
    x_json_writer ?= cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

    x_json_writer->open_element( name = 'object' ).
    x_json_writer->open_element( name = 'object' ).
    x_json_writer->write_attribute( name = 'name' value = 'update' ).

    loop at iv_update into data(wa).
      " enter uri-encoded wa-value
      x_json_writer->open_element( name = 'str' ).
      x_json_writer->write_attribute( value = wa-c_key   name  = 'name'  ).
      x_json_writer->write_value(     value = wa-c_value ).
      x_json_writer->close_element( ).
    endloop.

    x_json_writer->close_element( ).
    x_json_writer->close_element( ).


    ev_update = cast cl_sxml_string_writer( x_json_writer )->get_output( ).
  endmethod.


  method walk.
    data: xsep type string value ''.
    data: xarr type ref to ztty_eezz_json.

    if it_json is not bound.
      iv_writer->write( '{}' ).
      return.
    endif.

    iv_writer->write('{').

    loop at it_json->* into data(xl_json).
      if xl_json-c_key is initial.
        continue.
      endif.

      case xl_json-c_type.
        when 'str'.
          iv_writer->write( | { xsep } "{ xl_json-c_key }":"{ xl_json-c_value }" | ).
          xsep = ','.
        when 'num'.
          iv_writer->write( | { xsep } "{ xl_json-c_key }": { xl_json-c_value }  | ).
          xsep = ','.
        when 'array'.
          data x_is_scalar type abap_bool value abap_true.

          data xt_arr type ref to ztty_eezz_json.
          xt_arr  = cast #( xl_json-c_ref ).

          iv_writer->write( | { xsep } "{ xl_json-c_key }":[ | ).
          xsep = ''.

          loop at xt_arr->* into data(xl_arr).
            iv_writer->write( xsep ).

            if xl_arr-c_type cs 'object'.
              walk( iv_writer = iv_writer it_json = cast #( xl_arr-c_ref ) ).
            else.
              iv_writer->write( |"{ xl_arr-c_key }":"{ xl_arr-c_value }"| ).
            endif.
            xsep = ','.
          endloop.

          iv_writer->write( ']' ).
          xsep = ','.
        when 'event'.
          iv_writer->write( | { xsep } "event": | ).
          walk( iv_writer = iv_writer it_json = cast #( xl_json-c_ref ) ).
        when others.
          iv_writer->write( | { xsep } "{ xl_json-c_key }": | ).
          walk( iv_writer = iv_writer it_json = cast #( xl_json-c_ref ) ).
      endcase.

    endloop.

    iv_writer->write('}').

  endmethod.
ENDCLASS.
