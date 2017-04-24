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
*  Description:
*    The json class imports a json string and
*    converts it to a nested set of internal tables
*
*    The class generates method calls to cl_eezz_table objects 
* -------------------------------------------------------------
class zcl_eezz_json definition
  public
  final
  create public .

  public section.

    class-methods gen_header_event
      importing
        !iv_name         type string
        !iv_index        type int4
      returning
        value(ev_update) type string .
    methods get
      importing
        !iv_path      type string
      returning
        value(rt_obj) type ref to ztty_eezz_json .
    methods get_range
      importing
        !iv_names       type ref to ztty_eezz_row
      returning
        value(rt_range) type ref to ztty_eezz_row .
    methods constructor
      importing
        !iv_json       type string
        !iv_conv_qmark type boolean default 'X' .
    methods get_update
      returning
        value(et_eezz_json) type ref to ztty_eezz_parameter .
    methods callback
      importing
        !iv_symbols     type ref to ztty_global_symbols
      returning
        value(rt_table) type ref to zif_eezz_table .
    class-methods set_update
      importing
        !iv_update       type ztty_eezz_parameter
      returning
        value(ev_update) type string .
    methods get_value
      importing
        !iv_path        type string
      returning
        value(rv_value) type string .
    methods gen_row_event
      importing
        !iv_name         type string
        !iv_index        type int4
      returning
        value(ev_result) type string .
  protected section.
  private section.

    data m_json type string .
    data m_tbl_json type ref to ztty_eezz_json .
ENDCLASS.



CLASS ZCL_EEZZ_JSON IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_JSON->CALLBACK
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SYMBOLS                     TYPE REF TO ZTTY_GLOBAL_SYMBOLS
* | [<-()] RT_TABLE                       TYPE REF TO ZIF_EEZZ_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method callback.
    data:
      x_class_name    type string,
      x_method_name   type string,
      x_tbl_callback  type ref to ztty_eezz_json,
      x_tbl_parameter type ref to ztty_eezz_json,
      x_str_call      type zstr_eezz_json,
      x_dyn_param_wa  type abap_parmbind,
      x_dyn_param     type abap_parmbind_tab.

    field-symbols <fs_data> type any.
    data x_ref_data type ref to data.
    data x_ref_obj  type ref to zif_eezz_table.
    data x_tbl_obj  type ref to zif_eezz_table.

    if x_tbl_callback is initial.
      x_tbl_callback = me->get( 'callback' ).
    endif.

    if x_tbl_callback is initial.
      x_tbl_callback = me->get( 'eezzAgent.assign' ).
    endif.

    if x_tbl_callback is initial.
      return.
    endif.

    split x_tbl_callback->*[ 1 ]-c_key at '.' into x_class_name x_method_name.

    try.
        if strlen( x_method_name ) = 0.
          x_method_name = 'CONSTRUCTOR'.
          "x_tbl_parameter = cast #( x_tbl_callback->*[ 1 ]-c_ref ).
          "if lines( x_tbl_parameter->* ) > 0.
          "  create object x_tbl_obj type (x_class_name) exporting table_name = x_tbl_parameter->*[ 1 ]-c_value.
          "else.
          "  create object x_tbl_obj type (x_class_name).
          "endif.
          "rt_table ?= x_tbl_obj.
          "return.
          data(lo_cref)   = cast cl_abap_classdescr( cl_abap_classdescr=>describe_by_name( x_class_name ) ).
        else.
          translate x_method_name to upper case.
          x_ref_obj     ?= iv_symbols->*[ c_name = x_class_name ]-c_object.
          lo_cref        = cast cl_abap_classdescr( cl_abap_classdescr=>describe_by_object_ref( x_ref_obj ) ).
        endif.

        data(lo_params) = lo_cref->methods[ name = to_upper( x_method_name ) ]-parameters.
        x_tbl_parameter = cast #( x_tbl_callback->*[ 1 ]-c_ref ).

        loop at lo_params into data(x_class_params).
          if x_class_params-parm_kind = cl_abap_objectdescr=>importing.
            if x_class_params-type_kind = cl_abap_typedescr=>typekind_int.
              create data x_ref_data type i.
            elseif x_class_params-type_kind = cl_abap_typedescr=>typekind_string.
              create data x_ref_data type string.
            else.
              continue.
            endif.

            if not line_exists(  x_tbl_parameter->*[ c_key = to_lower( x_class_params-name ) ] ).
              continue.
            endif.

            assign x_ref_data->* to <fs_data>.
            data(x_params_js)    = x_tbl_parameter->*[ c_key = to_lower( x_class_params-name ) ].
            <fs_data>            = x_params_js-c_value.

            x_dyn_param_wa-kind  = cl_abap_objectdescr=>exporting.
            x_dyn_param_wa-name  = x_class_params-name.
            x_dyn_param_wa-value = x_ref_data.
            insert x_dyn_param_wa into table x_dyn_param.
            continue.
          endif.

          if x_class_params-parm_kind = cl_abap_objectdescr=>returning.
            if x_class_params-type_kind = cl_abap_typedescr=>typekind_oref.
              x_dyn_param_wa-kind       = cl_abap_objectdescr=>returning.
              x_dyn_param_wa-name       = x_class_params-name.
              get reference of x_tbl_obj into x_dyn_param_wa-value.
              insert x_dyn_param_wa into table x_dyn_param.
            endif.
            continue.
          endif.
        endloop.

        if x_method_name eq 'CONSTRUCTOR'.
          create object x_tbl_obj type (x_class_name) parameter-table x_dyn_param.
        else.
          call method x_ref_obj->(x_method_name) parameter-table x_dyn_param.
        endif.

        if x_tbl_obj is bound.
          x_ref_obj ?= x_tbl_obj.
        endif.
      catch cx_root into data(x_cx_root).
    endtry.

    rt_table ?= x_ref_obj.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_JSON->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_JSON                        TYPE        STRING
* | [--->] IV_CONV_QMARK                  TYPE        BOOLEAN (default ='X')
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method constructor.
* for develop and maintenance output:
    data(c_out) = cl_demo_output=>new( )->begin_section( `PARSE_EEZZ_JSON` ).
    data(x_dbg) = abap_false.
    m_json      = iv_json.

    if strlen( m_json ) = 0.
      return.
    endif.

    if iv_conv_qmark eq 'X'.
      replace all occurrences of '''' in m_json with '"'.
    endif.

* Temporary structure and table
* It would be possible to parse all in one pass
* Choosing two pass makes things easier to develop
    data: begin of x_wa_node,
            c_type  type string,
            c_name  type string,
            c_value type string,
            c_raw   type xstring,
          end of x_wa_node,
          x_tbl_nodes like table of x_wa_node.

    data(x_json)        = cl_abap_codepage=>convert_to( m_json ).
    data(x_json_reader) = cl_sxml_string_reader=>create( x_json ).

* Standard parser to ship the json into a table structure
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
    c_out->next_section( `Generate JSON tree` ).

* From the temp table create a recursive structure
* Define the root element and add elements
    data(x_tbl_root)   = new ztty_eezz_json( ).
    data(x_wajs_param) = new zstr_eezz_json( c_ref = cast #( x_tbl_root ) ).
    data x_wajs_parent  type ref to zstr_eezz_json.

    data x_tbl_stack    type table of ref to zstr_eezz_json.
    data x_tbl_json     type ref to ztty_eezz_json.

    data x_wa_node_key  like x_wa_node.
    data x_wa_node_val  like x_wa_node.

    insert x_wajs_param into x_tbl_stack index 1.
    data(x_cursor) = 1.

* Process the generated table. A fixed number prevents us from endless loops
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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_EEZZ_JSON=>GEN_HEADER_EVENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        STRING
* | [--->] IV_INDEX                       TYPE        INT4
* | [<-()] EV_UPDATE                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method gen_header_event.
    ev_update =
       '{"callback":{"' && iv_name && '.do_sort":{"index":' && |"{ iv_index }"| && '}},' &&
       ' "update":{"'   && iv_name && '.innerHTML":"*"}}'.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_JSON->GEN_ROW_EVENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        STRING
* | [--->] IV_INDEX                       TYPE        INT4
* | [<-()] EV_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method gen_row_event.
    data(x_json_update) = me->get( 'update' ).
    data(x_json_stream) = new cl_abap_string_c_writer(  ).

    x_json_stream->write( |\{"callback":\{"{ iv_name }.do_select"  :\{"index":"{ iv_index }"\}\}| ).
    x_json_stream->write( | ,"update"  :\{| ).

    if x_json_update is bound.
      loop at x_json_update->* into data(xwa_update).
        if sy-tabix > 1.
          x_json_stream->write( ',' ).
        endif.
        x_json_stream->write( |"{ xwa_update-c_key }":"{ xwa_update-c_value }"| ).
      endloop.
    endif.
    x_json_stream->write( '}}' ).

    ev_result = x_json_stream->get_result_string( ).
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_JSON->GET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PATH                        TYPE        STRING
* | [<-()] RT_OBJ                         TYPE REF TO ZTTY_EEZZ_JSON
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get.
    data:
      x_tbl_parameter type ref to ztty_eezz_json,
      x_str_call      type zstr_eezz_json.

    data(x_ref_tbl) = m_tbl_json.

    clear rt_obj.

    try.
        x_ref_tbl   = cast #( m_tbl_json->*[ 1 ]-c_ref ).
        x_str_call  = x_ref_tbl->*[ c_key = iv_path ].

        if x_str_call-c_ref is bound.
          rt_obj      = cast #( x_str_call-c_ref ).
        else.
          rt_obj      = cast #( x_ref_tbl ).
        endif.
      catch cx_sy_itab_line_not_found.
    endtry.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_JSON->GET_RANGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAMES                       TYPE REF TO ZTTY_EEZZ_ROW
* | [<-()] RT_RANGE                       TYPE REF TO ZTTY_EEZZ_ROW
* +--------------------------------------------------------------------------------------</SIGNATURE>
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
          if line_exists( iv_names->*[ c_field_name = xwa-c_value ] ).
            append iv_names->*[ c_field_name = xwa-c_value ] to x_range->*.
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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_JSON->GET_UPDATE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] ET_EEZZ_JSON                   TYPE REF TO ZTTY_EEZZ_PARAMETER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_update.
    "et_eezz_json = cast #( m_tbl_json->*[ c_key = 'update' ]->c_ref ).
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_JSON->GET_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PATH                        TYPE        STRING
* | [<-()] RV_VALUE                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_value.
    data:
      x_tbl_parameter type ref to ztty_eezz_json,
      x_str_call      type zstr_eezz_json.

    data(x_ref_tbl) = m_tbl_json.

    try.
        x_ref_tbl   = cast #( m_tbl_json->*[ 1 ]-c_ref ).
        x_str_call  = x_ref_tbl->*[ c_key = iv_path ].
        rv_value    = x_str_call-c_value.
      catch cx_sy_itab_line_not_found.
    endtry.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_EEZZ_JSON=>SET_UPDATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_UPDATE                      TYPE        ZTTY_EEZZ_PARAMETER
* | [<-()] EV_UPDATE                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
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
ENDCLASS.