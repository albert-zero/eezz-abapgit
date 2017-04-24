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
*    Representation of an IF_IXML_NODE with assoziated table    
*    This class is used to generate a HTML table from if_eezz_table objects
*
class zcl_eezz_table_node definition
  public
  final
  create public .

  public section.

    methods constructor
      importing
        !io_node     type ref to if_ixml_node
        !it_globals  type ref to ztty_global_symbols
        !io_eezz_tbl type ref to zif_eezz_table .
    methods get
      returning
        value(ev_table_node) type ref to if_ixml_node .
    methods create_body
      importing
        !iv_node        type ref to if_ixml_node
        !iv_parent      type ref to if_ixml_node
      returning
        value(rt_table) type ref to ztty_eezz_row .
  protected section.
  private section.

    data mt_range type ref to ztty_eezz_row .
    data m_dictionary type ref to ztty_dictionary .
    data m_table_name type string .
    data m_tbl_eezz type ref to zif_eezz_table .
    data m_tbl_global type ref to ztty_global_symbols .
    data m_table_node type ref to if_ixml_node .

    methods create_column_filter
      importing
        !iv_filter  type string
        !it_columns type ref to ztty_eezz_row .
    methods add_onclick_4_header
      importing
        !iv_parent type ref to if_ixml_node
        !iv_node   type ref to if_ixml_node
        !iv_idx    type int4 .
    methods add_onclick_4_row
      importing
        !iv_node type ref to if_ixml_node
        !iv_json type ref to zcl_eezz_json
        !iv_idx  type int4 .
    methods add_onclick_event
      importing
        !iv_node type ref to if_ixml_node .
    methods create_footer
      importing
        !iv_node   type ref to if_ixml_node
        !iv_parent type ref to if_ixml_node .
    methods create_header
      importing
        !iv_node   type ref to if_ixml_node
        !iv_parent type ref to if_ixml_node .
    methods create_row
      importing
        !iv_node              type ref to if_ixml_node
        !iv_parent            type ref to if_ixml_node
        !iv_row               type ref to ztty_eezz_row
      returning
        value(rt_replacement) type ref to ztty_eezz_row .
    methods create_tile
      importing
        !iv_node   type ref to if_ixml_node
        !iv_parent type ref to if_ixml_node
        !iv_row    type ref to ztty_eezz_row
        !iv_index  type int4 .
ENDCLASS.



CLASS ZCL_EEZZ_TABLE_NODE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_EEZZ_TABLE_NODE->ADD_ONCLICK_4_HEADER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PARENT                      TYPE REF TO IF_IXML_NODE
* | [--->] IV_NODE                        TYPE REF TO IF_IXML_NODE
* | [--->] IV_IDX                         TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method add_onclick_4_header.
    " Add onClick event
    cast if_ixml_element( iv_node )->set_attribute( name = 'onclick' value  = 'easyClick(event,this)' ).
    data(x_event_str) = zcl_eezz_json=>gen_header_event( iv_name = m_table_name iv_index = iv_idx ).
    cast if_ixml_element( iv_node )->set_attribute( name = 'data-eezz-event' value = x_event_str ).
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_EEZZ_TABLE_NODE->ADD_ONCLICK_4_ROW
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE                        TYPE REF TO IF_IXML_NODE
* | [--->] IV_JSON                        TYPE REF TO ZCL_EEZZ_JSON
* | [--->] IV_IDX                         TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method add_onclick_4_row.

    if iv_json is not bound.
      return.
    endif.

    cast if_ixml_element( iv_node )->set_attribute( name = 'onclick' value  = 'easyClick(event,this)' ).
    data(x_event_str) = iv_json->gen_row_event( iv_name = m_table_name iv_index = iv_idx ).
    cast if_ixml_element( iv_node )->set_attribute( name = 'data-eezz-event'  value = x_event_str ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_EEZZ_TABLE_NODE->ADD_ONCLICK_EVENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE                        TYPE REF TO IF_IXML_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method add_onclick_event.
    "cast if_ixml_element( iv_node )->set_attribute( name = 'onclick' value  = 'easyClick(event,this)' ).
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TABLE_NODE->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_NODE                        TYPE REF TO IF_IXML_NODE
* | [--->] IT_GLOBALS                     TYPE REF TO ZTTY_GLOBAL_SYMBOLS
* | [--->] IO_EEZZ_TBL                    TYPE REF TO ZIF_EEZZ_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method constructor.
    data x_refnode type ref to if_ixml_node.
    " clone 0 will only copy node, no childs
    m_table_node     = io_node->clone( 0 ).
    data(x_iterator) = io_node->create_iterator( 1 ).
    data(x_next)     = x_iterator->get_next( ).
    m_tbl_global     = it_globals.
    m_tbl_eezz       = io_eezz_tbl.
    m_dictionary     = io_eezz_tbl->get_dictionary( ).
    m_table_name     = cast if_ixml_element( m_table_node )->get_attribute( 'name' ).
* m_table_node
*    data(x_attributes) = m_table_node->get_attributes( ).
*    data(x_attr_iterator) = x_attributes->create_iterator( ).
*    data(x_attr_element) = x_attr_iterator->get_next( ).

** Get Dict from SFLIGHT
**    node_name->('method_name')
*
*    WHILE x_attr_element is bound.
*      data(x_attr_value) = x_attr_element->get_value( ).
*      IF x_attr_value+0(1) EQ '{'.
*        data(x_length) = strlen( x_attr_value ).
*        x_length = x_length - 2.
*        data(x_key) = x_attr_value+1(x_length).
** Create update -> Update TABLE
*        name_obj.
*
*      ENDIF.
*      x_attr_element = x_attr_iterator->get_next( ).
*    ENDwhile.

    " get attribute assign using zcl_eezz_json
    " execute and get the reference to the database table
    " data(x_eezz_json) = new zcl_eezz_json( )
    while x_next is bound.
      data(x_name) = x_next->get_name( ).
      if x_name eq 'tbody'.
        x_refnode = x_next->clone( 0 ).
        m_table_node->append_child( x_refnode ).
        me->create_body( iv_parent = x_refnode iv_node = x_next  ).
      elseif x_name eq 'thead'.
        x_refnode = x_next->clone( 0 ).
        m_table_node->append_child( x_refnode ).
        me->create_header( iv_parent = x_refnode iv_node = x_next  ).
      elseif x_name eq 'tfoot'.
        x_refnode = x_next->clone( 0 ).
        m_table_node->append_child( x_refnode ).
        me->create_footer( iv_parent = x_refnode iv_node = x_next  ).
      elseif x_name eq 'caption'.
        m_table_node->append_child( x_next->clone( ) ).
      endif.
      x_next = x_iterator->get_next( ).
    endwhile.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TABLE_NODE->CREATE_BODY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE                        TYPE REF TO IF_IXML_NODE
* | [--->] IV_PARENT                      TYPE REF TO IF_IXML_NODE
* | [<-()] RT_TABLE                       TYPE REF TO ZTTY_EEZZ_ROW
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method create_body.
    types:
      begin of ttemplates,
        c_key   type string,
        c_value type string,
        c_node  type ref to if_ixml_node,
        c_repl  type ref to ztty_eezz_row,
        c_json  type ref to zcl_eezz_json,
      end of ttemplates.

    data x_cwa_templ   type ttemplates.
    data x_tbl_templ   type table of ttemplates.
    data x_eezz_json   type ref to zcl_eezz_json.
    data x_reference   type ref to if_ixml_node.
    data: x_num_column type i,
          x_table_row  type ref to ztty_eezz_row,
          x_idx        type i,
          x_dbg_name   type string.
    field-symbols <fs_cw_templ> type ttemplates.

    "-----DATA(x_iterator)  = iv_node->create_iterator( 0 ).
    data(x_iterator) = iv_node->get_children( )->create_iterator( ).

    do 100 times.
      data(x_next) = x_iterator->get_next( ).
      if x_next is not bound.
        exit.
      endif.

      data(x_name) = x_next->get_name( ).
      if x_name eq 'tbody'.  "skip
        continue.
      elseif x_name ns 'tr'.
        " iv_parent->append_child( x_next ).
        continue.
      endif.

      " Add elements with no attributes
      data(x_attributes) = x_next->get_attributes( ).
      if x_attributes is not bound.
        iv_parent->append_child( x_next ).
        continue.
      endif.

      " Add elements with attributes not having data-eezz-template
      data(x_eezz_attr) = x_attributes->get_named_item_ns( 'data-eezz-template' ).
      if x_eezz_attr is not bound.
        iv_parent->append_child( x_next ).
        continue.
      endif.

      " Store elements with display template in local template table
      x_eezz_json = new zcl_eezz_json( x_eezz_attr->get_value( ) ).

      if x_eezz_json->get( 'display' )    is bound.
        data(x_jsonobj)     = x_eezz_json->get( 'display' ).
        x_cwa_templ-c_key   = x_jsonobj->*[ 1 ]-c_key.
        x_cwa_templ-c_value = x_jsonobj->*[ 1 ]-c_value.
        x_cwa_templ-c_node  = x_next->clone( ).
        append x_cwa_templ to x_tbl_templ.
        continue.
      endif.

      " Evaluate and add table-row elements
      if x_eezz_json->get( 'table-rows' ) is not bound.
        continue.
      endif.

      data(x_columns)     = m_tbl_eezz->get_column_names( ).
      data(x_range)       = x_eezz_json->get_range( x_columns ).
      x_num_column        = lines( x_columns->* ).

      x_cwa_templ-c_key   = 'default'.
      x_cwa_templ-c_node  = x_next->clone( ).
      append x_cwa_templ to x_tbl_templ.

      data(x_tiles) = x_eezz_json->get( 'table-rows' ).

      " Generate tiles and continue
      if x_tiles->*[ 1 ]-c_key eq 'tiles'.
        data(x_tiles_num) = x_tiles->*[ 1 ]-c_value.
        data(x_row_num) = m_dictionary->*[ c_key = 'table_next' ]-c_value.
        data(x_rows) = floor( x_row_num / x_tiles_num ).
        x_idx = 0.

        do x_rows times.
          clear x_reference.
          if x_reference is not bound.  " For each row I need a new clone
            x_dbg_name = x_next->get_name( ).
            " Clone only TR, TDs are added in create_tile( )
            x_reference = x_next->clone( 0 ).
          endif.
          iv_parent->append_child( x_reference ).
          do x_tiles_num times.
            add 1 to x_idx.
            x_table_row = m_tbl_eezz->get_row( x_idx ).
            if x_table_row is not bound.
              exit.
            endif.
            me->create_tile( iv_parent = x_reference iv_node = x_next  iv_row = x_table_row iv_index = x_idx ).
          enddo.
        enddo.
        continue.
      endif.

      " Generate table elements and continue
      do 100 times.
        clear x_reference.
        clear x_cwa_templ.

        if <fs_cw_templ> is assigned.
          unassign <fs_cw_templ>.
        endif.

        x_idx = sy-index.
        x_table_row = m_tbl_eezz->get_row( x_idx ).
        if x_table_row is not bound.
          exit.
        endif.

        " Check if the type is specified
        if line_exists( x_table_row->*[ x_num_column + 1 ] ).
          data(x_row_key) = x_table_row->*[ x_num_column + 1 ]-c_type.
          if line_exists( x_tbl_templ[ c_value = x_row_key ] ).
            assign x_tbl_templ[ c_value = x_row_key ] to <fs_cw_templ>.
          endif.
        endif.

        if <fs_cw_templ> is not assigned.
          assign x_tbl_templ[ c_key = 'default' ] to <fs_cw_templ>.
        endif.

        if <fs_cw_templ>-c_repl is not bound.
          data(x_tr_new_node)  = <fs_cw_templ>-c_node->clone( 0 ).
          data(x_tr_sub_node)  = <fs_cw_templ>-c_node.
          <fs_cw_templ>-c_repl = me->create_row( iv_parent = x_tr_new_node iv_node = x_tr_sub_node iv_row = x_table_row ).
          <fs_cw_templ>-c_node = x_tr_new_node.

          try.
              data(x_row_update) = cast if_ixml_element( x_tr_new_node )->get_attribute_ns( 'data-eezz_event' ).
              if strlen( x_row_update ) > 0.
                <fs_cw_templ>-c_json = new zcl_eezz_json( x_row_update ).
              else.
                data(x_global_entry) = m_tbl_global->*[  c_name =  m_table_name ].
                <fs_cw_templ>-c_json = x_global_entry-c_eezz_json.
              endif.
            catch cx_root.
          endtry.
        endif.

        loop at <fs_cw_templ>-c_repl->* into data(x_wa_repl).
          if line_exists( x_table_row->*[ c_field_name = x_wa_repl-c_field_name ] ).
            x_wa_repl-c_node->set_value( x_table_row->*[ c_field_name = x_wa_repl-c_field_name ]-c_value ).
          elseif line_exists( m_dictionary->*[ c_key = x_wa_repl-c_field_name ] ).
            data(x_attr_val)   = x_wa_repl-c_value.
            data(x_dict_value) = m_dictionary->*[ c_key = x_wa_repl-c_field_name ].
            read table m_dictionary->* with key c_key = x_wa_repl-c_field_name into x_dict_value.
            replace |\{{ x_wa_repl-c_field_name }\}| in x_attr_val with m_dictionary->*[ c_key = x_wa_repl-c_field_name ]-c_value.
            x_wa_repl-c_node->set_value( x_attr_val ).
          endif.
        endloop.

        add_onclick_4_row( iv_node = <fs_cw_templ>-c_node iv_json = <fs_cw_templ>-c_json iv_idx = x_idx ).
        iv_parent->append_child( <fs_cw_templ>-c_node->clone( ) ).
      enddo.
    enddo.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_EEZZ_TABLE_NODE->CREATE_COLUMN_FILTER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILTER                      TYPE        STRING
* | [--->] IT_COLUMNS                     TYPE REF TO ZTTY_EEZZ_ROW
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method create_column_filter.

    data: x_filter  type string,
          x_indexes type table of i,
          x_from_s  type string,
          x_to_s    type string.

    x_filter = iv_filter.
    if iv_filter co ':'. " display all
      " do nothing
      return.
    elseif iv_filter co '0123456789'. " just one column
      append x_filter to x_indexes.
    elseif iv_filter cs ':'. " range is defined
      split iv_filter at ':' into x_from_s x_to_s.
      data(x_from) = x_from_s.
      data(x_to) = x_to_s.
      do ( x_to - x_from ) + 1 times.
        append x_from to x_indexes.
        add 1 to x_from.
      enddo.
    elseif iv_filter cs ','. " list is defined
      split iv_filter at ',' into table data(x_list).
      loop at x_list into data(x_index).
        append x_index to x_indexes.
      endloop.
    endif.

    loop at it_columns->* into data(x_column).
      read table x_indexes with key table_line = x_column-position transporting no fields.
      if sy-subrc <> 0.
        delete it_columns->* where position = x_column-position.
      endif.
    endloop.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_EEZZ_TABLE_NODE->CREATE_FOOTER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE                        TYPE REF TO IF_IXML_NODE
* | [--->] IV_PARENT                      TYPE REF TO IF_IXML_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method create_footer.

    data(x_iterator) = iv_node->create_iterator( ).
    data(x_next)     = x_iterator->get_next( ).

    if x_next is not bound.
      exit.
    endif.

    if x_next->get_name( ) eq 'tfoot'.  "skip
      x_next = x_iterator->get_next( ).
    endif.

    if x_next is bound.
      iv_parent->append_child( x_next->clone( ) ).
    endif.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_EEZZ_TABLE_NODE->CREATE_HEADER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE                        TYPE REF TO IF_IXML_NODE
* | [--->] IV_PARENT                      TYPE REF TO IF_IXML_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method create_header.

    types:
      begin of ttemplates,
        c_key   type string,
        c_value type string,
        c_node  type ref to if_ixml_node,
      end of ttemplates.

    data x_cwa_templ  type ttemplates.
    data x_tbl_templ  type table of ttemplates.
    data(x_iterator)  = iv_node->create_iterator( 1 ).
    data x_eezz_json  type ref to zcl_eezz_json.
    data x_table_row  type ref to ztty_eezz_row.
    data x_column_idx type i.

    do 10 times.
      data(x_next) = x_iterator->get_next( ).
      if x_next is not bound.
        exit.
      endif.

      data(x_name) = x_next->get_name( ).
      if x_name eq 'thead'.  "skip
        continue.
      elseif x_name cn 'tr'.
        iv_parent->append_child( x_next ).
        continue.
      endif.

      data(x_attributes) = x_next->get_attributes( ).
      if x_attributes is not bound.
        iv_parent->append_child( x_next ).
        continue.
      endif.

      data(x_eezz_attr) = x_attributes->get_named_item_ns( 'data-eezz-template' ).
      if x_eezz_attr is bound.

        x_eezz_json = new zcl_eezz_json( x_eezz_attr->get_value( ) ).

        data(x_tbl_row) = x_eezz_json->get_value( 'table-rows' ).
        data(x_columns) = m_tbl_eezz->get_column_names( ).

        " get <tr> tag template
        data(x_tr) = x_next->clone( 0 ).
        iv_parent->append_child( x_tr ).

        data(x_th) = x_next->get_first_child( )->clone( 0 ).
        data(x_th_attributes) = x_th->get_attributes( ).
        data(x_th_eezz_attr)  = x_th_attributes->get_named_item_ns( 'data-eezz-template' ).
        data(x_th_eezz_json)  = new zcl_eezz_json( x_th_eezz_attr->get_value( ) ).

        mt_range              = x_th_eezz_json->get_range( x_columns ).
        "DATA(x_tbl_column) = x_th_eezz_json->get( 'table-columns' ).
        "create_column_filter( iv_filter = x_tbl_column->*[ 1 ]-c_value it_columns = x_columns ).

        loop at mt_range->* into data(x_wac_column).
          x_column_idx = sy-tabix.
          " get <th> tag template
          x_th = x_next->get_first_child( )->clone( 0 ).

          x_th->set_value( x_wac_column-c_value ).
          " add on_sort event
          add_onclick_4_header( iv_parent = iv_parent iv_node = x_th iv_idx = x_wac_column-position ). "x_column_idx ).

          data(x_name_) = x_th->get_name( ).
          x_tr->append_child( x_th ).
        endloop.
        exit.
      endif.
    enddo.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_EEZZ_TABLE_NODE->CREATE_ROW
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE                        TYPE REF TO IF_IXML_NODE
* | [--->] IV_PARENT                      TYPE REF TO IF_IXML_NODE
* | [--->] IV_ROW                         TYPE REF TO ZTTY_EEZZ_ROW
* | [<-()] RT_REPLACEMENT                 TYPE REF TO ZTTY_EEZZ_ROW
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method create_row.
    types:
      begin of ttemplates,
        c_key   type string,
        c_value type string,
        c_node  type ref to if_ixml_node,
      end of ttemplates.


    " find all <th> tags for a specific <tr> on the primary level
    data(x_iterator)   = iv_node->create_iterator( 0 ).
*    data(x_dictionary) = m_eezz_table->get_dictionary( ).
    data x_it_deep     type ref to if_ixml_node_iterator.
    data x_node_deep   type ref to if_ixml_node.
    data x_node_test   type ref to if_ixml_node_iterator.
    data x_str_tmp     type string.
    data x_tbl_string  type table of string.
    data x_wac_templ   type ttemplates.
    data x_tbl_templ   type table of ttemplates.
    data x_reference   type ref to if_ixml_node.
    data x_wa_repl     type zstr_cell.
    data x_replacement type ref to ztty_eezz_row.

    "x_replacement = create  ztty_eezz_row.
    create data x_replacement.

    do 100 times.
      data(x_next) = x_iterator->get_next( ).
      if x_next is not bound.
        exit.
      endif.

      data(x_name) = x_next->get_name( ).
      if x_name ns 'td'.
        continue.
      endif.

      data(x_attributes) = x_next->get_attributes( ).
      if x_attributes is not bound.
        iv_parent->append_child( x_next ).
        continue.
      endif.

      data(x_eezz_attr) = x_attributes->get_named_item_ns( 'data-eezz-template' ).
      if x_eezz_attr is not bound.
        iv_parent->append_child( x_next ).
        continue.
      endif.

      data(x_cl_json)  = new zcl_eezz_json( x_eezz_attr->get_value( ) ).
      data(x_template) = x_cl_json->get( 'display' ).

      if x_template is bound.
        data(x_jsonobj)     = x_cl_json->get( 'display' ).
        x_wac_templ-c_key   = x_jsonobj->*[ 1 ]-c_key.
        x_wac_templ-c_value = x_jsonobj->*[ 1 ]-c_value.
        x_wac_templ-c_node  = x_next.

        append x_wac_templ to x_tbl_templ.
        continue.
      endif.

      x_template = x_cl_json->get( 'table-columns' ).

      if mt_range is initial.
        data(x_columns) =  m_tbl_eezz->get_column_names( ).
        mt_range = x_cl_json->get_range( x_columns ).
      endif.

      if x_template is bound.
        "LOOP AT iv_row->* INTO DATA(x_wac_column).
        loop at mt_range->* into data(x_wa_range).
          clear x_reference.
          if x_wa_range-c_field_name is not initial.
            data(x_wac_column) = iv_row->*[ c_field_name = x_wa_range-c_field_name ].
          else.
            x_wac_column       = iv_row->*[ c_field_name = x_wa_range-c_value ].
          endif.

          if x_wac_column-c_type is not initial.
            if line_exists( x_tbl_templ[ c_value = x_wac_column-c_type ] ).
              x_reference = x_tbl_templ[ c_value = x_wac_column-c_type ]-c_node->clone( ).
            endif.
          endif.

          if x_reference is not bound. " For each td a new clone is needed?
            data(x_dbg_name) = x_next->get_name( ).
            x_reference = x_next->clone( ).
          endif.

          iv_parent->append_child( x_reference ).

          " set the value
          x_it_deep   = x_reference->create_iterator( ).
          do 100 times.
            x_node_deep = x_it_deep->get_next( ).
            if x_node_deep is not bound.
              exit.
            endif.
            x_dbg_name = x_node_deep->get_name( ).
            data(x_dbg_value) = x_node_deep->get_value( ).
            data(x_dbg_type)  = x_node_deep->get_type( ).

            if x_dbg_type = if_ixml_node=>co_node_text and x_node_deep->get_value( ) cs '{}'.
              x_wa_repl-c_field_name = x_wac_column-c_field_name.
              x_wa_repl-c_node       = x_node_deep.
              x_wa_repl-c_value      = x_node_deep->get_value( ).
              append x_wa_repl to x_replacement->*.
            else.
              data(x_element)        = cast if_ixml_element( x_node_deep ).
              data(x_attribute_item) = x_element->get_attribute_node_ns( name = 'data-eezz-attributes' ).
              if x_attribute_item is not bound.
                continue.
              endif.

              data(x_attribute_name) = x_element->get_attribute_node_ns( name = 'name' ).
              if line_exists(  m_tbl_global->*[ c_name = x_attribute_name->get_value( ) ] ).
                data(x_json) = m_tbl_global->*[ c_name = x_attribute_name->get_value( ) ]-c_eezz_json.
              endif.

              if x_json is not bound.
                x_json = new zcl_eezz_json( x_attribute_item->get_value( ) ).
              endif.

              data(x_style) = x_json->get( 'style' ).

              find all occurrences of regex '\{([a-zA-Z0-9]+)\}' in x_style->*[ 1 ]-c_value results data(x_dyn_attr).
              loop at x_dyn_attr into data(wac_attr).
                try.
                    data(x_match_reg) = wac_attr.
                    data(x_match_key) = wac_attr-submatches[ 1 ].
                    data(x_attr_val)  = x_style->*[ 1 ]-c_value.
                    data(x_dict_key)  = x_style->*[ 1 ]-c_value+x_match_key-offset(x_match_key-length).
                    x_element->set_attribute_ns( name = 'style' value = x_attr_val ).

                    x_wa_repl-c_field_name = x_dict_key.
                    x_wa_repl-c_node       = x_element->get_attribute_node_ns( 'style' ).
                    x_wa_repl-c_value      = x_attr_val.
                    append x_wa_repl to x_replacement->*.
                  catch cx_sy_itab_line_not_found.
                endtry.
              endloop.
            endif.
          enddo.
        endloop.
        continue.
      endif.
    enddo.
    rt_replacement = x_replacement.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_EEZZ_TABLE_NODE->CREATE_TILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE                        TYPE REF TO IF_IXML_NODE
* | [--->] IV_PARENT                      TYPE REF TO IF_IXML_NODE
* | [--->] IV_ROW                         TYPE REF TO ZTTY_EEZZ_ROW
* | [--->] IV_INDEX                       TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method create_tile.

    data: x_string type string.

    data(x_tr_iterator) = iv_node->create_iterator( ).
    x_tr_iterator->get_next( ).

    data(x_td_node) = x_tr_iterator->get_next( )->clone( ). " Get clone of <TD>
    data(x_iterator)   = x_td_node->create_iterator( ).

    do 100 times.
      data(x_next) = x_iterator->get_next( ).
      if x_next is not bound.
        exit.
      endif.
      data(x_name) = x_next->get_name( ).
      data(x_type) = x_next->get_type( ).
      data(x_value) = x_next->get_value( ).
      if x_name eq 'td'.
        data(x_global_entry) = m_tbl_global->*[  c_name =  m_table_name ].
        add_onclick_4_row( iv_node = x_next iv_json = x_global_entry-c_eezz_json  iv_idx = iv_index ).
      elseif x_type = if_ixml_node=>co_node_text and x_next->get_value( ) cp '*{*}'.
        split x_next->get_value( ) at '{' into data(x_label) data(x_fieldname).
        data(x_length) = strlen( x_fieldname ) - 1.
        x_string = x_fieldname+0(x_length).
        translate x_string to upper case.
        x_next->set_value( x_label && iv_row->*[ c_field_name = x_string ]-c_value ).
      endif.
    enddo.
    iv_parent->append_child( x_td_node ).
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TABLE_NODE->GET
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EV_TABLE_NODE                  TYPE REF TO IF_IXML_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get.
    ev_table_node = m_table_node.
  endmethod.
ENDCLASS.