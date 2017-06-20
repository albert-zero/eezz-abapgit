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
class ZCL_EEZZ_TABLE_NODE definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_NODE type ref to IF_IXML_NODE
      !IT_GLOBALS type ref to ZTTY_SYMBOLS
      !IO_EEZZ_TBL type ref to ZIF_EEZZ_TABLE .
  methods GET
    returning
      value(EV_TABLE_NODE) type ref to IF_IXML_NODE .
  methods CREATE_BODY
    importing
      !IV_TABLE type ref to IF_IXML_NODE
      !IV_NODE type ref to IF_IXML_NODE
      !IV_PARENT type ref to IF_IXML_NODE
    returning
      value(RT_TABLE) type ref to ZTTY_EEZZ_ROW .
  protected section.
private section.

  data MT_RANGE type ref to ZTTY_EEZZ_ROW .
  data M_DICTIONARY type ref to ZTTY_DICTIONARY .
  data M_TABLE_NAME type STRING .
  data M_TBL_EEZZ type ref to ZIF_EEZZ_TABLE .
  data M_TBL_GLOBAL type ref to ZTTY_SYMBOLS .
  data M_TABLE_NODE type ref to IF_IXML_NODE .

  methods CREATE_COLUMN_FILTER
    importing
      !IV_FILTER type STRING
      !IT_COLUMNS type ref to ZTTY_EEZZ_ROW .
  methods ADD_ONCLICK_4_HEADER
    importing
      !IV_PARENT type ref to IF_IXML_NODE
      !IV_NODE type ref to IF_IXML_NODE
      !IV_IDX type INT4 .
  methods ADD_ONCLICK_4_ROW
    importing
      !IV_NODE type ref to IF_IXML_NODE
      !IV_JSON type ref to ZCL_EEZZ_JSON
      !IV_IDX type INT4
      !IV_PATH type STRING optional .
  methods ADD_ONCLICK_EVENT
    importing
      !IV_NODE type ref to IF_IXML_NODE .
  methods CREATE_FOOTER
    importing
      !IV_NODE type ref to IF_IXML_NODE
      !IV_PARENT type ref to IF_IXML_NODE .
  methods CREATE_HEADER
    importing
      !IV_NODE type ref to IF_IXML_NODE
      !IV_PARENT type ref to IF_IXML_NODE .
  methods CREATE_ROW
    importing
      !IV_NODE type ref to IF_IXML_NODE
      !IV_PARENT type ref to IF_IXML_NODE
      !IV_ROW type ref to ZTTY_EEZZ_ROW
    returning
      value(RT_REPLACEMENT) type ref to ZTTY_EEZZ_ROW .
  methods CREATE_TILE
    importing
      !IV_NODE type ref to IF_IXML_NODE
      !IV_PARENT type ref to IF_IXML_NODE
      !IV_ROW type ref to ZTTY_EEZZ_ROW
      !IV_INDEX type INT4 .
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
* | [--->] IV_PATH                        TYPE        STRING(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method add_onclick_4_row.
    data x_update type string value 'innerHTML'.
    data x_path   type string.

    if iv_json is not bound.
      return.
    endif.

    data(x_class) = cast if_ixml_element( iv_node )->get_attribute_ns( |class| ).

    " Evaluate the attribute "innerHTML" for value "this" in update requests:
    if strlen( iv_path ) > 0.
      x_path   = |{ m_table_name }:{ iv_path }|.
      cast if_ixml_element( iv_node )->set_attribute_ns( name = 'id' value  = x_path ).
      x_update = cl_http_utility=>escape_url( |innerHTML.{ x_path }| ).
    endif.

    modify table m_dictionary->* from value #( c_key = 'innerHTML' c_value = x_update ).

    data(x_element_name) = cast if_ixml_element( iv_node )->get_attribute_ns( 'name' ).
    if strlen( x_element_name ) = 0.
      x_element_name = m_table_name.
    endif.

    cast if_ixml_element( iv_node )->set_attribute_ns( name = 'onclick' value  = 'easyClick(event,this)' ).
    data(x_event_str) = iv_json->gen_row_event( iv_name = x_element_name iv_index = iv_idx iv_dictionary = m_dictionary ).
    cast if_ixml_element( iv_node )->set_attribute_ns( name = 'data-eezz-event'  value = x_event_str ).

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
* | [--->] IT_GLOBALS                     TYPE REF TO ZTTY_SYMBOLS
* | [--->] IO_EEZZ_TBL                    TYPE REF TO ZIF_EEZZ_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method constructor.
    data x_refnode type ref to if_ixml_node.

    " clone 0 will only copy node, no childs
    m_table_node     = io_node->clone( 0 ).
    m_tbl_global     = it_globals.
    m_tbl_eezz       = io_eezz_tbl.
    m_dictionary     = io_eezz_tbl->get_dictionary( ).
    m_table_name     = cast if_ixml_element( m_table_node )->get_attribute( 'name' ).

    data(x_processor) = new cl_xslt_processor( ).
    x_processor->set_source_node( io_node ).
    x_processor->set_expression( |node()| ).
    x_processor->run( progname = space ).
    data(x_nodelist) = x_processor->get_nodes( ).

    data(x_iterator) = x_nodelist->create_iterator( ).
    data(x_next)     = x_iterator->get_next( ).
    data x_name type string.

    while x_next is bound.
      x_name = x_next->get_name( ).
      if x_name eq 'tbody'.
        x_refnode = x_next->clone( 0 ).
        m_table_node->append_child( x_refnode ).
        me->create_body( iv_table = io_node iv_parent = x_refnode iv_node = x_next  ).
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
* | [--->] IV_TABLE                       TYPE REF TO IF_IXML_NODE
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

    data x_genkey      type string.
    data x_path        type string.
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
    "---- data(x_iterator) = iv_node->get_children( )->create_iterator( ).
    data(x_processor) = new cl_xslt_processor( ).
    x_processor->set_source_node( iv_node ).
    x_processor->set_expression( |node()| ).
    x_processor->run( progname = space ).

    data(x_nodelist) = x_processor->get_nodes( ).
    data(x_iterator) = x_nodelist->create_iterator( ).

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
        clear x_path.

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
          x_genkey        = x_table_row->*[ x_num_column + 1 ]-c_genkey.

          if cast if_ixml_element( iv_table )->get_attribute_ns( |class| ) cs |eezzTree|.
            x_path = x_table_row->*[ x_num_column + 1 ]-c_value.
            modify table m_dictionary->* from value #( c_key = 'table_path' c_value = x_path ).
          endif.

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
              data(x_row_update) = cast if_ixml_element( x_tr_new_node )->get_attribute_ns( 'data-eezz-event' ).
              if strlen( x_row_update ) > 0.
                <fs_cw_templ>-c_json = new zcl_eezz_json( x_row_update ).
              else.
                data(x_global_entry) = m_tbl_global->*[  c_name =  m_table_name ].
                <fs_cw_templ>-c_json = cast #( x_global_entry-c_eezz_json ).
              endif.

              data(x_tr_attr)        = cast if_ixml_element( x_tr_new_node )->get_attribute_ns( 'data-eezz-attributes' ).
              data(x_tr_json_attr)   = new zcl_eezz_json( x_tr_attr )->get( ).

              loop at x_tr_json_attr->* into data(xwa_attr).
                cast if_ixml_element( x_tr_new_node )->set_attribute_ns( name = xwa_attr-c_key  value = || ).
                data(x_tr_node) = cast if_ixml_element( x_tr_new_node )->get_attribute_node_ns( xwa_attr-c_key ).
                insert value #( c_node = x_tr_node c_filter = xwa_attr-c_value c_value = xwa_attr-c_key ) into table <fs_cw_templ>-c_repl->*.
              endloop.
            catch cx_root.
          endtry.
        endif.

        " update td values
        loop at <fs_cw_templ>-c_repl->* into data(x_wa_repl).
          data(x_attr_val) = x_wa_repl-c_value.
          data x_attr_rep  type string.

          if line_exists( x_table_row->*[ c_field_name = x_wa_repl-c_filter ] ).
            replace '{}' in x_attr_val with x_table_row->*[ c_field_name = x_wa_repl-c_filter ]-c_value.
            x_wa_repl-c_node->set_value( x_attr_val ).
          elseif line_exists( m_dictionary->*[ c_key = x_wa_repl-c_filter ] ).
            data(x_dict_row) = m_dictionary->*[ c_key = x_wa_repl-c_filter ].
            data(x_dict_val) = x_dict_row-c_value.

            " generate a unique key using the row attribute by dictionary plus the column name:
            if x_wa_repl-c_filter eq 'eezz-genkey'.
              x_dict_val = |{ x_dict_val }-{ x_wa_repl-c_field_name }|.
            endif.

            replace |\{{ x_wa_repl-c_filter }\}| in x_attr_val with x_dict_val.
            if sy-subrc = 0.
              x_wa_repl-c_node->set_value( x_attr_val ).
            else.
              x_wa_repl-c_node->set_value( x_dict_val ).
            endif.

          endif.
        endloop.

        add_onclick_4_row( iv_node = <fs_cw_templ>-c_node iv_json = <fs_cw_templ>-c_json iv_idx = x_idx iv_path = x_path ).
        data(x_template_clone) = <fs_cw_templ>-c_node->clone( ).

        data(xxxdbg) = cast if_ixml_element( x_template_clone )->get_attribute_ns( |class| ).

        iv_parent->append_child( x_template_clone ).
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
      read table x_indexes with key table_line = x_column-c_position transporting no fields.
      if sy-subrc <> 0.
        delete it_columns->* where c_position = x_column-c_position.
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

    data(x_node_footer) = x_next->clone( ).
    data(x_filter)     = x_node_footer->create_filter_attribute_ns( name = 'data-eezz-template' ).
    x_iterator = x_node_footer->create_iterator_filtered( x_filter ).
    do 100 times.
      x_next = x_iterator->get_next( ).
      if x_next is not bound.
        exit.
      endif.

      data(x_template)   = cast if_ixml_element( x_next )->get_attribute_ns( name = 'data-eezz-template' ).
      data(x_json)       = new zcl_eezz_json( x_template ).
      data(x_json_tbl)   = x_json->get( 'display' ).

      if x_json_tbl is bound.
        data(x_display)  = x_json_tbl->*[ 1 ].
        if line_exists(
            m_dictionary->*[ c_key = x_display-c_key ] ) and
            m_dictionary->*[ c_key = x_display-c_key ]-c_value <> x_display-c_value.
          x_next->remove_node( ).
        endif.
      endif.

    enddo.
    if x_node_footer is bound.
      iv_parent->append_child( x_node_footer ).
    endif.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_EEZZ_TABLE_NODE->CREATE_HEADER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE                        TYPE REF TO IF_IXML_NODE
* | [--->] IV_PARENT                      TYPE REF TO IF_IXML_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_header.

    TYPES:
      BEGIN OF ttemplates,
        c_key   TYPE string,
        c_value TYPE string,
        c_node  TYPE REF TO if_ixml_node,
      END OF ttemplates.

    DATA x_cwa_templ  TYPE ttemplates.
    DATA x_tbl_templ  TYPE TABLE OF ttemplates.
    DATA x_eezz_json  TYPE REF TO zcl_eezz_json.
    DATA x_table_row  TYPE REF TO ztty_eezz_row.
    DATA: x_column_idx TYPE i,
          x_filter_flg TYPE boolean,
          x_field_name TYPE string,
          x_outputlen  TYPE string.

    DATA(x_processor) = NEW cl_xslt_processor( ).
    x_processor->set_source_node( iv_node ).
    x_processor->set_expression( |node()| ).
    x_processor->run( progname = space ).

    DATA(x_nodelist) = x_processor->get_nodes( ).
    DATA(x_iterator) = x_nodelist->create_iterator( ).

    DATA(x_name)  = iv_node->get_name( ).

    IF x_name EQ 'thead'.
*      IF sy-uname EQ 'SELLCH'.
      x_filter_flg = abap_false.
*      ENDIF.
    ENDIF.

    DO 10 TIMES.
      DATA(x_next) = x_iterator->get_next( ).
      IF x_next IS NOT BOUND.
        EXIT.
      ENDIF.

      x_name = x_next->get_name( ).
      IF x_name CN 'tr'.
        iv_parent->append_child( x_next ).
        CONTINUE.
      ENDIF.

      DATA(x_attributes) = x_next->get_attributes( ).
      IF x_attributes IS NOT BOUND.
        iv_parent->append_child( x_next ).
        CONTINUE.
      ENDIF.

      DATA(x_eezz_attr) = x_attributes->get_named_item_ns( 'data-eezz-template' ).
      IF x_eezz_attr IS BOUND.

        x_eezz_json = NEW zcl_eezz_json( x_eezz_attr->get_value( ) ).

        DATA(x_tbl_row) = x_eezz_json->get_value( 'table-rows' ).
        DATA(x_columns) = m_tbl_eezz->get_column_names( ).
***********************************************************
*   Create Header Row
***********************************************************
        " get <tr> tag template
        DATA(x_tr) = x_next->clone( 0 ).
        iv_parent->append_child( x_tr ).

        DATA(x_node_list) = x_next->get_children( ).
        DATA(x_node_list_it) = x_node_list->create_iterator( ).
        DATA(x_th_node) = x_node_list_it->get_next( ).
        WHILE x_th_node IS BOUND.
          DATA(x_th) = x_th_node->clone( 0 ).
          DATA(x_th_attributes) = x_th->get_attributes( ).
          DATA(x_th_eezz_attr)  = x_th_attributes->get_named_item_ns( 'data-eezz-template' ).
          IF x_th_eezz_attr IS INITIAL.
            x_tr->append_child( x_th_node->clone( ) ).
          ELSE.
            DATA(x_th_eezz_json)  = NEW zcl_eezz_json( x_th_eezz_attr->get_value( ) ).
            mt_range              = x_th_eezz_json->get_range( x_columns ).
            "DATA(x_tbl_column) = x_th_eezz_json->get( 'table-columns' ).
            "create_column_filter( iv_filter = x_tbl_column->*[ 1 ]-c_value it_columns = x_columns ).

            LOOP AT mt_range->* INTO DATA(x_wac_column).
              x_column_idx = sy-tabix.
              " get <th> tag template
              x_th = x_next->get_first_child( )->clone( 0 ).

              x_th->set_value( x_wac_column-c_value ).
              " add on_sort event
              add_onclick_4_header( iv_parent = iv_parent iv_node = x_th iv_idx = x_wac_column-c_position ). "x_column_idx ).

              DATA(x_name_) = x_th->get_name( ).
              x_tr->append_child( x_th ).
            ENDLOOP.
          ENDIF.
          x_th_node = x_node_list_it->get_next( ).
        ENDWHILE.
***********************************************************
*   Create Filter Row
***********************************************************
        IF x_filter_flg EQ abap_true.
          DATA x_xml_cl TYPE REF TO if_ixml.
          x_xml_cl         = cl_ixml=>create( ).
          DATA(x_document) = x_xml_cl->create_document( ).
          x_tr = x_document->create_element_ns( |tr| ).

          " x_tr = x_next->clone( 0 ).
          CONCATENATE m_table_name '.filter' INTO DATA(x_filter_name).
          CAST if_ixml_element( x_tr )->set_attribute_ns( name = 'name'  value = x_filter_name ).
          CAST if_ixml_element( x_tr )->set_attribute_ns( name = 'class' value = 'eezzFilter'  ).

          iv_parent->append_child( x_tr ).
          LOOP AT mt_range->* INTO x_wac_column.
            x_column_idx = sy-tabix.
            x_th = x_next->get_first_child( )->clone( 0 ).
            DATA(x_input) = x_th->clone( 0 ).
            x_input->set_name( 'input' ).
            x_field_name = x_wac_column-c_field_name.
            x_outputlen = x_wac_column-c_outputlen.
            CAST if_ixml_element( x_input )->set_attribute_ns( name = 'data-eezz-column' value = x_field_name ).
            CAST if_ixml_element( x_input )->set_attribute_ns( name = 'size' value = x_outputlen ).
            x_th->append_child( new_child = x_input ).
            x_tr->append_child( x_th ).
          ENDLOOP.
        ENDIF.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.


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
    "----data(x_iterator)   = iv_node->create_iterator( 0 ).
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
    data x_json        type ref to zcl_eezz_json.

    "x_replacement = create  ztty_eezz_row.
    create data x_replacement.
    data(x_processor) = new cl_xslt_processor( ).
    x_processor->set_source_node( iv_node ).
    x_processor->set_expression( |node()| ).
    x_processor->run( progname = space ).

    data(x_nodelist) = x_processor->get_nodes( ).
    data(x_iterator) = x_nodelist->create_iterator( ).

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
      if x_template is not bound.
        continue.
      endif.

      if mt_range is initial.
        data(x_columns) =  m_tbl_eezz->get_column_names( ).
        mt_range = x_cl_json->get_range( x_columns ).
      endif.

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

        " set the name of td
        "----data(x_field_name) = x_wa_range-c_field_name.
        "----data(x_genkey) = iv_row->*[ c_field_name = x_field_name ]-c_genkey.
        "----concatenate 'eezz-update-' m_table_name '-' x_field_name '-' x_genkey into data(x_td_name).
        "----translate x_td_name to lower case.
        "----cast if_ixml_element( x_reference )->set_attribute( name = 'name' value  = x_td_name ).

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

          if x_dbg_type = if_ixml_node=>co_node_text.
            data(x_text_value)   = x_node_deep->get_value( ).

            if x_text_value cs '{}'.
              x_wa_repl-c_field_name = x_wac_column-c_field_name.
              x_wa_repl-c_filter     = x_wac_column-c_field_name.
              x_wa_repl-c_node       = x_node_deep.
              x_wa_repl-c_value      = x_node_deep->get_value( ).
              append x_wa_repl to x_replacement->*.
            endif.
            continue.
          endif.

          data(x_element)    = cast if_ixml_element( x_node_deep ).
          data(x_style_attr) = x_element->get_attribute_ns( 'data-eezz-attributes' ).
          if x_style_attr is initial.
            continue.
          endif.

          x_json = new zcl_eezz_json( x_style_attr ).
          data(x_style_attr_list) = x_json->get( ).

          loop at x_style_attr_list->* into data(xwa_attr).
            find all occurrences of regex '\{([-a-zA-Z0-9]*)\}' in xwa_attr-c_value results data(x_entry).
            if lines( x_entry ) <> 1.
              " In the moment only one entry per key allowed
              " Need to support json array in any other case x_style_attr_list-c_type = array.
              " then loop this array to replace all occurences of {}
              " The replacement structure will get a reference to x_entry in this case for processing create_body
              continue.
            endif.

            " Create and set the attribute to default:
            x_element->set_attribute_ns( name = xwa_attr-c_key value = xwa_attr-c_value ).
            data(wac_entry)        = x_entry[ 1 ].
            data(xx_match_reg)     = wac_entry.
            data(xx_match_key)     = wac_entry-submatches[ 1 ].

            x_wa_repl-c_node       = x_element->get_attribute_node_ns( xwa_attr-c_key ).
            x_wa_repl-c_value      = xwa_attr-c_value.
            x_wa_repl-c_field_name = x_wac_column-c_field_name.

            if xx_match_key-length = 0.
              x_wa_repl-c_filter   = x_wac_column-c_field_name.
            else.
              x_wa_repl-c_filter   = xwa_attr-c_value+xx_match_key-offset(xx_match_key-length).
            endif.
            append x_wa_repl to x_replacement->*.
          endloop.
        enddo.
      endloop.
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

    data x_string type string.
    data x_json   type ref to zcl_eezz_json.

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
        x_json ?= x_global_entry-c_eezz_json.
        add_onclick_4_row( iv_node = x_next iv_json = x_json  iv_idx = iv_index ).
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