class ZCL_EEZZ_TABLE_NODE definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_TABLE_NAME type STRING optional
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

    data mt_range type ref to ztty_eezz_row .
    data m_dictionary type ref to ztty_dictionary .
    data m_table_name type string .
    data m_tbl_eezz type ref to zif_eezz_table .
    data m_tbl_global type ref to ztty_symbols .
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
        !iv_idx  type int4
        !iv_path type string optional .
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


  method add_onclick_4_header.
    " Add onClick event
    cast if_ixml_element( iv_node )->set_attribute( name = 'onclick' value  = 'easyClick(event,this)' ).
    data(x_event_str) = zcl_eezz_json=>gen_header_event( iv_name = m_table_name iv_index = iv_idx ).
    cast if_ixml_element( iv_node )->set_attribute( name = 'data-eezz-event' value = x_event_str ).
  endmethod.


  method add_onclick_4_row.
    data x_update type string value 'innerHTML'.
    data x_path   type string.

    if iv_json is not bound.
      return.
    endif.

    data(x_class) = cast if_ixml_element( iv_node )->get_attribute_ns( |class| ).
    modify table m_dictionary->* from value #( c_key = 'innerHTML' c_value = x_update ).

    data(x_this_name)    = cast if_ixml_element( iv_node )->get_attribute_ns( 'name' ).
    data(x_element_name) = m_table_name.

    cast if_ixml_element( iv_node )->set_attribute_ns( name = 'onclick' value  = 'easyClick(event,this)' ).
    data(x_event_str) = iv_json->gen_row_event( iv_name = x_element_name iv_this = x_this_name iv_index = iv_idx iv_dictionary = m_dictionary ).
    cast if_ixml_element( iv_node )->set_attribute_ns( name = 'data-eezz-event'  value = x_event_str ).

  endmethod.


  method add_onclick_event.
    "cast if_ixml_element( iv_node )->set_attribute( name = 'onclick' value  = 'easyClick(event,this)' ).
  endmethod.


  method constructor.
    data x_refnode type ref to if_ixml_node.

    " clone 0 will only copy node, no childs
    m_table_node      = io_node->clone( 0 ).
    m_tbl_global      = it_globals.
    m_tbl_eezz        = io_eezz_tbl.
    m_dictionary      = io_eezz_tbl->get_dictionary( ).
    m_table_name      = cast if_ixml_element( m_table_node )->get_attribute( 'name' ).
    data(x_path)      = cast if_ixml_element( m_table_node )->get_attribute( 'data-eezz-path' ).
    data(x_class)     = cast if_ixml_element( m_table_node )->get_attribute( 'class' ).

    if iv_table_name is not initial.
      m_table_name   = iv_table_name.
    endif.

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
        if x_path is not initial and x_class cs |eezzTreeTemplate|.
          cast if_ixml_element( x_refnode )->set_attribute_ns( name = |class|   value = |eezzTreeNode| ).
          "-----> cast if_ixml_element( x_refnode )->set_attribute_ns( name = |data-eezz-path| value = x_path  ).

          if iv_table_name is not initial.
            cast if_ixml_element( x_refnode )->set_attribute_ns( name = |name | value = m_table_name  ).
          endif.
        endif.
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
        data(x_value) = x_next->get_value( ).
        find all occurrences of regex '\{([-_a-zA-Z0-9]+)\}' in x_value results data(x_entry).
        if lines( x_entry ) = 1.
          data(xx_match_entry) = x_entry[ 1 ].
          data(xx_match_reg)   = xx_match_entry.
          data(xx_match_key)   = xx_match_entry-submatches[ 1 ].
          if line_exists( m_dictionary->*[ c_key = x_value+xx_match_key-offset(xx_match_key-length) ] ).
            data(xx_match_dict) = m_dictionary->*[ c_key = x_value+xx_match_key-offset(xx_match_key-length) ]-c_value.
            x_next->set_value( |{ x_value+0(xx_match_reg-offset) }{ xx_match_dict }| ).
          endif.
        endif.
        m_table_node->append_child( x_next->clone( ) ).
      endif.
      x_next = x_iterator->get_next( ).
    endwhile.

  endmethod.


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
    data x_constpath   type string.
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

    do 1000 times.
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
      x_constpath       = |{ m_dictionary->*[ c_key = |tree_path| ]-c_value }/{ m_dictionary->*[ c_key = |table_current| ]-c_value }|.
      data(x_eezz_attr) = x_attributes->get_named_item_ns( 'data-eezz-template' ).

      if x_eezz_attr is not bound.
        data(x_class_attr) = cast if_ixml_element( x_next )->get_attribute_ns( 'class' ).
        if x_class_attr cs |eezzTreeNode|.
          cast if_ixml_element( x_next )->set_attribute_ns( name = |data-eezz-path| value = x_constpath ).
        endif.
        iv_parent->append_child( x_next ).
        continue.
      endif.

      " Store elements with display template in local template table
      x_eezz_json = new zcl_eezz_json( iv_json = x_eezz_attr->get_value( ) ).

      " Check the dictionary to display special nodes:
      data(x_jsonobj) = x_eezz_json->get( iv_path = 'display' ).
      if x_jsonobj is bound.
        data(x_jsonrow) = x_jsonobj->*[ 1 ].

        if x_jsonrow-c_key co |type|.
          append value #( c_key = |type| c_value = x_jsonrow-c_value c_node = x_next ) to x_tbl_templ.
        elseif line_exists( m_dictionary->*[ c_key = x_jsonrow-c_key c_value = x_jsonrow-c_value ] ).
          append value #( c_key = x_jsonrow-c_key c_value = x_jsonrow-c_value c_node = x_next ) to x_tbl_templ.
        endif.

        continue.
      endif.

      " Evaluate and add table-row elements
      if x_eezz_json->get( iv_path = 'table-rows' ) is not bound.
        continue.
      endif.

      data(x_columns)     = m_tbl_eezz->get_column_names( ).
      x_num_column        = lines( x_columns->* ).

      x_cwa_templ-c_key   = 'default'.
      x_cwa_templ-c_node  = x_next->clone( ).
      append x_cwa_templ to x_tbl_templ.

      data(x_row_range)   = x_eezz_json->get( iv_path = 'table-rows' ).
      if x_row_range->*[ 1 ]-c_key cs |tiles|.
        clear x_reference.
        x_idx             = 0.
        data(x_num_tiles) = x_row_range->*[ 1 ]-c_value.
        data(x_items)     = x_num_tiles.

        if line_exists( m_dictionary->*[ c_key = |table_items| ] ).
          x_items = m_dictionary->*[ c_key = |table_items| ]-c_value.
        endif.

        do x_items times.
          if x_idx mod x_num_tiles = 0.
            x_reference = x_next->clone( 0 ).
            iv_parent->append_child( x_reference ).
          endif.

          add 1 to x_idx.
          x_table_row = m_tbl_eezz->get_row( x_idx ).

          if x_table_row is not bound.
            exit.
          endif.
          me->create_tile( iv_parent = x_reference iv_node = x_next  iv_row = x_table_row iv_index = x_idx ).
        enddo.

        continue.
      endif.


      " Generate table elements and continue
      do 1000 times.
        clear x_reference.
        clear x_cwa_templ.
        clear x_path.

        if <fs_cw_templ> is assigned.
          unassign <fs_cw_templ>.
        endif.

        x_idx       = sy-index.
        x_table_row = m_tbl_eezz->get_row( x_idx ).
        if x_table_row is not bound.
          exit.
        endif.

        " Check if the type is specified and select a template
        if line_exists( x_table_row->*[ c_field_name = '_eezz_row_cell_' ] ).
          data(x_cell) = x_table_row->*[ c_field_name = '_eezz_row_cell_' ].
          if line_exists( x_tbl_templ[ c_key = |type| c_value = x_cell-c_value ] ).
            assign x_tbl_templ[ c_key = |type| c_value = x_cell-c_value ] to <fs_cw_templ>.
          endif.
          x_path = x_cell-c_value.
        endif.

        " Assign the default if possible, else continue
        if <fs_cw_templ> is not assigned.
          if line_exists( x_tbl_templ[ c_key = 'default' ] ).
            assign x_tbl_templ[ c_key = 'default' ] to <fs_cw_templ>.
          else.
            continue.
          endif.
        endif.

        if 1 = 1 or <fs_cw_templ>-c_repl is not bound.
          data(x_tr_new_node)  = <fs_cw_templ>-c_node->clone( 0 ).
          data(x_tr_sub_node)  = <fs_cw_templ>-c_node->clone( ).
          <fs_cw_templ>-c_repl = me->create_row( iv_parent = x_tr_new_node iv_node = x_tr_sub_node iv_row = x_table_row ).
          " <fs_cw_templ>-c_node = x_tr_new_node.

          try.
              if x_path is not initial.
                "-----> cast if_ixml_element( x_tr_new_node )->set_attribute_ns( name = |data-eezz-path| value = x_path ).
              endif.

              data(x_row_update) = cast if_ixml_element( x_tr_new_node )->get_attribute_ns( 'data-eezz-event' ).
              if strlen( x_row_update ) > 0.
                <fs_cw_templ>-c_json = new zcl_eezz_json( iv_json = x_row_update ).
              else.
                data(x_global_entry) = m_tbl_global->*[  c_name =  m_table_name ].
                <fs_cw_templ>-c_json = cast #( x_global_entry-c_eezz_json ).
              endif.

              data(x_tr_attr)        = cast if_ixml_element( x_tr_new_node )->get_attribute_ns( 'data-eezz-attributes' ).

              if strlen( x_tr_attr ) > 0.
                data(x_tr_json_attr)   = new zcl_eezz_json( iv_json = x_tr_attr )->get( ).

                loop at x_tr_json_attr->* into data(xwa_attr).
                  cast if_ixml_element( x_tr_new_node )->set_attribute_ns( name = xwa_attr-c_key  value = || ).
                  data(x_tr_node) = cast if_ixml_element( x_tr_new_node )->get_attribute_node_ns( xwa_attr-c_key ).
                  insert value #( c_node = x_tr_node c_filter = xwa_attr-c_value c_value = xwa_attr-c_key ) into table <fs_cw_templ>-c_repl->*.
                endloop.
              endif.
            catch cx_root.
          endtry.
        endif.

        " update td values
        loop at <fs_cw_templ>-c_repl->* into data(x_wa_repl).
          data(x_attr_val) = x_wa_repl-c_value.
          data x_attr_rep   type string.
          data x_field_name type string.

          x_field_name = x_wa_repl-c_field_name.

          if line_exists( x_table_row->*[ c_field_name = x_field_name ] ).
            data(x_row_cell) = x_table_row->*[ c_field_name = x_wa_repl-c_field_name ].
            replace '{}' in x_attr_val with x_row_cell-c_value.
            x_wa_repl-c_node->set_value( x_attr_val ).
            cast if_ixml_element( x_wa_repl-c_reference )->set_attribute_ns( name = 'name' value = x_field_name ).
          elseif line_exists( m_dictionary->*[ c_key = x_wa_repl-c_field_name ] ).
            data(x_dict_row) = m_dictionary->*[ c_key = x_wa_repl-c_field_name ].
            data(x_dict_val) = x_dict_row-c_value.
            x_wa_repl-c_node->set_value( x_dict_val ).
            cast if_ixml_element( x_wa_repl-c_reference )->set_attribute_ns( name = 'name' value = x_field_name ).
          endif.
        endloop.

        if line_exists( x_table_row->*[ c_field_name = '_eezz_row_cell_' ] ).
          x_row_cell = x_table_row->*[ c_field_name = '_eezz_row_cell_' ].
          " replaced c_node by x_tr_new_node
          cast if_ixml_element( x_tr_new_node )->set_attribute_ns( name = 'name' value = x_row_cell-c_genkey ).

          data(x_treepath) = |{ m_dictionary->*[ c_key = |tree_path| ]-c_value }/{ x_row_cell-c_genkey }|.
          cast if_ixml_element( x_tr_new_node )->set_attribute_ns( name = |data-eezz-path| value = x_treepath ).
        endif.

        add_onclick_4_row( iv_node = x_tr_new_node iv_json = <fs_cw_templ>-c_json iv_idx = x_idx iv_path = x_path ).
        data(x_template_clone)     = x_tr_new_node->clone( ).

        iv_parent->append_child( x_template_clone ).
      enddo.
    enddo.

  endmethod.


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
      data(x_json)       = new zcl_eezz_json( iv_json = x_template ).
      data(x_json_tbl)   = x_json->get( iv_path = 'display' ).

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


  method create_header.

    types:
      begin of ttemplates,
        c_key   type string,
        c_value type string,
        c_node  type ref to if_ixml_node,
      end of ttemplates.

    data x_cwa_templ  type ttemplates.
    data x_tbl_templ  type table of ttemplates.
    data x_eezz_json  type ref to zcl_eezz_json.
    data x_table_row  type ref to ztty_eezz_row.
    data: x_column_idx type i,
          x_filter_flg type boolean,
          x_field_name type string,
          x_outputlen  type string.

    data(x_processor) = new cl_xslt_processor( ).
    x_processor->set_source_node( iv_node ).
    x_processor->set_expression( |node()| ).
    x_processor->run( progname = space ).

    data(x_nodelist) = x_processor->get_nodes( ).
    data(x_iterator) = x_nodelist->create_iterator( ).

    data(x_name)  = iv_node->get_name( ).

    if x_name eq 'thead'.
*      IF sy-uname EQ 'SELLCH'.
      x_filter_flg = abap_false.
*      ENDIF.
    endif.

    do 10 times.
      data(x_next) = x_iterator->get_next( ).
      if x_next is not bound.
        exit.
      endif.

      x_name = x_next->get_name( ).
      if x_name cn 'tr'.
        " append all elements within tr
        iv_parent->append_child( x_next ).
        continue.
      endif.

      data(x_attributes) = x_next->get_attributes( ).
      if x_attributes is not bound.
        iv_parent->append_child( x_next ).
        continue.
      endif.

      data(x_eezz_attr) = x_attributes->get_named_item_ns( 'data-eezz-template' ).
      if x_eezz_attr is not bound and x_name cs 'tr'.
        " append all elements which are not template
        iv_parent->append_child( x_next ).
        continue.
      endif.


      x_eezz_json = new zcl_eezz_json( iv_json = x_eezz_attr->get_value( ) ).

      data(x_tbl_row) = x_eezz_json->get_value( 'table-rows' ).
      data(x_columns) = m_tbl_eezz->get_column_names( ).
***********************************************************
*   Create Header Row
***********************************************************
      " get <tr> tag template
      data(x_tr) = x_next->clone( 0 ).
      iv_parent->append_child( x_tr ).

      data(x_node_list) = x_next->get_children( ).
      data(x_node_list_it) = x_node_list->create_iterator( ).
      data(x_th_node) = x_node_list_it->get_next( ).
      while x_th_node is bound.
        data(x_th) = x_th_node->clone( 0 ).
        data(x_th_attributes) = x_th->get_attributes( ).
        data(x_th_eezz_attr)  = x_th_attributes->get_named_item_ns( 'data-eezz-template' ).
        if x_th_eezz_attr is initial.
          x_tr->append_child( x_th_node->clone( ) ).
        else.
          data(x_th_eezz_json)  = new zcl_eezz_json( iv_json = x_th_eezz_attr->get_value( ) ).
          mt_range              = x_th_eezz_json->get_range( x_columns ).
          "DATA(x_tbl_column) = x_th_eezz_json->get( 'table-columns' ).
          "create_column_filter( iv_filter = x_tbl_column->*[ 1 ]-c_value it_columns = x_columns ).

          loop at mt_range->* into data(x_wac_column).
            x_column_idx = sy-tabix.
            " get <th> tag template
            x_th = x_next->get_first_child( )->clone( 0 ).

            x_th->set_value( x_wac_column-c_value ).
            " add on_sort event
            add_onclick_4_header( iv_parent = iv_parent iv_node = x_th iv_idx = x_wac_column-c_position ). "x_column_idx ).

            data(x_name_) = x_th->get_name( ).
            x_tr->append_child( x_th ).
          endloop.
        endif.
        x_th_node = x_node_list_it->get_next( ).
      endwhile.
***********************************************************
*   Create Filter Row
***********************************************************
      if x_filter_flg eq abap_true.
        data x_xml_cl type ref to if_ixml.
        x_xml_cl         = cl_ixml=>create( ).
        data(x_document) = x_xml_cl->create_document( ).
        x_tr = x_document->create_element_ns( |tr| ).

        " x_tr = x_next->clone( 0 ).
        concatenate m_table_name '.filter' into data(x_filter_name).
        cast if_ixml_element( x_tr )->set_attribute_ns( name = 'name'  value = x_filter_name ).
        cast if_ixml_element( x_tr )->set_attribute_ns( name = 'class' value = 'eezzFilter'  ).

        iv_parent->append_child( x_tr ).
        loop at mt_range->* into x_wac_column.
          x_column_idx = sy-tabix.
          x_th = x_next->get_first_child( )->clone( 0 ).
          data(x_input) = x_th->clone( 0 ).
          x_input->set_name( 'input' ).
          x_field_name = x_wac_column-c_field_name.
          x_outputlen = x_wac_column-c_outputlen.
          cast if_ixml_element( x_input )->set_attribute_ns( name = 'data-eezz-column' value = x_field_name ).
          cast if_ixml_element( x_input )->set_attribute_ns( name = 'size' value = x_outputlen ).
          x_th->append_child( new_child = x_input ).
          x_tr->append_child( x_th ).
        endloop.
      endif.
      exit.
    enddo.

  endmethod.


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
    field-symbols: <fs> type zstr_cell.

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

      " display elements related to the column type are collected for the table columns section
      data(x_cl_json)  = new zcl_eezz_json( iv_json = x_eezz_attr->get_value( ) ).
      data(x_tpl_type) = x_cl_json->get_value( iv_path = 'display' iv_key = 'type' ).
      if x_tpl_type is not initial.
        x_wac_templ-c_value = x_tpl_type.
        x_wac_templ-c_node  = x_next.

        append x_wac_templ to x_tbl_templ.
        continue.
      endif.

      " display elements related to row attributes are stable regarding their position
      " and are inserted, if the attributes matches the value exact
      data(x_template)  = x_cl_json->get( iv_path = 'display' ).
      if x_template is not initial and line_exists( iv_row->*[ c_field_name = |_eezz_row_cell_| ] ).
        data(x_jsonrow) = x_template->*[ 1 ].
        data(x_rowcell) = iv_row->*[ c_field_name = |_eezz_row_cell_| ].
        data(x_key)     = to_upper( x_jsonrow-c_key ).
        if m_dictionary is bound.
          if line_exists( m_dictionary->*[ c_key = x_jsonrow-c_key c_value = x_jsonrow-c_value ] ).
            iv_parent->append_child( x_next->clone( ) ).
          elseif line_exists( iv_row->*[ c_field_name = x_key c_value = x_jsonrow-c_value ] ).
            assign iv_row->*[ c_field_name = x_key c_value = x_jsonrow-c_value ] to <fs>.
            <fs>-c_type = x_key.
            append value #( c_value = x_key c_node = x_next ) to x_tbl_templ.
          elseif x_jsonrow-c_value cs '*'.
            assign iv_row->*[ c_field_name = x_key ] to <fs>.
            <fs>-c_type = x_key.
            append value #( c_value = x_key c_node = x_next ) to x_tbl_templ.
          endif.
        endif.
        continue.
      endif.

      " display elements not in the template description are stable regarding their position
      x_template = x_cl_json->get( iv_path = 'table-columns' ).
      if x_template is initial.
        iv_parent->append_child( x_next->clone( ) ).
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
              x_wa_repl-c_filter     = x_wac_column-c_filter.
              x_wa_repl-c_node       = x_node_deep.
              x_wa_repl-c_reference  = x_reference.
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

          x_json = new zcl_eezz_json( iv_json = x_style_attr ).
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


  method create_tile.

    data x_string type string.
    data x_json   type ref to zcl_eezz_json.

    data(x_tr_iterator) = iv_node->create_iterator( ).
    x_tr_iterator->get_next( ).

    data(x_td_node) = x_tr_iterator->get_next( )->clone( ). " Get clone of <TD>

    if line_exists( iv_row->*[ c_field_name = '_eezz_row_cell_' ] ).
      data(x_row_cell) = iv_row->*[ c_field_name = '_eezz_row_cell_' ].
      cast if_ixml_element( x_td_node )->set_attribute_ns( name = 'name' value = x_row_cell-c_genkey ).
    endif.

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


  method get.
    ev_table_node = m_table_node.
  endmethod.
ENDCLASS.
