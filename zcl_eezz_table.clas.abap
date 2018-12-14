class ZCL_EEZZ_TABLE definition
  public
  create public .

public section.

  interfaces ZIF_EEZZ_TABLE .

  aliases MT_COLUMN_NAMES
    for ZIF_EEZZ_TABLE~MT_COLUMN_NAMES .
  aliases MT_DICTIONARY
    for ZIF_EEZZ_TABLE~MT_DICTIONARY .
  aliases MT_ROW
    for ZIF_EEZZ_TABLE~MT_ROW .
  aliases MT_TABLE
    for ZIF_EEZZ_TABLE~MT_TABLE .
  aliases MT_UPDATE
    for ZIF_EEZZ_TABLE~MT_UPDATE .
  aliases M_SELECTED
    for ZIF_EEZZ_TABLE~M_SELECTED .
  aliases M_TABLE_DDIC
    for ZIF_EEZZ_TABLE~M_TABLE_DDIC .
  aliases M_TABLE_NAME
    for ZIF_EEZZ_TABLE~M_TABLE_NAME .
  aliases M_VISIBLE_BLOCK
    for ZIF_EEZZ_TABLE~M_VISIBLE_BLOCK .
  aliases M_VISIBLE_ITEMS
    for ZIF_EEZZ_TABLE~M_VISIBLE_ITEMS .
  aliases DO_NAVIGATE
    for ZIF_EEZZ_TABLE~DO_NAVIGATE .
  aliases DO_SELECT_DATABASE
    for ZIF_EEZZ_TABLE~DO_SELECT_DATABASE .
  aliases DO_SORT
    for ZIF_EEZZ_TABLE~DO_SORT .
  aliases GET_DICTIONARY
    for ZIF_EEZZ_TABLE~GET_DICTIONARY .
  aliases GET_HASH
    for ZIF_EEZZ_TABLE~GET_HASH .
  aliases GET_SELECTED_OBJ
    for ZIF_EEZZ_TABLE~GET_SELECTED_OBJ .
  aliases SEND_MESSAGE_PCP
    for ZIF_EEZZ_TABLE~SEND_MESSAGE_PCP .


  methods CONSTRUCTOR
    importing
      !IV_TABLE type ref to DATA optional
      !TABLE_NAME type STRING optional .
protected section.

  data M_RESORT type INT4 .
  data MT_COMPONENTS type CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE .
private section.
    "az---data mt_sort type ztty_sort .
ENDCLASS.



CLASS ZCL_EEZZ_TABLE IMPLEMENTATION.


  method constructor.
    data:
      x_wa_column   type zstr_cell,
      x_typedescr   type ref to cl_abap_typedescr,
      x_structdescr type ref to cl_abap_structdescr,
      x_tabledescr  type ref to cl_abap_tabledescr,
      x_table_comps type cl_abap_structdescr=>component_table.

    zif_eezz_table~m_visible_items = 30.
    zif_eezz_table~m_visible_block = 30.
    zif_eezz_table~m_offset        =  0.
    zif_eezz_table~m_topdown       =  1.
    m_resort                       =  0.

    mt_dictionary = value ztty_dictionary(
      ( c_key = 'table_nav_type' c_value = 'block' )
      ( c_key = 'table_pos'      c_value =  '0' )
      ( c_key = 'table_top'      c_value =  '1' )
      ( c_key = 'table_last'     c_value = '-1' )
      ( c_key = 'table_next'     c_value =  '2' )
      ( c_key = 'table_prev'     c_value = '-2' )
      ( c_key = 'table_current'  c_value =  '' )
      ( c_key = 'table_path'     c_value =  '/' )
      ( c_key = 'table_items'    c_value =  '20' )
      ( c_key = 'table_block'    c_value =  '20' )
      ( c_key = 'table_size'     c_value =  '0' )
      ( c_key = 'table_key'      c_value =  '' )
      ( c_key = 'table_type'     c_value =  '' )
      ( c_key = 'table_name'     c_value =  '' )
      ( c_key = 'table_header'   c_value =  '' )
      ( c_key = 'tree_path'      c_value =  '' )
      ( c_key = 'tree_key'       c_value =  '' )
      ( c_key = 'destination'    c_value =  '' )
      ( c_key = 'file_loader'    c_value =  '' )
      ( c_key = 'innerHTML'      c_value = 'innerHTML')
   ).

    try.
        if iv_table is not initial.
          " work with existing table
          mt_table       = iv_table.
          x_tabledescr  ?= cl_abap_typedescr=>describe_by_data_ref( iv_table ).
          x_structdescr ?= x_tabledescr->get_table_line_type( ).
          mt_components  = x_structdescr->get_components( ).
        elseif table_name is not initial.
          " try to find DDIC table
          m_table_name   = table_name.
          m_table_ddic   = table_name.
          x_structdescr ?= cl_abap_typedescr=>describe_by_name( p_name = m_table_ddic ).
          mt_components  = x_structdescr->get_components( ).
          x_tabledescr   = cl_abap_tabledescr=>create( x_structdescr ).

          create data mt_table type handle x_tabledescr.
        endif.
      catch cx_root.
        return.
    endtry.

    data lt_table_fields type table of dfies.
    data lt_table_ddic   type ddobjname.

    if strlen( m_table_ddic ) > 0.
      lt_table_ddic = m_table_ddic.
      call function 'DDIF_FIELDINFO_GET' exporting tabname = lt_table_ddic tables dfies_tab = lt_table_fields.
    endif.

    field-symbols <fs_wac> type line of abap_component_tab.
    loop at mt_components assigning <fs_wac>.
      clear x_wa_column.
      if <fs_wac>-name cp |_eezz_row_cell_|.
        continue.
      endif.

      x_wa_column-c_field_name = <fs_wac>-name.
      x_wa_column-c_sort       = 0.
      x_wa_column-c_value      = <fs_wac>-name.
      x_wa_column-c_position   = sy-tabix.
      x_wa_column-c_type       = <fs_wac>-type->type_kind.

      if line_exists( lt_table_fields[ fieldname = <fs_wac>-name ] ).
        x_wa_column-c_value = lt_table_fields[ fieldname = <fs_wac>-name ]-fieldtext.
        x_wa_column-c_outputlen = lt_table_fields[ fieldname = <fs_wac>-name ]-outputlen.
        x_wa_column-c_keyflag   = lt_table_fields[ fieldname = <fs_wac>-name ]-keyflag.
      elseif line_exists( x_tabledescr->key[ name = <fs_wac>-name ] ).
        x_wa_column-c_keyflag   = abap_true.
      endif.

      append x_wa_column to mt_column_names.
    endloop.

  endmethod.


  method ZIF_EEZZ_TABLE~CREATE_NODE.
    data(x_table_element)  = iv_symbols->*[ c_name = m_table_name ].
    data(x_table_zif_node) = new zcl_eezz_table_node( io_node = x_table_element-c_templ_node it_globals = iv_symbols io_eezz_tbl = me ).
    io_table_node          = x_table_zif_node->get( ).
  endmethod.


  method zif_eezz_table~do_navigate.
    field-symbols <fs_table> type any table.

    data(x_where)    = where * zif_eezz_table~m_topdown.
    data(x_position) = zif_eezz_table~m_offset.

    if x_where = 0.
      " Position
      x_position = pos.
    elseif x_where = 1.
      " Top
      x_position = 0.
    elseif x_where = -1.
      " Bottom
      x_position = 0.
      zif_eezz_table~m_topdown = zif_eezz_table~m_topdown * -1.
    elseif x_where = 2.
      " Next
      x_position = x_position + zif_eezz_table~m_visible_block.
    elseif x_where = -2.
      " Previous
      x_position = x_position - zif_eezz_table~m_visible_block.
    endif.

    x_position = nmax( val1 = x_position val2 = 0 ).

    if m_table_ddic is initial.
      assign mt_table->* to <fs_table>.
      x_position = nmin( val1 = lines( <fs_table> ) val2 = x_position ).
      zif_eezz_table~m_offset = x_position.
    else.
      zif_eezz_table~m_offset = x_position.
      me->zif_eezz_table~do_select_database( iv_offset = zif_eezz_table~m_offset ).
    endif.

    modify table mt_dictionary from value #( c_key = 'table_pos' c_value = zif_eezz_table~m_offset ).
    m_resort = 1.
  endmethod.


method zif_eezz_table~do_select.
endmethod.


  method zif_eezz_table~do_select_database.

    data: x_filter_values type table of string,
          x_skip_limit    type c.

    if m_table_ddic is initial.
      return.
    endif.

    data(x_table_ddic) = m_table_ddic.
    if x_table_ddic+0(1) eq '/'.
      concatenate '"' x_table_ddic '"' into x_table_ddic.
    endif.

    data(x_limit)  = iv_limit.
    data(x_offset) = iv_offset.

    if iv_limit is initial.
      x_limit = zif_eezz_table~m_visible_block.
    endif.

    if iv_offset is initial.
      x_offset = zif_eezz_table~m_offset.
    endif.

    field-symbols <fs_table> type any table.
    assign mt_table->* to <fs_table>.

    data(x_sqlstm_stream) = new cl_abap_string_c_writer(  ).
    x_sqlstm_stream->write( |SELECT * FROM { x_table_ddic } WHERE MANDT = ?  | ).

    data: x_asc type string value 'ASC',
          x_des type string value 'DESC'.

    if zif_eezz_table~m_topdown < 0.
      x_asc = 'DESC'.
      x_des = 'ASC'.
    endif.

    data x_order_str type string value space.
    loop at mt_column_names into data(xwa_column).
      if xwa_column-c_filter is not initial.
        x_sqlstm_stream->write( | AND { xwa_column-c_field_name } = ? | ).
        append xwa_column-c_filter to x_filter_values.
      endif.
    endloop.

    data x_order_def type string.
    x_order_str = | ORDER BY |.

    loop at mt_column_names into xwa_column.
       x_order_def = |{ x_order_str } { xwa_column-c_field_name } { x_asc }|.
       if not xwa_column-c_field_name cs |MANDT|.
         exit.
       endif.
    endloop.

    loop at mt_column_names into xwa_column.
      if     xwa_column-c_sort =  1.
        x_sqlstm_stream->write( |{ x_order_str } { xwa_column-c_field_name } { x_asc }| ).
        x_order_str = ','.
        x_order_def = ''.
      elseif xwa_column-c_sort = -1.
        x_sqlstm_stream->write( |{ x_order_str } { xwa_column-c_field_name } { x_des }| ).
        x_order_str = ','.
        x_order_def = ''.
      endif.
    endloop.

    x_sqlstm_stream->write( x_order_def ).

    if x_skip_limit eq space.
      x_sqlstm_stream->write( | limit ? offset ? | ).
    endif.
    data(x_sqlstm) = x_sqlstm_stream->get_result_string( ).

    try.
        clear <fs_table>.
        data(lr_connection) = cl_sql_connection=>get_connection( 'DEFAULT' ).
        data(lr_statement)  = lr_connection->create_statement( ).
        data(l_client)      = sy-mandt.

        lr_statement->set_param( data_ref = ref #( l_client ) ).
        " set filter values
        loop at x_filter_values into data(x_filter_value).
          lr_statement->set_param( data_ref = ref #( x_filter_value  ) ).
        endloop.

        if x_skip_limit eq space.
          lr_statement->set_param( data_ref = ref #( x_limit  ) ).
          lr_statement->set_param( data_ref = ref #( x_offset ) ).
        endif.
        data(lr_result) = lr_statement->execute_query( statement = x_sqlstm ).

        lr_result->set_param_table( ref #( <fs_table> ) ).
        if lr_result->next_package( ) > 0.
        endif.

        lr_connection->close( ).
      catch cx_sql_exception into data(cx_exc).
    endtry.
  endmethod.


  method zif_eezz_table~do_sort.
    field-symbols: <x_col_field> type zstr_cell.

    loop at mt_column_names assigning <x_col_field>.
      if sy-tabix = index and m_resort = 0.
        continue.
      else.
        <x_col_field>-c_sort = 0.
      endif.
    endloop.

    if m_resort = 1.
      m_resort  = 0.
      zif_eezz_table~m_offset  = 0.
      zif_eezz_table~m_topdown = 1.
    endif.

    data(x_sort) = mt_column_names[ index ]-c_sort.

    field-symbols <fs_table> type any table.
    assign mt_table->* to <fs_table>.

    if x_sort = 0.
      x_sort = -1.
    endif.

    x_sort     = x_sort * -1.
    m_selected = 1.
    if m_table_ddic is initial.
      data(x_colname) = mt_column_names[ index ]-c_field_name.
      if x_sort = 1.
        sort <fs_table> by value abap_sortorder_tab(
          ( name = x_colname descending = ' ' ) ).
      else.
        sort <fs_table> by value abap_sortorder_tab(
          ( name = x_colname descending = 'X' ) ).
      endif.

      " Create a new filter table
      field-symbols <fs_table_filter> type any table.
      field-symbols <fs_table_orig>   type any table.
      assign mt_table->* to <fs_table_orig>.
      data x_tabledescr  type ref to cl_abap_tabledescr.
      data x_structdescr type ref to cl_abap_structdescr.
      " x_structdescr ?= cl_abap_typedescr=>describe_by_data( mt_table ).
      x_tabledescr  ?= cl_abap_tabledescr=>describe_by_data( <fs_table_orig> ).
      create data zif_eezz_table~mt_table_filter type handle x_tabledescr.
      assign zif_eezz_table~mt_table_filter->* to <fs_table_filter>.

      "data(x_cond) = |FILENAME cs 'dev_'|.
      " ==> regex is a condition for all fields
      " try ... if successfull use new fs_table_filter for view
      "loop at <fs_table_orig>  assigning field-symbol(<fs_wa>) where (x_cond).
      "  insert <fs_wa> into table <fs_table_filter>.
      "endloop.
    else.
      me->do_select_database( ).
    endif.
    mt_column_names[ index ]-c_sort = x_sort.

  endmethod.


  method zif_eezz_table~get_column_names.
    get reference of mt_column_names into rt_column_names.
  endmethod.


  method zif_eezz_table~get_dictionary.
    get reference of mt_dictionary into rt_dictionary.
  endmethod.


  method ZIF_EEZZ_TABLE~GET_HASH.
    rv_hash = zcl_eezz_helper=>create_hash_for_table_key( it_rows = ref #( mt_column_names ) iv_line = iv_line iv_path = iv_path iv_clear = iv_clear ).
  endmethod.


  method zif_eezz_table~get_row.
    data ls_filtered  type abap_bool value 'X'.
    data ls_cell      type zstr_cell.
    data x_typedescr  type ref to cl_abap_tabledescr.
    data x_linedescr  type ref to cl_abap_structdescr.
    data x_index      type i.

    x_index       = iv_index.
    m_selected    = 0.

    field-symbols <fs_table> type standard table.
    field-symbols <fs_line>  type any.
    field-symbols <fs_value> type any.

    if mt_table is not bound.
      return. " should not appen.....
    endif.

    assign mt_table->* to <fs_table>.
    clear  mt_row.

    x_typedescr   ?= cl_abap_tabledescr=>describe_by_data( <fs_table> ).
    x_linedescr   ?= x_typedescr->get_table_line_type( ).

    data(x_tablesize) = lines( <fs_table> ).
    data(x_visible)   = lines( <fs_table> ).

    if x_tablesize <= 0.
      me->do_select_database( iv_offset = 0 ).
      x_tablesize = lines( <fs_table> ).
    endif.

    data(x_start)   = 0.
    data(x_ends)    = x_tablesize.
    data(x_range)   = zif_eezz_table~m_visible_items.

    if mt_dictionary[ c_key = 'table_nav_type' ]-c_value cs 'block'.
      x_range = zif_eezz_table~m_visible_block.
    endif.

    if m_table_ddic is initial.
      if zif_eezz_table~m_topdown < 0.
        x_start   = nmin( val1 = x_tablesize - x_range val2 = x_tablesize - zif_eezz_table~m_offset ).
      else.
        x_start   = nmin( val1 = zif_eezz_table~m_offset val2 = x_tablesize - x_range ).
      endif.
      x_start = nmax( val1 = x_start val2 = 0 ).
      x_ends  = nmin( val1 = x_start + x_range val2 = x_tablesize ).
      x_index = x_start + x_index.
    else.
      x_start = 0.
      x_ends  = nmin( val1 = x_start + x_range val2 = x_tablesize ).
      if zif_eezz_table~m_topdown < 0.
        x_index = x_index - 1.
        x_index = x_ends  - x_index.
      else.
        x_index = x_start + x_index.
      endif.
    endif.

    if x_index > x_ends or x_index <= x_start.
      return.
    endif.

    read table <fs_table> assigning <fs_line> index x_index.
    if sy-subrc ne 0.
      return.
    endif.

    m_selected = x_index.

    data(x_path) = ||. " mt_dictionary[ c_key = |tree_path| ]-c_value.
    get reference of mt_column_names into data(x_ref_row).

    data: x_genkey type string.
    x_genkey        = zcl_eezz_helper=>create_hash_for_table_key( iv_path = x_path it_rows = x_ref_row iv_line = <fs_line> iv_clear = abap_true ).
    data(x_genpath) = zcl_eezz_helper=>create_hash_for_table_key( iv_path = x_path it_rows = x_ref_row iv_line = <fs_line> iv_clear = abap_true ).
    data(x_locpath) = zcl_eezz_helper=>create_hash_for_table_key( it_rows = x_ref_row iv_line = <fs_line> iv_clear = abap_true ).

    assign component '_eezz_row_cell_' of structure <fs_line> to <fs_value>.
    if sy-subrc = 0.
      append value #( c_field_name = '_eezz_row_cell_' c_genkey = x_genkey c_value = <fs_value> ) to mt_row.
    else.
      append value #( c_field_name = '_eezz_row_cell_' c_genkey = x_genkey c_value = x_locpath  ) to mt_row.
    endif.

    loop at mt_column_names into data(x_wa_col).
      data x_field type string.
      x_field = x_wa_col-c_field_name.

      assign component x_wa_col-c_field_name of structure <fs_line> to <fs_value>.
      if sy-subrc eq 0.
        ls_cell-c_value      = <fs_value>.
        ls_cell-c_field_name = x_field.
        ls_cell-c_genkey     = |{ x_genkey }.{ x_field }|.
        ls_cell-c_type       = x_wa_col-c_type.

        try.
            if ls_cell-c_type co 'D'.
              call function 'CONVERT_DATE_TO_EXTERNAL' exporting date_internal = <fs_value> importing date_external = ls_cell-c_value.
            endif.
          catch cx_root.
            " keep value...
        endtry.

        append ls_cell to mt_row.
      endif.
    endloop.

    get reference of mt_row into rt_row.
  endmethod.


  method zif_eezz_table~get_selected_obj.
    if index = -1.
      m_visible_block = visible_block.
      m_visible_items = visible_items.
      m_table_name    = table_name.
      rt_eezz_table   = me.
      modify table me->mt_dictionary from value #( c_key = 'table_items' c_value = visible_items ).
    endif.

endmethod.


  METHOD zif_eezz_table~get_update.

    get reference of mt_update into rt_update.

  ENDMETHOD.


  method zif_eezz_table~has_changed.
    rv_has_changed = abap_true.
  endmethod.


  method zif_eezz_table~on_download.

    rv_message = new zcl_eezz_message( ).

    try.
        if iv_message->get_message_type( ) = iv_message->co_message_type_text.
          data(x_json)       = new zcl_eezz_json( iv_json = iv_message->get_text( ) ).
          data(x_progress)   = x_json->get_value( |progress| ).
          data(x_chunksize)  = x_json->get_value( |chunkSize| ).
          data(x_filesize)   = x_json->get_value( |file/size| ).
          data(x_filename)   = x_json->get_value( |file/name| ).
          data(x_transfered) = x_json->get_value( |file/chunkSize| ).
          data(x_sequence)   = x_json->get_value( |file/sequence| ).

          data(x_prog_seq)  = ( x_sequence + 1 ) * ( x_chunksize / x_filesize ) * 100.
          x_prog_seq        = nmin( val1 = x_prog_seq  val2 = 100 ).

          rv_message->add( iv_key = |{ x_progress }.innerHTML|   iv_value = |{ x_prog_seq }%| ).
          rv_message->add( iv_key = |{ x_progress }.style.width| iv_value = |{ x_prog_seq }%| ).
        endif.
      catch cx_apc_error into data(x_exception).
        rv_message->add( iv_status = 500 iv_key = |OnDownload| iv_exception = x_exception ).
        raise exception x_exception.
    endtry.
  endmethod.


  method zif_eezz_table~prepare_download.
    try.
        if iv_message->get_message_type( ) = iv_message->co_message_type_text.
          zcl_eezz_message=>set_request( iv_message->get_text( ) ).
          zcl_eezz_message=>set_status( 100 ).

          data(x_json)     = new zcl_eezz_json( iv_json = iv_message->get_text( ) ).
          data(x_json_tbl) = x_json->get( ).

          delete table x_json_tbl->* with table key c_key = 'update'.
          x_json        = new zcl_eezz_json( it_json = x_json_tbl ).
          rv_request    = x_json->dump( )->get_result_string( ).
          zcl_eezz_message=>set_status( 100 ).

        endif.
      catch cx_root into data(x_exception).
        zcl_eezz_message=>add( iv_status = 500 iv_key = |OnDownload| iv_exception = x_exception ).
        raise exception x_exception.
    endtry.
  endmethod.


  method zif_eezz_table~send_message_pcp.
    try.
        data(x_hash_name) = me->get_hash( iv_path = iv_path iv_line = iv_line  ).
        data(pcp_message) = cl_ac_message_type_pcp=>create( ).

        loop at it_fields->* into data(x_pcp).
          pcp_message->set_field( i_name = |{ x_hash_name }.{ x_pcp-name }|   i_value = |{ x_pcp-value }| ).
        endloop.

        cast if_amc_message_producer_pcp(
          cl_amc_channel_manager=>create_message_producer(
            i_application_id = 'Z_EEZZ_WS_MSG_CHANNEL'
            i_channel_id     = '/eezz/web' )
        )->send( i_message = pcp_message ).
      catch cx_amc_error cx_ac_message_type_pcp_error  into data(x_exception).
        data(x_error) = x_exception->get_text( ).
      catch CX_SY_MOVE_CAST_ERROR into data(x_exc_move).
        data(x_error_mv) = x_exc_move->get_text( ).
        cl_demo_output=>display( x_error_mv ).
    endtry.
  endmethod.
ENDCLASS.
