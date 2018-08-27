class ZCL_EEZZ_TABLE definition
  public
  create public .

public section.

  interfaces ZIF_EEZZ_TABLE .

  aliases DO_NAVIGATE
    for ZIF_EEZZ_TABLE~DO_NAVIGATE .
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

  aliases MT_UPDATE
    for ZIF_EEZZ_TABLE~MT_UPDATE .
  aliases M_OFFSET
    for ZIF_EEZZ_TABLE~M_OFFSET .
  aliases M_SELECTED
    for ZIF_EEZZ_TABLE~M_SELECTED .
  aliases M_TABLE_DDIC
    for ZIF_EEZZ_TABLE~M_TABLE_DDIC .
  aliases M_TABLE_NAME
    for ZIF_EEZZ_TABLE~M_TABLE_NAME .
  aliases M_TOPDOWN
    for ZIF_EEZZ_TABLE~M_TOPDOWN .
  aliases M_VISIBLE_BLOCK
    for ZIF_EEZZ_TABLE~M_VISIBLE_BLOCK .
  aliases M_VISIBLE_ITEMS
    for ZIF_EEZZ_TABLE~M_VISIBLE_ITEMS .
  aliases DO_SELECT_DATABASE
    for ZIF_EEZZ_TABLE~DO_SELECT_DATABASE .

  data M_RESORT type INT4 .
  data M_LIMIT type INT4 .
  data MT_DICTIONARY type ZTTY_DICTIONARY .
  data MT_COMPONENTS type CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE .
  data MT_ROW type ZTTY_EEZZ_ROW .
  data MT_COLUMN_NAMES type ZTTY_EEZZ_ROW .
  data MT_TABLE type ref to DATA .
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

    zif_eezz_table~m_visible_items = 10.
    zif_eezz_table~m_visible_block = 10.
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
      ( c_key = 'table_current'  c_value =  '0' )
      ( c_key = 'innerHTML'      c_value = 'innerHTML')
      ( c_key = 'table_path'     c_value =  'id000000' )
      ( c_key = 'table_items'    c_value =  '20' )
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

    loop at mt_components into data(wac).
      x_wa_column-c_field_name = wac-name.
      x_wa_column-c_sort       = 0.
      x_wa_column-c_value      = wac-name.
      x_wa_column-c_position   = sy-tabix.
      if line_exists( lt_table_fields[ fieldname = wac-name ] ).
        x_wa_column-c_value = lt_table_fields[ fieldname = wac-name ]-fieldtext.
        x_wa_column-c_outputlen = lt_table_fields[ fieldname = wac-name ]-outputlen.
        x_wa_column-c_keyflag = lt_table_fields[ fieldname = wac-name ]-keyflag.
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
      if x_position < 0.
        x_position = 0.
      endif.
    endif.

    me->zif_eezz_table~do_select_database( iv_offset = x_position ).

    " check the selection and keep position, if we reached the end of the table:
    field-symbols <fs_table> type any table.
    assign mt_table->* to <fs_table>.

    if lines( <fs_table> ) >= zif_eezz_table~m_visible_block.
      zif_eezz_table~m_offset = x_position.
      modify table mt_dictionary from value #( c_key = 'table_pos' c_value = zif_eezz_table~m_offset ).
    endif.

    m_resort = 1.
  endmethod.


  method zif_eezz_table~do_select.
  endmethod.


  method zif_eezz_table~do_select_database.

    data: x_filter_values type table of string,
          x_skip_limit    type c.

    if m_table_ddic is initial.
      return.
    else.
      data(x_table_ddic) = m_table_ddic.
      if x_table_ddic+0(1) eq '/'.
        concatenate '"' x_table_ddic '"' into x_table_ddic.
      endif.
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

    if m_resort = 1.
      m_resort  = 0.
      zif_eezz_table~m_offset  = 0.
      zif_eezz_table~m_topdown = 1.

      do lines( mt_column_names ) times.
        mt_column_names[ sy-index ]-c_sort = 0.
      enddo.
    endif.

    data(x_sort) = mt_column_names[ index ]-c_sort.

    if x_sort = 0.
      x_sort = -1.
    endif.

    x_sort = x_sort * -1.
    mt_column_names[ index ]-c_sort = x_sort.
    me->do_select_database( ).
  endmethod.


  method zif_eezz_table~get_column_names.
    get reference of mt_column_names into rt_column_names.
  endmethod.


  method zif_eezz_table~get_dictionary.
    get reference of mt_dictionary into rt_dictionary.
  endmethod.


  method ZIF_EEZZ_TABLE~GET_HASH.
    rv_hash = zcl_eezz_helper=>create_hash_for_table_key( it_rows = ref #( mt_column_names ) iv_line = iv_line ).
  endmethod.


  METHOD zif_eezz_table~get_row.
   DATA ls_cell  TYPE zstr_cell.

    DATA x_typedescr  TYPE REF TO cl_abap_tabledescr.
    DATA x_linedescr  TYPE REF TO cl_abap_structdescr.

    DATA(x_index) = iv_index.
    m_selected = 0.

    FIELD-SYMBOLS <fs_table> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <fs_line>  TYPE any.
    FIELD-SYMBOLS <fs_value> TYPE any.

    if mt_table is not bound.
      return. " should not appen.....
    endif.

    ASSIGN mt_table->* TO <fs_table>.
    CLEAR  mt_row.


    x_typedescr   ?= cl_abap_tabledescr=>describe_by_data( <fs_table> ).
    x_linedescr   ?= x_typedescr->get_table_line_type( ).

    DATA(x_visible) = lines( <fs_table> ).

    IF x_visible <= 0.
      me->do_select_database( iv_offset = 0 ).
      x_visible = lines( <fs_table> ).
    ENDIF.

    IF m_table_name IS INITIAL.
      x_visible = x_visible - zif_eezz_table~m_offset.
      x_index   = x_index   + zif_eezz_table~m_offset.
    ENDIF.

    IF mt_dictionary[ c_key = 'table_nav_type' ]-c_value CS 'block'.
      x_visible = nmin( val1 = x_visible val2 = zif_eezz_table~m_visible_block ).
    ELSE.
      x_visible = nmin( val1 = x_visible val2 = zif_eezz_table~m_visible_items ).
    ENDIF.

    IF iv_index > x_visible.
      RETURN.
    ENDIF.

    IF zif_eezz_table~m_topdown < 0.
      x_index = x_index   - 1.
      x_index = x_visible - x_index.
    ENDIF.

    READ TABLE <fs_table> ASSIGNING <fs_line> INDEX x_index.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
    m_selected = x_index.
    DATA: x_genkey TYPE string.
    GET REFERENCE OF mt_column_names INTO DATA(x_ref_row).


    LOOP AT mt_column_names INTO DATA(x_wa_col).
      data xField type string.
      xField   = x_wa_col-c_field_name.
      x_genkey = zcl_eezz_helper=>create_hash_for_table_key( it_rows = x_ref_row iv_line = <fs_line> iv_field = xField ).

      ASSIGN COMPONENT x_wa_col-c_field_name OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc EQ 0.
        ls_cell-c_value      = <fs_value>.
        ls_cell-c_field_name = x_wa_col-c_field_name.
        ls_cell-c_genkey     = x_genkey.

        TRY.
            IF x_linedescr->components[ name = x_wa_col-c_field_name ]-type_kind CO 'D'.
              CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL' EXPORTING date_internal = <fs_value> IMPORTING date_external = ls_cell-c_value.
            ENDIF.
          CATCH cx_root.
            " keep value...
        ENDTRY.

        APPEND ls_cell TO mt_row.
      ENDIF.
    ENDLOOP.

    x_genkey = zcl_eezz_helper=>create_hash_for_table_key( it_rows = x_ref_row iv_line = <fs_line> ).
    append value #( c_field_name = '_eezz_row_cell_' c_genkey = x_genkey ) to mt_row.

    GET REFERENCE OF mt_row INTO rt_row.
  ENDMETHOD.


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


  method zif_eezz_table~send_message_pcp.
    try.
        data(x_hash_name) = me->get_hash( iv_line ).
        data(pcp_message) = cl_ac_message_type_pcp=>create( ).

        loop at it_fields->* into data(x_pcp).
          pcp_message->set_field( i_name = x_pcp-name i_value = x_pcp-value ).
        endloop.

        pcp_message->set_field( i_name = |eezz_event_key| i_value = iv_event    ).
        pcp_message->set_field( i_name = |eezz_target|    i_value = x_hash_name ).

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


  method zif_eezz_table~set_range.
    if zif_eezz_table~mt_range is initial.
      zif_eezz_table~mt_range = iv_range.
    endif.
  endmethod.
ENDCLASS.
