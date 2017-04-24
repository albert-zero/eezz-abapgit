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
*    Creates an generic interface for tables
*    Internal tables are passed as iv_table into the constructor
*    DDIC tables are passed as iv_table_name.
*
class zcl_eezz_table definition
  public
  create public .

  public section.

    interfaces zif_eezz_table .

    aliases do_navigate
      for zif_eezz_table~do_navigate .
    aliases do_select
      for zif_eezz_table~do_select .
    aliases do_sort
      for zif_eezz_table~do_sort .
    aliases get_column_names
      for zif_eezz_table~get_column_names .
    aliases get_dictionary
      for zif_eezz_table~get_dictionary .
    aliases get_row
      for zif_eezz_table~get_row .
    aliases get_selected_obj
      for zif_eezz_table~get_selected_obj .
    aliases has_changed
      for zif_eezz_table~has_changed .

    methods constructor
      importing
        !iv_table   type ref to data optional
        !table_name type string optional .
  protected section.
  private section.

    aliases do_select_database
      for zif_eezz_table~do_select_database .

    data m_resort type int4 .
    data m_limit type int4 .
    data m_offset type int4 .
    data mt_dictionary type ztty_dictionary .
    data m_table_name type string .
    data mt_components type cl_abap_structdescr=>component_table .
    data mt_row type ztty_eezz_row .
    data mt_column_names type ztty_eezz_row .
    data mt_table type ref to data .
    data mt_sort type ztty_sort .
endclass.



class zcl_eezz_table implementation.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TABLE->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TABLE                       TYPE REF TO DATA(optional)
* | [--->] TABLE_NAME                     TYPE        STRING(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method constructor.
    data:
      x_wa_column   type zstr_cell,
      x_typedescr   type ref to cl_abap_typedescr,
      x_structdescr type ref to cl_abap_structdescr,
      x_tabledescr  type ref to cl_abap_tabledescr,
      x_table_comps type cl_abap_structdescr=>component_table.

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
          x_structdescr ?= cl_abap_typedescr=>describe_by_name( p_name = m_table_name ).
          mt_components  = x_structdescr->get_components( ).
          x_tabledescr   = cl_abap_tabledescr=>create( x_structdescr ).

          create data mt_table type handle x_tabledescr.
        endif.
      catch cx_root.
        return.
    endtry.

    data lt_table_fields type table of dfies.
    data lt_table_name   type ddobjname.

    if strlen( m_table_name ) > 0.
      lt_table_name = m_table_name.
      call function 'DDIF_FIELDINFO_GET' exporting tabname = lt_table_name tables dfies_tab = lt_table_fields.
    endif.

    loop at mt_components into data(wac).
      x_wa_column-c_field_name = wac-name.
      x_wa_column-c_sort       = 0.
      x_wa_column-c_value      = wac-name.
      x_wa_column-position     = sy-tabix.
      if line_exists( lt_table_fields[ fieldname = wac-name ] ).
        x_wa_column-c_value = lt_table_fields[ fieldname = wac-name ]-fieldtext.
      endif.

      append x_wa_column to mt_column_names.
    endloop.



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
      ( c_key = 'table_current'  c_value =  '0' ) ).


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TABLE->ZIF_EEZZ_TABLE~DO_NAVIGATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] WHERE                          TYPE        INT4
* | [--->] POS                            TYPE        INT4(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TABLE->ZIF_EEZZ_TABLE~DO_SELECT
* +-------------------------------------------------------------------------------------------------+
* | [--->] INDEX                          TYPE        I
* | [<-()] RT_EEZZ_TABLE                  TYPE REF TO ZIF_EEZZ_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_eezz_table~do_select.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TABLE->ZIF_EEZZ_TABLE~DO_SELECT_DATABASE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_LIMIT                       TYPE        INT4(optional)
* | [--->] IV_OFFSET                      TYPE        INT4(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_eezz_table~do_select_database.

    if m_table_name is initial.
      return.
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
    x_sqlstm_stream->write( |SELECT * FROM { m_table_name } WHERE MANDT = ? LIMIT ?, ?| ).

    data(x_order_str) = | ORDER BY |.
    data: x_asc type string value 'ASC',
          x_des type string value 'DESC'.

    if zif_eezz_table~m_topdown < 0.
      x_asc = 'DESC'.
      x_des = 'ASC'.
    endif.

    loop at mt_column_names into data(xwa_column).
      if xwa_column-c_sort = 0.
        continue.
      endif.

      x_sqlstm_stream->write( x_order_str ).
      x_order_str = ''.

      if xwa_column-c_sort = 1.
        x_sqlstm_stream->write( |{ xwa_column-c_field_name } { x_asc } { x_order_str }| ).
        x_order_str = ','.
      endif.

      if xwa_column-c_sort = -1.
        x_sqlstm_stream->write( |{ xwa_column-c_field_name } { x_des } { x_order_str }| ).
        x_order_str = ','.
      endif.
    endloop.

    data(x_sqlstm) = x_sqlstm_stream->get_result_string( ).

    try.
        clear <fs_table>.
        data(lr_connection) = cl_sql_connection=>get_connection( 'DEFAULT' ).
        data(lr_statement)  = lr_connection->create_statement( ).
        data(l_client)      = sy-mandt.

        lr_statement->set_param( data_ref = ref #( l_client ) ).
        lr_statement->set_param( data_ref = ref #( x_offset ) ).
        lr_statement->set_param( data_ref = ref #( x_limit  ) ).

        data(lr_result)     = lr_statement->execute_query( statement = x_sqlstm ).

        lr_result->set_param_table( ref #( <fs_table> ) ).
        if lr_result->next_package( ) > 0.
        endif.

        lr_connection->close( ).
      catch cx_sql_exception into data(cx_exc).
    endtry.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TABLE->ZIF_EEZZ_TABLE~DO_SORT
* +-------------------------------------------------------------------------------------------------+
* | [--->] INDEX                          TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TABLE->ZIF_EEZZ_TABLE~GET_COLUMN_NAMES
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_COLUMN_NAMES                TYPE REF TO ZTTY_EEZZ_ROW
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_eezz_table~get_column_names.
    get reference of mt_column_names into rt_column_names.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TABLE->ZIF_EEZZ_TABLE~GET_DICTIONARY
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_DICTIONARY                  TYPE REF TO ZTTY_DICTIONARY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_eezz_table~get_dictionary.
    get reference of mt_dictionary into rt_dictionary.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TABLE->ZIF_EEZZ_TABLE~GET_ROW
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INDEX                       TYPE        I
* | [<-()] RT_ROW                         TYPE REF TO ZTTY_EEZZ_ROW
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_eezz_table~get_row.
    data ls_cell  type zstr_cell.

    data x_typedescr  type ref to cl_abap_tabledescr.
    data x_linedescr  type ref to cl_abap_structdescr.

    field-symbols <fs_table> type standard table.
    field-symbols <fs_line>  type any.
    field-symbols <fs_value> type any.

    assign mt_table->* to <fs_table>.
    clear  mt_row.

    x_typedescr   ?= cl_abap_tabledescr=>describe_by_data( <fs_table> ).
    x_linedescr   ?= x_typedescr->get_table_line_type( ).

    data(x_visible) = lines( <fs_table> ).

    if x_visible = 0.
      me->do_select_database( iv_limit = zif_eezz_table~m_visible_block iv_offset = 0 ).
    endif.

    if mt_dictionary[ c_key = 'table_nav_type' ]-c_value cs 'block'.
      x_visible = nmax( val1 = x_visible val2 = zif_eezz_table~m_visible_block ).
    else.
      x_visible = nmax( val1 = x_visible val2 = zif_eezz_table~m_visible_items ).
    endif.

    if iv_index > x_visible.
      return.
    endif.

    data(x_index) = iv_index.
    if zif_eezz_table~m_topdown < 0.
      x_index = x_index   - 1.
      x_index = x_visible - x_index.
    endif.

    read table <fs_table> assigning <fs_line> index x_index.
    if sy-subrc ne 0.
      return.
    endif.

    loop at mt_column_names into data(x_wa_col).
      assign component x_wa_col-c_field_name of structure <fs_line> to <fs_value>.
      ls_cell-c_value      = <fs_value>.
      ls_cell-c_field_name = x_wa_col-c_field_name.

      try.
          if x_linedescr->components[ name = x_wa_col-c_field_name ]-type_kind co 'D'.
            call function 'CONVERT_DATE_TO_EXTERNAL' exporting date_internal = <fs_value> importing date_external = ls_cell-c_value.
          endif.
        catch cx_root.
          " keep value...
      endtry.

      append ls_cell to mt_row.
    endloop.

    get reference of mt_row into rt_row.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TABLE->ZIF_EEZZ_TABLE~GET_SELECTED_OBJ
* +-------------------------------------------------------------------------------------------------+
* | [--->] INDEX                          TYPE        INTEGER (default =-1)
* | [--->] VISIBLE_ITEMS                  TYPE        INTEGER (default =20)
* | [--->] VISIBLE_BLOCK                  TYPE        INTEGER (default =20)
* | [<-()] RT_EEZZ_TABLE                  TYPE REF TO ZIF_EEZZ_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_eezz_table~get_selected_obj.
    if index = -1.
      zif_eezz_table~m_visible_block = visible_block.
      zif_eezz_table~m_visible_items = visible_items.

      "modify table mt_dictionary from value #( c_key = 'table_prev' c_value = visible_block * -1 ).
      "modify table mt_dictionary from value #( c_key = 'table_next' c_value = visible_block ).

      rt_eezz_table = me.
    endif.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TABLE->ZIF_EEZZ_TABLE~HAS_CHANGED
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_HAS_CHANGED                 TYPE        XFELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_eezz_table~has_changed.
    rv_has_changed = abap_true.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_EEZZ_TABLE->ZIF_EEZZ_TABLE~SET_RANGE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_eezz_table~set_range.
    if zif_eezz_table~mt_range is initial.
      zif_eezz_table~mt_range = iv_range.
    endif.
  endmethod.
endclass.
