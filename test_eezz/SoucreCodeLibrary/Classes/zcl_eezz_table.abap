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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TABLE->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TABLE                       TYPE REF TO DATA(optional)
* | [--->] TABLE_NAME                     TYPE        STRING(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    DATA:
      x_wa_column   TYPE zstr_cell,
      x_typedescr   TYPE REF TO cl_abap_typedescr,
      x_structdescr TYPE REF TO cl_abap_structdescr,
      x_tabledescr  TYPE REF TO cl_abap_tabledescr,
      x_table_comps TYPE cl_abap_structdescr=>component_table.

    zif_eezz_table~m_visible_items = 10.
    zif_eezz_table~m_visible_block = 10.
    zif_eezz_table~m_offset        =  0.
    zif_eezz_table~m_topdown       =  1.
    m_resort                       =  0.

    mt_dictionary = VALUE ztty_dictionary(
      ( c_key = 'table_nav_type' c_value = 'block' )
      ( c_key = 'table_pos'      c_value =  '0' )
      ( c_key = 'table_top'      c_value =  '1' )
      ( c_key = 'table_last'     c_value = '-1' )
      ( c_key = 'table_next'     c_value =  '2' )
      ( c_key = 'table_prev'     c_value = '-2' )
      ( c_key = 'table_current'  c_value =  '0' )
      ( c_key = 'innerHTML'      c_value = 'innerHTML')
      ( c_key = 'table_path'     c_value =  'id000000' )
       ).

    TRY.
        IF iv_table IS NOT INITIAL.
          " work with existing table
          mt_table       = iv_table.
          x_tabledescr  ?= cl_abap_typedescr=>describe_by_data_ref( iv_table ).
          x_structdescr ?= x_tabledescr->get_table_line_type( ).
          mt_components  = x_structdescr->get_components( ).
        ELSEIF table_name IS NOT INITIAL.
          " try to find DDIC table
          m_table_ddic   = table_name.
          x_structdescr ?= cl_abap_typedescr=>describe_by_name( p_name = m_table_ddic ).
          mt_components  = x_structdescr->get_components( ).
          x_tabledescr   = cl_abap_tabledescr=>create( x_structdescr ).

          CREATE DATA mt_table TYPE HANDLE x_tabledescr.
        ENDIF.
      CATCH cx_root.
        RETURN.
    ENDTRY.

    DATA lt_table_fields TYPE TABLE OF dfies.
    DATA lt_table_ddic   TYPE ddobjname.

    IF strlen( m_table_ddic ) > 0.
      lt_table_ddic = m_table_ddic.
      CALL FUNCTION 'DDIF_FIELDINFO_GET' EXPORTING tabname = lt_table_ddic TABLES dfies_tab = lt_table_fields.
    ENDIF.

    LOOP AT mt_components INTO DATA(wac).
      x_wa_column-c_field_name = wac-name.
      x_wa_column-c_sort       = 0.
      x_wa_column-c_value      = wac-name.
      x_wa_column-c_position   = sy-tabix.
      IF line_exists( lt_table_fields[ fieldname = wac-name ] ).
        x_wa_column-c_value = lt_table_fields[ fieldname = wac-name ]-fieldtext.
        x_wa_column-c_outputlen = lt_table_fields[ fieldname = wac-name ]-outputlen.
        x_wa_column-c_keyflag = lt_table_fields[ fieldname = wac-name ]-keyflag.
      ENDIF.

      APPEND x_wa_column TO mt_column_names.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TABLE->ZIF_EEZZ_TABLE~CREATE_NODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SYMBOLS                     TYPE REF TO ZTTY_SYMBOLS
* | [<-()] IO_TABLE_NODE                  TYPE REF TO IF_IXML_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ZIF_EEZZ_TABLE~CREATE_NODE.
    data(x_table_element)  = iv_symbols->*[ c_name = m_table_name ].
    data(x_table_zif_node) = new zcl_eezz_table_node( io_node = x_table_element-c_templ_node it_globals = iv_symbols io_eezz_tbl = me ).
    io_table_node          = x_table_zif_node->get( ).
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
* | [--->] INDEX                          TYPE        I (default =1)
* | [--->] PATH                           TYPE        STRING(optional)
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

    x_order_str = | ORDER BY |.
    loop at mt_column_names into xwa_column.
      if xwa_column-c_sort =  1.
        x_sqlstm_stream->write( |{ x_order_str } { xwa_column-c_field_name } { x_asc }| ).
        x_order_str = ','.
      elseif
         xwa_column-c_sort = -1.
        x_sqlstm_stream->write( |{ x_order_str } { xwa_column-c_field_name } { x_des }| ).
        x_order_str = ','.
      endif.
    endloop.

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
  METHOD zif_eezz_table~get_row.
    DATA ls_cell  TYPE zstr_cell.

    DATA x_typedescr  TYPE REF TO cl_abap_tabledescr.
    DATA x_linedescr  TYPE REF TO cl_abap_structdescr.

    DATA(x_index) = iv_index.
    m_selected = 0.

    FIELD-SYMBOLS <fs_table> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <fs_line>  TYPE any.
    FIELD-SYMBOLS <fs_value> TYPE any.

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

* Create hash value for concatenate table key
    x_genkey = zcl_eezz_helper=>create_hash_for_table_key( it_rows = x_ref_row iv_line = <fs_line> ).

    LOOP AT mt_column_names INTO DATA(x_wa_col).
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

    GET REFERENCE OF mt_row INTO rt_row.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TABLE->ZIF_EEZZ_TABLE~GET_SELECTED_OBJ
* +-------------------------------------------------------------------------------------------------+
* | [--->] INDEX                          TYPE        INTEGER (default =-1)
* | [--->] TABLE_NAME                     TYPE        STRING(optional)
* | [--->] VISIBLE_ITEMS                  TYPE        INTEGER (default =20)
* | [--->] VISIBLE_BLOCK                  TYPE        INTEGER (default =20)
* | [<-()] RT_EEZZ_TABLE                  TYPE REF TO ZIF_EEZZ_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_eezz_table~get_selected_obj.
    if index = -1.
      m_visible_block = visible_block.
      m_visible_items = visible_items.
      m_table_name    = table_name.

      rt_eezz_table   = me.
    endif.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TABLE->ZIF_EEZZ_TABLE~GET_UPDATE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_UPDATE                      TYPE REF TO ZTTY_UPDATE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_eezz_table~get_update.

    get reference of mt_update into rt_update.

  ENDMETHOD.


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
ENDCLASS.