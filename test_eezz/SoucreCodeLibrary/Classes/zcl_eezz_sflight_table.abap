class zcl_eezz_sflight_table definition
  public
  final
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

    methods constructor
      importing
        !iv_table_name type string optional .
  protected section.
  private section.

    data m_has_changed type xfeld .
    data m_spfli type spfli .
    data m_sort_result_flg type xfeld .
    data m_sort_chg_flg type xfeld .
    data m_limit type int4 .
    data m_offset type int4 .
    data mt_sort type ztty_sort .
    data mt_dictionary type ztty_dictionary .
    data mt_row type ztty_eezz_row .
    data m_visible_items type int4 .
    data m_visible_blocks type int4 .
    data mt_column_names type ztty_eezz_row .
    data mt_sflight type sflight_tab1 .

    methods switch_sort_order .
ENDCLASS.



CLASS ZCL_EEZZ_SFLIGHT_TABLE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_SFLIGHT_TABLE->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TABLE_NAME                  TYPE        STRING(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method constructor.

    data: ls_dictionary type zstr_dictionary.

    mt_dictionary = value ztty_dictionary(
    ( c_key = 'table_prev' c_value = '-10' )
    ( c_key = 'table_pos' c_value = '0' )
    ( c_key = 'table_next' c_value = '10' )
    ( c_key = 'table_last' c_value = '-1' )
    ( c_key = 'table_top' c_value = '1' )
    ( c_key = 'table_current' c_value = '0' )
    ( c_key = 'priceppc' c_value = '0' )
    ( c_key = 'carrid_icon' c_value = 'ICON_INFORMATION' ) ).

    mt_sort = value ztty_sort(
    ( c_field_name = 'MANDT' c_sort = 'ASC' )
    ( c_field_name = 'CARRID' c_sort = 'ASC' )
    ( c_field_name = 'CONNID' c_sort = 'ASC' )
    ( c_field_name = 'FLDATE' c_sort = 'ASC' ) ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_EEZZ_SFLIGHT_TABLE->SWITCH_SORT_ORDER
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method switch_sort_order.

    loop at mt_sort assigning field-symbol(<fs_sort>).
      if <fs_sort>-c_sort = 'ASC'.
        <fs_sort>-c_sort = 'DESC'.
      else.
        <fs_sort>-c_sort = 'ASC'.
      endif.
    endloop.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_SFLIGHT_TABLE->ZIF_EEZZ_TABLE~DO_NAVIGATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] WHERE                          TYPE        INT4
* | [--->] POS                            TYPE        INT4(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_eezz_table~do_navigate.

    data: x_pages  type p decimals 2,
          l_where  type i,
          lt_order type abap_sortorder_tab,
          ls_order type abap_sortorder,
          ls_sort  like line of mt_sort.

    field-symbols: <fs_sort> type zstr_sort.

    try.
        data(l_flg_do_sort) = mt_sort[ c_do_sort = 'X' ].
      catch cx_sy_itab_line_not_found.
    endtry.

    try.
        data(ls_sort_desc) = mt_sort[ c_sort = 'DESC' ].
      catch cx_sy_itab_line_not_found.
    endtry.

    try.
        data(ls_sort_asc) = mt_sort[ c_sort = 'ASC' ].
      catch cx_sy_itab_line_not_found.
    endtry.

    if where eq 1.
      m_offset = 0.
      clear m_sort_result_flg.
      if l_flg_do_sort is initial.
        loop at mt_sort assigning <fs_sort>.
          <fs_sort>-c_sort = 'ASC'.
        endloop.
      elseif m_sort_chg_flg eq 'X'.
        clear m_sort_chg_flg.
        switch_sort_order( ).
      endif.
    elseif where eq -1.
      m_offset = 0.
      m_sort_result_flg = 'X'.
      if l_flg_do_sort is initial.
        loop at mt_sort assigning <fs_sort>.
          <fs_sort>-c_sort = 'DESC'.
        endloop.
      else.
        if m_sort_chg_flg is initial.
          m_sort_chg_flg = 'X'.
          switch_sort_order( ).
        endif.
      endif.
    else.
      if ls_sort_desc is not initial and l_flg_do_sort-c_do_sort eq 'X'.
        if m_sort_chg_flg = 'X'.
          l_where = where * -1.
        else.
          l_where = where.
        endif.
        m_offset = m_offset + l_where.
      elseif ls_sort_asc is not initial and l_flg_do_sort-c_do_sort eq 'X'.
        if m_sort_chg_flg = 'X'.
          l_where = where * -1.
        else.
          l_where = where.
        endif.
        m_offset = m_offset + l_where.
      elseif ls_sort_desc is not initial and l_flg_do_sort-c_do_sort eq space.
        l_where = where * -1.
        m_offset = m_offset + l_where.
      elseif ls_sort_asc is not initial and l_flg_do_sort-c_do_sort eq space.
        m_offset = m_offset + where.
      endif.
    endif.

    if m_offset < 0.
      clear m_offset.
    endif.
    " Set offset in dictionary
    modify table mt_dictionary from value #( c_key = 'table_pos' c_value = m_offset ).
    if sy-subrc <> 0.
      insert value #( c_key = 'table_pos' c_value = m_offset ) into table mt_dictionary .
    endif.
    me->zif_eezz_table~do_select_database( ).

    try.
        ls_sort_desc = mt_sort[ c_sort = 'DESC' ].
      catch cx_sy_itab_line_not_found.
    endtry.

    try.
        ls_sort_asc = mt_sort[ c_sort = 'ASC' ].
      catch cx_sy_itab_line_not_found.
    endtry.

    if m_sort_result_flg eq abap_true.
      if ls_sort_desc is not initial.
        loop at mt_sort into ls_sort.
          ls_order-name = ls_sort-c_field_name.
          append ls_order to lt_order.
        endloop.
      elseif ls_sort_asc is not initial.
        loop at mt_sort into ls_sort.
          ls_order-name = ls_sort-c_field_name.
          ls_order-descending = 'X'.
          append ls_order to lt_order.
        endloop.
      endif.
      sort mt_sflight by (lt_order).
    endif.


*    mt_dictionary = VALUE ztty_dictionary(
*    ( c_key = 'table_prev' c_value = '-10' )
*    ( c_key = 'table_pos' c_value = '0' )
*    ( c_key = 'table_next' c_value = '10' )
*    ( c_key = 'table_last' c_value = '-1' )
*    ( c_key = 'table_top' c_value = '1' )
*    ( c_key = 'table_current' c_value = '0' ) ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_SFLIGHT_TABLE->ZIF_EEZZ_TABLE~DO_SELECT
* +-------------------------------------------------------------------------------------------------+
* | [--->] INDEX                          TYPE        I
* | [<-()] RT_EEZZ_TABLE                  TYPE REF TO ZIF_EEZZ_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_eezz_table~do_select.

    read table mt_sflight into data(ls_flight) index index.
    data x_itab like table of m_spfli.

    if sy-subrc eq 0.
      select single * into m_spfli from spfli
        where carrid = ls_flight-carrid
          and connid = ls_flight-connid.

      insert m_spfli into table x_itab.
      get reference of x_itab into data(x_table).

      field-symbols <fs_value> type any.
      data(x_eezz_table)   = new zcl_eezz_table( iv_table = x_table ).
      data(x_column_names) = x_eezz_table->get_column_names( ).
      data(x_row)          = x_eezz_table->get_row( 1 ).

      loop at x_row->* into data(x_wa).
        "ASSIGN COMPONENT x_wa-c_field_name OF STRUCTURE m_spfli TO <fs_value>.
        modify table mt_dictionary from value #( c_key = x_wa-c_field_name c_value = x_wa-c_value ).
        if sy-subrc <> 0.
          insert value #( c_key = x_wa-c_field_name c_value = x_wa-c_value ) into table mt_dictionary.
        endif.
      endloop.
    endif.

    rt_eezz_table = me.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_SFLIGHT_TABLE->ZIF_EEZZ_TABLE~DO_SELECT_DATABASE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_LIMIT                       TYPE        INT4(optional)
* | [--->] IV_OFFSET                      TYPE        INT4(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_eezz_table~do_select_database.

    data: l_client        like sy-mandt.
    data: lv_sql      type string value 'SELECT * FROM SFLIGHT WHERE MANDT = ? LIMIT ?, ?',
          lv_order_by type string.

    " Create the ORDER_BY part
    loop at mt_sort into data(ls_sort).
      if sy-tabix <> 1.
        concatenate lv_order_by ',' into lv_order_by.
      endif.
      concatenate lv_order_by ls_sort-c_field_name ls_sort-c_sort into lv_order_by separated by space.
    endloop.

    clear mt_sflight.

    if lv_order_by is not initial.
      concatenate lv_sql 'ORDER BY' lv_order_by into lv_sql separated by space.
    endif.

    data(lr_connection) = cl_sql_connection=>get_connection( 'DEFAULT' ).
    data(lr_statement)  = lr_connection->create_statement( ).
    l_client = sy-mandt.
    lr_statement->set_param( data_ref = ref #( l_client ) ).
    lr_statement->set_param( data_ref = ref #( m_offset ) ).
    lr_statement->set_param( data_ref = ref #( m_limit ) ).
    data(lr_result)     = lr_statement->execute_query( statement = lv_sql ).
    lr_result->set_param_table( ref #( mt_sflight ) ).
    if lr_result->next_package( ) > 0.
    endif.
    lr_connection->close( ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_SFLIGHT_TABLE->ZIF_EEZZ_TABLE~DO_SORT
* +-------------------------------------------------------------------------------------------------+
* | [--->] INDEX                          TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_eezz_table~do_sort.

    read table mt_column_names into data(ls_cell) index index.
    try.
        data(ls_sort) = mt_sort[ c_field_name = ls_cell-c_field_name ].
      catch cx_sy_itab_line_not_found.
    endtry.
    if ls_sort is initial.
      clear mt_sort.
      mt_sort = value ztty_sort( ( c_field_name = ls_cell-c_field_name c_sort = 'ASC' c_do_sort = 'X' ) ).
    else.
      if ls_sort-c_sort eq 'ASC'.
        mt_sort[ c_field_name = ls_cell-c_field_name ]-c_sort = 'DESC'.
      else.
        mt_sort[ c_field_name = ls_cell-c_field_name ]-c_sort = 'ASC'.
      endif.
    endif.

    clear m_offset.
    clear m_sort_chg_flg.
    clear m_sort_result_flg.
    me->zif_eezz_table~do_select_database( ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_SFLIGHT_TABLE->ZIF_EEZZ_TABLE~GET_COLUMN_NAMES
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_COLUMN_NAMES                TYPE REF TO ZTTY_EEZZ_ROW
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_eezz_table~get_column_names.

    data: lt_table_fields type table of dfies,
          ls_column_name  type zstr_cell.

    if mt_column_names is initial.
      call function 'DDIF_FIELDINFO_GET'
        exporting
          tabname   = 'SFLIGHT'
        tables
          dfies_tab = lt_table_fields.

      loop at lt_table_fields into data(ls_field).
        clear ls_column_name.
        ls_column_name-c_value = ls_field-fieldtext.
        ls_column_name-c_field_name = ls_field-fieldname.
        ls_column_name-position = ls_field-position.
        append ls_column_name to mt_column_names.
      endloop.
    endif.

    get reference of mt_column_names into rt_column_names.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_SFLIGHT_TABLE->ZIF_EEZZ_TABLE~GET_DICTIONARY
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_DICTIONARY                  TYPE REF TO ZTTY_DICTIONARY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_eezz_table~get_dictionary.

    get reference of mt_dictionary into rt_dictionary.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_SFLIGHT_TABLE->ZIF_EEZZ_TABLE~GET_ROW
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INDEX                       TYPE        I
* | [<-()] RT_ROW                         TYPE REF TO ZTTY_EEZZ_ROW
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_eezz_table~get_row.

    data: ls_cell        type zstr_cell,
          ls_column_name type zstr_cell,
          ls_flight      type sflight.

    field-symbols: <fs_value> type any.

    if m_limit is initial.
      m_limit = m_visible_items.
    endif.

    if mt_sflight is initial.
      me->zif_eezz_table~do_select_database( ).
    endif.

    read table mt_sflight into ls_flight index iv_index.
    if sy-subrc <> 0.
      return.
    endif.

    data x_typedescr  type ref to cl_abap_tabledescr.
    data x_linedescr  type ref to cl_abap_structdescr.

    x_typedescr  ?= cl_abap_tabledescr=>describe_by_data( mt_sflight ).
    x_linedescr  ?= x_typedescr->get_table_line_type( ).

    clear mt_row.
    loop at mt_column_names into ls_column_name.
      " LOOP AT x_range->* INTO ls_column_name.
      clear ls_cell.

      assign component ls_column_name-c_field_name of structure ls_flight to <fs_value>.
      if sy-subrc eq 0.
        " Set type right-bound for integer, price and numbers
        if x_linedescr->components[ name = ls_column_name-c_field_name ]-type_kind ca 'IN'.
          ls_cell-c_field_name = ls_column_name-c_field_name.
          ls_cell-c_type  = 'numbers'.
          ls_cell-c_value = <fs_value>.
        elseif x_linedescr->components[ name = ls_column_name-c_field_name ]-type_kind ca 'P'.
          ls_cell-c_field_name = ls_column_name-c_field_name.
          ls_cell-c_value = <fs_value>.

          if  ls_column_name-c_field_name cs 'PRICE'.
            data x_value type p decimals 0.
            x_value = floor( nmin( val1 = 100 val2 = <fs_value> / '10.0' ) ).
            ls_cell-c_type  = 'relprice'.
            modify table mt_dictionary from value #( c_key = 'priceppc' c_value = |{ x_value }| ).
          else.
            ls_cell-c_type  = 'numbers'.
          endif.
        else.
          ls_cell-c_field_name = ls_column_name-c_field_name.

          if x_linedescr->components[ name = ls_column_name-c_field_name ]-type_kind co 'D'.
            call function 'CONVERT_DATE_TO_EXTERNAL'
              exporting
                date_internal = <fs_value>
              importing
                date_external = ls_cell-c_value.
          else.
            ls_cell-c_value = <fs_value>.
          endif.
        endif.

        append ls_cell to mt_row.
      endif.
    endloop.

    get reference of mt_row into rt_row.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_SFLIGHT_TABLE->ZIF_EEZZ_TABLE~GET_SELECTED_OBJ
* +-------------------------------------------------------------------------------------------------+
* | [--->] INDEX                          TYPE        INTEGER (default =-1)
* | [--->] VISIBLE_ITEMS                  TYPE        INTEGER (default =20)
* | [--->] VISIBLE_BLOCK                  TYPE        INTEGER (default =20)
* | [<-()] RT_EEZZ_TABLE                  TYPE REF TO ZIF_EEZZ_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_eezz_table~get_selected_obj.

    m_visible_blocks = visible_block.
    m_visible_items  = visible_items.

    modify table mt_dictionary from value #( c_key = 'table_prev' c_value = -1 * m_visible_items ).
    modify table mt_dictionary from value #( c_key = 'table_next' c_value =      m_visible_items ).

*    mt_dictionary = VALUE ztty_dictionary(
*    ( c_key = 'table_prev' c_value = '-10' )
*    ( c_key = 'table_pos' c_value = '0' )
*    ( c_key = 'table_next' c_value = '10' )
*    ( c_key = 'table_last' c_value = '-1' )
*    ( c_key = 'table_top' c_value = '1' )
*    ( c_key = 'table_current' c_value = '0' ) ).

    if index = -1.
      rt_eezz_table = me.
    endif.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_SFLIGHT_TABLE->ZIF_EEZZ_TABLE~HAS_CHANGED
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_HAS_CHANGED                 TYPE        XFELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method zif_eezz_table~has_changed.

    rv_has_changed = m_has_changed.

  endmethod.
ENDCLASS.