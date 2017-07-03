class ZCL_EEZZ_STATUS_MONITOR definition
  public
  inheriting from ZCL_EEZZ_TABLE
  final
  create public .

public section.

  methods CHECK
    importing
      !MESSAGE type ref to IF_AC_MESSAGE_TYPE_PCP .

  methods ZIF_EEZZ_TABLE~GET_COLUMN_NAMES
    redefinition .
  methods ZIF_EEZZ_TABLE~GET_ROW
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_EEZZ_STATUS_MONITOR IMPLEMENTATION.


  METHOD check.


    DATA x_row TYPE zmr_meq_st.
    DATA(x_event_key) = message->get_field( 'ISE_EVENT_KEY' ).
    DATA(x_equi_no) = x_event_key+2.

*    SELECT SINGLE * INTO x_row FROM zmr_meq_st
*      WHERE equnr = x_equi_no.
*    IF sy-subrc EQ 0.
*      DATA(x_url) = cl_bsp_mimes=>sap_icon( 'ICON_PRESENCE' ).
*      DATA(x_update_key)   = |{ m_table_name }-{ x_equi_no }-ICON.src|.
*      DATA(x_update_value) = x_url.
*      DATA(ls_update) = VALUE zstr_update( c_key = x_update_key c_value = x_update_value ).
*      APPEND ls_update TO mt_update.
*    ENDIF.

    mt_column_names[ c_field_name = 'EQUNR' ]-c_filter = x_equi_no.

    do_select_database( iv_limit = 10 iv_offset = 0 ).

    CLEAR mt_column_names[ c_field_name = 'EQUNR' ]-c_filter.

            MODIFY TABLE mt_dictionary FROM VALUE #( c_key = 'eezz_changed' c_value = 'eezzChanged' ).
        IF sy-subrc <> 0.
          INSERT VALUE #(  c_key = 'eezz_changed' c_value = 'eezzChanged' ) INTO TABLE mt_dictionary.
        ENDIF.

  ENDMETHOD.


  METHOD zif_eezz_table~get_column_names.

    CALL METHOD super->zif_eezz_table~get_column_names
      RECEIVING
        rt_column_names = rt_column_names.

*    TRY.
*        DATA(x_cell) = rt_column_names->*[ c_field_name = 'STATUS' ].
*      CATCH cx_sy_itab_line_not_found.
*        DATA: x_new_cell TYPE zstr_cell.
*        x_new_cell-c_value      = 'Status'.
*        x_new_cell-c_field_name = 'STATUS'.
*        x_new_cell-c_position   = 1.
*        x_new_cell-c_outputlen  = 4.
*        APPEND x_new_cell TO rt_column_names->*.
*        SORT rt_column_names->* BY c_position.
*
*        DATA: x_pos TYPE i VALUE 1.
*        LOOP AT rt_column_names->* ASSIGNING FIELD-SYMBOL(<fs_col_name>).
*          <fs_col_name>-c_position = x_pos.
*          ADD 1 TO x_pos.
*        ENDLOOP.
*
*    ENDTRY.

  ENDMETHOD.


  METHOD zif_eezz_table~get_row.

    rt_row = super->zif_eezz_table~get_row( iv_index ).

    IF rt_row IS BOUND.
      IF line_exists( rt_row->*[ c_field_name = 'ICONNAME' ] ).

        DATA(x_equnr)    = rt_row->*[ c_field_name = 'EQUNR' ].
        data(x_row_name) = |{ m_table_name }-{ x_equnr-c_value }| .
        ASSIGN rt_row->*[ c_field_name = 'ICON' ] TO FIELD-SYMBOL(<fs_cell>).

        MODIFY TABLE mt_dictionary FROM VALUE #( c_key = 'row_name' c_value = x_row_name ).
        IF sy-subrc <> 0.
          INSERT VALUE #(  c_key = 'row_name' c_value = x_row_name ) INTO TABLE mt_dictionary.
        ENDIF.

        <fs_cell>-c_type = 'status'.
        IF <fs_cell>-c_value IS NOT INITIAL.
          <fs_cell>-c_value = cl_bsp_mimes=>sap_icon( id = rt_row->*[ c_field_name = 'ICONNAME' ]-c_value ).
        ELSE.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
