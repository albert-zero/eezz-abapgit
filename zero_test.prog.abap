*&---------------------------------------------------------------------*
*& Report ZERO_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*


report zero_test.

    types: begin of tstr_line,
             inx             type i,
             name            type string,
           end of tstr_line.

    types: tty_table type table of tstr_line with key inx initial size 0.
    data x_tab type tty_table.

    if line_exists( x_tab[ 1 ] ).
      append value #( inx = 2 name = 'n2' ) to x_tab.
    else.
      append value #( inx = 1 name = 'n1' ) to x_tab.
      append value #( inx = 2 name = 'n2' ) to x_tab.
    endif.

    data(x_out) = cl_demo_output=>new( ).

    data xref1 type ref to data.
    create data xref1 type x length 1000.
    "FIELD-SYMBOLS <fs> type ref to x.

    data xxstr    type xstring.
    data xxbyte   type x length 100.
    data xxbyte1  type x.
    FIELD-SYMBOLS <fs> type x.

    xxstr = '494949'.
    xxbyte+4 = xxstr.
    xxbyte+8 = xxstr.

assign xref1->* to <fs>.
<fs>+4 = xxstr.

data(lv_buf) = conv xstring( xxstr ).

    data(xlen) = xstrlen( xxstr ).
    data(xzip) = new cl_abap_zip( ).
    " xzip->load( zip = xxstr ).

    TRY.
      x_tab[ inx = 1 ] = value #( name = 'test' ).
    CATCH cx_ai_system_fault.
    catch cx_root into data(x_ex).
      x_ex->GET_SOURCE_POSITION( importing PROGRAM_NAME = data(x_prg) source_line = data(x_line) ).
      x_out->write( |{ x_prg } line: { x_line } { x_ex->get_text( ) }| ).
    endtry.

    x_out->write_data( x_tab ).
    x_out->display(  ).
