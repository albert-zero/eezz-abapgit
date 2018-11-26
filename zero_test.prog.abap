*&---------------------------------------------------------------------*
*& Report ZERO_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*


report zero_test.

    types: begin of tstr_line,
             _eezz_row_cell_ type string,
             inx             type x length 1,
             name            type string,
           end of tstr_line.

    types: tty_table type table of tstr_line with key inx initial size 0.
    data x_tab type tty_table.
    data(x_out) = cl_demo_output=>new( ).

    TRY.
      x_tab[ inx = 1 ] = value #( name = 'test' ).
    CATCH cx_ai_system_fault.
    catch cx_root into data(x_ex).
      x_ex->GET_SOURCE_POSITION( importing PROGRAM_NAME = data(x_prg) source_line = data(x_line) ).
      x_out->write( |{ x_prg } line: { x_line } { x_ex->get_text( ) }| ).
    endtry.


    x_out->display(  ).
