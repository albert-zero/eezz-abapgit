*&---------------------------------------------------------------------*
*& Report Z_EEZZ_SEND_MSG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report z_eezz_send_msg.
data: lv_text type string value 'ok'.

parameters:
  p_text type c length 20 default 'ok' LOWER CASE.


data: x_dict type ztty_dictionary.
data: x_data type pcp_fields.

if p_text is not initial.
  lv_text = p_text.
  try.

      select * from sflight order by carrid ascending into table @data(x_result_table)  up to 10 rows.
      data(x_table)  = new zcl_eezz_table( table_name = |SFLIGHT| ).

      x_data = value #(
        ( name = |status.{ lv_text }| value = |*| )
      ).

      x_table->send_message_pcp(
        it_fields     = ref #( x_data )
        iv_line       = x_result_table[ 1 ] ).

    catch cx_amc_error cx_root into data(x_exception).
      cl_demo_output=>display( x_exception->get_text( ) ).
  endtry.
endif.
