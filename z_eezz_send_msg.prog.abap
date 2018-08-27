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
      data(x_name)   = x_table->get_hash( iv_line =  x_result_table[ 1 ] ).

      x_data = value #(
        ( name = 'status' value = lv_text )
      ).

      x_table->send_message_pcp(
        iv_event      = |TriggerChange|
        iv_line       = x_result_table[ 1 ]
        it_fields     = ref #( x_data ) ).

    catch cx_amc_error into data(x_exception).
      cl_demo_output=>display( x_exception->get_text( ) ).
  endtry.
endif.
