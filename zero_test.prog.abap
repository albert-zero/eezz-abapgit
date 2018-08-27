*&---------------------------------------------------------------------*
*& Report ZERO_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZERO_TEST.

data xStr type string value 'test'.

select * from spfli into table @data(xresult) up to 10 rows.
data(x_message) = cl_ac_message_type_pcp=>create( ).
x_message->set_field( i_name = |key1| i_value = |value1| ).
x_message->set_field( i_name = |key2| i_value = |value2| ).
cl_demo_output=>DISPLAY( x_message->serialize( ) ).
cl_demo_output=>DISPLAY( xresult ).
write | \{ "callback": "{ xStr }.do_sort"   | .
