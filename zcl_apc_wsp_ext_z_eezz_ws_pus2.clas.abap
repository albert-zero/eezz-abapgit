class ZCL_APC_WSP_EXT_Z_EEZZ_WS_PUS2 definition
  public
  inheriting from CL_APC_WSP_EXT_STATEFUL_BASE
  final
  create public .

public section.

  interfaces IF_AMC_MESSAGE_RECEIVER .
  interfaces IF_AMC_MESSAGE_RECEIVER_PCP .
  interfaces IF_AMC_MESSAGE_RECEIVER_TEXT .

  methods IF_APC_WSP_EXTENSION~ON_MESSAGE
    redefinition .
  methods IF_APC_WSP_EXTENSION~ON_START
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_APC_WSP_EXT_Z_EEZZ_WS_PUS2 IMPLEMENTATION.


  METHOD if_amc_message_receiver_pcp~receive.
  ENDMETHOD.


  method IF_AMC_MESSAGE_RECEIVER_TEXT~RECEIVE.
  endmethod.


  method IF_APC_WSP_EXTENSION~ON_MESSAGE.
    TRY.

        DATA(lo_message) = i_message_manager->create_message( ).

*       pass request to agent
        zcl_eezz_agent=>zif_eezz_agent~on_websocket(
          EXPORTING
            i_message         = i_message
            i_message_manager = i_message_manager
            i_context         = i_context ).


      CATCH cx_apc_error INTO DATA(lx_apc_error).

        MESSAGE lx_apc_error->get_text( ) TYPE 'E'.

    ENDTRY.
  endmethod.


  method if_apc_wsp_extension~on_start.
    try.
*       send the message on WebSocket connection
        data(lo_message) = i_message_manager->create_message( ).

        "lo_message->set_text( |{ sy-mandt }/{ sy-uname }: ON_START has been successfully executed !| ).
        "i_message_manager->send( lo_message ).

        data(lo_binding) = i_context->get_binding_manager( ).
        data(x_conn_id)  = i_context->get_connection_id( ).

        data(x_msg_cons) = cl_amc_channel_manager=>create_message_consumer(
                              i_application_id = 'Z_EEZZ_WS_MSG_CHANNEL'
                              i_channel_id     = '/eezz').
        x_msg_cons->start_message_delivery( i_receiver = me ).
        "cl_amc_channel_manager=>create_message_consumer(
        "           i_application_id = 'Z_EEZZ_WS_MSG_CHANNEL'
        "           i_channel_id     = '/eezz'
        "    )->start_message_delivery( i_receiver =  me ).
*
        lo_binding->bind_amc_message_consumer(
                   i_application_id = 'Z_EEZZ_WS_MSG_CHANNEL'
                   i_channel_id     = '/eezz'
                  ).
*        lo_binding->bind_amc_message_consumer(
*                       i_application_id = '/ISDFPS/LM'
*                       i_channel_id     = '/lm/stat'
*                      ).

      catch cx_apc_error into data(lx_apc_error).
        message lx_apc_error->get_text( ) type 'E'.
      catch cx_root into data(lx_root).
        data(xl_text) = lx_root->get_text( ).
        message lx_root->get_text( ) type 'E'.
    endtry.
  endmethod.
ENDCLASS.
