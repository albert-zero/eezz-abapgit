class ZCL_APC_WSP_EXT_Z_EEZZ_WS_PUS2 definition
  public
  inheriting from CL_APC_WSP_EXT_STATEFUL_BASE
  final
  create public .

public section.

  interfaces IF_AMC_MESSAGE_RECEIVER .
  interfaces IF_AMC_MESSAGE_RECEIVER_PCP .

  methods IF_APC_WSP_EXTENSION~ON_MESSAGE
    redefinition .
  methods IF_APC_WSP_EXTENSION~ON_START
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_APC_WSP_EXT_Z_EEZZ_WS_PUS2 IMPLEMENTATION.


  METHOD if_amc_message_receiver_pcp~receive.

    zcl_eezz_agent=>zif_eezz_agent~on_event(
         EXPORTING
           i_message         = i_message
           i_context         = i_context ).

  ENDMETHOD.


  method IF_APC_WSP_EXTENSION~ON_MESSAGE.
    TRY.

        DATA(lo_message) = i_message_manager->create_message( ).
* retrieve the text message
        DATA(lv_text) = i_message->get_text( ).

* pass request to agent
        zcl_eezz_agent=>zif_eezz_agent~on_websocket(
          EXPORTING
            i_message         = i_message
            i_message_manager = i_message_manager
            i_context         = i_context ).

** send 1st message
*        lo_message->set_text( |{ sy-mandt }/{ sy-uname }: ON_MESSAGE has been successfully executed !| ).
*        i_message_manager->send( lo_message ).
*
** send 2nd message, i.e. echo the incoming message
*        lo_message->set_text( lv_text ).
*        i_message_manager->send( lo_message ).

      CATCH cx_apc_error INTO DATA(lx_apc_error).

        MESSAGE lx_apc_error->get_text( ) TYPE 'E'.

    ENDTRY.
  endmethod.


  METHOD if_apc_wsp_extension~on_start.
    TRY.
* send the message on WebSocket connection
        DATA(lo_message) = i_message_manager->create_message( ).

        "lo_message->set_text( |{ sy-mandt }/{ sy-uname }: ON_START has been successfully executed !| ).
        "i_message_manager->send( lo_message ).

        DATA(lo_binding) = i_context->get_binding_manager( ).
*        lo_binding->bind_amc_message_consumer(
*          i_application_id = 'Z_EEZZ_WS_MSG_CHANNEL'
*          i_channel_id      = '/eezz' ).
*
*        lo_binding = i_context->get_binding_manager( ).
*        lo_binding->bind_amc_message_consumer(
*                     i_application_id = '/ISDFPS/LM'
*                     i_channel_id     = '/ise' ).
         data(x_conn_id) = i_context->GET_CONNECTION_ID( ).
         cl_amc_channel_manager=>create_message_consumer(
                   i_application_id = '/ISDFPS/LM'
                   i_channel_id     = '/ise'
*                   I_CHANNEL_EXTENSION_ID =
            )->start_message_delivery( i_receiver = me ).
*
*        lo_binding->bind_amc_message_consumer(
*                   i_application_id = '/ISDFPS/LM'
*                   i_channel_id     = '/lm/admin'
*                  ).
*        lo_binding->bind_amc_message_consumer(
*                       i_application_id = '/ISDFPS/LM'
*                       i_channel_id     = '/lm/stat'
*                      ).

      CATCH cx_apc_error INTO DATA(lx_apc_error).
        MESSAGE lx_apc_error->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
