class ZCL_APC_WSP_EXT_Z_EEZZ_WS_PUSH definition
  public
  inheriting from CL_APC_WSP_EXT_STATELESS_BASE
  final
  create public .

public section.

  interfaces IF_APC_WSP_BINDING_MANAGER .
  interfaces IF_APC_WS_BINDING_MANAGER .

  methods IF_APC_WSP_EXTENSION~ON_MESSAGE
    redefinition .
  methods IF_APC_WSP_EXTENSION~ON_START
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_APC_WSP_EXT_Z_EEZZ_WS_PUSH IMPLEMENTATION.


  METHOD if_apc_wsp_extension~on_message.
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
  ENDMETHOD.


  METHOD if_apc_wsp_extension~on_start.

    TRY.
* send the message on WebSocket connection
        DATA(lo_message) = i_message_manager->create_message( ).

        "lo_message->set_text( |{ sy-mandt }/{ sy-uname }: ON_START has been successfully executed !| ).
        "i_message_manager->send( lo_message ).

        DATA(lo_binding) = i_context->get_binding_manager( ).
        lo_binding->bind_amc_message_consumer(
          i_application_id = 'Z_EEZZ_WS_MSG_CHANNEL'
          i_channel_id      = '/eezz' ).

      CATCH cx_apc_error INTO DATA(lx_apc_error).
        MESSAGE lx_apc_error->get_text( ) TYPE 'E'.
    ENDTRY.

  ENDMETHOD.


  method IF_APC_WS_BINDING_MANAGER~BIND_AMC_MESSAGE_CONSUMER.
  endmethod.


  method IF_APC_WS_BINDING_MANAGER~UNBIND_AMC_MESSAGE_CONSUMER.
  endmethod.
ENDCLASS.
