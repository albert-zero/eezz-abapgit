interface ZIF_EEZZ_AGENT
  public .


  class-data M_EEZZ_AGENT type ref to ZIF_EEZZ_AGENT .

  class-methods ON_WEBSOCKET
    importing
      !I_MESSAGE type ref to IF_APC_WSP_MESSAGE
      !I_MESSAGE_MANAGER type ref to IF_APC_WSP_MESSAGE_MANAGER
      !I_CONTEXT type ref to IF_APC_WSP_SERVER_CONTEXT .
  methods ON_BINARY_TRANSFER .
endinterface.