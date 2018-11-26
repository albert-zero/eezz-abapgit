class ZCL_EEZZ_DRIVE definition
  public
  final
  create public
  shared memory enabled .

public section.

  interfaces IF_HTTP_EXTENSION .
  interfaces IF_SHM_BUILD_INSTANCE .

  data M_CONTENT type XSTRING .

  methods GET
    returning
      value(P_CONTENT) type XSTRING .
  methods CONSTRUCTOR .
  methods SET
    importing
      !P_CONTENT type XSTRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_EEZZ_DRIVE IMPLEMENTATION.


  method constructor.
  endmethod.


  method GET.
    p_content = m_content.
  endmethod.


  method if_http_extension~handle_request.

    try.
        " get the data from shared memory segment
        try.
            data(x_handle) = zcl_eezz_shm=>attach_for_read( ).
            data(x_result) = x_handle->root->get(  ).
            x_handle->detach( ).
          catch cx_root into data(xex98).
            if x_handle is bound.
              x_handle->detach( ).
            endif.
            return.
        endtry.

        server->response->set_content_type( |application/zip|  ).
        server->response->set_data( data = x_result ).

      catch cx_root.
    endtry.
  endmethod.


  method IF_SHM_BUILD_INSTANCE~BUILD.
  endmethod.


  method SET.
    m_content = p_content.
  endmethod.
ENDCLASS.
