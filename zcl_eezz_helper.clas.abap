class ZCL_EEZZ_HELPER definition
  public
  final
  create public .

public section.

  class-methods CREATE_HASH_FOR_TABLE_KEY
    importing
      !IT_ROWS type ref to ZTTY_EEZZ_ROW
      !IV_LINE type ANY
      !IV_FIELD type STRING optional
    returning
      value(RV_HASH) type STRING .
  class-methods CREATE_APC_URL
    returning
      value(R_URL) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_EEZZ_HELPER IMPLEMENTATION.


  METHOD create_apc_url.

    DATA: lv_hostname      TYPE char100,
          lv_full_hostname TYPE string,
          lv_https_port    TYPE string,
          lv_path          TYPE apc_appl_path.

*    lv_hostname = sy-host.
*    CALL FUNCTION 'SDC_FULL_HOSTNAME'
*      EXPORTING
*        iv_hostname      = lv_hostname
*      IMPORTING
*        ev_full_hostname = lv_full_hostname.

    CALL FUNCTION 'TH_GET_VIRT_HOST_DATA'
      EXPORTING
        protocol       = 2
        virt_idx       = 0
*       LOCAL          = 1
      IMPORTING
        hostname       = lv_full_hostname
        port           = lv_https_port
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE x021(zcl_eezz_message).
    ENDIF.

    SELECT SINGLE path INTO lv_path FROM apc_appl
      WHERE application_id = 'Z_EEZZ_WS_PUSH_CHANNEL'
        AND version = 'A'.
    IF sy-subrc <> 0.
      MESSAGE x020(zcl_eezz_message).
    ENDIF.

    CONCATENATE 'wss://' lv_full_hostname ':' lv_https_port lv_path INTO r_url.

  ENDMETHOD.


  method create_hash_for_table_key.

    data: x_digest          type ref to cl_abap_message_digest,
          x_converter       type ref to cl_abap_conv_out_ce,
          x_data_as_xstring type xstring,
          x_genkey          type string.

    loop at it_rows->* into data(x_wa_col) where c_keyflag eq abap_true.
      assign component x_wa_col-c_field_name of structure iv_line to field-symbol(<fs_value>).

      if sy-subrc eq 0.
        concatenate x_genkey <fs_value> into x_genkey.
      endif.
    endloop.

    if iv_field is not initial.
      concatenate x_genkey iv_field into x_genkey.
    endif.

    try.
        x_digest = cl_abap_message_digest=>get_instance( 'sha1' ).

        "update digest with input
        x_converter = cl_abap_conv_out_ce=>create( ).
        x_converter->convert(
          exporting
            data   = x_genkey
          importing
            buffer = x_data_as_xstring
        ).
        x_digest->update( if_data = x_data_as_xstring ).


        "finalise digest
        x_digest->digest( ).
        rv_hash = x_digest->to_base64( ).

      catch cx_abap_message_digest.
        clear rv_hash.
      catch cx_sy_conversion_codepage
            cx_parameter_invalid_type.
        clear rv_hash.
    endtry.


  endmethod.
ENDCLASS.
