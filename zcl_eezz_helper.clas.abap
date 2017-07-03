class ZCL_EEZZ_HELPER definition
  public
  final
  create public .

public section.

  class-methods CREATE_HASH_FOR_TABLE_KEY
    importing
      !IT_ROWS type ref to ZTTY_EEZZ_ROW
      !IV_LINE type ANY
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


  METHOD create_hash_for_table_key.

    DATA: x_digest          TYPE REF TO cl_abap_message_digest,
          x_converter       TYPE REF TO cl_abap_conv_out_ce,
          x_data_as_xstring TYPE xstring,
          x_genkey          TYPE string.

    LOOP AT it_rows->* INTO DATA(x_wa_col) WHERE c_keyflag EQ abap_true.
      ASSIGN COMPONENT x_wa_col-c_field_name OF STRUCTURE iv_line TO FIELD-SYMBOL(<fs_value>).
      IF sy-subrc EQ 0.
        CONCATENATE x_genkey <fs_value> INTO x_genkey.
      ENDIF.
    ENDLOOP.

    TRY.
        x_digest = cl_abap_message_digest=>get_instance( 'sha1' ).

        "update digest with input
        x_converter = cl_abap_conv_out_ce=>create( ).
        x_converter->convert(
          EXPORTING
            data   = x_genkey
          IMPORTING
            buffer = x_data_as_xstring
        ).
        x_digest->update( if_data = x_data_as_xstring ).

        "finalise digest
        x_digest->digest( ).
        rv_hash = x_digest->to_base64( ).

      CATCH cx_abap_message_digest.
        CLEAR rv_hash.
      CATCH cx_sy_conversion_codepage
            cx_parameter_invalid_type.
        CLEAR rv_hash.
    ENDTRY.


  ENDMETHOD.
ENDCLASS.
