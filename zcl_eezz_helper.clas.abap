class ZCL_EEZZ_HELPER definition
  public
  final
  create public .

public section.

  class-methods CREATE_HASH_FOR_TABLE_KEY
    importing
      !IT_ROWS type ref to ZTTY_EEZZ_ROW
      !IV_LINE type ANY optional
      !IV_PATH type STRING optional
      !IV_CLEAR type ABAP_BOOL default ABAP_FALSE
    returning
      value(RV_HASH) type STRING .
  class-methods CREATE_APC_URL
    returning
      value(R_URL) type STRING .
protected section.
private section.

  class-data MT_HASHVIEW type ref to DATA .
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
    types: begin of thash_struct,
             c_key   type string,
             c_value type string,
           end of thash_struct.

    types thash_tbl type table of thash_struct with key c_key initial size 0.
    data  xtbl_hash type ref to thash_tbl.

    data: x_digest          type ref to cl_abap_message_digest,
          x_converter       type ref to cl_abap_conv_out_ce,
          x_data_as_xstring type xstring,
          x_genkey          type string.

    if mt_hashview is initial.
      xtbl_hash   = new thash_tbl( ).
      mt_hashview = xtbl_hash.
    endif.

    "FIELD-SYMBOLS <fs> type any table.
    "assign mt_hashview->* to <fs>.
    xtbl_hash = cast thash_tbl( mt_hashview ).

    if iv_path is not initial.
      x_genkey = iv_path.
    endif.

    if it_rows is not initial and iv_line is not initial.
      loop at it_rows->* into data(x_wa_col) where c_keyflag eq abap_true.
        assign component x_wa_col-c_field_name of structure iv_line to field-symbol(<fs_value>).

        if sy-subrc eq 0.
          if x_genkey is initial.
            x_genkey = |{ <fs_value> }|.
          else.
            data(x_genfield) = |{ <fs_value> }|.
            concatenate x_genkey x_genfield into x_genkey separated by '/'.
          endif.
        endif.
      endloop.
    endif.

    if iv_clear = abap_true.
      rv_hash = x_genkey.
      return.
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

        append value #( c_key = rv_hash c_value = x_genkey ) to xtbl_hash->*.
      catch cx_abap_message_digest.
        clear rv_hash.
      catch cx_sy_conversion_codepage
            cx_parameter_invalid_type.
        clear rv_hash.
    endtry.


  endmethod.
ENDCLASS.
