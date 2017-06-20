class zcl_eezz_tree definition
  public
  inheriting from zcl_eezz_table
  create public .

  public section.

    methods constructor .

    methods get_selected_obj
        redefinition .
    methods get_row
        redefinition .
    methods do_select
        redefinition .
  protected section.
private section.

  data M_PATH type STRING .
ENDCLASS.



CLASS ZCL_EEZZ_TREE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TREE->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method constructor.
    types: begin of tstr_dir,
             c_name type string,
             c_attr type string,
             c_type type string,
           end of tstr_dir.

    types ttbl_dir type sorted table of tstr_dir with unique key c_name.
    data  ttbl_dir type ttbl_dir.

    data x_wa_column type zstr_cell.

    super->constructor( ).

    " The root node has a fixed id
    m_path = |id000000|.

    field-symbols <fs_table> type standard table.
    create data mt_table type table of tstr_dir.
    assign mt_table->* to <fs_table>.

    " The tree consists of a visible table and additonal attributes for each entry
    " For simplicity put in one table
    " The mt_column_names would acces the first two columns
    <fs_table> = value ttbl_dir(
      ( c_name = 'a' c_attr =  '1' c_type = 'directory' )
      ( c_name = 'b' c_attr =  '2' c_type = 'directory' )
      ( c_name = 'c' c_attr =  '3' c_type = 'directory' )
      ( c_name = 'd' c_attr =  '4' c_type = 'file' )
      ( c_name = 'e' c_attr =  '5' c_type = 'file' )
      ( c_name = 'f' c_attr =  '6' c_type = 'file' )
      ( c_name = 'g' c_attr =  '7' c_type = 'file' ) ).

    mt_column_names = value #(
      ( c_field_name = 'c_name' c_value = 'name'      c_sort = 0 C_POSITION = 1 )
      ( c_field_name = 'c_attr' c_value = 'attribute' c_sort = 0 C_POSITION = 2 ) ).
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TREE->ZIF_EEZZ_TABLE~DO_SELECT
* +-------------------------------------------------------------------------------------------------+
* | [--->] INDEX                          TYPE        I (default =1)
* | [--->] PATH                           TYPE        STRING(optional)
* | [<-()] RT_EEZZ_TABLE                  TYPE REF TO ZIF_EEZZ_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method do_select.
    " Prepare mt_table according to the path
    " in most cases this would be recursive
    data(x_enc) = cl_http_utility=>unescape_url( path ).
    split x_enc  at ':' into data(x_name) data(x_path).
    split x_path at '/' into table data(x_path_elements).

    " Navigate to the spot:
    loop at x_path_elements into data(xwa_path).
      if sy-tabix = lines( x_path_elements ).
        " execute like: ls ./<path>
        " x_processor->set_expression( |node() | ).
        " loop at result and add either eezz-node or eezz-leaf attributes
      else.
        " navigate like: cd ./<path>
        " x_processor->set_expression( |node()[{ xwa_path_as_index }]| ).
      endif.
    endloop.

    " set mt_table to prepare for zif_eezz_table~get_row callback
    m_path = x_path.
    rt_eezz_table ?= me.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TREE->ZIF_EEZZ_TABLE~GET_ROW
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INDEX                       TYPE        I
* | [<-()] RT_ROW                         TYPE REF TO ZTTY_EEZZ_ROW
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_row.
    "data x_index type i.

    rt_row = super->get_row( iv_index ).
    if m_selected = 0.
      return.
    endif.

    FIELD-SYMBOLS <fs_table> type table.
    assign mt_table->* to <fs_table>.

    " The class appends a row attribute
    try.
      field-symbols <fs_line>  type any.
      field-symbols <fs_value>  type any.
      data x_row_def type zstr_cell.

      read table <fs_table> assigning <fs_line> index iv_index.
      if <fs_line>  is not ASSIGNED.
        return.
      endif.

      assign component 'c_type' of structure <fs_line> to <fs_value>.

      " generate a path as unique id for the tree
      " the framework converts the c_value into an id attribute
      x_row_def-c_type  = <fs_value>.
      x_row_def-c_value = |{ m_path }/sub{ m_selected }|.

      append x_row_def to rt_row->*.
    catch cx_root.
    endtry.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EEZZ_TREE->ZIF_EEZZ_TABLE~GET_SELECTED_OBJ
* +-------------------------------------------------------------------------------------------------+
* | [--->] INDEX                          TYPE        INTEGER (default =-1)
* | [--->] TABLE_NAME                     TYPE        STRING(optional)
* | [--->] VISIBLE_ITEMS                  TYPE        INTEGER (default =20)
* | [--->] VISIBLE_BLOCK                  TYPE        INTEGER (default =20)
* | [<-()] RT_EEZZ_TABLE                  TYPE REF TO ZIF_EEZZ_TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_selected_obj.
    if index = -1.
      rt_eezz_table = me.
    endif.
  endmethod.
ENDCLASS.