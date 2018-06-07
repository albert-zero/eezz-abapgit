@EndUserText.label : 'Used to transport row and column info'
@AbapCatalog.enhancementCategory : #EXTENSIBLE_ANY
define type zstr_cell {
  c_type       : abap.string(0);
  c_value      : abap.string(0);
  c_field_name : fieldname;
  position     : abap.int4;
  c_node       : reference to if_ixml_node;

}
