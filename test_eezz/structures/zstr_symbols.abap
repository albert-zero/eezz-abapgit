@EndUserText.label : 'map object to name'
@AbapCatalog.enhancementCategory : #EXTENSIBLE_ANY
define type zstr_symbols {
  c_name       : abap.string(0);
  c_object     : reference to zif_eezz_table;
  c_tag        : abap.string(0);
  c_ref_node   : reference to if_ixml_node;
  c_templ_node : reference to if_ixml_node;
  c_eezz_json  : reference to zcl_eezz_json;

}