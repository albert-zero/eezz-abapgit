@EndUserText.label : 'json structure'
@AbapCatalog.enhancementCategory : #EXTENSIBLE_ANY
define type zstr_eezz_json {
  @EndUserText.label : 'object, array, str, num'
  c_type  : abap.string(0);
  @EndUserText.label : 'name attribute'
  c_key   : abap.string(0);
  @EndUserText.label : 'value attribute'
  c_value : abap.string(0);
  c_ref   : reference to data;

}