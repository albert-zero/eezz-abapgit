@EndUserText.label : 'dhash table for string-key to string-value'
@AbapCatalog.enhancementCategory : #EXTENSIBLE_ANY
define type zstr_update {
  c_key    : abap.string(0);
  c_value  : abap.string(0);
  priority : int4;

}