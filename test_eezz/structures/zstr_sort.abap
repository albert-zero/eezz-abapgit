@EndUserText.label : 'Sort'
@AbapCatalog.enhancementCategory : #EXTENSIBLE_ANY
define type zstr_sort {
  c_field_name : fieldname;
  c_sort       : abap.string(0);
  c_do_sort    : char01;

}