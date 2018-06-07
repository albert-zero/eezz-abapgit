@EndUserText.label : 'messages for output in HTML'
@AbapCatalog.enhancementCategory : #EXTENSIBLE_ANY
define type zstr_message {
  c_msgtext : abap.string(0);
  c_msgcls  : abap.string(0);
  c_msgnum  : abap.int1;

}