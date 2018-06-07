@EndUserText.label : 'ZSTR_SFLIGHT'
@AbapCatalog.enhancementCategory : #EXTENSIBLE_CHARACTER_NUMERIC
define type zstr_sflight {
  mandt      : s_mandt;
  carrid     : s_carr_id;
  connid     : s_conn_id;
  fldate     : s_date;
  @Semantics.amount.currencyCode : 'zstr_sflight.currency'
  price      : s_price;
  currency   : s_currcode;
  planetype  : s_planetye;
  seatsmax   : s_seatsmax;
  seatsocc   : s_seatsocc;
  @Semantics.amount.currencyCode : 'zstr_sflight.currency'
  paymentsum : s_sum;
  seatsmax_b : s_smax_b;
  seatsocc_b : s_socc_b;
  seatsmax_f : s_smax_f;
  seatsocc_f : s_socc_f;

}