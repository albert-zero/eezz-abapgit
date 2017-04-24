interface ZIF_EEZZ_TABLE
  public .


  data M_VISIBLE_BLOCK type INT4 .
  data M_VISIBLE_ITEMS type INT4 .

  methods GET_SELECTED_OBJ
    importing
      !INDEX type INTEGER default -1
      !VISIBLE_ITEMS type INTEGER default 20
      !VISIBLE_BLOCK type INTEGER default 20
    returning
      value(RT_EEZZ_TABLE) type ref to ZIF_EEZZ_TABLE .
  methods DO_SELECT_DATABASE .
  methods GET_ROW
    importing
      !IV_INDEX type I
    returning
      value(RT_ROW) type ref to ZTTY_EEZZ_ROW .
  methods GET_COLUMN_NAMES
    returning
      value(RT_COLUMN_NAMES) type ref to ZTTY_EEZZ_ROW .
  methods GET_DICTIONARY
    returning
      value(RT_DICTIONARY) type ref to ZTTY_DICTIONARY .
  methods DO_NAVIGATE
    importing
      !WHERE type INT4 .
  methods HAS_CHANGED
    returning
      value(RV_HAS_CHANGED) type XFELD .
  methods DO_SORT
    importing
      !INDEX type INT4 .
  methods DO_SELECT
    importing
      !INDEX type I
    returning
      value(RT_EEZZ_TABLE) type ref to ZIF_EEZZ_TABLE .
endinterface.