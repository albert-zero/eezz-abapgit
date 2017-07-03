interface ZIF_EEZZ_TABLE
  public .


  data M_TABLE_DDIC type STRING .
  data M_TABLE_NAME type STRING .
  data M_SELECTED type I .
  data M_OFFSET type INT4 .
  data M_TOPDOWN type INT4 .
  data M_VISIBLE_BLOCK type INT4 .
  data M_VISIBLE_ITEMS type INT4 .
  data MT_UPDATE type ZTTY_UPDATE .

  methods CREATE_NODE
    importing
      !IV_SYMBOLS type ref to ZTTY_SYMBOLS
    returning
      value(IO_TABLE_NODE) type ref to IF_IXML_NODE .
  methods GET_SELECTED_OBJ
    importing
      !INDEX type INTEGER default -1
      !TABLE_NAME type STRING optional
      !VISIBLE_ITEMS type INTEGER default 20
      !VISIBLE_BLOCK type INTEGER default 20
    returning
      value(RT_EEZZ_TABLE) type ref to ZIF_EEZZ_TABLE .
  methods DO_SELECT_DATABASE
    importing
      !IV_LIMIT type INT4 optional
      !IV_OFFSET type INT4 optional .
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
      !WHERE type INT4
      !POS type INT4 optional .
  methods HAS_CHANGED
    returning
      value(RV_HAS_CHANGED) type XFELD .
  methods DO_SORT
    importing
      !INDEX type INT4 .
  methods DO_SELECT
    importing
      !INDEX type I default 1
      !PATH type STRING optional
    returning
      value(RT_EEZZ_TABLE) type ref to ZIF_EEZZ_TABLE .
  methods GET_UPDATE
    returning
      value(RT_UPDATE) type ref to ZTTY_UPDATE .
endinterface.
