PROCESS BEFORE OUTPUT.
* MODULE STATUS_0110.

*
PROCESS AFTER INPUT.

  CHAIN.
    FIELD ls_headerdata-gross_amount.
    FIELD ls_headerdata-wmwst.
    MODULE check_amounts_basic_data ON CHAIN-REQUEST.
  ENDCHAIN.
