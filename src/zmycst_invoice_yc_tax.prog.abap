*&---------------------------------------------------------------------*
*& Include ZMYCST_INVOICE_YC_TAX
*&---------------------------------------------------------------------*

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_TAXDATA' ITSELF
CONTROLS: tc_taxdata TYPE TABLEVIEW USING SCREEN 9203.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_TAXDATA'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc_taxdata_change_tc_attr OUTPUT.
  DESCRIBE TABLE ttax LINES tc_taxdata-lines.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TC_TAXDATA'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE tc_taxdata_modify INPUT.
  MODIFY ttax
    FROM wtax
    INDEX tc_taxdata-current_line.
  CLEAR ls_headerdata-wmwst.
  LOOP AT ttax INTO DATA(lstax).
    ls_headerdata-wmwst += lstax-tax_amount.
  ENDLOOP.
  PERFORM cabd.
ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'TC_TAXDATA'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tc_taxdata_mark INPUT.
  DATA: g_TC_TAXDATA_wa2 LIKE LINE OF ttax.
  IF tc_taxdata-line_sel_mode = 1
  AND wtax-sel = 'X'.
    LOOP AT ttax INTO g_TC_TAXDATA_wa2
      WHERE sel = 'X'.
      g_TC_TAXDATA_wa2-sel = ''.
      MODIFY ttax
        FROM g_TC_TAXDATA_wa2
        TRANSPORTING sel.
    ENDLOOP.
  ENDIF.
  MODIFY ttax
    FROM wtax
    INDEX tc_taxdata-current_line
    TRANSPORTING sel.
ENDMODULE.
