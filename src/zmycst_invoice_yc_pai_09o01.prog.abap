*----------------------------------------------------------------------*
***INCLUDE ZMYCST_INVOICE_YC_PAI_09O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0900  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0900 INPUT.
  DATA:ok_code LIKE sy-ucomm.
  DATA:save_ok LIKE sy-ucomm.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR '&F03' OR '&F15' OR '&F12'.
      LEAVE TO SCREEN 0.
    WHEN 'MIR7'.
      PERFORM mir7.
    WHEN 'CALC'.
      PERFORM calc.
  ENDCASE.
  CLEAR:sy-ucomm.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  CASE sy-ucomm.
    WHEN 'BUTITEM'.
      gv_subscreen_900 = '9002'.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0902  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0902 INPUT.
  CASE sy-ucomm.
    WHEN 'BUTHEAD'.
      gv_subscreen_900 = '9001'.
  ENDCASE.
ENDMODULE.

FORM calc.
  CLEAR:ls_headerdata-wmwst,ls_headerdata-gross_amount,ttax.
  ls_headerdata-diff_inv = ls_headerdata-lifnr_new.
  LOOP AT gt_out INTO DATA(wout).
    ls_headerdata-wmwst += wout-miro_sum_se.
    ls_headerdata-gross_amount += wout-miro_sum_hs.
    CLEAR wtax.
    wtax-shkzg      = ''.
    wtax-tax_code   = wout-mwskz.
    wtax-tax_amount = wout-miro_sum_se.
    COLLECT wtax INTO ttax.
  ENDLOOP.

ENDFORM.
