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
    WHEN 'CABD'.
      PERFORM cabd.
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
  CLEAR:ls_headerdata-name1_new.
  CLEAR:ls_headerdata-rpdifn.
  CLEAR:ls_headerdata-diff_inv.
  ls_headerdata-iconname = icon_green_light.
  ls_headerdata-diff_inv = ls_headerdata-lifnr_new.
  SELECT SINGLE name1 FROM lfa1 WHERE lifnr = @ls_headerdata-lifnr_new INTO @ls_headerdata-name1_new.
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
*&---------------------------------------------------------------------*
*& Module CHECK_AMOUNTS_BASIC_DATA OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE check_amounts_basic_data INPUT.
  PERFORM cabd.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form cabd
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM cabd .
  CLEAR ls_headerdata-rpdifn.
  LOOP AT gt_out INTO DATA(wwout).
    ls_headerdata-rpdifn += wwout-miro_sum.
  ENDLOOP.
  ls_headerdata-rpdifn = ls_headerdata-gross_amount - ls_headerdata-rpdifn - ls_headerdata-wmwst.
  IF ls_headerdata-rpdifn NE 0.
    ls_headerdata-iconname = icon_red_light.
  ELSE.
    ls_headerdata-iconname = icon_green_light.
  ENDIF.
ENDFORM.
