*----------------------------------------------------------------------*
***INCLUDE ZMMXFT_INVOICE_IN_LIST_USERI01.
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
  ENDCASE.
  CLEAR:sy-ucomm.
ENDMODULE.
