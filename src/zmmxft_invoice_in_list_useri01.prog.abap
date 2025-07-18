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
    WHEN 'MIRO'.
      PERFORM miro.
  ENDCASE.
  CLEAR:sy-ucomm.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form miro
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM miro .
  CLEAR t_dataList.
  CALL METHOD alv_grid_head->get_selected_rows( IMPORTING et_row_no = DATA(lt_row) ).
  LOOP AT lt_row ASSIGNING FIELD-SYMBOL(<lt_row>).
    READ TABLE output-body-data_list ASSIGNING FIELD-SYMBOL(<data_list>) INDEX <lt_row>-row_id.
    APPEND <data_list> TO t_dataList.
  ENDLOOP.
  IF sy-subrc NE 0.
    MESSAGE e000(oo) WITH '请选择要核销的发票明细'.
  ELSE.
    EXPORT invoice = t_dataList TO MEMORY ID 'XFT_INVOICE_IN'.
    SET PARAMETER ID 'BUK' FIELD p_bukrs.
    CALL TRANSACTION 'MIRO' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.
