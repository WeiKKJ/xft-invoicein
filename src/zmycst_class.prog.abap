*&---------------------------------------------------------------------*
*& 包含               ZMMXFT_INVOICE_CLASS
*&---------------------------------------------------------------------*
CLASS lcl_event DEFINITION .
  PUBLIC SECTION.

    METHODS:
      handle_double_click_head FOR EVENT double_click  OF cl_gui_alv_grid"双击事件
        IMPORTING e_row e_column es_row_no,
      handle_data_changed_head FOR EVENT data_changed OF cl_gui_alv_grid"数据改动事件
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

ENDCLASS .

CLASS lcl_event IMPLEMENTATION.
  METHOD handle_double_click_head.      "双击事件
    CHECK es_row_no-row_id GT 0.
    READ TABLE output-body-data_list INTO w_dataList INDEX es_row_no-row_id.
    MODIFY w_dataList-details FROM w_details TRANSPORTING fphm WHERE fphm IS INITIAL.
    CASE e_column-fieldname.
      WHEN 'SJURL_PDF' OR 'FTPNAME'.
        PERFORM showpdf USING w_dataList-xfmc w_dataList-ftpname.
      WHEN 'BELNR' OR 'GJAHR' OR 'BUKRS'.
        IF w_dataList-bukrs IS NOT INITIAL.
          PERFORM fb03 IN PROGRAM zpubform IF FOUND USING w_dataList-belnr w_dataList-gjahr w_dataList-bukrs ''.
        ELSE.
          PERFORM mir4 IN PROGRAM zpubform IF FOUND USING w_dataList-belnr w_dataList-gjahr.
        ENDIF.
    ENDCASE.

    PERFORM frm_refresh_item.
  ENDMETHOD.
  METHOD handle_data_changed_head.        "数据改动事件

  ENDMETHOD.
ENDCLASS.
DATA event_handler TYPE REF TO lcl_event.
*&---------------------------------------------------------------------*
*& Form showpdf
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> W_DATALIST_XFMC
*&      --> W_DATALIST_FTPNAME
*&      <-- FILEX
*&---------------------------------------------------------------------*
FORM showpdf  USING    p_xfmc
                       p_ftpname.
  CHECK p_ftpname IS NOT INITIAL.
  DATA(zcl_mycst) = NEW zcl_mycst( ).
  DATA(filex) = zcl_mycst->load_file( lv_dir = CONV zcl_wd_ftp=>mty_directory( |/{ sy-mandt }/mycst/{ p_xfmc }| )
                                      impath = CONV string( p_ftpname ) ).

  CALL FUNCTION 'ZFM_CL_GUI_HTML_VIEWER'
    EXPORTING
      datax = filex
*     DTYPE = 'text'
*     DSUBTYPE       = 'html'
    .
ENDFORM.
