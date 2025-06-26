*&---------------------------------------------------------------------*
*& 包含               ZMMXFT_INVOICE_CLASS
*&---------------------------------------------------------------------*
CLASS lcl_event DEFINITION .
  PUBLIC SECTION.

    METHODS:
      handle_double_click_head FOR EVENT double_click  OF cl_gui_alv_grid"双击事件
        IMPORTING e_row e_column es_row_no.

ENDCLASS .

CLASS lcl_event IMPLEMENTATION.
  METHOD handle_double_click_head.      "双击事件
    CHECK es_row_no-row_id GT 0.
    READ TABLE output-body-data_list INTO w_dataList INDEX es_row_no-row_id.
    PERFORM frm_refresh_item.
  ENDMETHOD.
ENDCLASS.
DATA event_handler TYPE REF TO lcl_event.
