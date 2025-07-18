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
    w_details-invoice_number = w_dataList-invoice_number.
    MODIFY w_dataList-details FROM w_details TRANSPORTING invoice_number WHERE invoice_number IS INITIAL.
    PERFORM frm_refresh_item.
  ENDMETHOD.
  METHOD handle_data_changed_head.        "数据改动事件

  ENDMETHOD.
ENDCLASS.
DATA event_handler TYPE REF TO lcl_event.
