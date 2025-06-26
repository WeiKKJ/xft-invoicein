*----------------------------------------------------------------------*
***INCLUDE ZMMXFT_INVOICE_IN_LIST_STATO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0900 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0900 OUTPUT.
  SET PF-STATUS 'STA900'.
  SET TITLEBAR 'TIT900'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SHOWALV OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE showalv OUTPUT.
  IF alv_container IS INITIAL.
    alv_container = NEW #( repid = sy-repid dynnr = sy-dynnr side = cl_gui_docking_container=>dock_at_top extension = 500 ).
    alv_splitter_container = NEW #( parent = alv_container rows = 2 columns = 1 ).
    alv_splitter_container->set_row_height( id = 1 height = 45 ).
    ref_container_head = alv_splitter_container->get_container( row = 1 column = 1 ).
    ref_container_item = alv_splitter_container->get_container( row = 2 column = 1 ).
    alv_grid_head = NEW #( i_parent =  ref_container_head ).
    alv_grid_item = NEW #( i_parent =  ref_container_item ).

    PERFORM:callalv_head,callalv_item.
  ELSE.
    PERFORM:frm_refresh_head,frm_refresh_item.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form callalv_head
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM callalv_head .
  CREATE OBJECT event_handler.
  SET HANDLER event_handler->handle_double_click_head
            FOR alv_grid_head.

  PERFORM callalv_oo IN PROGRAM zvariant_compare IF FOUND TABLES output-body-data_list USING alv_grid_head gt_fldct_head 'P1' gs_slayt.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form callalv_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM callalv_item .
  PERFORM callalv_oo IN PROGRAM zvariant_compare IF FOUND TABLES w_dataList-details USING alv_grid_item gt_fldct_item 'P2' gs_slayt.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_refresh_head
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_refresh_head .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_refresh_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_refresh_item .
  DATA: lt_celltab TYPE lvc_t_styl,
        ls_celltab TYPE lvc_s_styl,
        ls_stable  TYPE lvc_s_stbl.

  ls_stable-row = 'X'.  "固定行
  ls_stable-col = 'X'.  "固定列

  CHECK alv_grid_item IS NOT INITIAL.

  CALL METHOD alv_grid_item->refresh_table_display
    EXPORTING
      is_stable = ls_stable
*     I_SOFT_REFRESH = 'X'
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CALL METHOD cl_gui_cfw=>flush.
ENDFORM.
