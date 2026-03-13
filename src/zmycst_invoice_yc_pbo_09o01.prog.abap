*----------------------------------------------------------------------*
***INCLUDE ZMYCST_INVOICE_YC_PBO_09O01.
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
*& Module MIR7 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE mir7 OUTPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INIT_TEXTEDIT OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE init_textedit OUTPUT.
  IF o_container IS INITIAL.
    o_container = NEW cl_gui_custom_container( container_name = 'CONTLONG' ).
    CREATE OBJECT o_textedit
      EXPORTING
        parent                     = o_container
*       wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_windowborder
*       wordwrap_position          = 45
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true
      EXCEPTIONS
        OTHERS                     = 1.
    o_textedit->set_readonly_mode( readonly_mode = 0 ).
    o_textedit->set_statusbar_mode( statusbar_mode = 1 ).
    o_textedit->set_toolbar_mode( toolbar_mode = 1 ).
    DATA(text) = `明细行物料编码的真实物料属性可能与所展示的不一致，严格以VA03界面展示为准！！！`
    && `深红色底色：未选择物料号，深黄色底色：注意核对物料号，深绿色底色：规格匹配，无底色：已生成了合同明细。`.
    o_textedit->set_textstream( text = text ).
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SHOWPO OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE showpo OUTPUT.
  IF container_po IS INITIAL.
    container_po = NEW cl_gui_custom_container( container_name = 'CONTPO' ).
    alv_grid_po = NEW #( i_parent =  container_po ).
*    CREATE OBJECT lcl_alv_receiver.
*    SET HANDLER lcl_alv_receiver->handle_double_click FOR alv_grid_item.
*    SET HANDLER lcl_alv_receiver->handle_data_changed FOR alv_grid_item.
*    SET HANDLER lcl_alv_receiver->handle_hotspot_click FOR alv_grid_item.
*    SET HANDLER lcl_alv_receiver->handle_toolbar FOR alv_grid_item.
*    SET HANDLER lcl_alv_receiver->handle_user_command FOR alv_grid_item.
*    SET HANDLER lcl_alv_receiver->handle_on_f4 FOR alv_grid_item.
    alv_grid_po->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified )."mc_evt_modified
    alv_grid_po->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter )."mc_evt_enter
    alv_grid_po->register_delayed_event( i_event_id = cl_gui_alv_grid=>mc_evt_delayed_change_select )."MC_EVT_DELAYED_CHANGE_SELECT
*    CLEAR:it_f4.
*    CLEAR:wa_f4.
*    wa_f4-fieldname = 'MATNR'.
*    wa_f4-register = 'X'.
*    wa_f4-getbefore = ''.
*    wa_f4-chngeafter = 'X'.
*    INSERT wa_f4 INTO TABLE it_f4.
*    alv_grid_po->register_f4_for_fields( it_f4 ).

    PERFORM catset TABLES gt_fldct_po USING:
          'EBELN      ' '' '' '采购订单号',
          'EBELP      ' '' '' '行号',
          'LIFNR      ' '' '' '供应商编码',
          'NAME1      ' '' '' '供应商名称',
          'EKGRP      ' '' '' '采购组',
          'EKNAM      ' '' '' '采购组名称',
          'MBLNR      ' '' '' '收货凭证',
          'MJAHR      ' '' '' '收货凭证年',
          'ZEILE      ' '' '' '收货凭证行',
          'BUDAT      ' '' '' '入库日期',
          'LGOBE      ' '' '' '入库仓库',
          'MATNR      ' '' '' '物料编码',
          'TXZ01      ' '' '' '短文本',
          'ZSL        ' '' '' '税率',
          'NETPR_HS   ' '' '' '含税单价',
          'MENGE_PO   ' '' '' '计划收货数量',
          'BSTME      ' '' '' '采购单位',
          'MENGE_PO101' '' '' '已收货数量',
          'MENGE_MIROEND' '' '' '已核销数量',
          'MENGE_MIR7 ' '' '' '已预制数量',
          'MENGE_MIRO ' '' '' '待核销数量',
          'MIRO_SUM   ' '' '' '待核销金额',
          'MIRO_SUM_SE' '' '' '待核销税额',
          'MIRO_SUM_HS' '' '' '待核销金额（含税）',
          'SEL' '' '' ''.

    PERFORM callalv_oo IN PROGRAM zvariant_compare IF FOUND
    TABLES gt_out USING alv_grid_po gt_fldct_po 'P1' gs_slayt_po.

  ELSE.
    PERFORM:frm_refresh_alv_po.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form frm_refresh_alv_po
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_refresh_alv_po .
  DATA: lt_celltab TYPE lvc_t_styl,
        ls_celltab TYPE lvc_s_styl,
        ls_stable  TYPE lvc_s_stbl.

  ls_stable-row = 'X'.  "固定行
  ls_stable-col = 'X'.  "固定列

  CHECK alv_grid_po IS NOT INITIAL.

  CALL METHOD alv_grid_po->refresh_table_display
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
*&---------------------------------------------------------------------*
*& Module SHOWJINSHUI OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE showjinshui OUTPUT.
  IF container_js IS INITIAL.
    container_js = NEW cl_gui_custom_container( container_name = 'CONTJS' ).
    alv_grid_js = NEW #( i_parent =  container_js ).
*    CREATE OBJECT lcl_alv_receiver.
*    SET HANDLER lcl_alv_receiver->handle_double_click FOR alv_grid_item.
*    SET HANDLER lcl_alv_receiver->handle_data_changed FOR alv_grid_item.
*    SET HANDLER lcl_alv_receiver->handle_hotspot_click FOR alv_grid_item.
*    SET HANDLER lcl_alv_receiver->handle_toolbar FOR alv_grid_item.
*    SET HANDLER lcl_alv_receiver->handle_user_command FOR alv_grid_item.
*    SET HANDLER lcl_alv_receiver->handle_on_f4 FOR alv_grid_item.
    alv_grid_js->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified )."mc_evt_modified
    alv_grid_js->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter )."mc_evt_enter
    alv_grid_js->register_delayed_event( i_event_id = cl_gui_alv_grid=>mc_evt_delayed_change_select )."MC_EVT_DELAYED_CHANGE_SELECT
*    CLEAR:it_f4.
*    CLEAR:wa_f4.
*    wa_f4-fieldname = 'MATNR'.
*    wa_f4-register = 'X'.
*    wa_f4-getbefore = ''.
*    wa_f4-chngeafter = 'X'.
*    INSERT wa_f4 INTO TABLE it_f4.
*    alv_grid_po->register_f4_for_fields( it_f4 ).

    DATA(ddic_header) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( p_name = 'ZTMYCSTHEAD' ) )->get_ddic_field_list( ).

    LOOP AT ddic_header ASSIGNING FIELD-SYMBOL(<h>) WHERE inttype = 'C' OR inttype = 'I'.
      PERFORM catset TABLES gt_fldct_js
                     USING: <h>-fieldname <h>-tabname <h>-fieldname <h>-fieldtext.
    ENDLOOP.
    PERFORM catset TABLES gt_fldct_js
                   USING: 'SEL' '' '' ''.
    PERFORM callalv_oo IN PROGRAM zvariant_compare IF FOUND
    TABLES t_dataList USING alv_grid_js gt_fldct_js 'P2' gs_slayt_js.

  ELSE.
    PERFORM:frm_refresh_alv_js.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form frm_refresh_alv_js
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_refresh_alv_js .
  DATA: lt_celltab TYPE lvc_t_styl,
        ls_celltab TYPE lvc_s_styl,
        ls_stable  TYPE lvc_s_stbl.

  ls_stable-row = 'X'.  "固定行
  ls_stable-col = 'X'.  "固定列

  CHECK alv_grid_js IS NOT INITIAL.

  CALL METHOD alv_grid_js->refresh_table_display
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
