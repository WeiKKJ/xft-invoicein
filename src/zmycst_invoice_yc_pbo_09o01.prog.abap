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
  IF p1 = 'X'.
    SET TITLEBAR 'TIT900' WITH '原材发票预制'.
  ELSE.
    SET TITLEBAR 'TIT900' WITH '辅料发票预制'.
  ENDIF.
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
*    DATA(text) = `明细行物料编码的真实物料属性可能与所展示的不一致，严格以VA03界面展示为准！！！`
*    && `深红色底色：未选择物料号，深黄色底色：注意核对物料号，深绿色底色：规格匹配，无底色：已生成了合同明细。`.
*    o_textedit->set_textstream( text = text ).
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
    CREATE OBJECT event_receiver.
    SET HANDLER event_receiver->handle_double_click FOR alv_grid_po.
    SET HANDLER event_receiver->handle_data_changed FOR alv_grid_po.
    SET HANDLER event_receiver->handle_user_command FOR alv_grid_po.
    SET HANDLER event_receiver->handle_toolbar      FOR alv_grid_po.
    SET HANDLER event_receiver->handle_menu_button  FOR alv_grid_po.

*    APPEND cl_gui_alv_grid=>mc_mb_variant TO gt_func.
*    APPEND cl_gui_alv_grid=>mc_mb_subtot  TO gt_func.
*    APPEND cl_gui_alv_grid=>mc_mb_export  TO gt_func.
*    APPEND cl_gui_alv_grid=>mc_mb_sum     TO gt_func.
*    APPEND cl_gui_alv_grid=>mc_mb_view    TO gt_func.
*    APPEND cl_gui_alv_grid=>mc_fc_info    TO gt_func.
*    APPEND cl_gui_alv_grid=>mc_mb_filter  TO gt_func.
*    APPEND cl_gui_alv_grid=>mc_fc_print   TO gt_func.
*    APPEND cl_gui_alv_grid=>mc_fc_graph   TO gt_func.


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
          'EBELP      ' '' '' '行号'.
    IF p2 = 'X'.
      PERFORM catset TABLES gt_fldct_po USING:
            'KSCHL     ' '' '' '杂费类别',
            'VTEXT     ' '' '' '杂费描述'.
    ENDIF.
    PERFORM catset TABLES gt_fldct_po USING:
          'LIFNR      ' '' '' '供应商编码',
          'NAME1      ' '' '' '供应商名称',
          'EKGRP      ' '' '' '采购组',
          'EKNAM      ' '' '' '采购组名称'.
    IF p1 = 'X'.
      PERFORM catset TABLES gt_fldct_po USING:
           'MBLNR      ' '' '' '收货凭证',
           'MJAHR      ' '' '' '收货凭证年',
           'ZEILE      ' '' '' '收货凭证行',
           'BUDAT      ' '' '' '入库日期',
           'LGOBE      ' '' '' '入库仓库',
           'MATNR      ' '' '' '物料编码'.
    ENDIF.
    PERFORM catset TABLES gt_fldct_po USING:
          'TXZ01      ' '' '' '短文本',
          'ZSL        ' '' '' '税率',
          'NETPR_HS   ' '' '' '含税单价'.
    IF p1 = 'X'.
      PERFORM catset TABLES gt_fldct_po USING:
          'MENGE_PO   ' '' '' '计划收货数量'.

    ENDIF.
    PERFORM catset TABLES gt_fldct_po USING:
          'BSTME      ' '' '' '采购单位',
          'MENGE_PO101' '' '' '已收货数量',
          'MENGE_MIROEND' '' '' '已核销数量',
          'MENGE_MIR7 ' '' '' '已预制数量',
          'MENGE_MIRO ' 'RSEG' 'MENGE' '待核销数量',
          'MIRO_SUM   ' 'RSEG' 'WRBTR' '待核销金额',
          'MIRO_SUM_SE' 'RSEG' 'WRBTR' '待核销税额',
          'MIRO_SUM_HS' 'RSEG' 'WRBTR' '待核销金额（含税）',
          'BELNR' '' '' '预制发票号',
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

    LOOP AT ddic_header ASSIGNING FIELD-SYMBOL(<h>) WHERE inttype = 'C' OR inttype = 'I' OR inttype = 'N'.
      CASE <h>-fieldname.
        WHEN 'KPJE' OR 'KPSE' OR 'JSHJ'.
          PERFORM catset TABLES gt_fldct_js
                         USING: <h>-fieldname 'RBKP' 'RMWWR' <h>-fieldtext.
        WHEN OTHERS.
          PERFORM catset TABLES gt_fldct_js
                         USING: <h>-fieldname <h>-tabname <h>-fieldname <h>-fieldtext.
      ENDCASE.
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
*&---------------------------------------------------------------------*
*& Module PBO_9202 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_9202 OUTPUT.
  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'LS_HEADERDATA-DIFF_INV'.
        IF p2 = 'X' OR p1 = 'X'.
          screen-input = 1.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_MWSKZ  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_mwskz INPUT.
  CALL FUNCTION 'FI_F4_MWSKZ'
    EXPORTING
      i_kalsm = 'TAXCN'
      i_stbuk = p_bukrs
*     I_XSHOW = ' '
      i_lstml = 'CN'
*     I_HKONT = ' '
      i_conct = 'X'
      i_gener = '1'
*     I_GLVOR =
*     I_TAX_COUNTRY         = ' '
*     I_TXDAT =
*     I_MWART = ' '
*     I_SHOW_ALL_TXCD       = ' '
    IMPORTING
      e_mwskz = wtax-tax_code.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_LIFNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_lifnr INPUT.
  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname           = 'LFA1'
      fieldname         = 'LIFNR'
*     SEARCHHELP        = ' '
*     SHLPPARAM         = ' '
*     DYNPPROG          = ' '
*     DYNPNR            = ' '
      dynprofield       = 'DIFF_INV'
*     STEPL             = 0
*     VALUE             = ' '
*     MULTIPLE_CHOICE   = ' '
*     DISPLAY           = ' '
*     SUPPRESS_RECORDLIST       = ' '
*     CALLBACK_PROGRAM  = ' '
*     CALLBACK_FORM     = ' '
*     CALLBACK_METHOD   =
*     SELECTION_SCREEN  = ' '
* IMPORTING
*     USER_RESET        =
* TABLES
*     RETURN_TAB        =
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_BLART  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_blart INPUT.

  PERFORM itabtolist(zpubform) IF FOUND TABLES t003 USING 'LS_HEADERDATA-DOC_TYPE'.

*  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
*    EXPORTING
*      tabname           = 'INVFO'
*      fieldname         = 'BLART'
**     SEARCHHELP        = ' '
**     SHLPPARAM         = ' '
**     DYNPPROG          = ' '
**     DYNPNR            = ' '
*      dynprofield       = 'LS_HEADERDATA-DOC_TYPE'
**     STEPL             = 0
**     VALUE             = ' '
**     MULTIPLE_CHOICE   = ' '
**     DISPLAY           = ' '
**     SUPPRESS_RECORDLIST       = ' '
**     CALLBACK_PROGRAM  = ' '
**     CALLBACK_FORM     = ' '
**     CALLBACK_METHOD   =
**     SELECTION_SCREEN  = ' '
** IMPORTING
**     USER_RESET        =
** TABLES
**     RETURN_TAB        =
*    EXCEPTIONS
*      field_not_found   = 1
*      no_help_for_field = 2
*      inconsistent_help = 3
*      no_values_found   = 4
*      OTHERS            = 5.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.
ENDMODULE.
