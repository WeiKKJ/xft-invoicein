*&---------------------------------------------------------------------*
*& Report ZMYCST_INVOICE_FK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmycst_invoice_fk.
DATA: gt_fldct TYPE lvc_t_fcat,
      gs_slayt TYPE lvc_s_layo,
      gs_varnt TYPE disvariant,
      gv_repid TYPE sy-repid.
DATA: BEGIN OF gs_out.
        INCLUDE TYPE zspoinfo.
DATA:sel,
      END OF gs_out.
DATA: gt_out LIKE TABLE OF gs_out.
DATA:dfies_tab TYPE TABLE OF dfies.
DATA cl_document TYPE REF TO cl_dd_document.
DEFINE mcr_html_field.
  g_text = &3.
  CALL METHOD document->add_text
    EXPORTING
      text         = g_text
      sap_emphasis = &2.
  CALL METHOD document->add_gap
    EXPORTING
      width = &1.
END-OF-DEFINITION.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE btxt2.
  PARAMETERS p1 RADIOBUTTON GROUP prd0 USER-COMMAND ss0 DEFAULT 'X'.
  PARAMETERS p2 RADIOBUTTON GROUP prd0.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE btxt1.
  PARAMETERS p_belnr TYPE rbkp-belnr MODIF ID m1.
  PARAMETERS p_gjahr TYPE rbkp-gjahr MODIF ID m1.
  PARAMETERS p_ebeln TYPE ekko-ebeln MODIF ID m2.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  btxt1 = '数据筛选'(t01).
  btxt2 = '功能选择'(t01).
  LOOP AT SCREEN.
    IF p1 = 'X'.
      IF screen-group1 = 'M1'.
        screen-active = 1.
      ELSEIF screen-group1 = 'M2'.
        screen-active = 0.
      ENDIF.
    ELSEIF p2 = 'X'.
      IF screen-group1 = 'M2'.
        screen-active = 1.
      ELSEIF screen-group1 = 'M1'.
        screen-active = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN. "PAI
  CASE sy-ucomm.
    WHEN 'ONLI'.
      PERFORM auth_check.
  ENDCASE.

INITIALIZATION.
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = 'ZSPOINFO'
      langu          = sy-langu
    TABLES
      dfies_tab      = dfies_tab
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.
  LOOP AT dfies_tab ASSIGNING FIELD-SYMBOL(<dfies_tab>).
    IF reverse( <dfies_tab>-fieldname ) CP 'OON_*'.
      CONTINUE.
    ENDIF.
    PERFORM catset TABLES gt_fldct USING:
          <dfies_tab>-fieldname 'ZSPOINFO' <dfies_tab>-fieldname <dfies_tab>-fieldtext.
  ENDLOOP.

START-OF-SELECTION.
  PERFORM savelog(zreplog) USING sy-repid '' IF FOUND.
  PERFORM getdata.
  PERFORM updatelog(zreplog) IF FOUND.
  PERFORM outdata.

*&---------------------------------------------------------------------*
*&      Form  auth_check
*&---------------------------------------------------------------------*
FORM auth_check.
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD sy-tcode.
  IF sy-subrc NE 0.
    MESSAGE e000(oo) WITH '无事务码权限:'(m02) sy-tcode.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& getdata
*&---------------------------------------------------------------------*
FORM getdata.
  CLEAR gt_out.
  DATA:outtab     TYPE TABLE OF zspoinfo,
       belnrgjahr TYPE char14.
  IF p1 = 'X'.
    belnrgjahr = |{ p_belnr }{ p_gjahr }|.
  ENDIF.
  CALL FUNCTION 'ZFM_XMD_OA_GETPO'
    EXPORTING
      belnrgjahr = belnrgjahr
      ebeln      = p_ebeln
    TABLES
      outtab     = outtab.

  IF outtab IS INITIAL.
    MESSAGE s000(oo) WITH '无数据'.
    RETURN.
  ENDIF.
  MOVE-CORRESPONDING outtab TO gt_out.
ENDFORM.

*---------------------------------------------------------------------*
* outdata
*---------------------------------------------------------------------*
FORM outdata.
  gv_repid        = sy-repid.
  gs_slayt-zebra  = 'X'.
  gs_slayt-box_fname  = 'SEL'.
  gs_varnt-report = sy-repid.
  gs_varnt-handle = 1.

  CHECK gt_out IS NOT INITIAL.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      it_fieldcat_lvc             = gt_fldct
      i_save                      = 'A'
      is_variant                  = gs_varnt
      is_layout_lvc               = gs_slayt
      i_callback_program          = gv_repid
      i_callback_user_command     = 'USER_COMMAND'
*     i_callback_pf_status_set    = 'SET_STATUS'
      i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
      i_html_height_top           = 7
      i_html_height_end           = 7
    TABLES
      t_outtab                    = gt_out.
ENDFORM.
FORM html_top_of_page USING document TYPE REF TO cl_dd_document.
  DESCRIBE TABLE gt_out LINES DATA(line).
  DATA: g_text TYPE sdydo_text_element.
  CALL METHOD document->initialize_document.
  SEARCH document->html_table FOR document->cursor.
  IF sy-subrc = 0.
    mcr_html_field 0 'Strong' '条目数:'.
    mcr_html_field 1 'Key' line.
*    CALL METHOD document->new_line.
*    mcr_html_field 0 'Strong' '分摊状态:'.
*    mcr_html_field 1 'Key' rtype.
*    mcr_html_field 0 'Strong' '分摊结果:'.
*    mcr_html_field 1 'Key' rtmsg.
*    mcr_html_field 0 'Strong' '分摊凭证:'.
*    mcr_html_field 1 'Key' mblnr.
*    mcr_html_field 0 'Strong' '分摊凭证年:'.
*    mcr_html_field 1 'Key' mjahr.
  ENDIF.
  CHECK cl_document IS INITIAL.
  cl_document = document.
ENDFORM. "HTML_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*& set_status
*&---------------------------------------------------------------------*
FORM set_status USING pt_extab TYPE slis_t_extab ##CALLED.
  SET PF-STATUS 'STD_FULL' EXCLUDING pt_extab.
ENDFORM.

*&--------------------------------------------------------------------*
*& ALV user_command
*&--------------------------------------------------------------------*
FORM user_command USING pv_ucomm TYPE sy-ucomm ##CALLED
                        pv_field TYPE slis_selfield.
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.
  lr_grid->get_selected_rows( IMPORTING et_index_rows = DATA(rows) ).
  CALL METHOD lr_grid->check_changed_data.
  READ TABLE gt_out INTO gs_out INDEX pv_field-tabindex.
  CASE pv_ucomm.
    WHEN '&IC1'.
      CASE pv_field-fieldname.
        WHEN 'MATNR' OR 'MAKTX'.
*          SET PARAMETER ID 'MXX' FIELD 'K'.
*          SET PARAMETER ID 'MAT' FIELD gs_out-matnr.
*          SET PARAMETER ID 'WRK' FIELD p_werks.
*          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
      ENDCASE.
    WHEN 'TCLIP'.
      PERFORM alvtoclip IN PROGRAM zpubform IF FOUND TABLES gt_out USING 'X'.
    WHEN 'REFRE'.
      PERFORM getdata.
      pv_field-row_stable = 'X'.
      pv_field-col_stable = 'X'.
      pv_field-refresh    = 'X'.
    WHEN 'BC'.

  ENDCASE.
  CALL METHOD lr_grid->refresh_table_display.
ENDFORM.

*---------------------------------------------------------------------*
* set fieldcat
*---------------------------------------------------------------------*
FORM catset TABLES t_fldcat
            USING pv_field pv_reftab pv_reffld pv_text.
  DATA: ls_fldcat TYPE lvc_s_fcat.

  ls_fldcat-fieldname =  pv_field.    "字段名
  ls_fldcat-scrtext_l =  pv_text.     "长描述
  ls_fldcat-scrtext_S =  pv_text.     "短描述
  ls_fldcat-scrtext_m =  pv_text.     "中描述
  ls_fldcat-selddictxt =  pv_text.    "布局
  ls_fldcat-coltext   =  pv_text.     "列描述
  ls_fldcat-ref_table =  pv_reftab.   "参考表名
  ls_fldcat-ref_field =  pv_reffld.   "参考字段名
  ls_fldcat-col_opt   = 'A'.          "自动优化列宽

  CASE ls_fldcat-fieldname.
    WHEN 'GSMNG'.
      ls_fldcat-qfieldname = 'MEINS'.
      ls_fldcat-no_zero    = 'X'.
    WHEN 'MENGE'.
      ls_fldcat-qfieldname = 'MEINS'.
      ls_fldcat-no_zero    = 'X'.
    WHEN 'WRBTR'.
      ls_fldcat-cfieldname = 'WAERS'.
    WHEN 'LIFNR' OR 'AUFNR' OR 'KUNNR'.
      ls_fldcat-edit_mask = '==ALPHA'.
    WHEN 'MATNR' OR 'IDNRK'.
      ls_fldcat-edit_mask = '==MATN1'.
    WHEN 'MEINS' .
      ls_fldcat-edit_mask = '==CUNIT'.
  ENDCASE.

  APPEND ls_fldcat TO t_fldcat.
  CLEAR ls_fldcat.
ENDFORM.
