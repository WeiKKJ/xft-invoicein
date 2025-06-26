*&---------------------------------------------------------------------*
*& Report ZMMXFT_INVOICE_IN_LIST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmmxft_invoice_in_list.
TABLES:mkpf.
DATA: gt_fldct      TYPE lvc_t_fcat,
      gs_varnt      TYPE disvariant,
      gs_slayt      TYPE lvc_s_layo,
      gv_repid      TYPE sy-repid,
      gt_fldct_head TYPE lvc_t_fcat,
      gt_fldct_item TYPE lvc_t_fcat.
DATA: BEGIN OF gs_out,
        plnum TYPE plaf-plnum,
        matnr TYPE plaf-matnr,
        maktx TYPE makt-maktx,
        gsmng TYPE plaf-gsmng,
        meins TYPE plaf-meins,
        psttr TYPE plaf-psttr,
        pedtr TYPE plaf-pedtr,
        dispo TYPE plaf-dispo,
        beskz TYPE plaf-beskz,
        berid TYPE plaf-berid,
        fevor TYPE marc-fevor,
        fetxt TYPE t024f-txt,
      END OF gs_out.
DATA: gt_out LIKE TABLE OF gs_out.
DATA cl_document TYPE REF TO cl_dd_document.
DATA:input       TYPE zsxft_in_que_inv_lis_in,
     output      TYPE zsxft_in_que_inv_lis_out,
     output_temp TYPE zsxft_in_que_inv_lis_out,
     rtype       TYPE bapi_mtype,
     rtmsg       TYPE bapi_msg,
     w_dataList  TYPE zsdatalist.
DATA:alv_grid_head          TYPE REF TO cl_gui_alv_grid,
     alv_grid_item          TYPE REF TO cl_gui_alv_grid,
     alv_container          TYPE REF TO cl_gui_docking_container,
     alv_splitter_container TYPE REF TO cl_gui_splitter_container,
     ref_container_head     TYPE REF TO cl_gui_container,
     ref_container_item     TYPE REF TO cl_gui_container.
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
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE btxt1.
  PARAMETERS p_bukrs TYPE t001-bukrs DEFAULT '2000' MEMORY ID p_bukrs.
  PARAMETERS p_dataTy TYPE zsxft_in_que_inv_lis_in-data_Type AS LISTBOX VISIBLE LENGTH 7 DEFAULT '1' MEMORY ID p_dataTy.
  PARAMETERS p_REDBLU TYPE zsxft_in_que_inv_lis_in-red_blue AS LISTBOX VISIBLE LENGTH 11 DEFAULT 'BLUE' MEMORY ID p_REDBLU.
*  PARAMETERS p_INTYP TYPE zsxft_in_que_inv_lis_in-invoice_types AS LISTBOX VISIBLE LENGTH 31 DEFAULT '101' MEMORY ID p_INTYP.
  SELECT-OPTIONS s_intyp FOR w_dataList-invoice_type NO INTERVALS MEMORY ID s_intyp.
  SELECT-OPTIONS s_Time FOR mkpf-budat NO-EXTENSION MEMORY ID s_Time.

SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  btxt1 = '数据筛选'(t01).

AT SELECTION-SCREEN. "PAI
  CASE sy-ucomm.
    WHEN 'ONLI'.
      PERFORM auth_check.
  ENDCASE.

INITIALIZATION.
  PERFORM catset TABLES gt_fldct
                 USING: 'PLNUM' 'PLAF ' 'PLNUM' '',
                        'MATNR' 'PLAF ' 'MATNR' '',
                        'MAKTX' 'MAKT ' 'MAKTX' '',
                        'GSMNG' 'PLAF ' 'GSMNG' '',
                        'MEINS' 'PLAF ' 'MEINS' '',
                        'PSTTR' 'PLAF ' 'PSTTR' '',
                        'PEDTR' 'PLAF ' 'PEDTR' '',
                        'DISPO' 'PLAF ' 'DISPO' '',
                        'BESKZ' 'PLAF ' 'BESKZ' '',
                        'BERID' 'PLAF ' 'BERID' '',
                        'FEVOR' 'MARC ' 'FEVOR' 'LINE'(f01),
                        'FETXT' 'T024F' 'TXT  ' 'LINE NAME'(f02).

START-OF-SELECTION.
  PERFORM init.
  PERFORM savelog(zreplog) USING sy-repid '' IF FOUND.
  PERFORM getdata.
  PERFORM updatelog(zreplog) IF FOUND.
*  PERFORM outdata.

*&---------------------------------------------------------------------*
*&      Form  auth_check
*&---------------------------------------------------------------------*
FORM auth_check.
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
        ID 'ACTVT' DUMMY
        ID 'BUKRS' FIELD p_bukrs.
  IF sy-subrc <> 0.
    MESSAGE e000(oo) WITH '无公司权限:'(m01) p_bukrs.
  ENDIF.
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD sy-tcode.
  IF sy-subrc NE 0.
    MESSAGE e000(oo) WITH '无事务码权限:'(m02) sy-tcode.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& getdata
*&---------------------------------------------------------------------*
FORM getdata.
  CLEAR:input,output,output_temp,rtype,rtmsg.
  input-data_type                 = p_dataTy.
  input-red_blue                  = p_REDBLU.
  input-include_invoice_file      = 1.
*  input-invoice_numbers           = ''.
*  input-invoice_number_suffixes   = ''.
  SELECT
    domvalue_l
    FROM dd07t
    WHERE domname = 'ZDINVOICETYPES'
    AND ddlanguage = @sy-langu
    AND as4local = 'A'
    AND domvalue_l IN @s_intyp
    ORDER BY domvalue_l
    INTO TABLE @input-invoice_types
    .
  input-issue_time_end            = s_Time-high.
  PERFORM format_date IN PROGRAM zfi025_7 CHANGING input-issue_time_end.
  input-issue_time_start          = s_Time-low.
  PERFORM format_date IN PROGRAM zfi025_7 CHANGING input-issue_time_start.
  input-page-page_Size = 316.

  output_temp-body-has_next_page = abap_true.
  output_temp-body-page_Number = 0.
  GET RUN TIME FIELD DATA(tf1).
  WHILE output_temp-body-has_next_page = abap_true.
    input-page-page_Number = output_temp-body-page_Number + 1.
    CLEAR:output_temp,rtype,rtmsg.
    CALL FUNCTION 'ZFM_XFT_IN_QUE_INV_LIS'
      EXPORTING
        input  = input
        bukrs  = p_bukrs
      IMPORTING
        output = output_temp
*       OUTPUTSTR       =
        rtype  = rtype
        rtmsg  = rtmsg
*       BODY   =
      .
    IF rtype = 'S'.
      APPEND LINES OF output_temp-body-data_list TO output-body-data_list.
    ELSE.
      MESSAGE s000(oo) WITH rtmsg DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    GET RUN TIME FIELD DATA(tf2).
    DATA(secds) = ( tf2 - tf1 ) / 1000000.
    IF secds GT 300.
      MESSAGE s000(oo) WITH '取数时间' secds '秒,可能发生了异常' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDWHILE.

  IF output-body-data_list IS INITIAL.
    MESSAGE s000(oo) WITH 'No Data'.
  ENDIF.
  CALL SCREEN 900.
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
      i_callback_pf_status_set    = 'SET_STATUS'
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
  ls_fldcat-scrtext_S =  pv_text.     "长描述
  ls_fldcat-scrtext_m =  pv_text.     "长描述
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

INCLUDE zmmxft_invoice_class.

INCLUDE zmmxft_invoice_in_list_stato01.

INCLUDE zmmxft_invoice_in_list_useri01.
*&---------------------------------------------------------------------*
*& Form init
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM init .
  CLEAR:gt_fldct_head,gt_fldct_item.
  DATA(ddic_header) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( p_name = 'ZSDATALIST' ) )->get_ddic_field_list( ).
  DATA(ddic_item) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( p_name = 'ZSDETAILS' ) )->get_ddic_field_list( ).
  LOOP AT ddic_header ASSIGNING FIELD-SYMBOL(<h>) WHERE inttype = 'C'.
    PERFORM catset TABLES gt_fldct_head
                   USING: <h>-fieldname <h>-tabname <h>-fieldname <h>-fieldtext.
  ENDLOOP.
  LOOP AT ddic_item ASSIGNING FIELD-SYMBOL(<i>) WHERE inttype = 'C'.
    PERFORM catset TABLES gt_fldct_item
                   USING: <i>-fieldname <i>-tabname <i>-fieldname <i>-fieldtext.
  ENDLOOP.
  gs_slayt-zebra  = 'X'.
  gs_slayt-box_fname  = 'SEL'.
ENDFORM.
