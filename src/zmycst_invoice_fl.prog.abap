*&---------------------------------------------------------------------*
*& Report ZMYCST_INVOICE_FL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmycst_invoice_fl.
DATA: gt_fldct  TYPE lvc_t_fcat,
      gs_slayt  TYPE lvc_s_layo,
      gs_varnt  TYPE disvariant,
      gv_repid  TYPE sy-repid,
      gt_fldctm TYPE lvc_t_fcat.
DATA: BEGIN OF gs_out,
        ebeln         TYPE ebeln,
        ebelp         TYPE ebelp,
        lifnr         TYPE lifnr,
        name1         TYPE name1,
        mblnr         TYPE mseg-mblnr,
        mjahr         TYPE mseg-mjahr,
        zeile         TYPE mseg-zeile,
        budat         TYPE budat,
        lgobe         TYPE lgobe,
        matnr         TYPE matnr,
        txz01         TYPE txz01,
        zsl           TYPE p DECIMALS 3,
        netpr_hs      TYPE p DECIMALS 2,
        bstme         TYPE bstme,
        menge_po      TYPE menge_d,
        menge_po101   TYPE menge_d,
        menge_miroend TYPE menge_d,
        menge_mir7    TYPE menge_d,
        menge_miro    TYPE menge_d,
        miro_sum      TYPE wrbtr,
        miro_sum_se   TYPE wrbtr,
        miro_sum_hs   TYPE wrbtr,
        belnr         TYPE char14,
        ekgrp         TYPE ekgrp,
        eknam         TYPE eknam,
        sel,
      END OF gs_out.
DATA: gt_out LIKE TABLE OF gs_out.
DATA:BEGIN OF gs_mir7,
       name1       TYPE name1,
       ebeln       TYPE ebeln,
       ebelp       TYPE ebelp,
       eknam       TYPE eknam,
       budat       TYPE budat,
       lgobe       TYPE lgobe,
       mblnr       TYPE mseg-mblnr,
       mjahr       TYPE mseg-mjahr,
       zeile       TYPE mseg-zeile,
       txz01       TYPE txz01,
       bstme       TYPE bstme,
       menge_miro  TYPE menge_d,
       netpr_hs    TYPE p DECIMALS 2,
       zsl         TYPE p DECIMALS 3,
       miro_sum    TYPE wrbtr,
       miro_sum_se TYPE wrbtr,
       miro_sum_hs TYPE wrbtr,
       fphm1       TYPE rseg-sgtxt,
       fphm2       TYPE rseg-sgtxt,
       fphm3       TYPE rseg-sgtxt,
       fphm4       TYPE rseg-sgtxt,
       fphm5       TYPE rseg-sgtxt,
       fphm6       TYPE rseg-sgtxt,
       fphm7       TYPE rseg-sgtxt,
       fphm8       TYPE rseg-sgtxt,
       mwskz       TYPE mwskz,
       belnr       TYPE char14,
       tfphm       TYPE TABLE OF rseg-sgtxt,
       sel,
     END OF gs_mir7,
     gt_mir7 LIKE TABLE OF gs_mir7 WITH EMPTY KEY.
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
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE btxt1.
  PARAMETERS p_bukrs TYPE ekko-bukrs OBLIGATORY DEFAULT '2000' MODIF ID m1.
  PARAMETERS p_werks TYPE ekpo-werks OBLIGATORY DEFAULT '2000' MODIF ID m1.
  PARAMETERS p_ekorg TYPE ekko-ekorg OBLIGATORY DEFAULT '2000' MODIF ID m1.
  PARAMETERS p_ekgrp TYPE t024-ekgrp MODIF ID m2.
  PARAMETERS p_lifnr TYPE ekko-lifnr MODIF ID m2.
  SELECT-OPTIONS s_budat FOR gs_out-budat MODIF ID m2.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE btxt2.
  PARAMETERS p1 RADIOBUTTON GROUP prd1 USER-COMMAND ss1 DEFAULT 'X'.
  PARAMETERS p2 RADIOBUTTON GROUP prd1.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  btxt1 = '数据筛选'(t01).
  btxt2 = '功能选择'.
  LOOP AT SCREEN.
    IF screen-name = 'P_EKGRP' OR screen-name = 'P_LIFNR'.
      screen-required = 2.
    ENDIF.
    CASE screen-group1.
      WHEN 'M1' OR 'M2'.
        IF p2 = 'X'.
          screen-active = 0.
        ELSE.
          screen-active = 1.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.


AT SELECTION-SCREEN. "PAI
  CASE sy-ucomm.
    WHEN 'ONLI'.
      PERFORM auth_check.
  ENDCASE.

INITIALIZATION.

START-OF-SELECTION.
  IF p1 = 'X'.
    PERFORM getdata.
  ELSE.
    PERFORM uploaddata.
  ENDIF.
  PERFORM outdata.
*&---------------------------------------------------------------------*
*&      Form  auth_check
*&---------------------------------------------------------------------*
FORM auth_check.
  AUTHORITY-CHECK OBJECT 'M_BEST_WRK'
        ID 'ACTVT' DUMMY
        ID 'WERKS' FIELD p_werks.
  IF sy-subrc <> 0.
    MESSAGE e000(oo) WITH '无工厂权限:'(m01) p_werks.
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
  DATA: t_po     TYPE zmm_po_item,
        t_po_sum TYPE zmm_po_item.
  SELECT
    ekpo~ebeln,
    ekpo~ebelp,
    ekko~lifnr
    FROM ekko
    JOIN ekpo ON ekko~ebeln = ekpo~ebeln
    WHERE ekko~lifnr = @p_lifnr
    AND ekko~bukrs = @p_bukrs
    AND ekko~ekorg = @p_ekorg
    AND ekko~ekgrp = @p_ekgrp
    AND ekpo~werks = @p_werks
    AND ekpo~webre = @abap_true
    AND ekko~zhtlx NE '01'
    AND ekko~aedat IN @s_budat
    INTO CORRESPONDING FIELDS OF TABLE @t_po
  .
  CALL FUNCTION 'ZMM_FM_PO_INVENCE_LIST'
    CHANGING
      po     = t_po
      po_sum = t_po_sum.
*  SORT t_po BY mblnr mjahr zeile.

  SELECT
    lgort,
    lgobe
    FROM t001l
    WHERE werks = @p_werks
    ORDER BY lgort
    INTO TABLE @DATA(tlgobe)
    .
  SELECT
    lifnr,
    name1
    FROM lfa1
    FOR ALL ENTRIES IN @t_po_sum
    WHERE lifnr = @t_po_sum-lifnr
    INTO TABLE @DATA(tname1)
    .
  SORT tname1 BY lifnr.
  SELECT
    ekgrp,
    eknam
    FROM t024
    ORDER BY ekgrp
    INTO TABLE @DATA(teknam)
    .
  CLEAR gt_out.
  LOOP AT t_po_sum ASSIGNING FIELD-SYMBOL(<tsum>).
    DATA(mengem) = CONV menge_d( <tsum>-menge_po101 - <tsum>-menge_fp ) .
    IF mengem EQ 0.
      CONTINUE.
    ENDIF.
    READ TABLE tname1 INTO DATA(name1) WITH KEY lifnr = <tsum>-lifnr BINARY SEARCH.
    READ TABLE t_po ASSIGNING FIELD-SYMBOL(<t_po>) WITH KEY lifnr = <tsum>-lifnr ebeln = <tsum>-ebeln ebelp = <tsum>-ebelp BINARY SEARCH.
    IF sy-subrc EQ 0.
      LOOP AT t_po ASSIGNING <t_po> FROM sy-tabix.
        IF NOT ( <t_po>-lifnr = <tsum>-lifnr AND <t_po>-ebeln = <tsum>-ebeln AND <t_po>-ebelp = <tsum>-ebelp ).
          EXIT.
        ENDIF.
        IF mengem EQ 0.
          EXIT.
        ENDIF.
        DATA(mengems) = CONV menge_d( <t_po>-menge_po101 - <t_po>-menge_fp ) .
        IF mengems EQ 0.
          CONTINUE.
        ENDIF.
        IF mengems GT 0.
          mengem -= mengems.
          CLEAR:gs_out.
          MOVE-CORRESPONDING <t_po> TO gs_out.
          gs_out-mblnr = <t_po>-mblnr_noo.
          gs_out-mjahr = <t_po>-mjahr_noo.
          gs_out-zeile = <t_po>-zeile_noo.
          gs_out-budat = <t_po>-budat_noo.
          gs_out-menge_mir7 = <t_po>-menge_yz.
          gs_out-menge_miroend = <t_po>-menge_fp.
          gs_out-menge_miro = mengems.
          gs_out-miro_sum_hs = gs_out-menge_miro * <t_po>-netpr_hs.
          gs_out-miro_sum = gs_out-miro_sum_hs / ( 1 + <t_po>-zsl ).
          gs_out-miro_sum_se = gs_out-miro_sum_hs - gs_out-miro_sum.
          gs_out-lifnr = <tsum>-lifnr.
          gs_out-name1 = name1-name1.
          READ TABLE tlgobe INTO DATA(lgobe) WITH KEY lgort = <t_po>-lgort_noo BINARY SEARCH.
          IF sy-subrc EQ 0.
            gs_out-lgobe = lgobe-lgobe.
          ENDIF.
          READ TABLE teknam INTO DATA(eknam) WITH KEY ekgrp = <t_po>-ekgrp BINARY SEARCH.
          IF sy-subrc EQ 0.
            gs_out-eknam = eknam-eknam.
          ENDIF.
          APPEND gs_out TO gt_out.
        ENDIF.
      ENDLOOP.
    ENDIF.
    CLEAR:mengem,name1.
  ENDLOOP.


  IF gt_out IS INITIAL.
    MESSAGE s000(oo) WITH 'No Data'.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* outdata
*---------------------------------------------------------------------*
FORM outdata.
  gv_repid        = sy-repid.
  gs_slayt-zebra  = 'X'.
  gs_slayt-box_fname  = 'SEL'.
  gs_varnt-report = sy-repid.
  PERFORM catset TABLES gt_fldct USING:
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
'MIRO_SUM_HS' '' '' '待核销金额（含税）'.
  PERFORM catset TABLES gt_fldctm USING:
        'NAME1      ' '' '' '供应商名称',
        'EBELN      ' '' '' '采购订单号',
        'EBELP      ' '' '' '行号',
        'EKNAM      ' '' '' '采购组名称',
        'BUDAT      ' '' '' '入库日期',
        'LGOBE      ' '' '' '入库仓库',
        'MBLNR      ' '' '' '收货凭证',
        'MJAHR      ' '' '' '收货凭证年',
        'ZEILE      ' '' '' '收货凭证行',
        'TXZ01      ' '' '' '短文本',
        'BSTME      ' '' '' '采购单位',
        'ZSL        ' '' '' '税率',
        'NETPR_HS   ' '' '' '含税单价',
        'MENGE_MIRO ' '' '' '待核销数量',
        'MIRO_SUM   ' '' '' '待核销金额',
        'MIRO_SUM_SE' '' '' '待核销税额',
        'MIRO_SUM_HS' '' '' '待核销金额（含税）',
        'FPHM1' '' '' '发票号码1',
        'FPHM2' '' '' '发票号码2',
        'FPHM3' '' '' '发票号码3',
        'FPHM4' '' '' '发票号码4',
        'FPHM5' '' '' '发票号码5',
        'FPHM6' '' '' '发票号码6',
        'FPHM7' '' '' '发票号码7',
        'FPHM8' '' '' '发票号码8'.
  IF p2 = 'X'.
    PERFORM catset TABLES gt_fldctm USING:

          'MWSKZ' '' '' '税码',
          'BELNR' '' '' '预制发票号'.
  ENDIF.

  IF gt_out IS NOT INITIAL.
    gs_varnt-handle = 1.
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
  ELSEIF gt_mir7 IS NOT INITIAL.
    gs_varnt-handle = 2.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        it_fieldcat_lvc             = gt_fldctm
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
        t_outtab                    = gt_mir7.
  ENDIF.
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
  IF p1 = 'X'.
    APPEND 'MIR7' TO pt_extab.
  ELSE.
    APPEND 'MIR7LIST ' TO pt_extab.
  ENDIF.
  DELETE pt_extab WHERE fcode = '&SUM'.
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
  IF sy-subrc NE 0.
    READ TABLE gt_mir7 INTO gs_mir7 INDEX pv_field-tabindex.
    MOVE-CORRESPONDING gs_mir7 TO gs_out.
  ENDIF.
  CASE pv_ucomm.
    WHEN '&IC1'.
      CASE pv_field-fieldname.
        WHEN 'MATNR' OR 'MAKTX'.
          SET PARAMETER ID 'MXX' FIELD 'K'.
          SET PARAMETER ID 'MAT' FIELD gs_out-matnr.
          SET PARAMETER ID 'WRK' FIELD p_werks.
          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
        WHEN 'EBELN' OR 'EBELP'.
          SET PARAMETER ID 'BES' FIELD gs_out-ebeln.
          SET PARAMETER ID 'BSP' FIELD gs_out-ebelp.
          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
        WHEN 'MBLNR' OR 'GJAHR' OR 'ZEILE'.
          CALL FUNCTION 'MIGO_DIALOG'
            EXPORTING
*             I_ACTION            = 'A04'
*             I_REFDOC            = 'R02'
*             I_NOTREE            = 'X'
*             I_NO_AUTH_CHECK     =
*             I_SKIP_FIRST_SCREEN = 'X'
*             I_DEADEND           = 'X'
*             I_OKCODE            = 'OK_GO'
*             I_LEAVE_AFTER_POST  =
*             I_NEW_ROLLAREA      = 'X'
*             I_SYTCODE           =
*             I_EBELN             =
*             I_EBELP             =
              i_mblnr             = gs_out-mblnr
              i_mjahr             = gs_out-mjahr
              i_zeile             = gs_out-zeile
*             I_TRANSPORT         =
*             I_ORDER_NUMBER      =
*             I_ORDER_ITEM        =
*             I_TRANSPORT_MEANS   =
*             I_TRANSPORTIDENT    =
*             I_INBOUND_DELIV     =
*             I_OUTBOUND_DELIV    =
*             I_RESERVATION_NUMB  =
*             I_RESERVATION_ITEM  =
*             EXT                 =
*             I_MOVE_TYPE         =
*             I_SPEC_STOCK        =
*             I_PSTNG_DATE        =
*             I_DOC_DATE          =
*             I_REF_DOC_NO        =
*             I_HEADER_TXT        =
            EXCEPTIONS
              illegal_combination = 1
              OTHERS              = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        WHEN 'BELNR'.
          PERFORM mir4(zpubform) USING gs_out-belnr(10) gs_out-belnr+10(4) .
      ENDCASE.
    WHEN 'TCLIP'.
      PERFORM alvtoclip IN PROGRAM zpubform IF FOUND TABLES gt_out USING 'X'.
    WHEN 'REFRE'.
      PERFORM getdata.
      pv_field-row_stable = 'X'.
      pv_field-col_stable = 'X'.
      pv_field-refresh    = 'X'.
    WHEN 'MIR7LIST'.
      PERFORM mir7list.
    WHEN 'MIR7'.
      PERFORM mir7.
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
*    WHEN 'FPHM'.
*      ls_fldcat-no_out = 'X'.
  ENDCASE.

  APPEND ls_fldcat TO t_fldcat.
  CLEAR ls_fldcat.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form mir7list
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM mir7list .
  CLEAR:gt_mir7.
  LOOP AT gt_out INTO gs_out WHERE sel = 'X'.
    MOVE-CORRESPONDING gs_out TO gs_mir7.
    APPEND gs_mir7 TO gt_mir7.
  ENDLOOP.
  IF sy-subrc NE 0.
    MESSAGE s000(oo) WITH '请选择数据' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CLEAR:zcl_abap2xlsx_tools=>exceltab.
  INSERT INITIAL LINE INTO TABLE zcl_abap2xlsx_tools=>exceltab ASSIGNING FIELD-SYMBOL(<exceltab>).
  <exceltab>-excel_sheetname = |供应商{ gs_mir7-name1 }待开票明细|.
  <exceltab>-excel_tabdref = REF #( gt_mir7 ).
  <exceltab>-excel_fieldcat = zcl_excel_common=>get_fieldcatalog( ip_table = gt_mir7 ).
*  DATA(lo_style) = NEW zcl_excel_style( ).
*  lo_style->number_format->format_code = zcl_excel_style_number_format=>c_format_text.
*  DATA(guid) = lo_style->get_guid( ).
  LOOP AT <exceltab>-excel_fieldcat ASSIGNING FIELD-SYMBOL(<lt_field_catalog>).
*    IF <lt_field_catalog>-abap_type = 'C'.
**      <lt_field_catalog>-style = guid.
**      <lt_field_catalog>-style_header = guid.
**      <lt_field_catalog>-style_total = guid.
**      <lt_field_catalog>-style_cond = guid.
*    ENDIF.
    READ TABLE gt_fldctm ASSIGNING FIELD-SYMBOL(<gt_fldct>) WITH KEY fieldname = <lt_field_catalog>-fieldname.
    IF sy-subrc EQ 0.
      <lt_field_catalog>-scrtext_m  = |{ <gt_fldct>-scrtext_L }|.
    ELSE.
      <lt_field_catalog>-dynpfld = abap_false.
    ENDIF.
  ENDLOOP.

  TRY.
      CALL METHOD zcl_abap2xlsx_tools=>download
        EXPORTING
*         lo_excel          =
*         iv_writerclass_name =
*         iv_info_message   = ABAP_TRUE
          gc_save_file_name = |供应商{ gs_mir7-name1 }待开票明细{ sy-datum }|
*         gc_email          = ''
*         method            = 'send_email'
          method            = 'download_frontend'
          t_exceltab        = zcl_abap2xlsx_tools=>exceltab.
    CATCH zcx_excel INTO DATA(err).
      MESSAGE s000(oo) WITH err->get_text( ) DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form mir7
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM mir7 .
  DATA:text          TYPE string,
       gt_text_table TYPE TABLE OF tline.
  DATA: ls_headerdata       TYPE bapi_incinv_create_header,
        lv_invoicedocnumber TYPE bapi_incinv_fld-inv_doc_no,
        lv_fiscalyear       TYPE bapi_incinv_fld-fisc_year,
        lt_itemdata         TYPE TABLE OF bapi_incinv_create_item WITH HEADER LINE,
        lt_taxdata          TYPE TABLE OF bapi_incinv_create_tax WITH HEADER LINE,
        lt_return           TYPE TABLE OF bapiret2 WITH HEADER LINE,
        lv_item             TYPE rblgp,
        taxps               TYPE taxps,
        lv_message          TYPE char200.

  DATA:bktxt     TYPE bktxt.
  CLEAR:lv_item.
  CLEAR:bktxt,ls_headerdata.
  bktxt = |辅料发票：|.

  ls_headerdata-invoice_ind = 'X'.
  ls_headerdata-doc_type = 'RE'. "凭证类型
  ls_headerdata-doc_date = sy-datum. "凭证日期
  ls_headerdata-pstng_date = sy-datum. "过账日期
  ls_headerdata-bline_date = sy-datum.  "基准日期
  ls_headerdata-comp_code = p_bukrs. "公司代码
  ls_headerdata-currency = 'CNY'. " 货币
  ls_headerdata-calc_tax_ind = ''."自动计算税
  ls_headerdata-deliv_posting = 'S'.
  ls_headerdata-return_posting = 'H'.
*    ls_headerdata-ref_doc_no = it_ekpo_output200-fph. " 参考
  ls_headerdata-header_txt = bktxt. " 抬头文本
  CLEAR:lt_itemdata[],lt_taxdata[],lt_taxdata[].
  LOOP AT gt_mir7 INTO gs_mir7 WHERE belnr IS INITIAL.
    CLEAR lt_itemdata.
    lv_item += 1.
    lt_itemdata-de_cre_ind  = space.
    lt_itemdata-invoice_doc_item = lv_item.
    lt_itemdata-po_number = gs_mir7-ebeln. " 采购订单号
    lt_itemdata-po_item = gs_mir7-ebelp. " 采购订单行号
    lt_itemdata-tax_code = gs_mir7-mwskz. " 税码
    lt_itemdata-item_amount = gs_mir7-miro_sum. " 订单金额
    lt_itemdata-quantity = gs_mir7-menge_miro. " 开票数
    lt_itemdata-po_unit = gs_mir7-bstme. " 单位
    lt_itemdata-ref_doc = gs_mir7-mblnr. "参考物料凭证
    lt_itemdata-ref_doc_year = gs_mir7-mjahr.  "参考年份
    lt_itemdata-ref_doc_it = gs_mir7-zeile.  "参考凭证行
*    lt_itemdata-item_text = gs_mir7-fphm.
    APPEND lt_itemdata.
    CLEAR lt_taxdata.
    lt_taxdata-tax_code = gs_mir7-mwskz.
    lt_taxdata-tax_amount = gs_mir7-miro_sum_se. " 税额
    COLLECT lt_taxdata.
    ls_headerdata-gross_amount += gs_mir7-miro_sum_hs.
  ENDLOOP.
  IF sy-subrc NE 0.
    MESSAGE '已经预制过了' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  CLEAR taxps.
  LOOP AT lt_taxdata.
    ADD 10 TO taxps.
    lt_taxdata-itemno_tax = taxps.
    MODIFY lt_taxdata.
  ENDLOOP.

*开票
  CALL FUNCTION 'BAPI_INCOMINGINVOICE_PARK'
    EXPORTING
      headerdata       = ls_headerdata
*     ADDRESSDATA      =
    IMPORTING
      invoicedocnumber = lv_invoicedocnumber
      fiscalyear       = lv_fiscalyear
    TABLES
      itemdata         = lt_itemdata
*     ACCOUNTINGDATA   =
*     GLACCOUNTDATA    =
*     MATERIALDATA     =
      taxdata          = lt_taxdata
*     WITHTAXDATA      =
*     VENDORITEMSPLITDATA       =
      return           = lt_return
*     EXTENSIONIN      =
*     TM_ITEMDATA      =
*     NFMETALLITMS     =
*     ASSETDATA        =
    .

  LOOP AT lt_return WHERE type CA 'EAX'.
    lv_message = lv_message && lt_return-message && '；'.
  ENDLOOP.
  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    lv_message = |{ lv_invoicedocnumber }_{ lv_fiscalyear }|.
    CLEAR text.
    LOOP AT gt_mir7 ASSIGNING FIELD-SYMBOL(<g>).
      <g>-belnr = |{ lv_invoicedocnumber }{ lv_fiscalyear }|.
      DO 8 TIMES.
        ASSIGN COMPONENT |FPHM{ sy-index }| OF STRUCTURE <g> TO FIELD-SYMBOL(<fphm>).
        IF sy-subrc EQ 0.
          IF <fphm> IS NOT INITIAL.
            IF text IS INITIAL.
              text = |{ <fphm> }|.
            ELSEIF text NS <fphm>.
              text = |{ <fphm> }\|{ text }|.
            ENDIF.
            UPDATE ztmycsthead SET belnr = lv_invoicedocnumber gjahr = lv_fiscalyear bukrs = '' WHERE fphm = <fphm>.
            UNASSIGN <fphm>.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDLOOP.
    COMMIT WORK.
    CLEAR gt_text_table.
    CALL FUNCTION 'VB_CP_CONVERT_STRING_2_ITF'
      EXPORTING
        i_string = text
      TABLES
        et_table = gt_text_table.

    PERFORM frm_save_note IN PROGRAM zmycst_invoice_yc TABLES gt_text_table USING <g>-belnr.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.
  MESSAGE s000(oo) WITH lv_message(50) lv_message+50(50) lv_message+100(50) lv_message+150(50) .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form uploaddata
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM uploaddata .
  DATA:ret2    TYPE TABLE OF bapiret2,
       rfphm   TYPE RANGE OF ztmycsthead-fphm,
       message TYPE char300.
  CLEAR:gt_mir7.
  TRY.
      CALL METHOD zcl_abap2xlsx_tools=>upload
        IMPORTING
          tab = gt_mir7
*         dref   =
        .
    CATCH zcx_excel INTO DATA(cx_excel).
      DATA(etext) = cx_excel->if_message~get_text( ).
      MESSAGE e000(oo) WITH etext.
  ENDTRY.

  IF gt_mir7 IS INITIAL.
    MESSAGE s000(oo) WITH '无数据'.
    EXIT.
  ENDIF.
  CLEAR rfphm.
  LOOP AT gt_mir7 ASSIGNING FIELD-SYMBOL(<g7>).
    DO 8 TIMES.
      ASSIGN COMPONENT |FPHM{ sy-index }| OF STRUCTURE <g7> TO FIELD-SYMBOL(<fphm>).
      IF sy-subrc EQ 0.
        IF <fphm> IS NOT INITIAL.
          APPEND <fphm> TO <g7>-tfphm.
          INSERT INITIAL LINE INTO TABLE rfphm ASSIGNING FIELD-SYMBOL(<rfphm>).
          <rfphm>(3) = 'IEQ'.
          <rfphm>-low = <fphm>.
        ENDIF.
        UNASSIGN <fphm>.
      ENDIF.
    ENDDO.
  ENDLOOP.
  DATA:zsl     TYPE p DECIMALS 3,
       t_ftaxp TYPE TABLE OF ftaxp.
  IF rfphm IS NOT INITIAL.
    LOOP AT rfphm ASSIGNING FIELD-SYMBOL(<r>) GROUP BY ( low = <r>-low
      size = GROUP SIZE
       ) ASSIGNING FIELD-SYMBOL(<group>).
      IF <group>-size NE 1.
*        PERFORM inmsg(zpubform) TABLES ret2 USING 'ZXMD_MSG' 'E' '000' '金税发票号码' <group>-low '上传重复次数：' <group>-size.
      ENDIF.
    ENDLOOP.
    SELECT
      z~fpzl,
      z~fphm,
      CASE WHEN b~stblg IS INITIAL OR b~stblg IS NULL THEN z~belnr
      ELSE @space END AS belnr,
      CASE WHEN b~stblg IS INITIAL OR b~stblg IS NULL THEN z~gjahr
      ELSE @space END AS gjahr,
      CASE WHEN b~stblg IS INITIAL OR b~stblg IS NULL THEN z~bukrs
      ELSE @space END AS bukrs,
      concat( concat( z~belnr,z~gjahr ),z~bukrs ) AS belnrgjahr
      FROM ztmycsthead AS z
      LEFT JOIN bkpf AS b ON z~belnr = b~belnr AND z~gjahr = b~gjahr AND z~bukrs = b~bukrs
      WHERE z~fphm IN @rfphm
      AND z~bukrs NE @space
    UNION
    SELECT
      z~fpzl,
      z~fphm,
      CASE WHEN ( r~stblg IS INITIAL OR r~stblg IS NULL ) AND r~rbstat NE '2' THEN z~belnr
      ELSE @space END AS belnr,
      CASE WHEN ( r~stblg IS INITIAL OR r~stblg IS NULL ) AND r~rbstat NE '2' THEN z~gjahr
      ELSE @space END AS gjahr,
      CASE WHEN ( r~stblg IS INITIAL OR r~stblg IS NULL ) AND r~rbstat NE '2' THEN z~bukrs
      ELSE @space END AS bukrs,
      concat( concat( z~belnr,z~gjahr ),z~bukrs ) AS belnrgjahr
      FROM ztmycsthead AS z
      LEFT JOIN rbkp AS r ON z~belnr = r~belnr AND z~gjahr = r~gjahr
      WHERE z~fphm IN @rfphm
      AND z~bukrs EQ @space
      ORDER BY fphm
      INTO TABLE @DATA(tfpyz)
      .
  ENDIF.

  SELECT
  t007a~kalsm,
  t007a~mwskz,
  t007s~text1,
  @zsl AS zsl
  FROM t007a
  JOIN t007s ON t007a~mwskz = t007s~mwskz AND t007a~kalsm = t007s~kalsm AND t007s~spras = @sy-langu
  WHERE t007a~kalsm = 'TAXCN'
  AND t007a~mwskz LIKE 'J%'
  ORDER BY t007a~mwskz
  INTO TABLE @DATA(lt_t007a)
  .
  LOOP AT lt_t007a ASSIGNING FIELD-SYMBOL(<lt_t007a>).
    CLEAR t_ftaxp.
    CALL FUNCTION 'GET_TAX_PERCENTAGE'
      EXPORTING
        aland   = 'CN'
        datab   = sy-datum
        mwskz   = <lt_t007a>-mwskz
        txjcd   = ' '
*       EXPORT  = ' '
      TABLES
        t_ftaxp = t_ftaxp.
    READ TABLE t_ftaxp INTO DATA(w_ftaxp) WITH KEY kschl = 'MWVS'.
    IF sy-subrc EQ 0.
      <lt_t007a>-zsl = w_ftaxp-kbetr / 1000."税率
    ENDIF.
  ENDLOOP.
  LOOP AT gt_mir7 ASSIGNING FIELD-SYMBOL(<gt_mir7>).
    IF <gt_mir7>-tfphm IS INITIAL.
      PERFORM inmsg(zpubform) TABLES ret2 USING 'ZXMD_MSG' 'E' '151' <gt_mir7>-mblnr <gt_mir7>-zeile '' ''.
    ELSE.
      DO 8 TIMES.
        ASSIGN COMPONENT |FPHM{ sy-index }| OF STRUCTURE <gt_mir7> TO <fphm>.
        IF sy-subrc EQ 0.
          IF <fphm> IS NOT INITIAL.
            READ TABLE tfpyz INTO DATA(wfpyz) WITH KEY fphm = <fphm> BINARY SEARCH.
            IF sy-subrc NE 0.
              PERFORM inmsg(zpubform) TABLES ret2 USING 'ZXMD_MSG' 'E' '000' '金税发票号码' <fphm> '未存在于票帮手，联系财务核实' ''.
            ELSE.
              IF wfpyz-belnr IS NOT INITIAL.
                PERFORM inmsg(zpubform) TABLES ret2 USING 'ZXMD_MSG' 'E' '150' <fphm> wfpyz-belnrgjahr '' ''.
              ELSE.
                CLEAR message.
                PERFORM ezzmycst IN PROGRAM zmycst_invoice_yc USING '' wfpyz-fpzl wfpyz-fphm CHANGING message.
                IF message IS NOT INITIAL.
                  PERFORM inmsg(zpubform) TABLES ret2 USING 'ZXMD_MSG' 'E' '000' message(50) message+50(50) message+150(50) message+200(50).
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
          UNASSIGN <fphm>.
        ENDIF.
      ENDDO.
    ENDIF.
    READ TABLE lt_t007a ASSIGNING FIELD-SYMBOL(<a>) WITH KEY zsl = <gt_mir7>-zsl.
    IF sy-subrc EQ 0.
      <gt_mir7>-mwskz = <a>-mwskz.
    ENDIF.
  ENDLOOP.
  IF ret2 IS NOT INITIAL.
    PERFORM showmsg(zpubform) TABLES ret2.
    CLEAR gt_mir7.
  ENDIF.
ENDFORM.
