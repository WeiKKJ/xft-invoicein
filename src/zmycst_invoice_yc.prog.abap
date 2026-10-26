*&---------------------------------------------------------------------*
*& Report ZMYCST_INVOICE_FL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmycst_invoice_yc.
INCLUDE zmycst_types.
DATA:gs_out TYPE ty_out,
     gt_out LIKE TABLE OF gs_out WITH EMPTY KEY.
DATA zxh TYPE int4.
DATA:wtax TYPE ty_tax,
     ttax TYPE TABLE OF ty_tax.

DATA:seltab LIKE TABLE OF rsparams.
DATA: w_dataList TYPE ty_list,
      t_dataList LIKE TABLE OF w_dataList.
DATA:gv_subscreen_900 TYPE sy-dynnr VALUE '9001'.
DATA:BEGIN OF ls_headerdata.
       INCLUDE TYPE bapi_incinv_create_header.
DATA:  wmwst      TYPE wmwst,
       wmwst_po   TYPE wmwst,
       lifnr_new  TYPE lifnr,
       name1      TYPE name1,
       name1_new  TYPE name1,
       rpdifn     TYPE rpdifn,
       rpdifnse   TYPE rpdifn,
       iconname   TYPE iconname,
       iconnamese TYPE iconname,
       mir7rows   TYPE i VALUE 850,
     END OF ls_headerdata.
DATA:o_container TYPE REF TO cl_gui_custom_container,
     o_textedit  TYPE REF TO cl_gui_textedit.
DATA:container_po TYPE REF TO cl_gui_custom_container,
     alv_grid_po  TYPE REF TO cl_gui_alv_grid,
     gt_fldct_po  TYPE lvc_t_fcat,
     gs_slayt_po  TYPE lvc_s_layo.
DATA:container_js TYPE REF TO cl_gui_custom_container,
     alv_grid_js  TYPE REF TO cl_gui_alv_grid,
     gt_fldct_js  TYPE lvc_t_fcat,
     gs_slayt_js  TYPE lvc_s_layo.
DATA: t_po      TYPE zmm_po_item,
      t_po_sum  TYPE zmm_po_item,
      t_po_ekbz TYPE zmm_po_item.
DATA:zsl     TYPE p DECIMALS 3,
     zslc    TYPE c LENGTH 30,
     t_ftaxp TYPE TABLE OF ftaxp,
     message TYPE string.
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
  PARAMETERS p_ekorg TYPE ekko-ekorg OBLIGATORY DEFAULT '1000' MODIF ID m1.
  SELECT-OPTIONS s_ekgrp FOR gs_out-ekgrp MODIF ID m2.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (31) t1 FOR FIELD p_lifnr MODIF ID m2.
    PARAMETERS p_lifnr TYPE ekko-lifnr MODIF ID m2.
    SELECTION-SCREEN COMMENT (35) t2 FOR FIELD p_lifnr MODIF ID m2.
  SELECTION-SCREEN END OF LINE.

  SELECT-OPTIONS s_aedat FOR gs_out-budat.
  SELECT-OPTIONS s_kpdat FOR gs_out-budat NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE btxt2.
  PARAMETERS p1 RADIOBUTTON GROUP prd1 USER-COMMAND ss1 DEFAULT 'X'.
  PARAMETERS p2 RADIOBUTTON GROUP prd1.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF SCREEN 1001 AS WINDOW.
  PARAMETERS:p_lifnrp LIKE gs_out-lifnr NO-DISPLAY.
  SELECT-OPTIONS:s_ebeln FOR gs_out-ebeln.
  SELECT-OPTIONS:s_budat FOR gs_out-budat.
SELECTION-SCREEN END OF SCREEN 1001.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_lifnr.
  PERFORM f4_lifnr.
  PERFORM update_t2.

AT SELECTION-SCREEN OUTPUT.
  btxt1 = '数据筛选'(t01).
  btxt2 = '功能选择'.
  t1 = '金税销方'.
  SELECT SINGLE name1 INTO @t2 FROM lfa1 WHERE lifnr = @p_lifnr.
  LOOP AT SCREEN.
    IF screen-name = 'P_EKGRP' OR screen-name = 'P_LIFNR' OR screen-name EQ 'S_AEDAT-LOW'.
      screen-required = 2.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN. "PAI
  CASE sy-ucomm.
    WHEN 'ONLI'.
      PERFORM auth_check.
  ENDCASE.
*  SELECT SINGLE name1 INTO t2 FROM lfa1 WHERE lifnr = p_lifnr.

INITIALIZATION.
  INCLUDE zmycst_invoice_yc_class.
  DATA:event_receiver TYPE REF TO lcl_event_receiver.

START-OF-SELECTION.
  IF p_lifnr IS INITIAL.
    MESSAGE '请输入金税销方' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ELSE.
    SELECT SINGLE name1 FROM lfa1 WHERE LIfnr = @p_lifnr INTO @DATA(name1).
    IF name1 IS INITIAL.
      MESSAGE s000(oo) WITH '金税销方' p_lifnr '不存在' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDIF.
  CLEAR:seltab.
  REPLACE ALL OCCURRENCES OF '(' IN name1 WITH '（'.
  REPLACE ALL OCCURRENCES OF ')' IN name1 WITH '）'.
  seltab = VALUE #(
  ( selname = 'S_XFMC' kind = 'S' sign = 'I' option = 'EQ' low = name1 )
  ( selname = 'P_BUKRS' kind = 'P' sign = 'I' option = 'EQ' low = p_bukrs )
  ( selname = 'P1' kind = 'P' sign = 'I' option = 'EQ' low = abap_true )
  ( selname = 'P_MIR7' kind = 'P' sign = 'I' option = 'EQ' low = abap_true )
  ( selname = 'S_KPRQ' kind = 'S' sign = s_kpdat-sign option = s_kpdat-option low = s_kpdat-low high = s_kpdat-high )
   ).
  REPLACE ALL OCCURRENCES OF '（' IN name1 WITH '('.
  REPLACE ALL OCCURRENCES OF '）' IN name1 WITH ')'.
  seltab = VALUE #( BASE seltab
  ( selname = 'S_XFMC' kind = 'S' sign = 'I' option = 'EQ' low = name1 )
   ).

  ls_headerdata-doc_date = ls_headerdata-pstng_date = ls_headerdata-bline_date = sy-datum.
  SUBMIT zmycst
  WITH SELECTION-TABLE seltab
  AND RETURN
  .
  CLEAR t_dataList.
  IMPORT invoice = t_dataList FROM MEMORY ID 'XFT_INVOICE_IN'.
  IF ( sy-subrc <> 0 OR t_dataList IS INITIAL ).
    MESSAGE s000(oo) WITH '请选择待核销的金税发票' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  FREE MEMORY ID 'XFT_INVOICE_IN'.
  LOOP AT t_dataList ASSIGNING FIELD-SYMBOL(<tmy>).
    CLEAR message.
    PERFORM ezzmycst USING '' <tmy>-fpzl <tmy>-fphm CHANGING message.
    IF message IS NOT INITIAL.
      MESSAGE s000(oo) WITH message DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDLOOP.
  SELECT
    blart,
    ltext
    FROM t003t
    WHERE spras = @sy-langu
    ORDER BY blart
    INTO TABLE @DATA(t003)
  .
  SELECT
    lgort,
    lgobe
    FROM t001l
    WHERE werks = @p_werks
    ORDER BY lgort
    INTO TABLE @DATA(tlgobe)
  .
  SELECT
    ekgrp,
    eknam
    FROM t024
    ORDER BY ekgrp
    INTO TABLE @DATA(teknam)
  .
  SELECT
  t007a~kalsm,
  t007a~mwskz,
  t007s~text1,
  @zsl AS zsl,
  @zslc AS zslc
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
      <lt_t007a>-zslc = <lt_t007a>-zsl * 100.
      CONDENSE <lt_t007a>-zslc NO-GAPS.
    ENDIF.
  ENDLOOP.
  PERFORM getdata.

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
  CLEAR:t_po,t_po_sum,t_po_ekbz.
  SELECT
    ekpo~ebeln,
    ekpo~ebelp,
    ekko~lifnr
    FROM ekko
    JOIN ekpo ON ekko~ebeln = ekpo~ebeln
    WHERE ekko~lifnr = @p_lifnr
    AND ekko~bukrs = @p_bukrs
    AND ekko~ekorg = @p_ekorg
    AND ekko~ekgrp IN @s_ekgrp
    AND ekpo~werks = @p_werks
    AND ekko~zhtlx NE '01'
    AND ekpo~webre = @abap_true
    AND ekko~aedat IN @s_aedat
    INTO CORRESPONDING FIELDS OF TABLE @t_po
  .

  CALL FUNCTION 'ZMM_FM_PO_INVENCE_LIST'
    EXPORTING
      ekbz    = p2
    CHANGING
      po      = t_po
      po_sum  = t_po_sum
      po_ekbz = t_po_ekbz.

  CLEAR gt_out.
  IF p1 = 'X'.
    PERFORM prepare_gtout TABLES t_po gt_out.
  ELSE.
    PERFORM prepare_gtout TABLES t_po_ekbz gt_out.
  ENDIF.
  IF p2 NE 'X'.
    IF gt_out IS INITIAL.
      MESSAGE s000(oo) WITH '没有待核销的收货信息了' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDIF.
  ls_headerdata-lifnr_new = p_lifnr.
  ls_headerdata-doc_type = 'RE'.
  ls_headerdata-currency = 'CNY'.
  PERFORM calc_js.
  CALL SCREEN 900.
  RETURN.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form prepare_gtout
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> TPO
*&---------------------------------------------------------------------*
FORM prepare_gtout  TABLES p_tpo STRUCTURE zmm_po_item_line
                           p_tout STRUCTURE gs_out.
  DATA:msg  TYPE bapi_msg,
       ret2 TYPE TABLE OF bapiret2.
  SELECT
    lifnr,
    name1
    FROM lfa1
    FOR ALL ENTRIES IN @p_tpo
    WHERE lifnr = @p_tpo-lifnr
    INTO TABLE @DATA(tname1)
  .
  SORT tname1 BY lifnr.
  LOOP AT p_tpo ASSIGNING FIELD-SYMBOL(<t_po>).
    IF p1 = 'X'.
      READ TABLE p_tout TRANSPORTING NO FIELDS WITH KEY mblnr = <t_po>-mblnr_noo mjahr = <t_po>-mjahr_noo zeile = <t_po>-zeile_noo.
    ELSE.
      READ TABLE p_tout TRANSPORTING NO FIELDS WITH KEY ebeln = <t_po>-ebeln ebelp = <t_po>-ebelp kschl = <t_po>-kschl_noo.
    ENDIF.
    IF sy-subrc EQ 0.
      CONTINUE.
    ENDIF.
    DATA(mengems) = CONV menge_d( <t_po>-menge_po101 - <t_po>-menge_fp ) .
    IF mengems EQ 0.
      CONTINUE.
    ELSEIF mengems GT 0.
      CLEAR msg.
      PERFORM ezpo USING '' <t_po>-ebeln <t_po>-ebelp CHANGING msg.
      IF msg IS NOT INITIAL.
        PERFORM inmsg(zpubform) TABLES ret2 USING 'ZXMD_MSG' 'E' '000' msg(50) msg+50(50) msg+100(50) msg+150(50).
        CONTINUE.
      ENDIF.
      READ TABLE tname1 INTO DATA(name1) WITH KEY lifnr = <t_po>-lifnr BINARY SEARCH.
      CLEAR:p_tout.
      MOVE-CORRESPONDING <t_po> TO p_tout.
      p_tout-kschl = <t_po>-kschl_noo.
      p_tout-vtext = <t_po>-vtext_noo.
      p_tout-mblnr = <t_po>-mblnr_noo.
      p_tout-mjahr = <t_po>-mjahr_noo.
      p_tout-zeile = <t_po>-zeile_noo.
      p_tout-budat = <t_po>-budat_noo.
      p_tout-menge_mir7 = <t_po>-menge_yz.
      p_tout-menge_miroend = <t_po>-menge_fp.
      p_tout-menge_miro = mengems.
      p_tout-miro_sum_hs = p_tout-menge_miro * <t_po>-netpr_hs.
      p_tout-miro_sum = p_tout-miro_sum_hs / ( 1 + <t_po>-zsl ).
      p_tout-miro_sum_se = p_tout-miro_sum_hs - p_tout-miro_sum.
      p_tout-lifnr = <t_po>-lifnr.
      p_tout-name1 = name1-name1.
      READ TABLE tlgobe INTO DATA(lgobe) WITH KEY lgort = <t_po>-lgort_noo BINARY SEARCH.
      IF sy-subrc EQ 0.
        p_tout-lgobe = lgobe-lgobe.
      ENDIF.
      READ TABLE teknam INTO DATA(eknam) WITH KEY ekgrp = <t_po>-ekgrp BINARY SEARCH.
      IF sy-subrc EQ 0.
        p_tout-eknam = eknam-eknam.
      ENDIF.
      APPEND p_tout.
    ENDIF.
    CLEAR:mengems,name1.
  ENDLOOP.
  IF ret2 IS NOT INITIAL.
    PERFORM showmsg(zpubform) TABLES ret2.
  ENDIF.
  CLEAR zxh.
  LOOP AT p_tout ASSIGNING FIELD-SYMBOL(<out>).
    zxh += 1.
    <out>-zxh = zxh.
  ENDLOOP.
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
      ls_fldcat-qfieldname = 'BSTME'.
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
    WHEN 'MENGE_MIRO' OR 'MIRO_SUM'.
      IF p2 = 'X' OR p1 = 'X'.
        ls_fldcat-edit = 'X'.
      ENDIF.
      IF ls_fldcat-fieldname = 'MIRO_SUM'.
        ls_fldcat-do_sum = 'X'.
      ENDIF.
    WHEN 'MIRO_SUM_SE'.
      ls_fldcat-do_sum = 'X'.
    WHEN 'KPJE' OR 'KPSE' OR 'JSHJ'.
      ls_fldcat-do_sum = 'X'.
  ENDCASE.

  APPEND ls_fldcat TO t_fldcat.
  CLEAR ls_fldcat.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form mir7
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM mir7 .
*  TYPES: BEGIN OF stream,
*           default TYPE c LENGTH  132,
*         END OF stream.
*  DATA: g_textstream  TYPE STANDARD TABLE OF stream,
*        gt_text_table TYPE TABLE OF tline.
*  DATA: wa_headerdata       TYPE bapi_incinv_create_header,
*        lv_invoicedocnumber TYPE bapi_incinv_fld-inv_doc_no,
*        lv_fiscalyear       TYPE bapi_incinv_fld-fisc_year,
*        lt_itemdata         TYPE TABLE OF bapi_incinv_create_item WITH HEADER LINE,
*        lt_return           TYPE TABLE OF bapiret2 WITH HEADER LINE,
*        lv_item             TYPE rblgp,
*        taxps               TYPE taxps,
*        lv_message          TYPE char200,
*        lt_taxdata          TYPE TABLE OF bapi_incinv_create_tax,
*        wa_taxdata          TYPE bapi_incinv_create_tax.
*  DATA:bktxt     TYPE bktxt.
*  IF ls_headerdata-rpdifn NE 0.
*    MESSAGE s000(oo) WITH '请处理余额后再操作' DISPLAY LIKE 'E'.
*    RETURN.
*  ENDIF.
*  LOOP AT t_dataList TRANSPORTING NO FIELDS WHERE fps IS NOT INITIAL.
*    EXIT.
*  ENDLOOP.
*  IF sy-subrc EQ 0.
*    MESSAGE '金税发票已经预制过了' TYPE 'S' DISPLAY LIKE 'E'.
*    RETURN.
*  ENDIF.
*
*  CLEAR:lv_item.
*  CLEAR:bktxt,wa_headerdata.
*  bktxt = |原材发票：|.
*  MOVE-CORRESPONDING ls_headerdata TO wa_headerdata.
*  wa_headerdata-invoice_ind = 'X'.
**  wa_headerdata-doc_type = 'RE'. "凭证类型
*  wa_headerdata-comp_code = p_bukrs. "公司代码
**  wa_headerdata-currency = 'CNY'. " 货币
*  wa_headerdata-calc_tax_ind = ''."自动计算税
*  wa_headerdata-deliv_posting = 'S'.
*  wa_headerdata-return_posting = 'H'.
**    wa_headerdata-ref_doc_no = it_ekpo_output200-fph. " 参考
*
*  CLEAR:lt_itemdata[],lt_taxdata,wa_taxdata.
*  LOOP AT gt_out INTO gs_out WHERE belnr IS INITIAL.
*    CLEAR lt_itemdata.
*    lv_item += 1.
*    lt_itemdata-de_cre_ind  = space.
*    lt_itemdata-invoice_doc_item = lv_item.
*    lt_itemdata-po_number = gs_out-ebeln. " 采购订单号
*    lt_itemdata-po_item = gs_out-ebelp. " 采购订单行号
*    lt_itemdata-tax_code = gs_out-mwskz. " 税码
*    lt_itemdata-item_amount = gs_out-miro_sum. " 订单金额
*    lt_itemdata-quantity = gs_out-menge_miro. " 开票数
*    lt_itemdata-po_unit = gs_out-bstme. " 单位
*    lt_itemdata-ref_doc = gs_out-mblnr. "参考物料凭证
*    lt_itemdata-ref_doc_year = gs_out-mjahr.  "参考年份
*    lt_itemdata-ref_doc_it = gs_out-zeile.  "参考凭证行
*    lt_itemdata-cond_type = gs_out-kschl.  "参考凭证行
**    lt_itemdata-item_text = gs_out-fphm.
*    APPEND lt_itemdata.
*  ENDLOOP.
*  IF sy-subrc NE 0.
*    MESSAGE '采购订单已经预制过了' TYPE 'S' DISPLAY LIKE 'E'.
*    RETURN.
*  ENDIF.
*  CLEAR:lt_taxdata,taxps.
*  LOOP AT ttax INTO wtax.
*    CLEAR:wa_taxdata.
*    ADD 10 TO taxps.
*    wa_taxdata-itemno_tax = taxps.
*    wa_taxdata-tax_code = wtax-tax_code.
*    wa_taxdata-tax_amount = wtax-tax_amount.
*    APPEND wa_taxdata TO lt_taxdata.
*  ENDLOOP.
*
**开票
*  CALL FUNCTION 'BAPI_INCOMINGINVOICE_PARK'
*    EXPORTING
*      headerdata       = wa_headerdata
**     ADDRESSDATA      =
*    IMPORTING
*      invoicedocnumber = lv_invoicedocnumber
*      fiscalyear       = lv_fiscalyear
*    TABLES
*      itemdata         = lt_itemdata
**     ACCOUNTINGDATA   =
**     GLACCOUNTDATA    =
**     MATERIALDATA     =
*      taxdata          = lt_taxdata
**     WITHTAXDATA      =
**     VENDORITEMSPLITDATA       =
*      return           = lt_return
**     EXTENSIONIN      =
**     TM_ITEMDATA      =
**     NFMETALLITMS     =
**     ASSETDATA        =
*    .
*
*  LOOP AT lt_return WHERE type CA 'EAX'.
*    lv_message = lv_message && lt_return-message && '；'.
*  ENDLOOP.
*  IF sy-subrc NE 0.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.
*    lv_message = |{ lv_invoicedocnumber }_{ lv_fiscalyear }|.
*    LOOP AT gt_out ASSIGNING FIELD-SYMBOL(<g>).
*      <g>-belnr = |{ lv_invoicedocnumber }{ lv_fiscalyear }|.
*    ENDLOOP.
*    LOOP AT t_dataList ASSIGNING FIELD-SYMBOL(<t>).
*      <t>-belnr = lv_invoicedocnumber.
*      <t>-gjahr = lv_fiscalyear.
*      UPDATE ztmycsthead SET belnr = lv_invoicedocnumber gjahr = lv_fiscalyear bukrs = '' WHERE fphm = <t>-fphm.
*    ENDLOOP.
*    COMMIT WORK.
*    IF o_textedit IS BOUND.
**取屏幕上输入的文本
*      CALL METHOD o_textedit->get_text_as_stream
*        IMPORTING
*          text                   = g_textstream
*        EXCEPTIONS
*          error_dp               = 1
*          error_cntl_call_method = 2
*          OTHERS                 = 3.
*      IF sy-subrc = 0.
***将文本流转为内表
*        CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
*          EXPORTING
*            language    = sy-langu
*          TABLES
*            itf_text    = gt_text_table
*            text_stream = g_textstream.
*        PERFORM frm_save_note TABLES gt_text_table USING <g>-belnr.
*      ENDIF.
*    ENDIF.
*  ELSE.
*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*  ENDIF.
*  MESSAGE s000(oo) WITH lv_message(50) lv_message+50(50) lv_message+100(50) lv_message+150(50) .
*ENDFORM.

FORM mir7_999 USING p_save_ok.
  DATA: lv_from    TYPE sy-tabix VALUE 1,
        lv_to      TYPE sy-tabix,
        tfp        LIKE gt_out,
        tztmycstfp TYPE TABLE OF ztmycstfp.
  TYPES: BEGIN OF stream,
           default TYPE c LENGTH  132,
         END OF stream.
  DATA: g_textstream  TYPE STANDARD TABLE OF stream,
        gt_text_table TYPE TABLE OF tline.
  DATA: wa_headerdata       TYPE bapi_incinv_create_header,
        lv_invoicedocnumber TYPE bapi_incinv_fld-inv_doc_no,
        lv_fiscalyear       TYPE bapi_incinv_fld-fisc_year,
        lt_itemdata         TYPE TABLE OF bapi_incinv_create_item WITH HEADER LINE,
        lt_return           TYPE TABLE OF bapiret2 WITH HEADER LINE,
        lv_item             TYPE rblgp,
        taxps               TYPE taxps,
        lv_message          TYPE char200,
        lt_taxdata          TYPE TABLE OF bapi_incinv_create_tax,
        wa_taxdata          TYPE bapi_incinv_create_tax.
  DATA:bktxt TYPE bktxt,
       ret2  TYPE TABLE OF bapiret2 WITH HEADER LINE.
  DATA msg TYPE bapi_msg.
  DATA lv_float TYPE decfloat16.
  DATA:se          LIKE gs_out-miro_sum_se,
       miro_sum    LIKE gs_out-miro_sum,
       miro_sum_se LIKE gs_out-miro_sum_se
       .
  IF ls_headerdata-rpdifn NE 0." OR ls_headerdata-rpdifnse NE 0.
    MESSAGE s000(oo) WITH '请处理余额后再操作' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  IF ls_headerdata-mir7rows LT 1.
    MESSAGE '预制凭证行数不能小于1' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  LOOP AT t_dataList TRANSPORTING NO FIELDS WHERE fps IS NOT INITIAL.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    MESSAGE '金税发票已经预制过了' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CLEAR:bktxt,wa_headerdata.
  bktxt = |原材发票：|.
  MOVE-CORRESPONDING ls_headerdata TO wa_headerdata.
  wa_headerdata-invoice_ind = 'X'.
  wa_headerdata-comp_code = p_bukrs. "公司代码
  wa_headerdata-calc_tax_ind = ''."自动计算税
  wa_headerdata-deliv_posting = 'S'.
  wa_headerdata-return_posting = 'H'.
*  wa_headerdata-ref_doc_no = ||. " 参考
  wa_headerdata-business_place = 'MYCT'.
  LOOP AT gt_out INTO gs_out WHERE belnr IS INITIAL.
    EXIT.
  ENDLOOP.
  IF sy-subrc NE 0.
    MESSAGE '采购订单已经预制过了' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  tfp = gt_out.
  DELETE tfp WHERE belnr IS NOT INITIAL.
  lv_float = lines( tfp ) / ls_headerdata-mir7rows.
  DATA(mir7count) = CONV i( ceil( lv_float ) ).
*  IF mir7count NE 1.
*    MESSAGE '程序优化中，暂不支持操作' TYPE 'S' DISPLAY LIKE 'E'.
*    RETURN.
*  ENDIF.
  CLEAR:miro_sum_se,miro_sum.
  WHILE lv_from <= lines( tfp ).
    lv_to = lv_from + ls_headerdata-mir7rows - 1.
    IF lv_to > lines( tfp  ).
      lv_to = lines( tfp  ).
    ENDIF.
    CLEAR:lt_itemdata[],wa_taxdata,lv_item,wa_headerdata-gross_amount.
    CLEAR:lt_taxdata.
    LOOP AT tfp INTO DATA(wa) FROM lv_from TO lv_to.
      CLEAR lt_itemdata.
      lv_item += 1.
      lt_itemdata-de_cre_ind  = space.
      lt_itemdata-invoice_doc_item = lv_item.
      lt_itemdata-po_number = wa-ebeln. " 采购订单号
      lt_itemdata-po_item = wa-ebelp. " 采购订单行号
      lt_itemdata-tax_code = wa-mwskz. " 税码
      lt_itemdata-item_amount = wa-miro_sum. " 订单金额
      lt_itemdata-quantity = wa-menge_miro. " 开票数
      lt_itemdata-po_unit = wa-bstme. " 单位
      lt_itemdata-ref_doc = wa-mblnr. "参考物料凭证
      lt_itemdata-ref_doc_year = wa-mjahr.  "参考年份
      lt_itemdata-ref_doc_it = wa-zeile.  "参考凭证行
      lt_itemdata-cond_type = wa-kschl.  "参考凭证行
      APPEND lt_itemdata.
      wa_headerdata-gross_amount += wa-miro_sum_hs. "SAP预制凭证金额
      CLEAR:wa_taxdata.
      wa_taxdata-tax_code = wa-mwskz.
      wa_taxdata-tax_amount = wa-miro_sum_se.
      COLLECT wa_taxdata INTO lt_taxdata.
      miro_sum += lt_itemdata-item_amount.
    ENDLOOP.
    CLEAR:taxps,se.
    IF mir7count = 1.
      wa_headerdata-gross_amount = ls_headerdata-gross_amount.
      CLEAR lt_taxdata.
      LOOP AT ttax INTO wtax.
        CLEAR:wa_taxdata.
        ADD 10 TO taxps.
        wa_taxdata-itemno_tax = taxps.
        wa_taxdata-tax_code = wtax-tax_code.
        wa_taxdata-tax_amount = wtax-tax_amount.
        se += wa_taxdata-tax_amount.
        miro_sum_se += wa_taxdata-tax_amount.
        APPEND wa_taxdata TO lt_taxdata.
      ENDLOOP.
    ELSE.
      LOOP AT lt_taxdata INTO wa_taxdata.
        ADD 10 TO taxps.
        wa_taxdata-itemno_tax = taxps.
        se += wa_taxdata-tax_amount.
        miro_sum_se += wa_taxdata-tax_amount.
        MODIFY lt_taxdata FROM wa_taxdata.
      ENDLOOP.
      IF lv_to = lines( tfp  ). " 生成多张预置凭证的最后一张需要处理税额、含税金额的尾差  28.08.2026 17:21:45 by kkw
        wa_headerdata-gross_amount += ls_headerdata-rpdifnse.
        se += ls_headerdata-rpdifnse.
        miro_sum_se += ls_headerdata-rpdifnse.
        READ TABLE lt_taxdata ASSIGNING FIELD-SYMBOL(<lt_taxdata>) INDEX 1.
        IF sy-subrc EQ 0.
          <lt_taxdata>-tax_amount += ls_headerdata-rpdifnse.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR:lv_invoicedocnumber,lv_fiscalyear,lt_return.
    IF p_save_ok = 'MIR7T'.
      msg = |含税金额：{ wa_headerdata-gross_amount }，不含税金额：{ wa_headerdata-gross_amount - se }，税额：{ se }|.
      PERFORM inmsg(zpubform) TABLES ret2 USING 'ZXMD_MSG' 'S' '000' msg(50) msg+50(50) msg+100(50) msg+150(50).
    ELSE.
*开票
      CALL FUNCTION 'BAPI_INCOMINGINVOICE_PARK'
        EXPORTING
          headerdata       = wa_headerdata
*         ADDRESSDATA      =
        IMPORTING
          invoicedocnumber = lv_invoicedocnumber
          fiscalyear       = lv_fiscalyear
        TABLES
          itemdata         = lt_itemdata
*         ACCOUNTINGDATA   =
*         GLACCOUNTDATA    =
*         MATERIALDATA     =
          taxdata          = lt_taxdata
*         WITHTAXDATA      =
*         VENDORITEMSPLITDATA       =
          return           = lt_return
*         EXTENSIONIN      =
*         TM_ITEMDATA      =
*         NFMETALLITMS     =
*         ASSETDATA        =
        .
      LOOP AT lt_return WHERE type CA 'EAX'.
        PERFORM inmsg(zpubform) TABLES ret2 USING lt_return-id lt_return-type lt_return-number lt_return-message_v1
              lt_return-message_v2 lt_return-message_v3 lt_return-message_v4.
      ENDLOOP.
      IF sy-subrc NE 0.
        msg = |预制凭证号：{ lv_invoicedocnumber },预制凭证年：{ lv_fiscalyear },含税金额：{ wa_headerdata-gross_amount }，| &&
        |不含税金额：{ wa_headerdata-gross_amount - miro_sum_se }，税额：{ miro_sum_se }|.
*      PERFORM inmsg(zpubform) TABLES ret2 USING 'ZXMD_MSG' 'S' '000' '预制凭证号：' lv_invoicedocnumber '预制凭证年：' lv_fiscalyear.
        PERFORM inmsg(zpubform) TABLES ret2 USING 'ZXMD_MSG' 'S' '000' msg(50) msg+50(50) msg+100(50) msg+150(50).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        LOOP AT tfp INTO wa FROM lv_from TO lv_to.
          READ TABLE gt_out ASSIGNING FIELD-SYMBOL(<g>) WITH KEY zxh = wa-zxh.
          IF sy-subrc EQ 0.
            <g>-belnr = |{ lv_invoicedocnumber }{ lv_fiscalyear }|.
          ENDIF.
        ENDLOOP.
        CLEAR tztmycstfp.
        LOOP AT t_dataList ASSIGNING FIELD-SYMBOL(<t>).
          <t>-belnr = lv_invoicedocnumber.
          <t>-gjahr = lv_fiscalyear.
          APPEND INITIAL LINE TO tztmycstfp ASSIGNING FIELD-SYMBOL(<tztmycstfp>).
          <tztmycstfp>-belnr  = lv_invoicedocnumber.
          <tztmycstfp>-gjahr  = lv_fiscalyear.
          <tztmycstfp>-bukrs  = ''.
          <tztmycstfp>-fpzl   = <t>-fpzl.
          <tztmycstfp>-fphm   = <t>-fphm.
        ENDLOOP.
        IF tztmycstfp IS NOT INITIAL.
          INSERT ztmycstfp FROM TABLE tztmycstfp.
          COMMIT WORK.
        ENDIF.
        IF o_textedit IS BOUND.
*取屏幕上输入的文本
          CALL METHOD o_textedit->get_text_as_stream
            IMPORTING
              text                   = g_textstream
            EXCEPTIONS
              error_dp               = 1
              error_cntl_call_method = 2
              OTHERS                 = 3.
          IF sy-subrc = 0.
**将文本流转为内表
            CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
              EXPORTING
                language    = sy-langu
              TABLES
                itf_text    = gt_text_table
                text_stream = g_textstream.
            PERFORM frm_save_note TABLES gt_text_table USING <g>-belnr.
          ENDIF.
        ENDIF.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.
    ENDIF.
    lv_from = lv_to + 1.
  ENDWHILE.
  msg = |总含税金额：{ ls_headerdata-gross_amount }，总不含税金额：{ miro_sum }，总税额：{ miro_sum_se }|.
  PERFORM inmsg(zpubform) TABLES ret2 USING 'ZXMD_MSG' 'S' '000' msg(50) msg+50(50) msg+100(50) msg+150(50).
  PERFORM showmsg(zpubform) TABLES ret2.
ENDFORM.

FORM frm_save_note TABLES p_text_table STRUCTURE tline
                   USING p_belnr .
  DATA:ls_header TYPE thead.

  CLEAR ls_header .
  ls_header-tdobject = 'RBKP' .
  ls_header-tdtitle = 'Note' .
  ls_header-tdid      = '0001'.
  ls_header-tdspras   = sy-langu.
  ls_header-tdname = p_belnr.

  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      client          = sy-mandt
      header          = ls_header
      savemode_direct = 'X'
    TABLES
      lines           = p_text_table[].
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    COMMIT WORK AND WAIT.
  ENDIF.
ENDFORM.

FORM ezpo USING p_unlock l_ebeln l_ebelp CHANGING l_msg.
  DATA:l_usrefus TYPE usrefus.
  CLEAR:l_msg.
  IF p_unlock NE 'X'."加锁.
    CALL FUNCTION 'ENQUEUE_EMEKPOE'
      EXPORTING
*       MODE_EKPO      = 'E'
*       MANDT          = SY-MANDT
        ebeln          = l_ebeln
        ebelp          = l_ebelp
*       X_EBELN        = ' '
*       X_EBELP        = ' '
        _scope         = '1'
*       _WAIT          = ' '
*       _COLLECT       = ' '
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      SELECT SINGLE *
      INTO l_usrefus
      FROM usrefus
      WHERE bname = sy-msgv1.
      l_msg = |用户{ sy-msgv1 }({ l_usrefus-useralias })正在处理{ l_ebeln }_{ l_ebelp }|.
    ENDIF.
  ELSE.
    CALL FUNCTION 'DEQUEUE_EMEKPOE'
      EXPORTING
*       MODE_EKPO       = 'E'
*       MANDT  = SY-MANDT
        ebeln  = l_ebeln
        ebelp  = l_ebelp
*       X_EBELN         = ' '
*       X_EBELP         = ' '
        _scope = '1'
*       _SYNCHRON       = ' '
*       _COLLECT        = ' '
      .
  ENDIF.
ENDFORM.

FORM ezzmycst USING p_unlock l_FPZL l_FPHM CHANGING l_msg.
  DATA:l_usrefus TYPE usrefus.
  CLEAR:l_msg.
  IF p_unlock NE 'X'."加锁.
    CALL FUNCTION 'ENQUEUE_EZMYCST'
      EXPORTING
*       MODE_ZTMYCSTHEAD       = 'E'
*       MANDT          = SY-MANDT
        fpzl           = l_FPZL
        fphm           = l_FPHM
*       X_FPZL         = ' '
*       X_FPHM         = ' '
        _scope         = '1'
*       _WAIT          = ' '
*       _COLLECT       = ' '
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      SELECT SINGLE *
      INTO l_usrefus
      FROM usrefus
      WHERE bname = sy-msgv1.
      l_msg = |用户{ sy-msgv1 }({ l_usrefus-useralias })正在处理{ l_FPZL }_{ l_FPHM }|.
    ENDIF.
  ELSE.
    CALL FUNCTION 'DEQUEUE_EZMYCST'
      EXPORTING
*       MODE_ZTMYCSTHEAD       = 'E'
*       MANDT  = SY-MANDT
        fpzl   = l_FPZL
        fphm   = l_FPHM
*       X_FPZL = ' '
*       X_FPHM = ' '
        _scope = '1'
*       _SYNCHRON              = ' '
*       _COLLECT               = ' '
      .
  ENDIF.
ENDFORM.


INCLUDE zmycst_invoice_yc_pbo_09o01.

INCLUDE zmycst_invoice_yc_pai_09o01.

*&SPWizard: Data incl. inserted by SP Wizard. DO NOT CHANGE THIS LINE!
INCLUDE zmycst_invoice_yc_tax .
INCLUDE zmycst_invoice_yc_data_item .
INCLUDE zmycst_invoice_yc_data_head .
*&---------------------------------------------------------------------*
*& Form f4_lifnr
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f4_lifnr .
  DATA: lt_return TYPE TABLE OF ddshretval.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname    = 'EKKO'
      fieldname  = 'LIFNR'
    TABLES
      return_tab = lt_return.

  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
  IF sy-subrc = 0.
    p_lifnr = ls_return-fieldval.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form update_t2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_t2 .
  DATA(lifnr) = |{ p_lifnr ALPHA = IN }|.
  SELECT SINGLE name1
    INTO @DATA(lv_name)
    FROM lfa1
    WHERE lifnr = @lifnr.

  t2 = lv_name.
  DATA(dynpfields) = VALUE dynpread_tty(
          ( fieldname = 'T2' fieldvalue = t2 )
        ).
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = sy-repid
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.
