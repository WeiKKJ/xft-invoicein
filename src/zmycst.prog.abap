*&---------------------------------------------------------------------*
*& Report ZMMXFT_INVOICE_IN_LIST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmycst.
TABLES:sscrfields,rbkp.

INCLUDE zmycst_types.
DATA: gs_slayt      TYPE lvc_s_layo,
      gt_fldct_head TYPE lvc_t_fcat,
      gt_fldct_item TYPE lvc_t_fcat.
DATA:input      TYPE zsmycst_jxmxin,
     output     TYPE ty_output,
     rtype      TYPE bapi_mtype,
     rtmsg      TYPE bapi_msg,
     w_dataList TYPE ty_list,
     t_dataList LIKE TABLE OF w_dataList,
     w_details  TYPE LINE OF ty_list-details.
DATA:alv_grid_head          TYPE REF TO cl_gui_alv_grid,
     alv_grid_item          TYPE REF TO cl_gui_alv_grid,
     alv_container          TYPE REF TO cl_gui_docking_container,
     alv_splitter_container TYPE REF TO cl_gui_splitter_container,
     ref_container_head     TYPE REF TO cl_gui_container,
     ref_container_item     TYPE REF TO cl_gui_container.
DATA:rkprq TYPE RANGE OF char10 WITH HEADER LINE.
DATA:BEGIN OF w_mycst,
       fps  TYPE tytfp,
       nofi,
       zh   TYPE ztmycsthead,
     END OF w_mycst,
     t_mycst LIKE TABLE OF w_mycst.
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
  PARAMETERS p_bukrs TYPE bukrs AS LISTBOX VISIBLE LENGTH 30 DEFAULT '2000' USER-COMMAND ss1.
  PARAMETERS p_qysh LIKE input-qysh.
  PARAMETERS p_fphm LIKE input-fphm MEMORY ID p_fphm MODIF ID m1.
  SELECT-OPTIONS s_fphm FOR input-fphm MODIF ID m2.
  PARAMETERS p_xfmc LIKE input-xfmc MEMORY ID p_xfmc MODIF ID m1.
  SELECT-OPTIONS s_xfmc FOR input-xfmc MODIF ID m2.
  PARAMETERS p_fpzl LIKE input-fpzl MEMORY ID p_fpzl MODIF ID m1.
  SELECT-OPTIONS s_fpzl FOR input-fpzl MODIF ID m2.
  PARAMETERS p_khmc LIKE input-khmc MEMORY ID p_khmc MODIF ID m1.
  SELECT-OPTIONS s_khmc FOR input-khmc MODIF ID m2.

  SELECT-OPTIONS s_kprq FOR input-kprqq NO-EXTENSION MEMORY ID s_kprq.
  SELECT-OPTIONS s_belnr FOR rbkp-belnr.
  SELECT-OPTIONS s_gjahr FOR rbkp-gjahr.
SELECTION-SCREEN END OF BLOCK b1.
PARAMETERS p_mir7 TYPE char1 NO-DISPLAY.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE btxt2.
  PARAMETERS:p1 RADIOBUTTON GROUP prd1 USER-COMMAND ss1 DEFAULT 'X',
             p2 RADIOBUTTON GROUP prd1,
             p3 RADIOBUTTON GROUP prd1.
  PARAMETERS:pre AS CHECKBOX DEFAULT ''.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE btxt3.
  SELECTION-SCREEN: COMMENT /1(79) txt001 MODIF ID txt.
  SELECTION-SCREEN: COMMENT /1(79) txt002 MODIF ID txt.
*  SELECTION-SCREEN: COMMENT /1(79) txt003 MODIF ID txt.
SELECTION-SCREEN END OF BLOCK b3.

*SELECTION-SCREEN FUNCTION KEY 1.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p1 = 'X' OR p3 = 'X'.
      CASE screen-group1.
        WHEN 'M1'.
          screen-active = 0.
        WHEN 'M2'.
          screen-active = 1.
      ENDCASE.
    ELSEIF p2 = 'X'.
      CASE screen-group1.
        WHEN 'M1'.
          screen-active = 1.
        WHEN 'M2'.
          screen-active = 0.
      ENDCASE.
    ENDIF.
    IF screen-name = 'P_QYSH'.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN. "PAI
  CASE sy-ucomm.
    WHEN 'ONLI'.
      PERFORM auth_check.
    WHEN 'FC01'.
      PERFORM set_scr_para_def USING '1000' 'S'.
    WHEN 'SS1'.
      PERFORM zf4_qysh.
  ENDCASE.

INITIALIZATION.
  btxt1 = '数据筛选'(t01).
  btxt2 = '功能选择'(t02).
  btxt3 = '注意'(t03).
  sscrfields-functxt_01  = '存为默认'.
  txt001 = '【会计凭证号码】对应的【公司代码】为空（非结算价采购订单发票核销）为MIR4凭证，'.
  txt002 = '不为空（结算价采购订单发票核销）为FB03凭证。'.
  IF sy-calld = abap_false AND sy-batch = abap_false.
    PERFORM set_scr_para_def USING '1000' 'R'.
  ENDIF.
  PERFORM zf4_qysh.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_bukrs.
  SELECT
    bukrs,
    butxt
    FROM t001
    WHERE spras = @sy-langu
    AND xtemplt = @space
    ORDER BY bukrs
    INTO TABLE @DATA(tbukrs)
    .
  PERFORM itabtolist(zpubform) TABLES tbukrs USING  'P_BUKRS'.

START-OF-SELECTION.
  PERFORM init.
  PERFORM savelog(zreplog) USING sy-repid '' IF FOUND.
  PERFORM getdata.
  PERFORM updatelog(zreplog) IF FOUND.

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
  DATA:it_enq TYPE TABLE OF seqg3 WITH HEADER LINE.
  CLEAR:input,output,rtype,rtmsg.
  CLEAR p_qysh.
  SELECT SINGLE
    remark
    FROM t001
    JOIN adrct ON t001~adrnr = adrct~addrnumber
    WHERE t001~bukrs = @p_bukrs
    INTO @p_qysh
  .
  IF p_qysh IS INITIAL.
    MESSAGE s000(oo) WITH p_bukrs '公司税号未维护'  DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF p2 = abap_true.
    DATA(zcl_mycst) = NEW zcl_mycst( ).
    IF NOT zcl_mycst IS BOUND.
      MESSAGE s000(oo) WITH '创建zcl_mycst对象出现了问题'  DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    input-kprqq = s_kprq-low.
    input-kprqz = s_kprq-high.
    input-qysh = p_qysh.
    input-fphm = p_fphm.
    input-xfmc = p_xfmc.
    input-fpzl = p_fpzl.
    input-khmc = p_khmc.
    DATA(msgmycst) = zcl_mycst->get_jxmx( EXPORTING jxmxin = input ).
    IF msgmycst(1) = 'E'.
      MESSAGE msgmycst TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDIF.
  IF s_belnr[] IS INITIAL AND s_gjahr[] IS INITIAL.
    SELECT
      zh~*
      FROM ztmycsthead AS zh
      WHERE zh~khsh = @p_qysh
      AND zh~fphm IN @s_fphm
      AND zh~xfmc IN @s_xfmc
      AND zh~fpzl IN @s_fpzl
      AND zh~fpzl IN @s_fpzl
      AND zh~kprq IN @rkprq
      INTO TABLE @DATA(t_mycstall)
      .
  ELSE.
    SELECT
      zh~*
      FROM ztmycsthead AS zh
      WHERE zh~khsh = @p_qysh
      AND zh~fphm IN @s_fphm
      AND zh~xfmc IN @s_xfmc
      AND zh~fpzl IN @s_fpzl
      AND zh~fpzl IN @s_fpzl
      AND zh~kprq IN @rkprq
      AND zh~bukrs NE @space
      AND EXISTS ( SELECT * FROM ztmycstfp AS zfp WHERE zfp~fpzl = zh~fpzl AND zfp~fphm = zh~fphm
                                                    AND zfp~belnr IN @s_belnr AND zfp~gjahr IN @s_gjahr )
      INTO TABLE @t_mycstall
      .
    SELECT
      zh~*
      FROM ztmycsthead AS zh
      WHERE zh~khsh = @p_qysh
      AND zh~fphm IN @s_fphm
      AND zh~xfmc IN @s_xfmc
      AND zh~fpzl IN @s_fpzl
      AND zh~fpzl IN @s_fpzl
      AND zh~kprq IN @rkprq
      AND zh~bukrs EQ @space
      AND EXISTS ( SELECT * FROM ztmycstfp AS zfp WHERE zfp~fpzl = zh~fpzl AND zfp~fphm = zh~fphm
                                                    AND zfp~belnr IN @s_belnr AND zfp~gjahr IN @s_gjahr )
      APPENDING TABLE @t_mycstall
    .
  ENDIF.
  IF t_mycstall IS INITIAL.
    MESSAGE s000(oo) WITH 'No Data' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  SELECT
    zh~fpzl,
    zh~fphm,
    zh~bukrs,
    r~budat,
    r~belnr,
    r~gjahr,
    r~stblg,
    r~rbstat
    FROM ztmycstfp AS zh
    JOIN rbkp AS r ON zh~belnr = r~belnr AND zh~gjahr = r~gjahr
    FOR ALL ENTRIES IN @t_mycstall
    WHERE zh~bukrs EQ @space
    AND zh~fpzl = @t_mycstall-fpzl
    AND zh~fphm = @t_mycstall-fphm
    INTO TABLE @DATA(tfp)
    .
  SELECT
    zh~fpzl,
    zh~fphm,
    zh~bukrs,
    b~budat,
    b~belnr,
    b~gjahr,
    b~stblg
    FROM ztmycstfp AS zh
    JOIN bkpf AS b ON zh~belnr = b~belnr AND zh~gjahr = b~gjahr AND zh~bukrs = b~bukrs
    FOR ALL ENTRIES IN @t_mycstall
    WHERE zh~bukrs NE @space
    AND zh~fpzl = @t_mycstall-fpzl
    AND zh~fphm = @t_mycstall-fphm
    APPENDING TABLE @tfp
    .
  LOOP AT tfp ASSIGNING FIELD-SYMBOL(<tfp>).
    IF <tfp>-bukrs IS INITIAL.
      IF NOT ( <tfp>-stblg IS INITIAL AND <tfp>-rbstat NE '2' ).
        CLEAR <tfp>-belnr.
      ENDIF.
    ELSE.
      IF NOT <tfp>-stblg IS INITIAL.
        CLEAR <tfp>-belnr.
      ENDIF.
    ENDIF.
  ENDLOOP.
  DELETE tfp WHERE belnr IS INITIAL.
  SORT tfp BY fpzl fphm.
  IF pre = 'X'.
    SELECT
      fp~belnr AS belnr_fp,
      fp~gjahr AS gjahr_fp,
      bkpf~bukrs,
      bkpf~belnr,
      bkpf~gjahr,
      bkpf~budat,
      fp~fpzl,
      fp~fphm
      FROM @tfp AS fp
      JOIN rbkp ON fp~belnr = rbkp~belnr AND fp~gjahr = rbkp~gjahr
      JOIN bkpf ON substring( bkpf~awkey,1,10 ) = rbkp~belnr AND substring( bkpf~awkey,11,4 ) = rbkp~gjahr
      WHERE bkpf~blart = 'RE'
      AND bkpf~awtyp = 'RMRP'
      AND bkpf~stblg = @space
      AND fp~bukrs = @space
      AND rbkp~stblg = @space
*      AND rbkp~rbstat = '5'
      ORDER BY rbkp~belnr,rbkp~gjahr
      INTO TABLE @DATA(tbelnr)
      .
    LOOP AT tfp ASSIGNING FIELD-SYMBOL(<tfpc>) WHERE bukrs = ''.
      READ TABLE tbelnr INTO DATA(wbelnr) WITH KEY belnr_fp = <tfpc>-belnr gjahr_fp = <tfpc>-gjahr BINARY SEARCH.
      IF sy-subrc EQ 0.
        <tfpc>-belnr = wbelnr-belnr.
        <tfpc>-gjahr = wbelnr-gjahr.
        <tfpc>-bukrs = wbelnr-bukrs.
        <tfpc>-budat = wbelnr-budat.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CLEAR t_mycst.
  LOOP AT t_mycstall ASSIGNING FIELD-SYMBOL(<tall>).
    APPEND INITIAL LINE TO t_mycst ASSIGNING FIELD-SYMBOL(<t>).
    MOVE-CORRESPONDING <tall> TO <t>-zh.
    READ TABLE tfp INTO DATA(wfp) WITH KEY fpzl = <t>-zh-fpzl fphm = <t>-zh-fphm BINARY SEARCH.
    IF sy-subrc EQ 0.
      DATA(tabix)  = sy-tabix.
      LOOP AT tfp INTO wfp FROM tabix.
        IF NOT ( wfp-fpzl = <t>-zh-fpzl AND wfp-fphm = <t>-zh-fphm ).
          EXIT.
        ENDIF.
        APPEND INITIAL LINE TO <t>-fps ASSIGNING FIELD-SYMBOL(<fps>).
        <fps>-fpzl = <t>-zh-fpzl.
        <fps>-fphm = <t>-zh-fphm.
        <fps>-belnr = wfp-belnr.
        <t>-zh-budat = wfp-budat.
        IF pre = 'X'.
          <t>-zh-bukrs = wfp-bukrs.
        ENDIF.
        <fps>-bukrs = <t>-zh-bukrs.
        <fps>-bukrs = wfp-bukrs.
        <fps>-gjahr = wfp-gjahr.
        <fps>-budat = wfp-budat.
        IF s_belnr[] IS NOT INITIAL.
          IF NOT ( <fps>-belnr IN s_belnr[] ).
            <fps>-nofi = 'X'.
          ENDIF.
        ENDIF.
        IF s_gjahr[] IS NOT INITIAL.
          IF NOT ( <fps>-gjahr IN s_gjahr[] ).
            <fps>-nofi = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF <t>-fps IS NOT INITIAL AND NOT line_exists( <t>-fps[ nofi = '' ] ).
        <t>-nofi = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF p_mir7 = 'X'.
    DELETE t_mycst WHERE fps IS NOT INITIAL.
    CLEAR it_enq.
    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
*       GCLIENT               = SY-MANDT
        gname                 = 'ZTMYCSTHEAD'
*       GARG                  = ' '
        guname                = ' '
*       LOCAL                 = ' '
*       FAST                  = ' '
*       GARGNOWC              = ' '
*     IMPORTING
*       NUMBER                =
*       SUBRC                 =
      TABLES
        enq                   = it_enq
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.
    LOOP AT it_enq.
      DELETE t_mycst WHERE zh-fpzl = it_enq-garg+3(3) AND zh-fphm = it_enq-garg+6(30).
    ENDLOOP.
  ENDIF.
  IF s_belnr[] IS NOT INITIAL OR s_gjahr[] IS NOT INITIAL.
    DELETE t_mycst WHERE nofi = 'X' OR fps IS INITIAL.
  ENDIF.

  IF t_mycst IS INITIAL.
    MESSAGE s000(oo) WITH 'No Data' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  SELECT
    *
    FROM ztmycstmxlist AS i
    FOR ALL ENTRIES IN @t_mycst
    WHERE i~fpzl = @t_mycst-zh-fpzl
    AND i~fphm = @t_mycst-zh-fphm
    INTO TABLE @DATA(tmxlist)
    .
  SORT tmxlist BY fpzl fphm.

  CLEAR:w_dataList-details,output.
  LOOP AT t_mycst ASSIGNING <t>.
    APPEND INITIAL LINE TO output-body-data_list ASSIGNING FIELD-SYMBOL(<o>).
    MOVE-CORRESPONDING <t>-zh TO <o>.
    <o>-fps = <t>-fps.
    <o>-fpsicon = |{ icon_list }[ { lines( <o>-fps ) } ]|.
    CLEAR:<o>-belnr,<o>-gjahr,<o>-bukrs.
    IF lines( <o>-fps ) EQ 1.
      READ TABLE <o>-fps INTO DATA(wwfps) INDEX 1.
      <o>-belnr = wwfps-belnr.
      <o>-gjahr = wwfps-gjahr.
      <o>-bukrs = wwfps-bukrs.
      CLEAR wwfps.
    ENDIF.
    READ TABLE tmxlist INTO DATA(wmxlist) WITH KEY fpzl = <t>-zh-fpzl fphm = <t>-zh-fphm BINARY SEARCH.
    IF sy-subrc EQ 0.
      DATA(tabix2) = sy-tabix.
      LOOP AT tmxlist INTO wmxlist FROM tabix2.
        IF NOT ( wmxlist-fpzl = <t>-zh-fpzl AND wmxlist-fphm = <t>-zh-fphm ).
          EXIT.
        ENDIF.
        <o>-irows += 1.
        APPEND INITIAL LINE TO <o>-details ASSIGNING FIELD-SYMBOL(<d>).
        MOVE-CORRESPONDING wmxlist TO <d>.
        <d>-kprq = <t>-zh-kprq.
        IF p3 = 'X'.
          APPEND <d> TO w_dataList-details.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
  SORT output-body-data_list BY fpzl fphm.
  FREE:t_mycst,t_mycstall.
  CALL SCREEN 900.
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
*  ls_fldcat-selddictxt =  pv_text.    "布局
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

INCLUDE zmycst_class.

INCLUDE zmycst_stato01.

INCLUDE zmycst_useri01.
*&---------------------------------------------------------------------*
*& Form init
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM init .
  READ TABLE s_kprq INDEX 1.
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING s_kprq TO rkprq.
    PERFORM format_date IN PROGRAM zfi025_1 IF FOUND CHANGING rkprq-low.
    PERFORM format_date IN PROGRAM zfi025_1 IF FOUND CHANGING rkprq-high.
    IF rkprq-high IS INITIAL.
      rkprq-high = rkprq-low.
    ENDIF.
    APPEND rkprq.
  ENDIF.
  IF p2 = 'X'.
    CLEAR:s_fphm,s_xfmc,s_fpzl,s_khmc.
    CLEAR:s_fphm[],s_xfmc[],s_fpzl[],s_khmc[].
    IF s_kprq-high IS INITIAL.
      s_kprq-high = s_kprq-low.
    ENDIF.
    IF p_fphm IS NOT INITIAL.
      s_fphm(3) = |IEQ|.
      s_fphm-low = p_fphm.
      APPEND s_fphm.
    ENDIF.
    IF p_xfmc IS NOT INITIAL.
      s_xfmc(3) = |IEQ|.
      s_xfmc-low = p_xfmc.
      APPEND s_xfmc.
    ENDIF.
  ENDIF.
  IF p_fpzl IS NOT INITIAL.
    s_fpzl(3) = |IEQ|.
    s_fpzl-low = p_fpzl.
    APPEND s_fpzl.
  ENDIF.
  IF p_khmc IS NOT INITIAL.
    s_khmc(3) = |IEQ|.
    s_khmc-low = p_khmc.
    APPEND s_khmc.
  ENDIF.
  CLEAR:gt_fldct_head,gt_fldct_item.
  DATA(ddic_header) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( p_name = 'ZTMYCSTHEAD' ) )->get_ddic_field_list( ).
  DATA(ddic_item) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( p_name = 'ZTMYCSTMXLIST' ) )->get_ddic_field_list( ).
  LOOP AT ddic_header ASSIGNING FIELD-SYMBOL(<h>) WHERE inttype = 'C' OR inttype = 'I' OR inttype = 'N' OR inttype = 'D'.
    CASE <h>-fieldname.
      WHEN 'MANDT'.
        CONTINUE.
      WHEN 'KPJE' OR 'KPSE' OR 'JSHJ'.
        PERFORM catset TABLES gt_fldct_head
                       USING: <h>-fieldname 'RBKP' 'RMWWR' <h>-fieldtext.
      WHEN 'IROWS'.
        PERFORM catset TABLES gt_fldct_head
                       USING: 'FPSICON' '' '' '凭证条目数'.
        PERFORM catset TABLES gt_fldct_head
                       USING: <h>-fieldname <h>-tabname <h>-fieldname <h>-fieldtext.
      WHEN OTHERS.
        PERFORM catset TABLES gt_fldct_head
                       USING: <h>-fieldname <h>-tabname <h>-fieldname <h>-fieldtext.
    ENDCASE.
  ENDLOOP.
  PERFORM catset TABLES gt_fldct_head
                 USING: 'SEL' '' '' ''.
  LOOP AT ddic_item ASSIGNING FIELD-SYMBOL(<i>) WHERE inttype = 'C' OR inttype = 'I' OR inttype = 'N'.
    CASE <i>-fieldname.
      WHEN 'MANDT' OR 'BELNR' OR 'GJAHR' OR 'BUKRS'.
        CONTINUE.
      WHEN 'CPDJ' OR 'BHSJE' OR 'SE' OR 'HSJE'.
        PERFORM catset TABLES gt_fldct_item
                       USING: <i>-fieldname 'RBKP' 'RMWWR' <i>-fieldtext.
*      WHEN 'CPSL'.
*        PERFORM catset TABLES gt_fldct_item
*                       USING: <i>-fieldname 'MCHB' 'MENGE' <i>-fieldtext.
      WHEN OTHERS.
        PERFORM catset TABLES gt_fldct_item
                       USING: <i>-fieldname <i>-tabname <i>-fieldname <i>-fieldtext.
    ENDCASE.
  ENDLOOP.
  PERFORM catset TABLES gt_fldct_item
                 USING: 'KPRQ' 'ZTMYCSTHEAD' 'KPRQ' '开票日期'   .
*  gs_slayt-zebra  = 'X'.
*  gs_slayt-box_fname  = 'SEL'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_scr_para_def
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM set_scr_para_def  USING  pv_dynnr pv_method.
  DATA lt_rsscr TYPE TABLE OF rsscr WITH HEADER LINE.
  DATA lt_paras TYPE TABLE OF char10 WITH HEADER LINE.
  DATA lt_list  TYPE TABLE OF spopli WITH HEADER LINE.
  DATA lv_strfd TYPE bf4indx-strfd.
  DATA lv_subrc TYPE sy-subrc.
  DATA lv_fsstr TYPE string.
  DATA ls_scr   TYPE rsscr.
  DATA lv_fnumb TYPE i.
  DATA lv_tnumb TYPE i.
  DATA lv_rcode TYPE c.
  FIELD-SYMBOLS <ptxt> TYPE any.

  lv_fnumb = pv_dynnr * 1000 + 1.
  lv_tnumb = pv_dynnr * 1000 + 999.
  CONCATENATE sy-repid sy-uname sy-mandt 'SCRD' pv_dynnr INTO lv_strfd.
  PERFORM load_sscr(rsdbrunt) TABLES   lt_rsscr
                              USING    sy-repid
                              CHANGING lv_subrc.
  LOOP AT lt_rsscr WHERE numb BETWEEN lv_fnumb AND lv_tnumb AND
                         kind CA 'PS' AND
                         flag1 NE '20'. "NO-DISPLAY
    READ TABLE lt_rsscr INTO ls_scr WITH KEY miscell = lt_rsscr-name.
    IF sy-subrc = 0.
      lv_fsstr = ls_scr-name.
    ELSE.
      CONCATENATE '%_' lt_rsscr-name '_%_app_%-text' INTO lv_fsstr.
    ENDIF.
    ASSIGN (lv_fsstr) TO <ptxt>.

    CONCATENATE  lt_rsscr-name <ptxt> INTO lt_list-varoption SEPARATED BY ''.
    lt_list-selflag = 'X'.
    APPEND lt_list.

    IF lt_rsscr-kind = 'P'.
      lt_paras  = lt_rsscr-name.
    ELSE.
      CONCATENATE lt_rsscr-name '[]' INTO lt_paras.
    ENDIF.
    COLLECT lt_paras.
  ENDLOOP.
  CHECK lt_paras[] IS NOT INITIAL.

  CASE pv_method.
    WHEN 'S'.
      APPEND ' 删除默认设置' TO lt_list.
      CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
        EXPORTING
          start_col = 30
          start_row = 1
          mark_flag = 'X'
          mark_max  = 99
          textline1 = '请勾选需要将当前值保存为默认值的项'
          textline2 = '如果勾选最后的删除配置则删除默认配置'
          titel     = '选择'
        IMPORTING
          answer    = lv_rcode
        TABLES
          t_spopli  = lt_list
        EXCEPTIONS
          OTHERS    = 3.
      CHECK sy-subrc = 0 AND lv_rcode <> 'A'.

      READ TABLE lt_list WITH KEY selflag = 'X'
                                  varoption = '删除默认设置'.
      IF sy-subrc = 0.
        DELETE FROM DATABASE bf4indx(zs) ID lv_strfd.
        MESSAGE s000(oo) WITH 'Deleted'.
      ELSE.
        LOOP AT lt_paras.
          READ TABLE lt_list INDEX sy-tabix.
          IF lt_list-selflag = ''.
            DELETE lt_list INDEX sy-tabix.
            DELETE lt_paras.
          ENDIF.
        ENDLOOP.
        CHECK lt_paras[] IS NOT INITIAL.
        EXPORT (lt_paras) TO DATABASE bf4indx(zs) ID lv_strfd.
        MESSAGE s000(oo) WITH 'Saved'.
      ENDIF.
    WHEN 'R'.
      TRY .
          IMPORT (lt_paras) FROM DATABASE bf4indx(zs) ID lv_strfd.
          CHECK sy-subrc = 0.
*          MESSAGE s000(oo) WITH '已填充默认值'.
        CATCH cx_root.
      ENDTRY.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf4_qysh
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zf4_qysh .
*  DATA: lt_dynpread TYPE STANDARD TABLE OF dynpread WITH HEADER LINE.
*  DATA: lw_dynpread TYPE dynpread .
*  lw_dynpread-fieldname = 'P_BUKRS'.
*  APPEND lw_dynpread TO lt_dynpread .
*  CALL FUNCTION 'DYNP_VALUES_READ'
*    EXPORTING
*      dyname               = sy-repid
*      dynumb               = sy-dynnr
*    TABLES
*      dynpfields           = lt_dynpread
*    EXCEPTIONS
*      invalid_abapworkarea = 1
*      invalid_dynprofield  = 2
*      invalid_dynproname   = 3
*      invalid_dynpronummer = 4
*      invalid_request      = 5
*      no_fielddescription  = 6
*      invalid_parameter    = 7
*      undefind_error       = 8
*      double_conversion    = 9
*      stepl_not_found      = 10
*      OTHERS               = 11.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*  READ TABLE lt_dynpread INTO lw_dynpread INDEX 1.
*  DATA(bukrs) = lw_dynpread-fieldvalue.
  CLEAR p_qysh.
  SELECT SINGLE
    remark
    FROM t001
    JOIN adrct ON t001~adrnr = adrct~addrnumber
    WHERE t001~bukrs = @p_bukrs
    INTO @p_qysh
  .
ENDFORM.
