*&---------------------------------------------------------------------*
*& 包含               ZMYCST_INVOICE_YC_CLASS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& lcl_event_receiver
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& lcl_event_receiver
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS handle_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column es_row_no.
    METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
    METHODS handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object e_interactive.
    METHODS handle_menu_button
      FOR EVENT menu_button OF cl_gui_alv_grid
      IMPORTING e_object e_ucomm.
ENDCLASS.                    "lcl_event_receiver DEFINITION

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_double_click.
*    PERFORM grid_double_clik USING e_row e_column es_row_no.
  ENDMETHOD.                    "handle_double_click
  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN 'FC_DELE'.
        PERFORM delpo.
      WHEN 'PO' OR 'BP'.
        PERFORM addpo USING e_ucomm.
    ENDCASE.
    PERFORM:frm_refresh_alv_po.
  ENDMETHOD.                    "handle_user_command

  METHOD handle_toolbar.
    PERFORM add_button USING e_object: '3' ' ' '       ' '' '   '.
    IF p1 = 'X'.
      PERFORM add_button USING e_object: '2' 'X' 'FC_PO' '@17@' '添加采购订单'.
    ELSE.
      PERFORM add_button USING e_object: '2' '' 'FC_PO' '@17@' '添加采购订单'.
    ENDIF.
    PERFORM add_button USING e_object: '0' '' 'FC_DELE' '@18@' ''.
  ENDMETHOD.                    "handle_toolbar

  METHOD handle_menu_button .
    DATA lv_numc TYPE numc2.
    DATA lv_code TYPE char10.
    DATA lv_text TYPE string.

    CASE e_ucomm.
      WHEN 'FC_PO'.
        PERFORM button_add_func USING e_object 'PO' 'PO 基于采购订单'.
        PERFORM button_add_func USING e_object 'BP' 'BP 基于供应商'.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.                    "handle_menu_button
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
*&---------------------------------------------------------------------*
*& ALV自带工具栏添加按钮
*&---------------------------------------------------------------------*
FORM add_button USING pr_object TYPE REF TO cl_alv_event_toolbar_set
                      pv_type pv_disa pv_func pv_icon pv_text .
  DATA: ls_toolbar  TYPE stb_button.

  CLEAR ls_toolbar.
  ls_toolbar-butn_type = pv_type.
  ls_toolbar-function  = pv_func.
  ls_toolbar-icon      = pv_icon.
  ls_toolbar-text      = pv_text.
  ls_toolbar-disabled  = pv_disa.

  APPEND ls_toolbar TO pr_object->mt_toolbar.
ENDFORM.                    "add_button

*&---------------------------------------------------------------------*
*&      Form  button_add_func
*&---------------------------------------------------------------------*
FORM button_add_func USING pr_ctmenu TYPE REF TO cl_ctmenu
                           pv_code pv_text.
  DATA: lv_fcode TYPE ui_func,
        lv_text	 TYPE gui_text.

  lv_fcode = pv_code.
  lv_text  = pv_text.
  pr_ctmenu->add_function( EXPORTING icon  = icon_okay
                                     fcode = lv_fcode
                                     text  = lv_text ).
ENDFORM.                    "button_add_func

FORM delpo.
  CALL METHOD alv_grid_po->get_selected_rows
    IMPORTING
      et_row_no = DATA(lt_row).
  LOOP AT lt_row ASSIGNING FIELD-SYMBOL(<lt_row>).
    READ TABLE gt_out ASSIGNING FIELD-SYMBOL(<gs_out>) INDEX <lt_row>-row_id.
    <gs_out>-del = 'X'.
  ENDLOOP.
  DELETE gt_out WHERE del = 'X'.
  cl_gui_cfw=>set_new_ok_code( 'CALC' ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form addpo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_COMM
*&---------------------------------------------------------------------*
FORM addpo  USING    p_e_ucomm.
  DATA rbudat TYPE TABLE OF rsdsselopt.
  DATA: t_selbestko TYPE mrm_tab_selbest,
        t_selbestpo TYPE mrm_tab_selbest.
  CLEAR:t_po,t_po_sum,t_po_ekbz.
  CASE p_e_ucomm.
    WHEN 'PO'.
      CLEAR:seltab.
      seltab = VALUE #(
      ( selname = 'SO_LIFNR' kind = 'S' sign = 'I' option = 'EQ' low = p_lifnr )
      ( selname = 'PA_BUKRS' kind = 'P' sign = 'I' option = 'EQ' low = p_bukrs )
       ).
*      SUBMIT rm08rl82 VIA SELECTION-SCREEN AND RETURN
*                      WITH so_lifnr IN rg_lifnr       "Selektionslieferant
*                      WITH pa_bukrs EQ rbkpv-bukrs
*                      WITH so_werks IN rg_werks
*                      WITH pa_xdcfl = lv_xdcfl
*                      WITH pa_rbsta = rbkpv-rbstat.    " Hardening 1811
      SUBMIT rm08rl82 VIA SELECTION-SCREEN AND RETURN
      WITH SELECTION-TABLE seltab
      .
      CLEAR: t_selbestko,t_selbestpo.
      IMPORT t_selbest = t_selbestko FROM MEMORY ID 'RM08RL82_SEL'.
      IF ( sy-subrc <> 0 ).
        EXIT.
      ENDIF.
      FREE MEMORY ID 'RM08RL82_SEL'.
      t_selbestpo = t_selbestko.
      DELETE t_selbestko WHERE ebelp NE '00000'.
      DELETE t_selbestpo WHERE ebelp EQ '00000'.
      SELECT
        ekpo~ebeln,
        ekpo~ebelp,
        ekko~lifnr
        FROM ekpo
        JOIN ekko ON ekpo~ebeln = ekko~ebeln
        JOIN @t_selbestko AS ko ON ekpo~ebeln = ko~ebeln
        WHERE ekko~bukrs = @p_bukrs
        AND ekpo~webre = @abap_true
*    AND ekko~ekorg = @p_ekorg
*    AND ekko~ekgrp = @p_ekgrp
*    AND ekko~zhtlx NE '01'
        INTO CORRESPONDING FIELDS OF TABLE @t_po
        .
      SELECT
        ekpo~ebeln,
        ekpo~ebelp,
        ekko~lifnr
        FROM ekpo
        JOIN ekko ON ekpo~ebeln = ekko~ebeln
        JOIN @t_selbestko AS po ON ekpo~ebeln = po~ebeln AND ekpo~ebelp = po~ebelp
        AND ekpo~webre = @abap_true
*    AND ekko~ekorg = @p_ekorg
*    AND ekko~ekgrp = @p_ekgrp
*    AND ekko~zhtlx NE '01'
        APPENDING CORRESPONDING FIELDS OF TABLE @t_po
        .
      SORT t_po BY ebeln ebelp lifnr.
      DELETE ADJACENT DUPLICATES FROM t_po COMPARING ebeln ebelp lifnr.
    WHEN 'BP'.
      CALL SELECTION-SCREEN 1001 STARTING AT 10 5.
      CHECK sy-subrc EQ 0.
      ls_headerdata-lifnr_new = p_lifnrp.
      CLEAR:t_po,t_po_sum,t_po_ekbz.
      SELECT
        ekpo~ebeln,
        ekpo~ebelp,
        ekko~lifnr
        FROM ekko
        JOIN ekpo ON ekko~ebeln = ekpo~ebeln
        WHERE ekko~lifnr = @p_lifnrp
        AND ekko~bukrs = @p_bukrs
*    AND ekko~ekorg = @p_ekorg
*    AND ekko~ekgrp = @p_ekgrp
        AND ekpo~werks = @p_werks
*    AND ekko~zhtlx NE '01'
        AND ekpo~webre = @abap_true
        AND ekko~aedat IN @s_budat
        AND NOT EXISTS ( SELECT * FROM ztmycst_miro WHERE ebeln = ekpo~ebeln AND ebelp = ekpo~ebelp )
        INTO CORRESPONDING FIELDS OF TABLE @t_po
      .
      MOVE-CORRESPONDING s_budat[] TO rbudat.
  ENDCASE.
  CALL FUNCTION 'ZMM_FM_PO_INVENCE_LIST'
    TABLES
      rbudat  = rbudat
    CHANGING
      po      = t_po
      po_sum  = t_po_sum
      po_ekbz = t_po_ekbz.
  LOOP AT t_po_ekbz ASSIGNING FIELD-SYMBOL(<ekbz>).
    READ TABLE gt_out TRANSPORTING NO FIELDS WITH KEY ebeln = <ekbz>-ebeln ebelp = <ekbz>-ebelp.
    IF sy-subrc EQ 0.
      <ekbz>-sign_noo = abap_true.
    ENDIF.
  ENDLOOP.
  DELETE t_po_ekbz WHERE sign_noo = abap_true.
  IF t_po_ekbz IS NOT INITIAL.
    PERFORM prepare_gtout TABLES t_po_ekbz.
  ENDIF.

  cl_gui_cfw=>set_new_ok_code( 'CALC' ).
ENDFORM.

FORM lock.

ENDFORM.
