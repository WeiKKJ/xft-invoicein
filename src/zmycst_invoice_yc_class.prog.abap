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
    METHODS handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.
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
    CHECK es_row_no-row_id GT 0.
    READ TABLE gt_out ASSIGNING FIELD-SYMBOL(<out>) INDEX es_row_no-row_id.
    CHECK <out> IS ASSIGNED.
    CASE e_column-fieldname.
      WHEN 'MATNR' OR 'TXZ01'.
        SET PARAMETER ID 'MXX' FIELD 'E'.
        SET PARAMETER ID 'MAT' FIELD <out>-matnr.
        SET PARAMETER ID 'WRK' FIELD p_werks.
        CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
      WHEN 'EBELN' OR 'EBELP'.
        SET PARAMETER ID 'BES' FIELD <out>-ebeln.
        SET PARAMETER ID 'BSP' FIELD <out>-ebelp.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      WHEN 'MBLNR' OR 'MJAHR' OR 'ZEILE'.
        CALL FUNCTION 'MIGO_DIALOG'
          EXPORTING
*           I_ACTION            = 'A04'
*           I_REFDOC            = 'R02'
*           I_NOTREE            = 'X'
*           I_NO_AUTH_CHECK     =
*           I_SKIP_FIRST_SCREEN = 'X'
*           I_DEADEND           = 'X'
*           I_OKCODE            = 'OK_GO'
*           I_LEAVE_AFTER_POST  =
*           I_NEW_ROLLAREA      = 'X'
*           I_SYTCODE           =
*           I_EBELN             =
*           I_EBELP             =
            i_mblnr             = <out>-mblnr
            i_mjahr             = <out>-mjahr
            i_zeile             = <out>-zeile
*           I_TRANSPORT         =
*           I_ORDER_NUMBER      =
*           I_ORDER_ITEM        =
*           I_TRANSPORT_MEANS   =
*           I_TRANSPORTIDENT    =
*           I_INBOUND_DELIV     =
*           I_OUTBOUND_DELIV    =
*           I_RESERVATION_NUMB  =
*           I_RESERVATION_ITEM  =
*           EXT                 =
*           I_MOVE_TYPE         =
*           I_SPEC_STOCK        =
*           I_PSTNG_DATE        =
*           I_DOC_DATE          =
*           I_REF_DOC_NO        =
*           I_HEADER_TXT        =
          EXCEPTIONS
            illegal_combination = 1
            OTHERS              = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      WHEN 'BELNR'.
        PERFORM mir4 IN PROGRAM zpubform IF FOUND USING <out>-belnr(10) <out>-belnr+10(4).
    ENDCASE.
  ENDMETHOD.                    "handle_double_click
  METHOD handle_data_changed.        "数据改动事件
    PERFORM frm_handle_data_changed USING er_data_changed.
  ENDMETHOD.
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
*    IF p1 = 'X'.
*      PERFORM add_button USING e_object: '2' 'X' 'FC_PO' '@17@' '添加采购订单'.
*    ELSE.
    PERFORM add_button USING e_object: '2' '' 'FC_PO' '@17@' '添加采购订单'.
*    ENDIF.
    PERFORM add_button USING e_object: '0' '' 'FC_DELE' '@18@' ''.
  ENDMETHOD.                    "handle_toolbar

  METHOD handle_menu_button .
    DATA lv_numc TYPE numc2.
    DATA lv_code TYPE char10.
    DATA lv_text TYPE string.

    CASE e_ucomm.
      WHEN 'FC_PO'.
        PERFORM button_add_func USING e_object 'PO' 'PO 基于采购订单'.
        IF p2 = 'X'.
          PERFORM button_add_func USING e_object 'BP' 'BP 基于供应商'.
        ENDIF.
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
  DATA msg TYPE bapi_msg.
  CALL METHOD alv_grid_po->get_selected_rows
    IMPORTING
      et_row_no = DATA(lt_row).
  LOOP AT lt_row ASSIGNING FIELD-SYMBOL(<lt_row>).
    READ TABLE gt_out ASSIGNING FIELD-SYMBOL(<gs_out>) INDEX <lt_row>-row_id.
    <gs_out>-del = 'X'.
    PERFORM ezpo USING 'X' <gs_out>-ebeln <gs_out>-ebelp CHANGING msg.
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
  DATA:rbudat TYPE TABLE OF rsdsselopt,
       rlifnr TYPE RANGE OF lifnr.
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
      IF p1 = 'X'.
        rlifnr = VALUE #( sign = 'I' option = 'EQ'
        ( low = p_lifnr )
         )
         .
      ENDIF.
      SELECT
        ekpo~ebeln,
        ekpo~ebelp,
        ekko~lifnr
        FROM ekpo
        JOIN ekko ON ekpo~ebeln = ekko~ebeln
        JOIN @t_selbestko AS ko ON ekpo~ebeln = ko~ebeln
        WHERE ekko~lifnr IN @rlifnr
        AND ekko~bukrs = @p_bukrs
        AND ekpo~webre = @abap_true
        AND ekko~ekorg = @p_ekorg
        AND ekko~ekgrp IN @s_ekgrp
        AND ekko~zhtlx NE '01'
        INTO CORRESPONDING FIELDS OF TABLE @t_po
        .
      SELECT
        ekpo~ebeln,
        ekpo~ebelp,
        ekko~lifnr
        FROM ekpo
        JOIN ekko ON ekpo~ebeln = ekko~ebeln
        JOIN @t_selbestko AS po ON ekpo~ebeln = po~ebeln AND ekpo~ebelp = po~ebelp
        WHERE ekko~lifnr IN @rlifnr
        AND ekko~bukrs = @p_bukrs
        AND ekpo~webre = @abap_true
        AND ekko~ekorg = @p_ekorg
        AND ekko~ekgrp IN @s_ekgrp
        AND ekko~zhtlx NE '01'
        APPENDING CORRESPONDING FIELDS OF TABLE @t_po
        .
      SORT t_po BY ebeln ebelp lifnr.
      DELETE ADJACENT DUPLICATES FROM t_po COMPARING ebeln ebelp lifnr.
    WHEN 'BP'.
      CALL SELECTION-SCREEN 1001 STARTING AT 10 5.
      CHECK sy-subrc EQ 0.
      ls_headerdata-lifnr_new = p_lifnrp.
      CLEAR ls_headerdata-name1_new.
      SELECT SINGLE name1 FROM lfa1 WHERE lifnr = @ls_headerdata-lifnr_new INTO @ls_headerdata-name1_new.
      CLEAR:t_po,t_po_sum,t_po_ekbz.
      SELECT
        ekpo~ebeln,
        ekpo~ebelp,
        ekko~lifnr
        FROM ekko
        JOIN ekpo ON ekko~ebeln = ekpo~ebeln
        WHERE ekko~lifnr = @p_lifnrp
        AND ekko~bukrs = @p_bukrs
        AND ekko~ekorg = @p_ekorg
        AND ekko~ekgrp IN @s_ekgrp
        AND ekpo~werks = @p_werks
        AND ekko~zhtlx NE '01'
        AND ekpo~webre = @abap_true
        AND ekko~aedat IN @s_budat
        INTO CORRESPONDING FIELDS OF TABLE @t_po
      .
      MOVE-CORRESPONDING s_budat[] TO rbudat.
  ENDCASE.
  CALL FUNCTION 'ZMM_FM_PO_INVENCE_LIST'
    EXPORTING
      ekbz    = p2
    TABLES
      rbudat  = rbudat
    CHANGING
      po      = t_po
      po_sum  = t_po_sum
      po_ekbz = t_po_ekbz.

  IF p1 = 'X'.
    PERFORM prepare_gtout TABLES t_po.
  ELSE.
    PERFORM prepare_gtout TABLES t_po_ekbz.
  ENDIF.

  cl_gui_cfw=>set_new_ok_code( 'CALC' ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_handle_data_changed
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ER_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM frm_handle_data_changed  USING    p_er_data_changed TYPE REF TO cl_alv_changed_data_protocol.
  LOOP AT p_er_data_changed->mt_mod_cells INTO DATA(mod).
    READ TABLE gt_out ASSIGNING FIELD-SYMBOL(<item>) INDEX mod-row_id.
    CHECK sy-subrc EQ 0.
    CASE mod-fieldname.
*      WHEN 'MENGE_MIRO'.
*        <item>-miro_sum_hs = mod-value * <item>-netpr_hs.
*        <item>-miro_sum    = <item>-miro_sum_hs / ( 1 + <item>-zsl ).
*        <item>-miro_sum_se = <item>-miro_sum_hs - <item>-miro_sum.
*        cl_gui_cfw=>set_new_ok_code( 'CALC' ).
      WHEN 'MIRO_SUM'.
        PERFORM delqfw IN PROGRAM zpubform IF FOUND CHANGING mod-value.
        <item>-miro_sum_se = mod-value * <item>-zsl .
        <item>-miro_sum_hs = mod-value + <item>-miro_sum_se.
        cl_gui_cfw=>set_new_ok_code( 'CABD' ).
    ENDCASE.
  ENDLOOP.
ENDFORM.
