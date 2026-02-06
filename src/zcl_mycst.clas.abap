class ZCL_MYCST definition
  public
  final
  create public .

public section.

  methods GET_JXMX
    importing
      value(JXMXIN) type ZSMYCST_JXMXIN
    returning
      value(RTMSG) type BAPI_MSG .
  methods LOAD_FILE
    importing
      value(LV_DIR) type ZCL_WD_FTP=>MTY_DIRECTORY
      value(IMPATH) type STRING
    returning
      value(FILEX) type XSTRING .
  methods CONSTRUCTOR .
protected section.
private section.

  data MO_FTP type ref to ZCL_WD_FTP .

  methods GET_TOKEN
    returning
      value(TOKEN) type STRING .
  methods STOR_FILE
    importing
      value(LV_DIR) type ZCL_WD_FTP=>MTY_DIRECTORY
      value(IMPATH) type STRING
      value(PDFXSTRING) type XSTRING optional
      value(URL) type STRING optional
    returning
      value(RTMSG) type BAPI_MSG .
  methods FTP_INIT .
  methods STRUST_INSTALLER
    importing
      value(P_CONT) type PSECONTEXT default 'SSLC'
      value(P_APPL) type SSFAPPL default 'ANONYM'
      value(P_DOMAIN) type STRING
      value(P_SSL_ID) type SSFAPPL default 'ANONYM'
      value(P_ROOT) type ABAP_BOOL default ABAP_TRUE
      value(P_MAIN) type ABAP_BOOL default ABAP_FALSE
      value(P_TEXT) type STRING
    exporting
      value(RTMSG) type BAPI_MSG
    returning
      value(RESULT) type /APMG/CL_STRUST=>TY_UPDATE_RESULT .
ENDCLASS.



CLASS ZCL_MYCST IMPLEMENTATION.


  METHOD constructor.
    ftp_init( ).
  ENDMETHOD.


  METHOD ftp_init.
    IF mo_ftp IS NOT BOUND.
      SELECT SINGLE jz
        INTO @DATA(cbs_ftp)
        FROM ztsap_pubdata
        WHERE zj = 'CBS_FTP'
        AND sxbs NE 'X'.
      SPLIT cbs_ftp AT ',' INTO TABLE DATA(t_para).
      IF lines( t_para ) EQ 4.
        TRY.
            mo_ftp = NEW zcl_wd_ftp( iv_hostname = CONV zcl_wd_ftp=>mty_hostname( t_para[ 1 ] )
                                     iv_port     = CONV i( t_para[ 2 ] )
                                     iv_username = CONV zcl_wd_ftp=>mty_username( t_para[ 3 ] )
                                     iv_password = CONV zcl_wd_ftp=>mty_password( t_para[ 4 ] ) ).
          CATCH zcx_wd_ftp_error INTO DATA(gx_error).

        ENDTRY.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_jxmx.
    TYPES: BEGIN OF t_MXLIST3,
             mxxh  TYPE string,
             cpmc  TYPE string,
             cpxh  TYPE string,
             cpdw  TYPE string,
             cpsl  TYPE string,
             cpdj  TYPE string,
             bhsje TYPE string,
             sl    TYPE string,
             se    TYPE string,
             hsje  TYPE string,
             flbm  TYPE string,
           END OF t_MXLIST3.
    TYPES: tt_MXLIST3 TYPE STANDARD TABLE OF t_MXLIST3 WITH DEFAULT KEY.
    TYPES: BEGIN OF t_ROWS2,
             fpzl                TYPE string,
             fpzlmc              TYPE string,
             fphm                TYPE string,
             fpid                TYPE string,
             kprq                TYPE string,
             time                TYPE string,
             kpje                TYPE string,
             kpse                TYPE string,
             jshj                TYPE string,
             khsh                TYPE string,
             khmc                TYPE string,
             khdz                TYPE string,
             khkhyhz             TYPE string,
             xfsh                TYPE string,
             xfmc                TYPE string,
             tdywlx              TYPE string,
             xfdz                TYPE string,
             xfyhzh              TYPE string,
             bz                  TYPE string,
             shrrmc              TYPE string,
             jqbh                TYPE string,
             jym                 TYPE string,
             xym                 TYPE string,
             lcpbz               TYPE string,
             departure_station   TYPE string,
             train_number        TYPE string,
             arrival_station     TYPE string,
             boarding_time       TYPE string,
             landing_time        TYPE string,
             fare                TYPE string,
             fuel_surcharge      TYPE string,
             issued_by           TYPE string,
             civil_aviation_fund TYPE string,
             flight_from         TYPE string,
             flight_to           TYPE string,
             flight_number       TYPE string,
             seat_class          TYPE string,
             flight_time         TYPE string,
             fare_basis          TYPE string,
             insurance           TYPE string,
             flight_date         TYPE string,
             other_taxes         TYPE string,
             fpzt                TYPE string,
             fpztms              TYPE string,
             jlsj                TYPE string,
             gxbz                TYPE string,
             shbz                TYPE string,
             gxrrmc              TYPE string,
             gxsj                TYPE string,
             rzbz                TYPE string,
             rzssy               TYPE string,
             updatetime          TYPE string,
             fpmw                TYPE string,
             kpr                 TYPE string,
             skr                 TYPE string,
             fhr                 TYPE string,
             jdhm                TYPE string,
             pc                  TYPE string,
             kdkse               TYPE string,
             mxdq                TYPE string,
             dqrq                TYPE string,
             yt                  TYPE string,
             tsbz                TYPE string,
             rzdq                TYPE string,
             fxdj                TYPE string,
             hzsd                TYPE string,
             glfphm              TYPE string,
             ptdq                TYPE string,
             cxewm               TYPE string,
             zdyzd1              TYPE string,
             zdyzd2              TYPE string,
             zdyzd3              TYPE string,
             zdyzd4              TYPE string,
             zdyzd5              TYPE string,
             xzsj                TYPE string,
             sjly                TYPE string,
             cqzh                TYPE string,
             cph                 TYPE string,
             cxr                 TYPE string,
             yxsfzh              TYPE string,
             cxrq                TYPE string,
             cfd                 TYPE string,
             ddd                 TYPE string,
             dj                  TYPE string,
             jtgjlx              TYPE string,
             lkxm                TYPE string,
             cyr                 TYPE string,
             hbh                 TYPE string,
             rq                  TYPE string,
             sj                  TYPE string,
             cxcc                TYPE string,
             fxbz                TYPE string,
             fxsm                TYPE string,
             sjurl_pdf           TYPE string,
             sjurl_ofd           TYPE string,
             sjurl_xml           TYPE string,
             sjurl               TYPE string,
             url                 TYPE string,
             qysh                TYPE string,
             rzsj                TYPE string,
             flight_type         TYPE string,
             flight_carrier      TYPE string,
             train_dzkph         TYPE string,
             train_cx            TYPE string,
             train_xw            TYPE string,
             train_ywlx          TYPE string,
             rown                TYPE string,
             mxxh                TYPE string,
             cpmc                TYPE string,
             cpxh                TYPE string,
             cpdw                TYPE string,
             cpsl                TYPE string,
             cpdj                TYPE string,
             bhsje               TYPE string,
             sl                  TYPE string,
             se                  TYPE string,
             hsje                TYPE string,
             flbm                TYPE string,
             lslbz               TYPE string,
             mxlist              TYPE tt_MXLIST3,
           END OF t_ROWS2.
    TYPES: tt_ROWS2 TYPE STANDARD TABLE OF t_ROWS2 WITH DEFAULT KEY.
    TYPES: BEGIN OF t_JSON1,
             result  TYPE string,
             code    TYPE string,
             message TYPE string,
             total   TYPE i,
             rows    TYPE tt_ROWS2,
           END OF t_JSON1.
    TYPES:ty_head_ind TYPE ztmycsthead WITH INDICATORS ind,
          tt_head_ind TYPE SORTED TABLE OF ty_head_ind WITH UNIQUE KEY fpzl fphm,
          ty_item_ind TYPE ztmycstmxlist WITH INDICATORS ind,
          tt_item_ind TYPE SORTED TABLE OF ty_item_ind WITH UNIQUE KEY fpzl fphm mxxh.
    DATA:url        TYPE string,
         out_put    TYPE string,
         otmsg      TYPE string,
         input      TYPE string,
         status     TYPE i,
         wout       TYPE t_JSON1,
         trows      TYPE tt_ROWS2,
         tmxlist    TYPE tt_MXLIST3,
         thead_ind  TYPE tt_head_ind,
         whead_ind  LIKE LINE OF thead_ind,
         whead_indx LIKE LINE OF thead_ind,
         titem_ind  TYPE tt_item_ind,
         witem_ind  LIKE LINE OF titem_ind,
         witem_indx LIKE LINE OF titem_ind,
         thead      TYPE TABLE OF ztmycsthead,
         titem      TYPE TABLE OF ztmycstmxlist.
    DATA:BEGIN OF wa_key,
           sapmk TYPE ztlongtext-sapmk,
           tdid  TYPE ztlongtext-tdid,
           sapno TYPE ztlongtext-sapno,
         END OF wa_key.
    DATA:rows       TYPE sy-dbcnt,
         tokentimes TYPE sy-dbcnt.
    DATA:t_para  TYPE TABLE OF string.
    FIELD-SYMBOLS:<fs> TYPE any.

    PERFORM publock IN PROGRAM zfi025_1 IF FOUND USING 'PROG' jxmxin-qysh 'E' '1' CHANGING rtmsg.
    IF rtmsg IS NOT INITIAL.
      rtmsg = |E:{ rtmsg }|.
      MESSAGE rtmsg TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    ftp_init( ).
    IF NOT mo_ftp IS BOUND.
      MESSAGE e000(oo) WITH 'E:' 'ZMM000配置【CBS_FTP】信息' INTO rtmsg.
    ENDIF.

    wa_key-sapmk = 'mycs'.
    wa_key-tdid  = 'id' .
    wa_key-sapno = ''.
    IMPORT id = jxmxin-token FROM DATABASE ztlongtext(id) ID wa_key .
    IF jxmxin-token IS INITIAL.
      jxmxin-token = get_token( ).
    ENDIF.

    jxmxin-pageindex = 1.

    url = 'https://mycst.cn/NEWKP/JXFPAPI/JXMX'.

    WHILE rows LT wout-total OR rows = 0.
      IF tokentimes GE 2.
        rtmsg = |E:更新失败,获取token出现了问题|.
        mo_ftp->disconnect( ).
        RETURN.
      ENDIF.
      input = |token={ jxmxin-token }&qysh={ jxmxin-qysh }&kprqq={ jxmxin-kprqq }&kprqz={ jxmxin-kprqz }&pageindex={ jxmxin-pageindex }|.
      IF jxmxin-fphm IS NOT INITIAL.
        input = |{ input }&fphm={ jxmxin-fphm }|.
      ENDIF.
      IF jxmxin-xfmc IS NOT INITIAL.
        input = |{ input }&xfmc={ jxmxin-xfmc }|.
      ENDIF.
      IF jxmxin-fpzl IS NOT INITIAL.
        input = |{ input }&fpzl={ jxmxin-fpzl }|.
      ENDIF.
      IF jxmxin-khmc IS NOT INITIAL.
        input = |{ input }&khmc={ jxmxin-khmc }|.
      ENDIF.
      CLEAR:out_put,otmsg,status.
      CALL METHOD zcl_dingtalk=>create_http_client
        EXPORTING
          input     = input
          url       = url
*         username  =
*         password  =
          reqmethod = 'POST'
*         http1_1   = ABAP_TRUE
*         proxy     =
          bodytype  = 'X-WWW-FORM-URLENCODED'
*         header    = t_header
        IMPORTING
          output    = out_put
          rtmsg     = otmsg
          status    = status.
      CLEAR wout.
      /ui2/cl_json=>deserialize( EXPORTING json = out_put pretty_name = /ui2/cl_json=>pretty_mode-none CHANGING data = wout ).
      IF wout-result = '1' AND lines( wout-rows ) GT 0.
        APPEND LINES OF wout-rows TO trows.
        rows += lines( wout-rows ).
        jxmxin-pageindex += 1.
        EXIT.
      ELSEIF wout-code BETWEEN '2001' AND '2009' .
        jxmxin-token = get_token( ).
        tokentimes += 1.
      ELSE.
        EXIT.
      ENDIF.
    ENDWHILE.
    IF trows IS NOT INITIAL.
      CLEAR:thead,titem,thead_ind,titem_ind.
      SELECT
        fpzl,
        fphm,
        ftpname
        FROM ztmycsthead
        ORDER BY fpzl,fphm
        INTO TABLE @DATA(tcheck)
        .
      SELECT
        tabname,
        fieldname
        FROM dd03l
        WHERE tabname IN ( 'ZTMYCSTHEAD','ZTMYCSTMXLIST' )
        AND keyflag   = 'X'
        AND as4local  = 'A'
        ORDER BY tabname,fieldname
        INTO TABLE @DATA(lt_key_fields)
        .
      DATA(comph) = CAST cl_abap_structdescr(
                        CAST cl_abap_tabledescr(
                             cl_abap_tabledescr=>describe_by_data( trows )
                             )->get_table_line_type( )
                        )->components.
      LOOP AT comph ASSIGNING FIELD-SYMBOL(<ch>).
        READ TABLE lt_key_fields WITH KEY tabname = 'ZTMYCSTHEAD' fieldname = <ch>-name TRANSPORTING NO FIELDS BINARY SEARCH.
        IF sy-subrc EQ 0.
          CONTINUE.
        ENDIF.
        DATA(name) = |WHEAD_INDX-IND-{ <ch>-name }|.
        UNASSIGN <fs>.
        ASSIGN (name) TO <fs>.
        IF sy-subrc EQ 0.
          <fs> = '01'.
        ENDIF.
      ENDLOOP.

      DATA(compi) = CAST cl_abap_structdescr(
                        CAST cl_abap_tabledescr(
                             cl_abap_tabledescr=>describe_by_data( tmxlist )
                             )->get_table_line_type( )
                        )->components.
      LOOP AT compi ASSIGNING FIELD-SYMBOL(<ci>).
        READ TABLE lt_key_fields WITH KEY tabname = 'ZTMYCSTMXLIST' fieldname = <ch>-name TRANSPORTING NO FIELDS BINARY SEARCH.
        IF sy-subrc EQ 0.
          CONTINUE.
        ENDIF.
        DATA(namei) = |WITEM_INDX-IND-{ <ci>-name }|.
        UNASSIGN <fs>.
        ASSIGN (namei) TO <fs>.
        IF sy-subrc EQ 0.
          <fs> = '01'.
        ENDIF.
      ENDLOOP.

      LOOP AT trows ASSIGNING FIELD-SYMBOL(<row>).
        READ TABLE tcheck INTO DATA(wcheck) WITH KEY fpzl = <row>-fpzl fphm = <row>-fphm BINARY SEARCH.
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING <row> TO whead_ind.
          MOVE-CORRESPONDING whead_indx-ind TO whead_ind-ind.
          IF wcheck-ftpname IS INITIAL AND <row>-sjurl_pdf IS NOT INITIAL.
            rtmsg = stor_file( lv_dir = |/{ sy-mandt }/mycst/{ <row>-xfmc }|
                               impath = |{ <row>-fpzl }_{ <row>-fphm }_{ <row>-kprq }.pdf|
                               url    = <row>-sjurl_pdf ).
            IF rtmsg(1) = 'S'.
              whead_ind-ftpname = |{ <row>-fpzl }_{ <row>-fphm }_{ <row>-kprq }.pdf|.
              whead_ind-ind-ftpname = '01'.
            ENDIF.
          ENDIF.
          INSERT whead_ind INTO TABLE thead_ind.
          LOOP AT <row>-mxlist ASSIGNING FIELD-SYMBOL(<mx>).
            MOVE-CORRESPONDING <mx> TO witem_ind.
            witem_ind-fpzl = <row>-fpzl.
            witem_ind-fphm = <row>-fphm.
            MOVE-CORRESPONDING witem_indx-ind TO witem_ind-ind.
            INSERT witem_ind INTO TABLE titem_ind.
          ENDLOOP.
        ELSE.
          APPEND INITIAL LINE TO thead ASSIGNING FIELD-SYMBOL(<head>).
          MOVE-CORRESPONDING <row> TO <head>.
          IF <row>-sjurl_pdf IS NOT INITIAL.
            rtmsg = stor_file( lv_dir = |/{ sy-mandt }/mycst/{ <row>-xfmc }|
                               impath  = |{ <row>-fpzl }_{ <row>-fphm }_{ <row>-kprq }.pdf|
                               url =  <row>-sjurl_pdf ).
            IF rtmsg(1) = 'S'.
              <head>-ftpname = |{ <row>-fpzl }_{ <row>-fphm }_{ <row>-kprq }.pdf|.
            ENDIF.
          ENDIF.

          LOOP AT <row>-mxlist ASSIGNING <mx>.
            APPEND INITIAL LINE TO titem ASSIGNING FIELD-SYMBOL(<item>).
            MOVE-CORRESPONDING <mx> TO <item>.
            <item>-fpzl = <row>-fpzl.
            <item>-fphm = <row>-fphm.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
      IF thead IS NOT INITIAL.
        INSERT ztmycsthead FROM TABLE @thead.
        INSERT ztmycstmxlist FROM TABLE @titem.
        COMMIT WORK AND WAIT.
      ENDIF.
      IF thead_ind IS NOT INITIAL.
        UPDATE ztmycsthead FROM TABLE @thead_ind INDICATORS SET STRUCTURE ind.
        UPDATE ztmycstmxlist FROM TABLE @titem_ind INDICATORS SET STRUCTURE ind.
        COMMIT WORK AND WAIT.
      ENDIF.
      rtmsg = |S:更新成功|.
    ELSE.
      rtmsg = |E:更新失败,[{ status }],[{ otmsg }],[{ out_put }]|.
    ENDIF.
    mo_ftp->disconnect( ).
  ENDMETHOD.


  METHOD get_token.
    TYPES: BEGIN OF t_JSON1,
             _result         TYPE string,
             _message        TYPE string,
             _c_z_y_i_d      TYPE string,
             _domain         TYPE string,
             _t_e_x_t        TYPE string,
             _i_d            TYPE string,
             _c_o_d_e        TYPE string,
             _chage_password TYPE string,
           END OF t_JSON1.
    DATA:url     TYPE string,
         out_put TYPE string,
         otmsg   TYPE string,
         input   TYPE string,
         status  TYPE i,
         wout    TYPE t_JSON1.
    DATA:BEGIN OF wa_key,
           sapmk TYPE ztlongtext-sapmk,
           tdid  TYPE ztlongtext-tdid,
           sapno TYPE ztlongtext-sapno,
         END OF wa_key.
    wa_key-sapmk = 'mycs'.
    wa_key-tdid  = 'id' .
    wa_key-sapno = ''.
    SELECT SINGLE jz
      INTO @DATA(author)
      FROM ztsap_pubdata
      WHERE zj = 'MYCSTLOGIN'
      AND sxbs NE 'X'.
    SPLIT author AT ',' INTO DATA(uname) DATA(passwd).
    IF uname IS INITIAL OR passwd IS INITIAL.
      RETURN.
    ENDIF.
    input = |UserName={ uname }&Password1={ passwd }|.
    url = 'https://mycst.cn/NEWKP/LOGIN/GetToken'.
    CALL METHOD zcl_dingtalk=>create_http_client
      EXPORTING
        input     = input
        url       = url
*       username  =
*       password  =
        reqmethod = 'POST'
*       http1_1   = ABAP_TRUE
*       proxy     =
        bodytype  = 'X-WWW-FORM-URLENCODED'
*       header    = t_header
      IMPORTING
        output    = out_put
        rtmsg     = otmsg
        status    = status.
    /ui2/cl_json=>deserialize( EXPORTING json = out_put pretty_name = /ui2/cl_json=>pretty_mode-camel_case CHANGING data = wout ).
    IF wout-_result = '1'.
      token = wout-_i_d.
      EXPORT id = wout-_i_d TO DATABASE ztlongtext(id) ID wa_key .
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD load_file.
    ftp_init( ).
    CHECK mo_ftp IS BOUND.
    TRY.
        CALL METHOD mo_ftp->change_directory
          EXPORTING
            iv_directory = CONV zcl_wd_ftp=>mty_directory( |{ lv_dir }| ).
        CALL METHOD mo_ftp->download_xstring
          EXPORTING
            iv_filename = CONV char255( |{ impath }| )
          RECEIVING
            rv_bin_data = filex.

        IF xstrlen( filex ) LT 1000.
          CLEAR filex.
        ENDIF.
      CATCH zcx_wd_ftp_error INTO DATA(gx_error).
        MESSAGE gx_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
    mo_ftp->disconnect( ).
  ENDMETHOD.


  METHOD stor_file.
    CHECK mo_ftp IS BOUND.
    DATA:status TYPE i,
         rmsg   TYPE string.
    IF url IS NOT INITIAL.
      WHILE status NE 200.
        IF sy-index GT 2.
          EXIT.
        ENDIF.
        CLEAR:pdfxstring,status,rmsg.
        CALL METHOD zcl_dingtalk=>create_http_client
          EXPORTING
*           input     =
            url       = url
*           username  =
*           password  =
            reqmethod = 'GET'
*           http1_1   = ABAP_TRUE
*           proxy     =
            bodytype  = ''
*           header    =
          IMPORTING
*           output    =
            rtmsg     = rmsg
            status    = status
*           fields    =
            outputx   = pdfxstring.
        IF status = '407' AND rmsg CP '*SSL handshake with*Client certificate verification failed*'.
          CALL METHOD me->strust_installer
            EXPORTING
*             p_cont   = 'SSLC'
*             p_appl   = 'ANONYM'
              p_appl   = 'DFAULT'
              p_domain = url
*             p_ssl_id = 'ANONYM'
              p_root   = abap_true
              p_main   = abap_false
              p_text   = '票帮手'
            IMPORTING
              rtmsg    = DATA(rmsgstrust)
            RECEIVING
              result   = DATA(result).
          IF rmsgstrust(1) NE 'S'.
            rmsg = rmsgstrust.
            EXIT.
          ELSE.
            CALL FUNCTION 'ICM_SHUTDOWN_ICM'
              EXPORTING
                global              = 1
                how                 = 15
              EXCEPTIONS
                icm_op_failed       = 1
                icm_get_serv_failed = 2
                icm_auth_failed     = 3
                OTHERS              = 4.
            IF sy-subrc <> 0.
* Implement suitable error handling here
            ELSE.
              WAIT UP TO 5 SECONDS.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDWHILE.
    ENDIF.
    IF NOT xstrlen( pdfxstring ) GT 1000.
      rtmsg = |E:{ rmsg }|.
      RETURN.
    ENDIF.
    TRY.
        CALL METHOD mo_ftp->change_directory
          EXPORTING
            iv_directory = lv_dir.
      CATCH zcx_wd_ftp_error.
        TRY.
            CALL METHOD mo_ftp->change_directory
              EXPORTING
                iv_directory = '/'.
            CALL METHOD mo_ftp->create_directory
              EXPORTING
                iv_directory = lv_dir.
          CATCH zcx_wd_ftp_error INTO DATA(gx_error).
            rtmsg =  |E:{ gx_error->get_text( ) }|.
            RETURN.
        ENDTRY.
    ENDTRY.
    TRY.
        CALL METHOD mo_ftp->upload_xstring
          EXPORTING
            iv_filename = |{ impath }|
            iv_bin_data = pdfxstring.
      CATCH zcx_wd_ftp_error INTO gx_error.
        rtmsg =  |E:{ gx_error->get_text( ) }|.
        RETURN.
    ENDTRY.
    rtmsg = 'S:存储成功'.
  ENDMETHOD.


  METHOD strust_installer.

    CALL FUNCTION 'HTTP_PARSE_URL'
      EXPORTING
        url      = p_domain
      IMPORTING
*       SCHEME   =
        hostname = p_domain
*       SERVICE  =
*       URI      =
*       URL_IS_RELATIVE       =
      .
    CALL FUNCTION 'SSFPSE_PARAMETER'
      EXPORTING
        context       = p_cont
        applic        = p_appl
      EXCEPTIONS
        pse_not_found = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      rtmsg = |E:PSE not found|.
      RETURN.
    ENDIF.
    TRY.
        DATA(strust) = /apmg/cl_strust=>create(
          context     = p_cont
          application = p_appl
          password    = '' )->load( ).
      CATCH /apmg/cx_error INTO DATA(error).
        rtmsg = |E:{ error->get_text( ) }|.
        RETURN.
    ENDTRY.
    TRY.
        DATA(json) = /apmg/cl_strust_cert_api=>get_certificates(
          ssl_id        = p_ssl_id
          domain        = p_domain
          host          = /apmg/cl_strust_cert_api=>c_api_host
          endpoint      = /apmg/cl_strust_cert_api=>c_api_endpoint
          ).
      CATCH /apmg/cx_error INTO error.
        rtmsg = |E:{ error->get_text( ) }|.
        RETURN.
    ENDTRY.
    TRY.
        DATA(ajson) = zcl_ajson=>parse( json ).
      CATCH zcx_ajson_error INTO DATA(ajson_error).
        rtmsg = |E:Error parsing API response:{ ajson_error->get_text( ) }|.
        RETURN.
    ENDTRY.

    IF ajson->get( '/error' ) IS NOT INITIAL.
      rtmsg = |E:Error getting certificates from API:{ ajson->get( '/error' ) }|.
      RETURN.
    ENDIF.

    " Keep fingers crossed that the response matches what we need for the update
    DATA(cert_domain) = ajson->get( '/domain' ).

    IF cert_domain <> p_domain AND p_domain NA '*'.
      rtmsg = |E:Certificates domain does not match request:{ cert_domain }|.
      RETURN.
    ENDIF.

    " We finally have a certificate that can be used for the install, yay!

    " Root and intermediate certificates
    IF p_root = abap_true.
      TRY.
          LOOP AT ajson->members( '/intermediateCertificates' ) INTO DATA(member).
            DATA(inter_pem)       = ajson->get( '/intermediateCertificates/' && member && '/pem' ).
            DATA(inter_date_from) = ajson->get( '/intermediateCertificates/' && member && '/validFrom' ).
            DATA(inter_date_to)   = ajson->get( '/intermediateCertificates/' && member && '/validTo' ).
            DATA(inter_subject)   = 'CN=' && ajson->get( '/intermediateCertificates/' && member && '/subject/CN' ).
            IF inter_subject = 'CN='.
              inter_subject = 'O=' && ajson->get( '/intermediateCertificates/' && member && '/subject/O' ).
            ENDIF.
            IF strlen( inter_subject ) > 78.
              inter_subject = inter_subject(75) && '...'.
            ENDIF.
            strust->add_pem( inter_pem ).
          ENDLOOP.
          rtmsg = |S:Root/intermediate certificate added|.
        CATCH /apmg/cx_error INTO error.
          rtmsg = |E:Error updating Root/intermediate certificate:{ error->get_text( ) }|.
      ENDTRY.
    ENDIF.

    " Domain certificate
    IF p_main = abap_true.
      TRY.
          DATA(peer_pem)       = ajson->get( '/peerCertificate/pem' ).
          DATA(peer_date_from) = ajson->get( '/peerCertificate/validFrom' ).
          DATA(peer_date_to)   = ajson->get( '/peerCertificate/validTo' ).
          DATA(peer_subject)   = 'CN=' && ajson->get( '/peerCertificate/subject/CN' ).
          IF peer_subject = 'CN='.
            peer_subject = 'O=' && ajson->get( '/peerCertificate/subject/O' ).
          ENDIF.
          IF strlen( peer_subject ) > 78.
            peer_subject = peer_subject(75) && '...'.
          ENDIF.
          strust->add_pem( peer_pem ).
          rtmsg = |S:New certificate added|.

        CATCH /apmg/cx_error INTO error.
          rtmsg = |E:Error updating certificate:{ error->get_text( ) }|.
      ENDTRY.
    ENDIF.
    TRY.
        result = strust->update( p_text ).
      CATCH /apmg/cx_error INTO error.
        rtmsg = |E:{ error->get_text( ) }|.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
