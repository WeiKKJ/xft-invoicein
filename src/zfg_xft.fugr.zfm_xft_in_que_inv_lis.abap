FUNCTION zfm_xft_in_que_inv_lis.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(INPUT) TYPE  ZSXFT_IN_QUE_INV_LIS_IN
*"     VALUE(BUKRS) TYPE  BUKRS
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  ZSXFT_IN_QUE_INV_LIS_OUT
*"     VALUE(OUTPUTSTR) TYPE  STRING
*"     VALUE(RTYPE) TYPE  BAPI_MTYPE
*"     VALUE(RTMSG) TYPE  BAPI_MSG
*"     VALUE(BODY) TYPE  STRING
*"----------------------------------------------------------------------
  zfmdatasave1 ''.
  zfmdatasave2 'B'.
  CLEAR:output,
         outputstr,
         rtype,
         rtmsg,
         body,
         xfturl.
  DATA(pub_data) = |{ header_gd-name }_{ bukrs }|.
  PERFORM getdata(zpub_data) IF FOUND USING pub_data CHANGING xfturl.
  IF xfturl IS INITIAL.
    rtype = 'E'.
    rtmsg = |ZMM000需配置[{ pub_data }]的请求地址|.
    zfmdatasave2 'R'.
    RETURN.
  ENDIF.
  body = /ui2/cl_json=>serialize(
             data          = input
             compress      = abap_false
             pretty_name   = /ui2/cl_json=>pretty_mode-camel_case ).

  CALL METHOD zcl_dingtalk=>create_http_client
    EXPORTING
      input     = body
      url       = xfturl
*     username  =
*     password  =
      reqmethod = 'POST'
*     http1_1   = ABAP_TRUE
*     proxy     =
*     bodytype  = 'JSON'
*     header    =
    IMPORTING
      output    = outputstr
      rtmsg     = DATA(msg)
      status    = DATA(status)
*     fields    =
*     outputx   =
    .

  /ui2/cl_json=>deserialize(
    EXPORTING
      json          = outputstr
      pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
      name_mappings = VALUE /ui2/cl_json=>name_mappings(
                                ( abap = 'qualificationcertificatenumber' json = 'qualificationCertificateNumber' ) )
    CHANGING
      data          = output ).

  IF status = '200' AND output-return_Code = 'SUC0000'.
    rtype = 'S'.
    rtmsg = |进销项发票查询成功,当前获取条目数[{ lines( output-body-data_list ) }],总条目数:[{ output-body-total_Size }]|.
    IF output-body-page_size * output-body-page_number LT output-body-total_size.
      output-body-has_next_page = abap_true.
    ENDIF.
  ELSE.
    rtype = 'E'.
    rtmsg = |进销项发票查询失败:[{ output-error_Msg }],http响应状态码[{ status }],http响应消息[{ msg }]|.
  ENDIF.

  zfmdatasave2 'R'.
ENDFUNCTION.
