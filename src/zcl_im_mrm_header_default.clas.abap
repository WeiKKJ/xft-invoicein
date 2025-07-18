class ZCL_IM_MRM_HEADER_DEFAULT definition
  public
  final
  create public .

public section.

  interfaces IF_EX_MRM_HEADER_DEFAULT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_MRM_HEADER_DEFAULT IMPLEMENTATION.


  METHOD if_ex_mrm_header_default~header_default_set.
    DATA:t_dataList TYPE zldatalist,
         wrbtr      TYPE wrbtr.

    IMPORT invoice = t_dataList FROM MEMORY ID 'XFT_INVOICE_IN'.
    IF sy-subrc EQ 0.
      LOOP AT t_dataList ASSIGNING FIELD-SYMBOL(<t>).
        wrbtr += CONV wrbtr( <t>-tax_included_amount ).
      ENDLOOP.
      e_bktxt = wrbtr.
      CONDENSE e_bktxt NO-GAPS.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
