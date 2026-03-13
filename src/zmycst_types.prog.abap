*&---------------------------------------------------------------------*
*& 包含               ZMYCST_TYPES
*&---------------------------------------------------------------------*
TYPES:BEGIN OF ty_list.
        INCLUDE TYPE ztmycsthead.
TYPES:  sel,
        details TYPE TABLE OF ztmycstmxlist WITH EMPTY KEY,
      END OF ty_list,
      tt_list TYPE TABLE OF ty_list WITH EMPTY KEY,
      BEGIN OF ty_body,
        data_list TYPE tt_list,
      END OF ty_body,
      BEGIN OF ty_output,
        body TYPE ty_body,
      END OF ty_output,
      BEGIN OF ty_tax,
        shkzg      TYPE shkzg,
        tax_code   TYPE mwskz,
        tax_amount TYPE bapiwmwst,
        sel,
      END OF ty_tax.
