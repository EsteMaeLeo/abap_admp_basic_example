*&---------------------------------------------------------------------*
*& Report zpg_cl_002
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_cl_002.

types:  tt_sbook   TYPE STANDARD TABLE OF sbook WITH EMPTY KEY.
data: lt_sbook TYPE tt_sbook.
      break-point.
lt_sbook = zcl_data_001=>get_sbook_all(  ).

break-point.
