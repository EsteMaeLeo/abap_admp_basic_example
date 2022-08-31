*&---------------------------------------------------------------------*
*& Report ZPG_CL_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_cl_001.
TYPES:
  BEGIN OF ty_flights_line,
    airline           TYPE /dmo/carrier_name,
    flight_connection TYPE /dmo/connection_id,
    price             TYPE /dmo/flight_price,
    currency          TYPE /dmo/currency_code,
  END OF ty_flights_line,

  tt_flight_table TYPE STANDARD TABLE OF ty_flights_line WITH EMPTY KEY.

DATA(zcl_new1) = NEW zcl_basis_amdp_ims( ).

DATA: lv_timestamp_start   TYPE timestampl,
      lv_timestamp_end     TYPE timestampl,
      lv_timestamp_elapsed TYPE timestampl,
      lv_elapsed           TYPE string.

TRY.
    GET TIME STAMP FIELD lv_timestamp_start.

    zcl_basis_amdp_ims=>get_flights(
        IMPORTING
            result = DATA(lt_result) ).

    GET TIME STAMP FIELD lv_timestamp_end.

    DATA(lv_timepassed_secods) = cl_abap_tstmp=>subtract(
                                  tstmp1 = lv_timestamp_end
                                  tstmp2 = lv_timestamp_start ).

  CATCH cx_amdp_execution_error INTO DATA(lx_amdp).
    cl_demo_output=>display( lx_amdp->get_longtext(  ) ).
ENDTRY.

CLEAR: lv_timestamp_start,
       lv_timestamp_end.

GET TIME STAMP FIELD lv_timestamp_start.

DATA(result) = zcl_new1->get_flg2( ).

GET TIME STAMP FIELD lv_timestamp_end.

DATA(lv_timepassed_secods2) = cl_abap_tstmp=>subtract(
                                  tstmp1 = lv_timestamp_end
                                  tstmp2 = lv_timestamp_start ).

CALL TRANSFORMATION id SOURCE results = lt_result
                       RESULT XML DATA(xml).

cl_demo_output=>new(
        )->begin_section( |Time Seconds | && lv_timepassed_secods
        )->write_data( lt_result
        )->end_section(
        )->next_section( |Time Seconds | && lv_timepassed_secods2
        )->end_section(
        )->next_section( 'XML'
        )->write_xml( xml
        )->display( ).
*cl_demo_output=>display( lt_result ).
