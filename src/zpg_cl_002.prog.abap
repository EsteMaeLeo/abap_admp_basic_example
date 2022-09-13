*&---------------------------------------------------------------------*
*& Report zpg_cl_002
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_cl_002.

    TYPES: tt_booking TYPE STANDARD TABLE OF /dmo/booking WITH EMPTY KEY,
           tt_sbook   TYPE STANDARD TABLE OF sbook WITH EMPTY KEY.

    DATA: lt_sbook TYPE tt_sbook.

    DATA: lv_timestamp_start   TYPE timestampl,
          lv_timestamp_end     TYPE timestampl,
          lv_timestamp_elapsed TYPE timestampl,
          lv_elapsed           TYPE string.

    GET TIME STAMP FIELD lv_timestamp_start.

    lt_sbook = zcl_data_001=>get_sbook_all(  ).

    GET TIME STAMP FIELD lv_timestamp_end.

    DATA(lv_timepassed_secods) = cl_abap_tstmp=>subtract(
                                  tstmp1 = lv_timestamp_end
                                  tstmp2 = lv_timestamp_start ).

BREAK-POINT.

    CLEAR: lv_timestamp_start,
           lv_timestamp_end.

** AMP CLASS

    TRY.
        GET TIME STAMP FIELD lv_timestamp_start.

        zcl_amp_001=>get_sbook_all(
            IMPORTING
                result = DATA(lt_result) ).

        GET TIME STAMP FIELD lv_timestamp_end.

        DATA(lv_timepassed_secods2) = cl_abap_tstmp=>subtract(
                                      tstmp1 = lv_timestamp_end
                                      tstmp2 = lv_timestamp_start ).

      CATCH cx_amdp_execution_error INTO DATA(lx_amdp).
        cl_demo_output=>display( lx_amdp->get_longtext(  ) ).
    ENDTRY.

BREAK-POINT.


    cl_demo_output=>new(
            )->begin_section( |Time Seconds regular | && lv_timepassed_secods
           ")->write_data( lt_result
            )->end_section(
            )->next_section( |Time Seconds AMDP | && lv_timepassed_secods2
            )->end_section(
            )->display( ).
