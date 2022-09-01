CLASS zcl_amp_001 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_amdp_marker_hdb,
      if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: tt_booking TYPE STANDARD TABLE OF /dmo/booking,
           tt_sbook   TYPE STANDARD TABLE OF sbook.

    CLASS-METHODS:
      get_booking_all
        EXPORTING
                  VALUE(result) TYPE tt_booking
        RAISING   cx_amdp_execution_error,

      get_sbook_all
        EXPORTING
                  VALUE(result) TYPE tt_sbook
        RAISING   cx_amdp_execution_error.
ENDCLASS.



CLASS zcl_amp_001 IMPLEMENTATION.

  METHOD get_booking_all BY DATABASE PROCEDURE
        FOR HDB
        LANGUAGE SQLSCRIPT
        OPTIONS READ-ONLY
        USING
        /dmo/booking.

    result = select
                   b.client,
                   b.travel_id,
                   b.booking_id,
                   b.booking_date,
                   b.customer_id,
                   b.carrier_id,
                   b.connection_id,
                   b.flight_date,
                   b.flight_price,
                   b.currency_code
                from "/DMO/BOOKING" as b;


  ENDMETHOD.


  METHOD get_sbook_all BY DATABASE PROCEDURE
        FOR HDB
        LANGUAGE SQLSCRIPT
        OPTIONS READ-ONLY
        USING
        sbook.

    result = select s.mandt,
                    s.carrid,
                    s.connid,
                    s.fldate,
                    s.bookid,
                    s.customid,
                    s.custtype,
                    s.smoker,
                    s.luggweight,
                    s.wunit,
                    s.invoice,
                    s.class,
                    s.forcuram,
                    s.forcurkey,
                    s.loccuram,
                    s.loccurkey,
                    s.order_date,
                    s.counter,
                    s.agencynum,
                    s.cancelled,
                    s.reserved,
                    s.passname,
                    s.passform,
                    s.passbirth
               FROM "SBOOK" as s;
  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    TRY.
        me->get_booking_all(
            IMPORTING
                result = DATA(lt_result) ).
      CATCH cx_amdp_execution_error INTO DATA(lx_amdp).
        out->write( lx_amdp->get_longtext(  ) ).
    ENDTRY.

    out->write( '*******************' ).
    out->write( lt_result ).
    out->write( '*******************' ).



  ENDMETHOD.
ENDCLASS.
