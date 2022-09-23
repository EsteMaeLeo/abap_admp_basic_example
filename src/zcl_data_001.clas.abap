CLASS zcl_data_001 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES: if_oo_adt_classrun.

    TYPES: tt_booking TYPE STANDARD TABLE OF /dmo/booking WITH EMPTY KEY,
           tt_sbook   TYPE STANDARD TABLE OF sbook WITH EMPTY KEY.

    CLASS-METHODS:
      get_booking_all RETURNING VALUE(rt_booking) TYPE tt_booking,
      get_sbook_all RETURNING VALUE(rt_sbook) TYPE tt_sbook.

  PROTECTED SECTION.
  PRIVATE SECTION.



ENDCLASS.




CLASS zcl_data_001 IMPLEMENTATION.

  METHOD get_booking_all.
  ENDMETHOD.

  METHOD get_sbook_all.

    SELECT s~mandt,
           s~carrid,
           s~connid,
           s~fldate,
           s~bookid,
           s~customid,
           s~custtype,
           s~smoker,
           s~luggweight,
           s~wunit,
           s~invoice,
           s~class,
           s~forcuram,
           s~forcurkey,
           s~loccuram,
           s~loccurkey,
           s~order_date,
           s~counter,
           s~agencynum,
           s~cancelled,
           s~reserved,
           s~passname,
           s~passform,
           s~passbirth
      INTO TABLE @rt_sbook
      FROM sbook AS s.

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.


    DATA: lt_sbook TYPE tt_sbook.

    lt_sbook = me->get_sbook_all( ).

    out->write( 'DATA *******************' ).
    out->write( lt_sbook ).
    out->write( '*******************' ).
    out->write( '*******************' ).
    out->write( '*******************' ).
    out->write( '*******************' ).


  ENDMETHOD.

ENDCLASS.
