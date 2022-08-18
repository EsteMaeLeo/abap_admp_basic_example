CLASS zcl_basis_amdp_ims DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES: if_amdp_marker_hdb,
      if_oo_adt_classrun.

    TYPES:
      BEGIN OF ty_flights_line,
        airline           TYPE /dmo/carrier_name,
        flight_connection TYPE /dmo/connection_id,
        price             TYPE /dmo/flight_price,
        currency          TYPE /dmo/currency_code,
      END OF ty_flights_line,

      tt_flight_table TYPE STANDARD TABLE OF ty_flights_line WITH EMPTY KEY.

    CLASS-METHODS:
      get_flights
        EXPORTING
                  VALUE(result) TYPE tt_flight_table
        RAISING   cx_amdp_execution_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_basis_amdp_ims IMPLEMENTATION.

  METHOD get_flights BY DATABASE PROCEDURE
      FOR HDB
      LANGUAGE SQLSCRIPT
      OPTIONS READ-ONLY
      USING
          /dmo/flight
          /dmo/carrier.

    result = select DISTINCT
                 c.name as airline,
                 f.connection_id as flight_connection,
                 f.price as price,
                 f.currency_code as currency
             from "/DMO/FLIGHT" as f
             inner join "/DMO/CARRIER" as c
             on f.carrier_id = c.carrier_id;


  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    TRY.
        me->get_flights(
            IMPORTING
                result = DATA(lt_result) ).
      CATCH cx_amdp_execution_error INTO DATA(lx_amdp).
        out->write( lx_amdp->get_longtext(  ) ).
    ENDTRY.

    out->write( lt_result ).

  ENDMETHOD.

ENDCLASS.