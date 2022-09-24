*&---------------------------------------------------------------------*
*& Report zpg_exercism_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_exercism_001.

CLASS zcl_itab_basics DEFINITION
    FINAL
    CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES group TYPE c LENGTH 1.


    TYPES: BEGIN OF initial_type,
             group       TYPE group,
             number      TYPE i,
             description TYPE string,
           END OF initial_type,
           itab_data_type TYPE STANDARD TABLE OF initial_type WITH EMPTY KEY.

    METHODS fill_itab
      RETURNING
        VALUE(initial_data) TYPE itab_data_type.

    METHODS add_to_itab
      IMPORTING initial_data        TYPE itab_data_type
      RETURNING
                VALUE(updated_data) TYPE itab_data_type.

    METHODS sort_itab
      IMPORTING initial_data        TYPE itab_data_type
      RETURNING
                VALUE(updated_data) TYPE itab_data_type.

    METHODS search_itab
      IMPORTING initial_data        TYPE itab_data_type
      RETURNING
                VALUE(result_index) TYPE i.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_itab_basics IMPLEMENTATION.
  METHOD fill_itab.

    initial_data = VALUE itab_data_type( ( group = 'A' number = '10'  description = 'Group A-2' )
                                         ( group = 'B' number = '5'   description = 'Group B' )
                                         ( group = 'A' number = '6'   description = 'Group A-1' )
                                         ( group = 'C' number = '22'  description = 'Group C-1' )
                                         ( group = 'A' number = '13'  description = 'Group A-3' )
                                         ( group = 'C' number = '500' description = 'Group C-2' ) ).

    "add solution here
  ENDMETHOD.

  METHOD add_to_itab.
    updated_data = initial_data.
    APPEND VALUE #( group = 'A' number = '19'  description = 'Group A-4' ) TO updated_data.

    "add solution here
  ENDMETHOD.

  METHOD sort_itab.
    updated_data = initial_data.
    "add solution here
    SORT updated_data BY group ASCENDING AS TEXT number DESCENDING.
  ENDMETHOD.

  METHOD search_itab.
    DATA(temp_data) = initial_data.

    result_index = line_index( temp_data[ number = '6' ] ).
    "add solution here
  ENDMETHOD.

ENDCLASS.

CLASS zcl_itab_combination DEFINITION
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF alphatab_type,
             cola TYPE string,
             colb TYPE string,
             colc TYPE string,
           END OF alphatab_type.
    TYPES alphas TYPE STANDARD TABLE OF alphatab_type.

    TYPES: BEGIN OF numtab_type,
             col1 TYPE string,
             col2 TYPE string,
             col3 TYPE string,
           END OF numtab_type.
    TYPES nums TYPE STANDARD TABLE OF numtab_type.

    TYPES: BEGIN OF combined_data_type,
             colx TYPE string,
             coly TYPE string,
             colz TYPE string,
           END OF combined_data_type.
    TYPES combined_data TYPE STANDARD TABLE OF combined_data_type WITH EMPTY KEY.

    METHODS perform_combination
      IMPORTING
        alphas               TYPE alphas
        nums                 TYPE nums
      RETURNING
        VALUE(combined_data) TYPE combined_data.

    METHODS perform_combination2
      IMPORTING
        alphas               TYPE alphas
        nums                 TYPE nums
      RETURNING
        VALUE(combined_data) TYPE combined_data.

  PROTECTED SECTION.
  PRIVATE SECTION.


ENDCLASS.

CLASS zcl_itab_combination IMPLEMENTATION.

  METHOD perform_combination.

    DATA(combined_temp) = VALUE combined_data(
        FOR ls_alphatab IN  alphas
        FOR ls_numtab IN nums
        ( colx = ls_alphatab-cola + ls_numtab-col1
          coly = ls_alphatab-colb + ls_numtab-col2
          colz = ls_alphatab-colc + ls_numtab-col3 ) ).

    combined_data = combined_temp.

  ENDMETHOD.

  METHOD perform_combination2.

    combined_data = VALUE #(
        FOR ls_alphatab IN  alphas INDEX INTO lv_index
        ( colx = ls_alphatab-cola && nums[ lv_index ]-col1
          coly = ls_alphatab-colb && nums[ lv_index ]-col2
          colz = ls_alphatab-colc && nums[ lv_index ]-col3 ) ).

  ENDMETHOD.

ENDCLASS.



CLASS zcl_itab_aggregation DEFINITION
  FINAL
  CREATE PUBLIC ..

  PUBLIC SECTION.
    TYPES group TYPE c LENGTH 1.
    TYPES: BEGIN OF initial_numbers_type,
             group  TYPE group,
             number TYPE i,
           END OF initial_numbers_type,
           initial_numbers TYPE STANDARD TABLE OF initial_numbers_type WITH EMPTY KEY.

    TYPES: BEGIN OF aggregated_data_type,
             group   TYPE group,
             count   TYPE i,
             sum     TYPE i,
             min     TYPE i,
             max     TYPE i,
             average TYPE f,
           END OF aggregated_data_type,
           aggregated_data TYPE STANDARD TABLE OF aggregated_data_type WITH EMPTY KEY.

    METHODS perform_aggregation
      IMPORTING
        initial_numbers        TYPE initial_numbers
      RETURNING
        VALUE(aggregated_data) TYPE aggregated_data.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_itab_aggregation IMPLEMENTATION.
  METHOD perform_aggregation.
    " add solution here
  ENDMETHOD.

ENDCLASS.

"*************************************************************************************************

CLASS zcl_main DEFINITION
  FINAL
  CREATE PUBLIC ..

  PUBLIC SECTION.
    TYPES group TYPE c LENGTH 1.
    TYPES: BEGIN OF initial_numbers_type,
             group  TYPE group,
             number TYPE i,
           END OF initial_numbers_type,
           initial_numbers TYPE STANDARD TABLE OF initial_numbers_type WITH EMPTY KEY.

    TYPES: BEGIN OF aggregated_data_type,
             group   TYPE group,
             count   TYPE i,
             sum     TYPE i,
             min     TYPE i,
             max     TYPE i,
             average TYPE f,
           END OF aggregated_data_type,
           aggregated_data TYPE STANDARD TABLE OF aggregated_data_type WITH EMPTY KEY,
           aggdata TYPE aggregated_data_type.

    METHODS fill_itab
      RETURNING
        VALUE(initial_data) TYPE initial_numbers.


    METHODS perform_aggregation
      IMPORTING
        initial_numbers        TYPE initial_numbers
      RETURNING
        VALUE(aggregated_data) TYPE aggregated_data.

    METHODS example_reduce.

    METHODS example_groupby.

    CLASS-DATA:  lt_initial TYPE initial_numbers.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_main  IMPLEMENTATION.

  METHOD fill_itab.

    initial_data = VALUE initial_numbers( ( group = 'A' number = '10' )
                                           ( group = 'B' number = '5' )
                                           ( group = 'A' number = '6' )
                                           ( group = 'C' number = '22' )
                                           ( group = 'A' number = '13'  )
                                           ( group = 'C' number = '500' ) ).
    lt_initial = initial_data.

  ENDMETHOD.

  METHOD example_reduce.

    DATA lt_test TYPE TABLE OF i WITH EMPTY KEY.

    lt_test = VALUE #( FOR j = 1 WHILE j <= 10 ( j ) ).

    cl_demo_output=>new( )->next_section( |Simple sum usgin Reduce| )->write(
      REDUCE i( INIT s = 0
                FOR i = 1 UNTIL i > 10
                NEXT s = s + i )
      )->next_section( |String concatation using Reduce| )->write(
          REDUCE string( INIT text = |Count down: |
                          FOR i = 10 THEN i - 1 UNTIL i = 0
                          NEXT text = text && | { i  } |
          )
       )->next_section( |Table| )->write(
       lt_test
       )->next_section( |Use reduce table example| )->write(
       REDUCE i( INIT x = 0
                 FOR ls_test IN LT_Test
                 NEXT x = x + ls_test
               )
      )->display( ).


  ENDMETHOD.

  METHOD example_groupby.

    SELECT *
    FROM spfli
    INTO TABLE @DATA(lt_spfli).

    DATA members LIKE lt_spfli.
    DATA lt_grp LIKE lt_spfli.

    cl_demo_output=>display( lt_spfli ).

    LOOP AT lt_spfli INTO DATA(ls_spfli)
        GROUP BY ( carrier = ls_spfli-carrid city_from = ls_spfli-cityfrom )
        ASCENDING
        ASSIGNING FIELD-SYMBOL(<lfs_group>).

      LOOP AT GROUP <lfs_group> ASSIGNING FIELD-SYMBOL(<lfs_spfli_grp>).
        members = VALUE #( BASE members ( <lfs_spfli_grp> ) ).
      ENDLOOP.

    ENDLOOP.
    cl_demo_output=>display( members ).

    UNASSIGN: <lfs_group>, <lfs_spfli_grp>.
    FREE members.

    DATA count TYPE i.
    DATA distance TYPE spfli-distance.
    TYPES: BEGIN OF t_datos,
             city     TYPE s_from_cit,
             distance TYPE s_distance,
             avg      TYPE s_distance,
             total    TYPE s_distance,
             max      TYPE s_distance,
             min      TYPE s_distance,
           END OF t_datos.

    DATA wa_dato TYPE t_datos.
    DATA it_dato TYPE STANDARD TABLE OF t_datos.

*    LOOP AT lt_spfli INTO ls_spfli
*    GROUP BY ls_spfli-cityfrom
*    ASCENDING
*    INTO DATA(s_group).
*
*      LOOP AT GROUP s_group ASSIGNING <lfs_spfli_grp>.
*
*
*        count = count + 1.
*        distance = <lfs_spfli_grp>-distance + distance.
*        wa_dato-avg = distance / count.
*        members = VALUE #( BASE members ( <lfs_spfli_grp> ) ).
*        wa_dato-city = <lfs_spfli_grp>-cityfrom.
*        wa_dato-total = count.
*
*        IF <lfs_spfli_grp>-distance > wa_dato-max.
*             wa_dato-max =  <lfs_spfli_grp>-distance.
*        elseif  <lfs_spfli_grp>-distance < wa_dato-max .
*        wa_dato-min =  <lfs_spfli_grp>-distance.
*        ENDIF.
*
*      ENDLOOP.
*
*
*
*      wa_dato-distance =  distance.
*      APPEND wa_dato TO it_dato.
*
*      clear: wa_dato,
*              count.
*
*    ENDLOOP.

    LOOP AT lt_spfli ASSIGNING FIELD-SYMBOL(<ls_spfli>)
    GROUP BY ( cityfrom = <ls_spfli>-cityfrom
               size = GROUP SIZE
               index = GROUP INDEX
              )
    ASCENDING
    REFERENCE INTO DATA(s_group).


      LOOP AT GROUP s_group ASSIGNING <lfs_spfli_grp>.


        count = count + 1.
        distance = <lfs_spfli_grp>-distance + distance.

        "members = VALUE #( BASE members ( <lfs_spfli_grp> ) ).
        "wa_dato-city = <lfs_spfli_grp>-cityfrom.
        "wa_dato-total = count.
        IF count = 1.
          wa_dato-max =  <lfs_spfli_grp>-distance.
          wa_dato-min = <lfs_spfli_grp>-distance.
        ELSE.
          IF <lfs_spfli_grp>-distance > wa_dato-max.
            wa_dato-max =  <lfs_spfli_grp>-distance.
          ENDIF.
          IF  <lfs_spfli_grp>-distance < wa_dato-min .
            wa_dato-min =  <lfs_spfli_grp>-distance.
          ENDIF.
        ENDIF.
      ENDLOOP.

      wa_dato-total = s_group->size.

      wa_dato-city = s_group->cityfrom.

      wa_dato-distance =  distance.
      wa_dato-avg = wa_dato-distance / s_group->size.
      APPEND wa_dato TO it_dato.

      CLEAR: wa_dato,
              count.

    ENDLOOP.

    cl_demo_output=>display( it_dato ).


    TYPES: BEGIN OF ty_s_value,
             sparte TYPE char2,
             vkont  TYPE char12,
             exbel  TYPE char16,
           END OF ty_s_value.
    TYPES: ty_t_value TYPE STANDARD TABLE OF ty_s_value WITH EMPTY KEY.

    DATA(lt_tab) =
    VALUE ty_t_value(
                     ( sparte = '05' vkont = '800000008422' exbel = '1NSN150900000058')
                     ( sparte = 'L2' vkont = '800000008422' exbel = '1NSN150900000058')
                     ( sparte = '05' vkont = '800000008422' exbel = '1NSN150900000037')
                     ( sparte = 'L2' vkont = '800000008422' exbel = '1NSN150900000037')
                     ( sparte = '05' vkont = '800000008422' exbel = '1NSN150900000013')
                     ( sparte = 'L2' vkont = '800000008422' exbel = '1NSN150900000013')
                     ( sparte = '05' vkont = '800000008415' exbel = '1HSN151200000009')
                     ( sparte = 'S1' vkont = '800000008415' exbel = '1HSN151200000009')
                     ( sparte = '05' vkont = '800000008415' exbel = '1HSN151200000008')
                     ( sparte = 'S1' vkont = '800000008415' exbel = '1HSN151200000008')
                     ( sparte = 'L1' vkont = '800000008422' exbel = '1NSN150900000050')
                     ( sparte = 'L1' vkont = '800000008422' exbel = '1NSN150900000029')
                    ).

    DATA(lt_result) =
    VALUE ty_t_value( FOR GROUPS <group_key> OF <wa> IN lt_tab
                      GROUP BY ( sparte = <wa>-sparte vkont = <wa>-vkont )
                      LET max2 =
                      REDUCE #( INIT max =
                                VALUE ty_s_value( )
                                FOR <m> IN GROUP <group_key>
                                NEXT max = COND #( WHEN <m>-exbel > max-exbel THEN <m> ELSE max ) )
                      IN ( max2 ) ).
    .
    cl_demo_output=>display( lt_result ).

  ENDMETHOD.

  METHOD perform_aggregation.
    " add solution here
*    aggregated_data = VALUE aggregated_data(
*                                FOR GROUPS group1 OF wa_agg IN initial_numbers
*                                GROUP BY ( group1 =  wa_agg-group )
*                                (
*                                 group = wa_agg-group
*                                 count = wa_agg-number
*                                ) ).
*

    TYPES : BEGIN OF ty_flight,
              seq_num TYPE i,
              carrier TYPE s_carrname,
              connect TYPE s_conn_id,
              fldate  TYPE s_date,
            END OF ty_flight.

    DATA lt_new_flights TYPE STANDARD TABLE OF ty_flight.

    SELECT * FROM sflight INTO TABLE @DATA(lt_flights).
    IF sy-subrc EQ 0.
      SELECT * FROM scarr INTO TABLE @DATA(lt_scarr).

    ENDIF.

    "FOR Iteration
    lt_new_flights =
      VALUE #(
        FOR ls_flight IN lt_flights INDEX INTO lv_index
                                    WHERE ( carrid = 'AA' AND
                                            connid = '0017' )
        LET lv_carrname = lt_scarr[ carrid = ls_flight-carrid ]-carrname
        IN  carrier = lv_carrname
        ( seq_num = lv_index
          connect = ls_flight-connid
          fldate  = ls_flight-fldate
        )
      ).

    cl_demo_output=>display( lt_new_flights ).


    cl_demo_output=>display(
       VALUE aggregated_data(
          FOR GROUPS grp OF  wa_agg IN initial_numbers
          GROUP BY wa_agg-group
                ASCENDING
        WITHOUT MEMBERS
          (
           group = grp

           ) ) ).


    aggregated_data = VALUE aggregated_data(
        FOR GROUPS grp OF  wa_agg IN initial_numbers
        INDEX INTO lv_index
        GROUP BY wa_agg-group
      WITHOUT MEMBERS
      LET suma =  1
      IN count =  suma
        (
         group = grp
         "count = lv_index
         ) ).

    aggregated_data = VALUE aggregated_data(
        FOR  wa_agg IN initial_numbers
        (
        group = wa_agg-group
        count = wa_agg-number
        ) ).

        data  aggdata2 TYPE aggregated_data_type.

    aggregated_data = VALUE aggregated_data(
        FOR GROUPS ls_grp OF  <fs_agg> IN initial_numbers
        GROUP BY ( group = <fs_agg>-group
                   count = group size )
              ( value #( LET lv_sum = REDUCE i( INIT lv_val TYPE i
                                      FOR ls_gg IN GROUP ls_grp
                                      NEXT lv_val = lv_val + <fs_agg>-number )



        IN base corresponding aggregated_data_type( ls_grp )

         group = ls_grp-group
         count = lv_sum
         "count = lv_index
         ) ) ).


  ENDMETHOD.

ENDCLASS.



START-OF-SELECTION.

  DATA lt_agg TYPE zcl_main=>aggregated_data.

  DATA(lo_main) = NEW zcl_main(  ).

  lo_main->fill_itab(  ).

  lo_main->example_groupby(  ).

  lo_main->example_reduce(  ).


  lt_agg = lo_main->perform_aggregation( zcl_main=>lt_initial  ).

  SELECT * FROM spfli INTO TABLE @DATA(spfli).

  TYPES group_keys TYPE STANDARD TABLE OF spfli-carrid WITH EMPTY KEY.

  cl_demo_output=>display(
    VALUE group_keys(
      FOR GROUPS carrier OF wa IN spfli
      GROUP BY wa-carrid
      ASCENDING
      WITHOUT MEMBERS
      ( carrier ) ) ).


  "cl_demo_output=>display( zcl_main=>lt_initial ).
  DATA(v1) = REDUCE i( INIT sum = 0 FOR i = 1 THEN i + 1 UNTIL i > 10 NEXT sum = sum + i ).

  cl_demo_output=>new(
        )->begin_section( |First Table |
        )->write_data( zcl_main=>lt_initial
        )->end_section(
        )->next_section( |Second Table|
        )->write_data( lt_agg
        )->write_data( v1
        )->end_section(
        )->display( ).
