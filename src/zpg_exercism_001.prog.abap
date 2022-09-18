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
