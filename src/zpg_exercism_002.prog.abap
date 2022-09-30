*&---------------------------------------------------------------------*
*& Report zpg_exercism_002
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_exercism_002.


CLASS zcl_itab_nesting DEFINITION
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF artists_type,
             artist_id   TYPE string,
             artist_name TYPE string,
           END OF artists_type.
    TYPES artists TYPE STANDARD TABLE OF artists_type WITH KEY artist_id.
    TYPES: BEGIN OF albums_type,
             artist_id  TYPE string,
             album_id   TYPE string,
             album_name TYPE string,
           END OF albums_type.
    TYPES albums TYPE STANDARD TABLE OF albums_type WITH KEY artist_id album_id.
    TYPES: BEGIN OF songs_type,
             artist_id TYPE string,
             album_id  TYPE string,
             song_id   TYPE string,
             song_name TYPE string,
           END OF songs_type.
    TYPES songs TYPE STANDARD TABLE OF songs_type WITH KEY artist_id album_id song_id.


    TYPES: BEGIN OF song_nested_type,
             song_id   TYPE string,
             song_name TYPE string,
           END OF song_nested_type.
    TYPES: BEGIN OF album_song_nested_type,
             album_id   TYPE string,
             album_name TYPE string,
             songs      TYPE STANDARD TABLE OF song_nested_type WITH KEY song_id,
           END OF album_song_nested_type.
    TYPES: BEGIN OF artist_album_nested_type,
             artist_id   TYPE string,
             artist_name TYPE string,
             albums      TYPE STANDARD TABLE OF album_song_nested_type WITH KEY album_id,
           END OF artist_album_nested_type.
    TYPES nested_data TYPE STANDARD TABLE OF artist_album_nested_type WITH KEY artist_id.

    DATA: lt_artists     TYPE artists,
          lt_albums      TYPE albums,
          lt_songs       TYPE songs,
          lt_nested_data TYPE nested_data.

    METHODS fill_itab.

    METHODS perform_nesting
      IMPORTING
        artists            TYPE artists
        albums             TYPE albums
        songs              TYPE songs
      RETURNING
        VALUE(nested_data) TYPE nested_data.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_itab_nesting IMPLEMENTATION.

  METHOD fill_itab.

    lt_artists = VALUE artists( ( artist_id = '1' artist_name = 'Godsmack' )
                             ( artist_id = '2' artist_name = 'Shinedown' ) ).

    lt_albums = VALUE albums( ( artist_id = '1' album_id = '1' album_name = 'Faceless' )
                              ( artist_id = '1' album_id = '2' album_name = 'When Lengends Rise' )
                              ( artist_id = '1' album_id = '1' album_name = 'The Sound of Madness' )
                              ( artist_id = '1' album_id = '2' album_name = 'Planet Zero' ) ).



    lt_songs = VALUE songs( ( artist_id = '1' album_id = '1'  song_id = '1' song_name = 'Straight Out Of Line')
                            ( artist_id = '1' album_id = '1'  song_id = '2' song_name = 'Changes' )
                            ( artist_id = '1' album_id = '2'  song_id = '1' song_name = 'Bullet Proof' )
                            ( artist_id = '1' album_id = '2'  song_id = '2' song_name = 'Under Your Scars' )
                            ( artist_id = '2' album_id = '1'  song_id = '1' song_name = 'Second Chance' )
                            ( artist_id = '2' album_id = '1'  song_id = '2' song_name = 'Breaking Inside' )
                            ( artist_id = '2' album_id = '2'  song_id = '1' song_name = 'Dysfunctional You' )
                            ( artist_id = '2' album_id = '2'  song_id = '2' song_name = 'Daylight' ) ).


  ENDMETHOD.

  METHOD perform_nesting.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  "DATA lt_agg TYPE zcl_itab_nesting=>aggregated_data.

  DATA(lo_main) = NEW zcl_itab_nesting(  ).
