*&---------------------------------------------------------------------*
*& Report ZPG_NEW_SYNTAXIS_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_new_syntaxis_001.


*---------* OLD *---------*
DATA: lv_int1 TYPE i.
lv_int1 = 100.
WRITE lv_int1.

DATA: wa_spfli TYPE spfli,
      it_spfli TYPE STANDARD TABLE OF spfli.

FIELD-SYMBOLS: <fs_spfli> TYPE spfli.

SELECT *
  FROM spfli
  INTO TABLE it_spfli.

IF sy-subrc EQ 0.
  LOOP AT it_spfli INTO wa_spfli.

    WRITE: / wa_spfli-connid, wa_spfli-carrid, wa_spfli-cityfrom, wa_spfli-cityto.

  ENDLOOP.
ENDIF.

WRITE: / 'READ',/.

READ TABLE it_spfli INTO wa_spfli INDEX 1.
WRITE: / wa_spfli-connid, wa_spfli-carrid, wa_spfli-cityfrom, wa_spfli-cityto.

WRITE: / 'READ ASSIGN',/.
READ TABLE it_spfli ASSIGNING <fs_spfli> INDEX 2.
WRITE: / <fs_spfli>-connid,  <fs_spfli>-carrid,  <fs_spfli>-cityfrom,  <fs_spfli>-cityto, /.

WRITE: / 'DESCRIBE',/.
CLEAR lv_int1.
DESCRIBE TABLE it_spfli LINES lv_int1.
WRITE: / 'Lines  of table', lv_int1, /.

*---------* NEW *---------*
**********INLINE***********
DATA(lv_int2) = 100.
WRITE: / lv_int2, /.

SELECT *
  FROM spfli
  INTO TABLE @DATA(it_spfli2).

IF sy-subrc EQ 0.

  LOOP AT it_spfli2 INTO DATA(wa_spfli2).

    WRITE: / wa_spfli2-connid, wa_spfli2-carrid, wa_spfli2-cityfrom, wa_spfli2-cityto.

  ENDLOOP.

ENDIF.

WRITE: / 'READ',/.

READ TABLE it_spfli INTO DATA(wa_readspfli) INDEX 1.
WRITE: / wa_readspfli-connid, wa_readspfli-carrid, wa_readspfli-cityfrom, wa_readspfli-cityto.

WRITE: / 'READ ASSIGN',/.
READ TABLE it_spfli ASSIGNING FIELD-SYMBOL(<fs_spfli2>) INDEX 2.
WRITE: / <fs_spfli2>-connid,  <fs_spfli2>-carrid,  <fs_spfli2>-cityfrom,  <fs_spfli2>-cityto, /.

WRITE: / 'DESCRIBE',/.
CLEAR lv_int1.
DESCRIBE TABLE it_spfli LINES DATA(lv_lines).
WRITE: / 'Lines  of table', lv_lines, /.

DATA(lv_num_lines) = lines( it_spfli ).
WRITE: / 'USING lines statement: ', lv_num_lines, /..
