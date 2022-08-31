*&---------------------------------------------------------------------*
*& Report zpg_time_consume_example
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_time_consume_example.

DATA: t1      TYPE i,
      t2      TYPE i,
      elapsed TYPE i.

* Get the time and put in T1.
GET RUN TIME FIELD t1.


* Wait 3 seconds.
CALL FUNCTION 'ENQUE_SLEEP'
  EXPORTING
    seconds = 3.

* Get the time and put in T2.
GET RUN TIME FIELD t2.
WRITE: t2.

* Calculate the different between T2 and T1.
elapsed = t2 - t1.            " In microseconds.
elapsed = elapsed / 1000000.  " In seconds.

* Display the runtime.
WRITE:/ ' Runtime =', elapsed, 'seconds'.

DATA: lv_timestamp_start   TYPE timestampl,
      lv_timestamp_end     TYPE timestampl,
      lv_timestamp_elapsed TYPE timestampl,
      lv_elapsed           TYPE string.

* Get the start time stamp.
GET TIME STAMP FIELD lv_timestamp_start.

********************************************
* Wait 11 seconds.
CALL FUNCTION 'ENQUE_SLEEP'
  EXPORTING
    seconds = 3.


* Get the end time stamp.
GET TIME STAMP FIELD lv_timestamp_end.

* Output the time stamps.
WRITE: / 'Start   : ', lv_timestamp_start.
WRITE: / 'End     : ', lv_timestamp_end.

* Calculate the elapsed time.
lv_timestamp_elapsed = lv_timestamp_end - lv_timestamp_start.
lv_elapsed = |{ lv_timestamp_elapsed TIMESTAMP = ISO TIMEZONE = 'UTC   ' }|.

* Output elapsed time stamp.
CONCATENATE lv_elapsed+11(10) '' INTO lv_elapsed. " Extract the time only.
WRITE: / 'Runtime : ', lv_elapsed.


*Another


CLEAR: lv_timestamp_start,
       lv_timestamp_end,
       lv_timestamp_elapsed,
       lv_elapsed.

GET TIME STAMP FIELD lv_timestamp_start.


CALL FUNCTION 'ENQUE_SLEEP'
  EXPORTING
    seconds = 3.

GET TIME STAMP FIELD lv_timestamp_end.

data(lv_timepassed_secods) = cl_abap_tstmp=>subtract(
                                tstmp1 = lv_timestamp_end
                                tstmp2 = lv_timestamp_start ).

WRITE: / 'Runtime : ', lv_timepassed_secods.
