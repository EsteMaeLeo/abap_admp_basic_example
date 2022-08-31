*&---------------------------------------------------------------------*
*& Report ZPG_CL_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_cl_001.

DATA(zcl_new1) = NEW zcl_basis_amdp_ims( ).



TRY.
    zcl_basis_amdp_ims=>get_flights(
        IMPORTING
            result = DATA(lt_result) ).
  CATCH cx_amdp_execution_error INTO DATA(lx_amdp).
    cl_demo_output=>display( lx_amdp->get_longtext(  ) ).
ENDTRY.

cl_demo_output=>display( lt_result ).
