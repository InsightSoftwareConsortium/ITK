// This is vxl/vnl/vnl_error.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
//
// Copyright (C) 1991 Texas Instruments Incorporated.
// Copyright (C) 1993 General Electric Company.
//
// Permission is granted to any individual or institution to use, copy, modify,
// and distribute this software, provided that this complete copyright and
// permission notice is maintained, intact, in all copies and supporting
// documentation.
//
// Texas Instruments Incorporated and General Electric Company
// provides this software "as is" without express or implied warranty.

#include "vnl_error.h"

#include <vcl_cstdio.h>
#include <vcl_cstdlib.h>

//: Raise exception for invalid index
void vnl_error_vector_index (char const* fcn, int index) {
  //RAISE Error, SYM(vnl_error_vector), SYM(Invalid_Index),
  vcl_printf ("vnl_error_vector_index:%s: Invalid value %d specified for index.\n", 
              fcn, index);
  vcl_abort();
}

//: Raise exception for invalid dimensions
void vnl_error_vector_dimension (char const* fcn, int l1, int l2) {
  //RAISE Error, SYM(vnl_error_vector), SYM(Invalid_Dim),
  vcl_printf ("vnl_error_vector_dimension:%s: Dimensions [%d] and [%d] do not match.\n", 
              fcn, l1, l2);
  vcl_abort();
}


//: Raise exception for using class objects, or chars in (...)
void vnl_error_vector_va_arg (int n) {
  //RAISE Error, SYM(vnl_error_vector), SYM(Invalid_Va_Arg),
  vcl_printf ("vnl_error_vector_va_arg: Invalid type in ... or wrong alignment with %d bytes.\n",
              n);
  vcl_abort();
}

//--------------------------------------------------------------------------------

void vnl_error_matrix_row_index (char const* fcn, int r)  {
  //RAISE Error, SYM(vnl_error_matrix), SYM(Invalid_Row),
  vcl_printf ("vnl_error_matrix_row_index:%s: Invalid value %d specified for row.\n",
              fcn, r);
  vcl_abort();
}


//: Raise exception for invalid col index.
void vnl_error_matrix_col_index (char const* fcn, int c) {
  //RAISE Error, SYM(vnl_error_matrix), SYM(Invalid_Col),
  vcl_printf ("vnl_error_matrix_col_index:%s: Invalid value %d specified for column.\n",
              fcn, c);
  vcl_abort();
}

//: Raise exception for invalid dimensions
void vnl_error_matrix_dimension (char const* fcn, int r1, int c1, int r2, int c2) {
  //RAISE Error, SYM(vnl_error_matrix), SYM(Invalid_Dim),
  vcl_printf ("vnl_error_matrix_dimension:%s: Dimensions [%d,%d] and [%d,%d] do not match.\n",
              fcn, r1, c1, r2, c2);
  vcl_abort();
}


//: Raise exception for invalid dimensions
void vnl_error_matrix_nonsquare (char const* fcn) {
  //RAISE Error, SYM(vnl_error_matrix), SYM(Invalid_Dim),
  vcl_printf ("vnl_error_matrix_nonsquare:%s: Matrix must be square.\n", fcn);
  vcl_abort();
}

//: Raise exception for using class objects, or chars in (...)
void vnl_error_matrix_va_arg (int n) {
  //RAISE Error, SYM(vnl_error_matrix), SYM(Invalid_Va_Arg),
  vcl_printf ("vnl_error_matrix_va_arg: Invalid type in ... or wrong alignment with %d bytes.\n",
              n);
  vcl_abort();
}
