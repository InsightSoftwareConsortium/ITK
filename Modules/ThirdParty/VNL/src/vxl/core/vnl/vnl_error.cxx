// This is core/vnl/vnl_error.cxx
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

#include <iostream>

#include "vnl/vnl_error.h"

//: Raise exception for invalid index.
void vnl_error_vector_index (char const* fcn, int index)
{
  //RAISE Error, SYM(vnl_error_vector), SYM(Invalid_Index),
  std::cerr << "vnl_error_vector_index:" << fcn
           << ": Invalid value " << index << " specified for index.\n";
  throw 0;
}

//: Raise exception for invalid dimension.
void vnl_error_vector_dimension (char const* fcn, int l1, int l2)
{
  //RAISE Error, SYM(vnl_error_vector), SYM(Invalid_Dim),
  std::cerr << "vnl_error_vector_dimension:" << fcn << ": Dimensions ["
           << l1 << "] and [" << l2 << "] do not match.\n";
  throw 0;
}


//: Raise exception for using class objects, or chars in (...).
void vnl_error_vector_va_arg (int n)
{
  //RAISE Error, SYM(vnl_error_vector), SYM(Invalid_Va_Arg),
  std::cerr << "vnl_error_vector_va_arg: Invalid type in ..."
           << " or wrong alignment with " << n << " bytes.\n";
  throw 0;
}

//--------------------------------------------------------------------------------

//: Raise exception for invalid row index.
void vnl_error_matrix_row_index (char const* fcn, int r)
{
  //RAISE Error, SYM(vnl_error_matrix), SYM(Invalid_Row),
  std::cerr << "vnl_error_matrix_row_index:" << fcn
           << ": Invalid value " << r << " specified for row.\n";
  throw 0;
}


//: Raise exception for invalid col index.
void vnl_error_matrix_col_index (char const* fcn, int c)
{
  //RAISE Error, SYM(vnl_error_matrix), SYM(Invalid_Col),
  std::cerr << "vnl_error_matrix_col_index:" << fcn << ": Invalid value "
           << c << " specified for column.\n";
  throw 0;
}

//: Raise exception for invalid dimensions.
void vnl_error_matrix_dimension (char const* fcn, int r1, int c1, int r2, int c2)
{
  //RAISE Error, SYM(vnl_error_matrix), SYM(Invalid_Dim),
  std::cerr << "vnl_error_matrix_dimension:" << fcn << ": Dimensions [" << r1
           << ',' << c1 << "] and [" << r2 << ',' << c2 << "] do not match.\n";
  throw 0;
}


//: Raise exception for a nonsquare matrix.
void vnl_error_matrix_nonsquare (char const* fcn)
{
  //RAISE Error, SYM(vnl_error_matrix), SYM(Invalid_Dim),
  std::cerr << "vnl_error_matrix_nonsquare:" << fcn
           << ": Matrix must be square.\n";
  throw 0;
}

//: Raise exception for using class objects, or chars in (...).
void vnl_error_matrix_va_arg (int n)
{
  //RAISE Error, SYM(vnl_error_matrix), SYM(Invalid_Va_Arg),
  std::cerr << "vnl_error_matrix_va_arg: Invalid type in ..."
           << " or wrong alignment with " << n << " bytes.\n";
  throw 0;
}
