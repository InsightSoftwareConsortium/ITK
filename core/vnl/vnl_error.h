// This is core/vnl/vnl_error.h
#ifndef vnl_error_h_
#define vnl_error_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \author fsm
#include "vnl/vnl_export.h"

//: Raise exception for invalid index.
extern VNL_EXPORT void vnl_error_vector_index (const char* fcn, int index);

//: Raise exception for invalid dimension.
extern VNL_EXPORT void vnl_error_vector_dimension (const char* fcn, int l1, int l2);

//: Raise exception for using class objects, or chars in (...).
extern VNL_EXPORT void vnl_error_vector_va_arg (int n);

//: Raise exception for invalid row index.
extern VNL_EXPORT void vnl_error_matrix_row_index (char const* fcn, int r);

//: Raise exception for invalid col index.
extern VNL_EXPORT void vnl_error_matrix_col_index (char const* fcn, int c);

//: Raise exception for invalid dimensions.
extern VNL_EXPORT void vnl_error_matrix_dimension (char const* fcn, int r1, int c1, int r2, int c2);

//: Raise exception for a nonsquare matrix.
extern VNL_EXPORT void vnl_error_matrix_nonsquare (char const* fcn);

//: Raise exception for using class objects, or chars in (...).
extern VNL_EXPORT void vnl_error_matrix_va_arg (int n);

#endif // vnl_error_h_
