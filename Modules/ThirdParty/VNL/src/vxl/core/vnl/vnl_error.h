// This is core/vnl/vnl_error.h
#ifndef vnl_error_h_
#define vnl_error_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \author fsm

//
extern void vnl_error_vector_index (const char* fcn, int index);
extern void vnl_error_vector_dimension (const char* fcn, int l1, int l2);
extern void vnl_error_vector_va_arg (int n);

//
extern void vnl_error_matrix_row_index (char const* fcn, int r);
extern void vnl_error_matrix_col_index (char const* fcn, int c);
extern void vnl_error_matrix_dimension (char const* fcn, int r1, int c1, int r2, int c2);
extern void vnl_error_matrix_nonsquare (char const* fcn);
extern void vnl_error_matrix_va_arg (int n);

#endif // vnl_error_h_
