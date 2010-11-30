// This is core/vnl/vnl_linear_operators_3.h
#ifndef vnl_linear_operators_3_h_
#define vnl_linear_operators_3_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief 3D linear algebra operations
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   04 Aug 96
//
//  Specialized linear operators for 3D vectors and matrices.
//  Include this file if you're inlining or compiling linear algebra
//  code for speed.
//
// \verbatim
//  Modifications
//   LSB (Manchester) 23/3/01 Tidied documentation
// \endverbatim
//-----------------------------------------------------------------------------

#include <vnl/vnl_double_3.h>
#include <vnl/vnl_double_3x3.h>

//: The binary multiplication operator
// \relatesalso vnl_matrix_fixed
inline
vnl_double_3 operator* (const vnl_double_3x3& A, const vnl_double_3& x)
{
  const double* a = A.data_block();
  double r0 = a[0] * x[0] + a[1] * x[1] + a[2] * x[2];
  double r1 = a[3] * x[0] + a[4] * x[1] + a[5] * x[2];
  double r2 = a[6] * x[0] + a[7] * x[1] + a[8] * x[2];
  return vnl_double_3(r0, r1, r2);
}

//: The binary addition operator
// \relatesalso vnl_vector_fixed
inline
vnl_double_3 operator+ (const vnl_double_3& a, const vnl_double_3& b)
{
  double r0 = a[0] + b[0];
  double r1 = a[1] + b[1];
  double r2 = a[2] + b[2];
  return vnl_double_3(r0, r1, r2);
}

#endif // vnl_linear_operators_3_h_
