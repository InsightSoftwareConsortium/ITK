#ifndef vnl_linear_operators_3_h_
#define vnl_linear_operators_3_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME	vnl_linear_operators_3 - 3D linear algebra operations
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_linear_operators_3.h
// .FILE	vnl_linear_operators_3.cxx
//
// .SECTION Description
//    Specialized linear operators for 3D vectors and matrices.
//    Include this file if you're inlining or compiling linear algebra
//    code for speed.
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 04 Aug 96
//
// .SECTION Modifications:
//     <none yet>
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_double_3.h>
#include <vnl/vnl_double_3x3.h>

// --
inline
vnl_double_3 operator* (const vnl_double_3x3& A, const vnl_double_3& x)
{
  const double* a = A.data_block();
  double r0 = a[0] * x[0] + a[1] * x[1] + a[2] * x[2];
  double r1 = a[3] * x[0] + a[4] * x[1] + a[5] * x[2];
  double r2 = a[6] * x[0] + a[7] * x[1] + a[8] * x[2];
  return vnl_double_3(r0, r1, r2);
}

// -- 
inline
vnl_double_3 operator+ (const vnl_double_3& a, const vnl_double_3& b)
{
  double r0 = a[0] + b[0];
  double r1 = a[1] + b[1];
  double r2 = a[2] + b[2];
  return vnl_double_3(r0, r1, r2);
}

#endif // vnl_linear_operators_3_h_
