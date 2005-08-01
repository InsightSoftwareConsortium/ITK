// This is core/vnl/vnl_double_3.h
#ifndef vnl_double_3_h_
#define vnl_double_3_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Contains class vnl_double_3 and function vnl_cross_3d()
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   31 Dec 96
//
// \verbatim
//  Modifications
//   Peter Vanroose, 25 June 1999: vnl_vector_fixed<double,3> already instantiated
//   Peter Vanroose, 28 Mar. 2004: renamed cross_3d() to vnl_cross_3d()
// \endverbatim

#include <vnl/vnl_T_n.h>

//: class vnl_double_3 : a vnl_vector of 3 doubles.
vnl_T_n_impl(double,3);

//: Cross product of two 3-vectors
inline
vnl_double_3 vnl_cross_3d(vnl_double_3 const& v1, vnl_double_3 const& v2)
{
  vnl_double_3 result;
  result[0] = v1[1] * v2[2] - v1[2] * v2[1];
  result[1] = v1[2] * v2[0] - v1[0] * v2[2];
  result[2] = v1[0] * v2[1] - v1[1] * v2[0];
  return result;
}

//: deprecated
#define cross_3d vnl_cross_3d

#endif // vnl_double_3_h_
