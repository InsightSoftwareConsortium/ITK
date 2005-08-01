// This is core/vnl/vnl_float_3.h
#ifndef vnl_float_3_h_
#define vnl_float_3_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Contains class vnl_float_3
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   05 Aug 96
//
// \verbatim
//  Modifications
//   Peter Vanroose, 25 June 1999: vnl_vector_fixed<float,3> already instantiated
//   Peter Vanroose, 23 Apr. 2004: added vnl_cross_3d()
// \endverbatim

#include <vnl/vnl_T_n.h>

//: class vnl_float_3 : a vnl_vector of 3 floats.
vnl_T_n_impl(float,3);

//: Cross product of two 3-vectors
inline
vnl_float_3 vnl_cross_3d(vnl_float_3 const& v1, vnl_float_3 const& v2)
{
  vnl_float_3 result;
  result[0] = v1[1] * v2[2] - v1[2] * v2[1];
  result[1] = v1[2] * v2[0] - v1[0] * v2[2];
  result[2] = v1[0] * v2[1] - v1[1] * v2[0];
  return result;
}

//: deprecated
#define cross_3d vnl_cross_3d

#endif // vnl_float_3_h_
