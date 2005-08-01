// This is core/vnl/vnl_float_2.h
#ifndef vnl_float_2_h_
#define vnl_float_2_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Contains class vnl_float_2
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   05 Aug 96
//
// \verbatim
//  Modifications
//   Peter Vanroose, 25 June 1999: vnl_vector_fixed<float,2> already instantiated
//   Peter Vanroose, 23 Apr. 2004: added vnl_cross_2d()
// \endverbatim

#include <vnl/vnl_T_n.h>

//: class vnl_float_2 : a vnl_vector of 2 floats.
vnl_T_n_impl(float,2);

//: Cross product of two 2-vectors
inline
float vnl_cross_2d(vnl_float_2 const& v1, vnl_float_2 const& v2)
{
  return v1[0] * v2[1] - v1[1] * v2[0];
}

//: deprecated
#define cross_2d vnl_cross_2d

#endif // vnl_float_2_h_
