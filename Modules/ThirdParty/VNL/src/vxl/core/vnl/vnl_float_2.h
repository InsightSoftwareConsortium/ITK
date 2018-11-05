// This is core/vnl/vnl_float_2.h
#ifndef vnl_float_2_h_
#define vnl_float_2_h_
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
//   Peter Vanroose, 24 Mar. 2007: removed deprecated cross_2d() alias
// \endverbatim

#include <vnl/vnl_T_n.h>

//: class vnl_float_2 : a vnl_vector of 2 floats.
vnl_T_n_impl(float,2);

//: Cross product of two 2-vectors
//  \relatesalso vnl_vector_fixed
inline
float vnl_cross_2d(vnl_float_2 const& v1, vnl_float_2 const& v2)
{
  return v1[0] * v2[1] - v1[1] * v2[0];
}

#endif // vnl_float_2_h_
