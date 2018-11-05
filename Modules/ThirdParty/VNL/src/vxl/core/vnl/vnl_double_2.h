// This is core/vnl/vnl_double_2.h
#ifndef vnl_double_2_h_
#define vnl_double_2_h_
//:
//  \file
//  \brief  alias for vnl_vector_fixed<double,2>
//  \author Andrew W. Fitzgibbon, Oxford RRG
//  \date   31 Dec 96
//
// \verbatim
//  Modifications
//   Peter Vanroose, 25 June 1999: vnl_vector_fixed<double,2> already instantiated
//   Peter Vanroose, 28 Mar. 2004: renamed cross_2d() to vnl_cross_2d()
//   Peter Vanroose, 24 Mar. 2007: removed deprecated cross_2d() alias
// \endverbatim

#include <vnl/vnl_T_n.h>

//: class vnl_double_2 : a vnl_vector of 2 doubles.
vnl_T_n_impl(double,2);

//: Cross product of two 2-vectors
//  \relatesalso vnl_vector_fixed
inline
double vnl_cross_2d(vnl_double_2 const& v1, vnl_double_2 const& v2)
{
  return v1[0] * v2[1] - v1[1] * v2[0];
}

#endif // vnl_double_2_h_
