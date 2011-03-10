// This is core/vnl/vnl_double_4.h
#ifndef vnl_double_4_h_
#define vnl_double_4_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief  alias for vnl_vector_fixed<double,4>
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   05 Aug 96
//
// \verbatim
//  Modifications
//   Peter Vanroose, 25 June 1999: vnl_vector_fixed<double,4> already instantiated
// \endverbatim

#include <vnl/vnl_T_n.h>

//: class vnl_double_4 : a vnl_vector of 4 doubles.
vnl_T_n_impl(double,4);

#endif // vnl_double_4_h_
