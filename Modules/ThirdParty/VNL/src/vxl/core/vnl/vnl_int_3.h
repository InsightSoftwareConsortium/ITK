// This is core/vnl/vnl_int_3.h
#ifndef vnl_int_3_h_
#define vnl_int_3_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief  alias for vnl_vector_fixed<int,3>
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   31 Aug 96
//
// \verbatim
//  Modifications
//   Peter Vanroose, 21 Oct 1999: vnl_vector_fixed<int,3> already instantiated
// \endverbatim

#include <vnl/vnl_T_n.h>

//: class vnl_int_3 : a vnl_vector of 3 ints.
vnl_T_n_impl(int,3);

#endif // vnl_int_3_h_
