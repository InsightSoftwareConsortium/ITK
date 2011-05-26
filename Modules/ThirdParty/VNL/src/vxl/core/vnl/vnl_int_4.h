// This is core/vnl/vnl_int_4.h
#ifndef vnl_int_4_h_
#define vnl_int_4_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief  alias for vnl_vector_fixed<int,4>
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   31 Aug 96
//
// \verbatim
//  Modifications
//   Peter Vanroose, 21 Oct 1999: vnl_vector_fixed<int,4> already instantiated
// \endverbatim

#include <vnl/vnl_T_n.h>

//: class vnl_int_4 : a vnl_vector of 4 ints.
vnl_T_n_impl(int,4);

#endif // vnl_int_4_h_
