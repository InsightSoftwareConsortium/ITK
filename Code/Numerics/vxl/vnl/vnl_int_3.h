#ifndef vnl_int_3_h_
#define vnl_int_3_h_
#ifdef __GNUC__
#pragma interface
#endif
// This is vxl/vnl/vnl_int_3.h


//: \file
// \brief
// \author Andrew W. Fitzgibbon, Oxford RRG, 31 Aug 96

// Modifications:
//   Peter Vanroose, 21 Oct 1999: vnl_vector_fixed<int,3> already instantiated
// LSB (Manchester) Tidied documentation

#include <vnl/vnl_T_n.h>

//: class vnl_int_3 : a vnl_vector of 3 ints.
vnl_T_n_impl(int,3);

#endif // vnl_int_3_h_
