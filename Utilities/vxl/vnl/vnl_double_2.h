// This is vxl/vnl/vnl_double_2.h
#ifndef vnl_double_2_h_
#define vnl_double_2_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief
//  \author Andrew W. Fitzgibbon, Oxford RRG
//  \date   31 Dec 96
//
// \verbatim
// Modifications:
//   Peter Vanroose, 25 June 1999: vnl_vector_fixed<double,2> already instantiated
// \endverbatim

#include <vnl/vnl_T_n.h>

//: class vnl_double_2 : a vnl_vector of 2 doubles.
vnl_T_n_impl(double,2);

// Karen is right: there should not be any inline functions defined
// here. they are provided in vnl_T_n.h in a generic way. fsm

#endif // vnl_double_2_h_
