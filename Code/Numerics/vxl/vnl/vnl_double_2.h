#ifndef vnl_double_2_h_
#define vnl_double_2_h_
#ifdef __GNUC__
#pragma interface
#endif
// .NAME	vnl_double_2
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_double_2.h
// .FILE	vnl_double_2.cxx
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 31 Dec 96
//
// .SECTION Modifications:
//   Peter Vanroose, 25 June 1999: vnl_vector_fixed<double,2> already instantiated

#include <vnl/vnl_T_n.h>

//: class vnl_double_2 : a vnl_vector of 2 doubles.
vnl_T_n_impl(double,2);

// Karen is right: there should not be any inline functions defined
// here. they are provided in vnl_T_n.h in a generic way. fsm

#endif // vnl_double_2_h_
