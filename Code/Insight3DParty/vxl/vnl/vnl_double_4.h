#ifndef vnl_double_4_h_
#define vnl_double_4_h_
#ifdef __GNUC__
#pragma interface
#endif
// .NAME	vnl_double_4
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_double_4.h
// .FILE	vnl_double_4.cxx
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 05 Aug 96
//
// .SECTION Modifications:
//   Peter Vanroose, 25 June 1999: vnl_vector_fixed<double,4> already instantiated

#include <vnl/vnl_T_n.h>

//: class vnl_double_4 : a vnl_vector of 4 doubles.
vnl_T_n_impl(double,4);

#endif // vnl_double_4_h_
