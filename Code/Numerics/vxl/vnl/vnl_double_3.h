#ifndef vnl_double_3_h_
#define vnl_double_3_h_
#ifdef __GNUC__
#pragma interface
#endif
// .NAME	vnl_double_3
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_double_3.h
// .FILE	vnl_double_3.cxx
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 31 Dec 96
//
// .SECTION Modifications:
//   Peter Vanroose, 25 June 1999: vnl_vector_fixed<double,3> already instantiated

#include <vnl/vnl_T_n.h>

//: class vnl_double_3 : a vnl_vector of 3 doubles.
vnl_T_n_impl(double,3);

#endif // vnl_double_3_h_
