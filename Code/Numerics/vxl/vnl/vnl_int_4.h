#ifndef vnl_int_4_h_
#define vnl_int_4_h_
#ifdef __GNUC__
#pragma interface
#endif
// .NAME	vnl_int_4
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_int_4.h
// .FILE	vnl_int_4.cxx
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 31 Aug 96
//
// .SECTION Modifications:
//   Peter Vanroose, 21 Oct 1999: vnl_vector_fixed<int,4> already instantiated

#include <vnl/vnl_T_n.h>

//: class vnl_int_4 : a vnl_vector of 4 ints.
vnl_T_n_impl(int,4);

#endif // vnl_int_4_h_
