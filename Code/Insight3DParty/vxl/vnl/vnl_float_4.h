#ifndef vnl_float_4_h_
#define vnl_float_4_h_
#ifdef __GNUC__
#pragma interface
#endif
// .NAME	vnl_float_4
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_float_4.h
// .FILE	vnl_float_4.cxx
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 05 Aug 96
//
// .SECTION Modifications:
//   Peter Vanroose, 25 June 1999: vnl_vector_fixed<float,4> already instantiated

#include <vnl/vnl_T_n.h>

//: class vnl_float_4 : a vnl_vector of 4 floats.
vnl_T_n_impl(float,4);

#endif // vnl_float_4_h_
