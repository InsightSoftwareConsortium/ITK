#ifndef vnl_double_2x2_h_
#define vnl_double_2x2_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME	vnl_double_2x2 - 2x2 Matrix of double
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_double_2x2.h
// .FILE	vnl_double_2x2.cxx
//
// .SECTION Description
//    vnl_double_2x2 is a vnl_matrix<double> of fixed size 2x2.  It is
//    merely a typedef for vnl_matrix_fixed<double,2,2>
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 04 Aug 96
//
// .SECTION Modifications:
//     <none yet>
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<double,2,2> vnl_double_2x2;

#endif // vnl_double_2x2_h_
