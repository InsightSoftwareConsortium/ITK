#ifndef vnl_double_4x4_h_
#define vnl_double_4x4_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME	vnl_double_4x4 - 4x4 Matrix of double
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_double_4x4.h
// .FILE	vnl_double_4x4.cxx
//
// .SECTION Description
//    vnl_double_4x4 is a vnl_matrix<double> of fixed size 4x4.  It is
//    merely a typedef for vnl_matrix_fixed<double,4,4>
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 04 Aug 96
//
// .SECTION Modifications:
//     <none yet>
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<double,4,4> vnl_double_4x4;

#endif // vnl_double_4x4_h_
