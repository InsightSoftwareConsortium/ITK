#ifndef vnl_double_4x3_h_
#define vnl_double_4x3_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME	vnl_double_4x3 - 4x3 Matrix of double
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_double_4x3.h
// .FILE	vnl_double_4x3.cxx
//
// .SECTION Description
//    vnl_double_4x3 is a vnl_matrix<double> of fixed size 4x3.  It is
//    merely a typedef for vnl_matrix_fixed<double,4,3>
//
// .SECTION Author
//     Geoff Cross
//
// .SECTION Modifications:
//     <none yet>
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<double,4,3> vnl_double_4x3;

#endif // vnl_double_4x3_h_
