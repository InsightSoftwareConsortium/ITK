#ifndef vnl_double_4x3_h_
#define vnl_double_4x3_h_
#ifdef __GNUC__
#pragma interface
#endif
// This is vxl/vnl/vnl_double_4x3.h

//: \file
//  \brief 4x3 Matrix of double
//  \author Geoff Cross
//    vnl_double_4x3 is a vnl_matrix<double> of fixed size 4x3.  It is
//    merely a typedef for vnl_matrix_fixed<double,4,3>
//

// Modifications:
// 4/4/01 LSB (<Manchester) Tidied documentation
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<double,4,3> vnl_double_4x3;

#endif // vnl_double_4x3_h_
