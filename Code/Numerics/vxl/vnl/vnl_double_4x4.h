#ifndef vnl_double_4x4_h_
#define vnl_double_4x4_h_
#ifdef __GNUC__
#pragma interface
#endif
// This is vxl/vnl/vnl_double_4x4.h

//: \file
//  \brief
//  \author Andrew W. Fitzgibbon, Oxford RRG, 04 Aug 96
//    vnl_double_4x4 is a vnl_matrix<double> of fixed size 4x4.  It is
//    merely a typedef for vnl_matrix_fixed<double,4,4>

// Modifications:
// 4/4/01 LSB (Manchester) Tidied documentation
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<double,4,4> vnl_double_4x4;

#endif // vnl_double_4x4_h_
