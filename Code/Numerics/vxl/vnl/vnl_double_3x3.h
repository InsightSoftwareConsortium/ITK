#ifndef vnl_double_3x3_h_
#define vnl_double_3x3_h_
#ifdef __GNUC__
#pragma interface
#endif
// This is vxl/vnl/vnl_double_3x3.h

//: \file
//  \brief vnl_double_3x3 is a vnl_matrix<double> of fixed size 3x3.  It is merely a typedef for vnl_matrix_fixed<double,3,3>
//  \author Andrew W. Fitzgibbon, Oxford RRG, 04 Aug 96
//
//  Modifications:
//  LSB (Manchester) 26/3/01 Tidied documentation
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<double,3,3> vnl_double_3x3;

#endif // vnl_double_3x3_h_
