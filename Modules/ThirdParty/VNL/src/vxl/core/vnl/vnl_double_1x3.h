// This is core/vnl/vnl_double_1x3.h
#ifndef vnl_double_1x3_h_
#define vnl_double_1x3_h_
//:
//  \file
//  \brief 1x3 matrix of double
//
//    vnl_double_1x3 is a vnl_matrix<double> of fixed size 1x3.
//    It is merely a typedef for vnl_matrix_fixed<double,1,3>
//
//  \author Peter Vanroose
//  \date   1 April 2003
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<double,1,3> vnl_double_1x3;

#endif // vnl_double_1x3_h_
