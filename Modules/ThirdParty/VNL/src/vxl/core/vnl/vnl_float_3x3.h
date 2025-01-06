// This is core/vnl/vnl_float_3x3.h
#ifndef vnl_float_3x3_h_
#define vnl_float_3x3_h_
//:
//  \file
//  \brief 3x3 matrix of float
//
//    vnl_float_3x3 is a vnl_matrix<float> of fixed size 3x3.
//    It is merely a typedef for vnl_matrix_fixed<float,3,3>
//
//  \author Peter Vanroose
//  \date   1 April 2003
//
//-----------------------------------------------------------------------------

#include "vnl_matrix_fixed.h"

typedef vnl_matrix_fixed<float,3,3> vnl_float_3x3;

#endif // vnl_float_3x3_h_
