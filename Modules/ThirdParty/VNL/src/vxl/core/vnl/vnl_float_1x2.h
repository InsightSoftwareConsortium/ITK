// This is core/vnl/vnl_float_1x2.h
#ifndef vnl_float_1x2_h_
#define vnl_float_1x2_h_
//:
//  \file
//  \brief 1x2 matrix of float
//
//    vnl_float_1x2 is a vnl_matrix<float> of fixed size 1x2.
//    It is merely a typedef for vnl_matrix_fixed<float,1,2>
//
//  \author Peter Vanroose
//  \date   29 June 2003
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<float,1,2> vnl_float_1x2;

#endif // vnl_float_1x2_h_
