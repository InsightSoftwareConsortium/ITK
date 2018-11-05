// This is core/vnl/vnl_float_2x2.h
#ifndef vnl_float_2x2_h_
#define vnl_float_2x2_h_
//:
//  \file
//  \brief 2x2 matrix of float
//
//    vnl_float_2x2 is a vnl_matrix<float> of fixed size 2x2.  It is
//    merely a typedef for vnl_matrix_fixed<float,2,2>
//
//  \author Peter Vanroose
//  \date   9 Febr 2003
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<float,2,2> vnl_float_2x2;

#endif // vnl_float_2x2_h_
