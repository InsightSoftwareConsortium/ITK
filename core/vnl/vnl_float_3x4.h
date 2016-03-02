// This is core/vnl/vnl_float_3x4.h
#ifndef vnl_float_3x4_h_
#define vnl_float_3x4_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief 3x4 matrix of float
//
//    vnl_float_3x4 is a vnl_matrix<float> of fixed size 3x4.
//    It is merely a typedef for vnl_matrix_fixed<float,3,4>
//
//  \author Peter Vanroose
//  \date   1 April 2003
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<float,3,4> vnl_float_3x4;

#endif // vnl_float_3x4_h_
