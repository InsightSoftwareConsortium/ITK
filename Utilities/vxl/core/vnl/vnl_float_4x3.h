// This is core/vnl/vnl_float_4x3.h
#ifndef vnl_float_4x3_h_
#define vnl_float_4x3_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief 4x3 matrix of float
//
//    vnl_float_4x3 is a vnl_matrix<float> of fixed size 4x3.
//    It is merely a typedef for vnl_matrix_fixed<float,4,3>
//
//  \author Peter Vanroose
//  \date   1 April 2003
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<float,4,3> vnl_float_4x3;

#endif // vnl_float_4x3_h_
