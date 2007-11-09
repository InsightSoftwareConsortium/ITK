// This is core/vnl/vnl_float_2x1.h
#ifndef vnl_float_2x1_h_
#define vnl_float_2x1_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief 2x1 matrix of float
//
//    vnl_float_2x1 is a vnl_matrix<float> of fixed size 2x1.
//    It is merely a typedef for vnl_matrix_fixed<float,2,1>
//
//  \author Peter Vanroose
//  \date   29 June 2003
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<float,2,1> vnl_float_2x1;

#endif // vnl_float_2x1_h_
