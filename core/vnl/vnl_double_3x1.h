// This is core/vnl/vnl_double_3x1.h
#ifndef vnl_double_3x1_h_
#define vnl_double_3x1_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief 3x1 matrix of double
//
//    vnl_double_3x1 is a vnl_matrix<double> of fixed size 3x1.
//    It is merely a typedef for vnl_matrix_fixed<double,3,1>
//
//  \author Peter Vanroose
//  \date   1 April 2003
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<double,3,1> vnl_double_3x1;

#endif // vnl_double_3x1_h_
