// This is core/vnl/vnl_double_1x1.h
#ifndef vnl_double_1x1_h_
#define vnl_double_1x1_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief 1x1 matrix of double
//
//    vnl_double_1x1 is a vnl_matrix<double> of fixed size 1x1.
//    It is merely a typedef for vnl_matrix_fixed<double,1,1>
//
//  \author Peter Vanroose
//  \date   29 June 2003
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<double,1,1> vnl_double_1x1;

#endif // vnl_double_1x1_h_
