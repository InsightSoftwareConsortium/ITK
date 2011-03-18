// This is core/vnl/vnl_double_1x2.h
#ifndef vnl_double_1x2_h_
#define vnl_double_1x2_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief 1x2 matrix of double
//
//    vnl_double_1x2 is a vnl_matrix<double> of fixed size 1x2.
//    It is merely a typedef for vnl_matrix_fixed<double,1,2>
//
//  \author Peter Vanroose
//  \date   29 June 2003
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<double,1,2> vnl_double_1x2;

#endif // vnl_double_1x2_h_
