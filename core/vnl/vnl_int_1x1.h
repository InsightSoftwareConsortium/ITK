// This is core/vnl/vnl_int_1x1.h
#ifndef vnl_int_1x1_h_
#define vnl_int_1x1_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief 1x1 matrix of int
//
//    vnl_int_1x1 is a vnl_matrix<int> of fixed size 1x1.
//    It is merely a typedef for vnl_matrix_fixed<int,1,1>
//
//  \author Peter Vanroose
//  \date   29 June 2003
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<int,1,1> vnl_int_1x1;

#endif // vnl_int_1x1_h_
