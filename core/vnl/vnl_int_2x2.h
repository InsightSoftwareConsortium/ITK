// This is core/vnl/vnl_int_2x2.h
#ifndef vnl_int_2x2_h_
#define vnl_int_2x2_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief 2x2 matrix of int
//
//    vnl_int_2x2 is a vnl_matrix<int> of fixed size 2x2.  It is
//    merely a typedef for vnl_matrix_fixed<int,2,2>
//
//  \author Peter Vanroose
//  \date   24 Febr 2003
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<int,2,2> vnl_int_2x2;

#endif // vnl_int_2x2_h_
