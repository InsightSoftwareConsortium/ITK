// This is core/vnl/vnl_double_4x3.h
#ifndef vnl_double_4x3_h_
#define vnl_double_4x3_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief 4x3 matrix of double
//
//    vnl_double_4x3 is a vnl_matrix<double> of fixed size 4x3.  It is
//    merely a typedef for vnl_matrix_fixed<double,4,3>
//
//  \author Geoff Cross
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<double,4,3> vnl_double_4x3;

#endif // vnl_double_4x3_h_
