// This is core/vnl/vnl_double_4x4.h
#ifndef vnl_double_4x4_h_
#define vnl_double_4x4_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief 4x4 matrix of double
//
//    vnl_double_4x4 is a vnl_matrix<double> of fixed size 4x4.  It is
//    merely a typedef for vnl_matrix_fixed<double,4,4>
//
//  \author Andrew W. Fitzgibbon, Oxford RRG
//  \date   04 Aug 96
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<double,4,4> vnl_double_4x4;

#endif // vnl_double_4x4_h_
