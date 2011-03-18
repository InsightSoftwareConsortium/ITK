// This is core/vnl/vnl_double_2x2.h
#ifndef vnl_double_2x2_h_
#define vnl_double_2x2_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief 2x2 matrix of double
//
//    vnl_double_2x2 is a vnl_matrix<double> of fixed size 2x2.  It is
//    merely a typedef for vnl_matrix_fixed<double,2,2>
//
//  \author Andrew W. Fitzgibbon, Oxford RRG
//  \date   04 Aug 96
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<double,2,2> vnl_double_2x2;

#endif // vnl_double_2x2_h_
