// This is core/vnl/vnl_double_3x3.h
#ifndef vnl_double_3x3_h_
#define vnl_double_3x3_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief 3x3 matrix of double
//
//  vnl_double_3x3 is a vnl_matrix<double> of fixed size 3x3.
//  It is merely a typedef for vnl_matrix_fixed<double,3,3>
//
//  \author Andrew W. Fitzgibbon, Oxford RRG
//  \date   04 Aug 96
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<double,3,3> vnl_double_3x3;

#endif // vnl_double_3x3_h_
