// This is vxl/vnl/vnl_unary_function.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//
// UnaryFunction
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 28 Nov 98
//
//-----------------------------------------------------------------------------

#include "vnl_unary_function.h"
#include "vnl_identity.h"
#include <vnl/vnl_vector.h>

template class vnl_unary_function<int,int>;
template class vnl_identity<int>;

template class vnl_unary_function<double, vnl_vector<double> >;
