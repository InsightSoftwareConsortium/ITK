// This is vxl/vnl/vnl_cross_product_matrix.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   19 Sep 96
//-----------------------------------------------------------------------------

#include "vnl_cross_product_matrix.h"

//: Construct a vnl_cross_product_matrix from a vector of 3 doubles.
void vnl_cross_product_matrix::set(const double* v)
{
  double e1 = v[0];
  double e2 = v[1];
  double e3 = v[2];

  vnl_cross_product_matrix & E = *this;

  E(0,0) =   0; E(0,1) = -e3; E(0,2) =  e2;
  E(1,0) =  e3; E(1,1) =   0; E(1,2) = -e1;
  E(2,0) = -e2; E(2,1) =  e1; E(2,2) =   0;
}
