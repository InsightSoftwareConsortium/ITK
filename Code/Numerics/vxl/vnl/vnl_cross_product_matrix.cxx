// This is vxl/vnl/vnl_cross_product_matrix.cxx

//-*- c++ -*-------------------------------------------------------------------
#ifdef __GNUC__
#pragma implementation
#endif
//
// vnl_cross_product_matrix
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 19 Sep 96
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_vector.h>
#include "vnl_cross_product_matrix.h"

//: Construct a vnl_cross_product_matrix from a vector of 3 doubles.
void vnl_cross_product_matrix::set(const double* v) 
{
  double e1 = v[0];
  double e2 = v[1];
  double e3 = v[2];
  
  vnl_matrix<double> & E = *this; //win32 doesn't like E(*this);
  
  E(0,0) =   0; E(0,1) = -e3; E(0,2) =  e2;
  E(1,0) =  e3; E(1,1) =   0; E(1,2) = -e1;
  E(2,0) = -e2; E(2,1) =  e1; E(2,2) =   0;
}
