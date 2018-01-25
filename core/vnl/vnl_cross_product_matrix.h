// This is core/vnl/vnl_cross_product_matrix.h
#ifndef vnl_cross_product_matrix_h_
#define vnl_cross_product_matrix_h_
//:
// \file
// \brief 3x3 cross-product matrix of vector
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   19 Sep 96
//
// \verbatim
//  Modifications
//   4/4/01 LSB (Manchester) Tidied Documentation
//   27 Jun 2003 - Peter Vanroose - made set() inlined and removed .cxx file.
//   24 Oct 2010 - Peter Vanroose - mutators and setters now return *this
// \endverbatim
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_double_3x3.h>
#include "vnl/vnl_export.h"

//: Calculates the 3x3 skew symmetric cross product matrix from a vector.
//
// vnl_cross_product_matrix(e) is the matrix [e]_ x:
// \verbatim
//     0    -e_3   e_2
//     e_3   0    -e_1
//    -e_2   e_1   0
// \endverbatim
class VNL_TEMPLATE_EXPORT vnl_cross_product_matrix : public vnl_double_3x3
{
 public:
  typedef vnl_double_3x3 base;

  vnl_cross_product_matrix(vnl_vector_fixed<double,3> const& v) { set(v.data_block()); }
  vnl_cross_product_matrix(vnl_vector<double> const& v) { set(v.data_block()); }
  vnl_cross_product_matrix(const double* v) { set(v); }
  vnl_cross_product_matrix(vnl_cross_product_matrix const& that): base(that) {}
 ~vnl_cross_product_matrix() {}

  vnl_cross_product_matrix& operator=(const vnl_cross_product_matrix& that) {
    base::operator= (that);
    return *this;
  }

  //: Construct a vnl_cross_product_matrix from a C-array of 3 doubles.
  //  Overrides a method in vnl_matrix.
  inline vnl_cross_product_matrix& set(const double* v)
  {
    double const& e1 = v[0];
    double const& e2 = v[1];
    double const& e3 = v[2];

    vnl_cross_product_matrix & E = *this;

    E(0,0) =   0; E(0,1) = -e3; E(0,2) =  e2;
    E(1,0) =  e3; E(1,1) =   0; E(1,2) = -e1;
    E(2,0) = -e2; E(2,1) =  e1; E(2,2) =   0;

    return *this;
  }
};

#endif // vnl_cross_product_matrix_h_
