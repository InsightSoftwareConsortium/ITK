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

#include "vnl_vector_fixed.h"
#include "vnl_double_3x3.h"
#include "vnl/vnl_export.h"

//: Calculates the 3x3 skew symmetric cross product matrix from a vector.
//
// vnl_cross_product_matrix(e) is the matrix [e]_ x:
// \verbatim
//     0    -e_3   e_2
//     e_3   0    -e_1
//    -e_2   e_1   0
// \endverbatim
class vnl_cross_product_matrix : public vnl_double_3x3
{
public:
  typedef vnl_double_3x3 base;

  vnl_cross_product_matrix(const vnl_vector_fixed<double, 3> & v) { set(v.data_block()); }
  vnl_cross_product_matrix(const vnl_vector<double> & v) { set(v.data_block()); }
  vnl_cross_product_matrix(const double * v) { set(v); }
  vnl_cross_product_matrix(const vnl_cross_product_matrix & that) = default;
  ~vnl_cross_product_matrix() = default;

  vnl_cross_product_matrix &
  operator=(const vnl_cross_product_matrix & that) = default;

  //: Construct a vnl_cross_product_matrix from a C-array of 3 doubles.
  //  Overrides a method in vnl_matrix.
  inline vnl_cross_product_matrix &
  set(const double * v)
  {
    const double & e1 = v[0];
    const double & e2 = v[1];
    const double & e3 = v[2];

    vnl_cross_product_matrix & E = *this;

    E(0, 0) = 0;
    E(0, 1) = -e3;
    E(0, 2) = e2;
    E(1, 0) = e3;
    E(1, 1) = 0;
    E(1, 2) = -e1;
    E(2, 0) = -e2;
    E(2, 1) = e1;
    E(2, 2) = 0;

    return *this;
  }
};

#endif // vnl_cross_product_matrix_h_
