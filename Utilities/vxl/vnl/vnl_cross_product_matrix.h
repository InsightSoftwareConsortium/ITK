// This is vxl/vnl/vnl_cross_product_matrix.h
#ifndef vnl_cross_product_matrix_h_
#define vnl_cross_product_matrix_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief 3x3 cross-product matrix of vector
//  \author Andrew W. Fitzgibbon, Oxford RRG
//  \date   19 Sep 96
//
// \verbatim
// Modifications:
// 4/4/01 LSB (Manchester) Tidied Documentation
// \endverbatim
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_double_3x3.h>


//:  Calculates the 3x3 skew symmetric cross product matrix from a vector.
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

  vnl_cross_product_matrix(const vnl_vector<double>& v) { set(v.data_block()); }
  vnl_cross_product_matrix(const double* v) { set(v); }
  vnl_cross_product_matrix(const vnl_cross_product_matrix& that): base(that) {}
 ~vnl_cross_product_matrix() {}

  vnl_cross_product_matrix& operator=(const vnl_cross_product_matrix& that) {
    base::operator= (that);
    return *this;
  }

  void set(const double* v); // override method in vnl_vector
};

#endif // vnl_cross_product_matrix_h_
