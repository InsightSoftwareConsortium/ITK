#ifndef vnl_cross_product_matrix_h_
#define vnl_cross_product_matrix_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME	vnl_cross_product_matrix - 3x3 cross-product matrix of vector
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_cross_product_matrix.h
// .FILE	vnl_cross_product_matrix.cxx
//
// .SECTION Description
//    @{ vnl_cross_product_matrix forms the $3 \times 3$ skew symmetric cross product
//    matrix from a vector.
//
//    \noindent {\tt vnl_cross_product_matrix(e)} is the matrix $[e]_\times$:
//   \[
//   \left( \begin{array}{ccc}
//          0  & -e_3  &  e_2 \\ %
//        e_3  &  0    & -e_1 \\ %
//       -e_2  &  e_1  &  0
//   \end{array} \right)
//   \]@}
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 19 Sep 96
//
// .SECTION Modifications:
//     <none yet>
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_double_3x3.h>

class vnl_cross_product_matrix : public vnl_double_3x3 {
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
