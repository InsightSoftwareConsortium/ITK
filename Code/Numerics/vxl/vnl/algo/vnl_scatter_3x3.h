#ifndef vnl_scatter_3x3_h_
#define vnl_scatter_3x3_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME	vnl_scatter_3x3
// .LIBRARY	vnl-algo
// .HEADER	vxl package
// .INCLUDE	vnl/algo/vnl_scatter_3x3.h
// .FILE	vnl_scatter_3x3.txx
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 02 Oct 96
//
// .SECTION Modifications:
//     18 Feb 2000. fsm: templated.
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_vector_fixed.h>

template <class T>
class vnl_scatter_3x3 : public vnl_matrix_fixed<T,3,3> {
public:
  typedef vnl_matrix_fixed<T,3,3> base;
  typedef vnl_vector_fixed<T,3> vect;

  // -- Constructor.  Fills with zeros.
  vnl_scatter_3x3();

  // -- Add v*v' to scatter.
  void add_outer_product(const vnl_vector_fixed<T,3> & v);

  // -- Add v*u' to scatter.
  void add_outer_product(const vnl_vector_fixed<T,3> & u, const vnl_vector_fixed<T,3> & v);

  // -- Subtract v*v' from scatter.
  void sub_outer_product(const vnl_vector_fixed<T,3> & v);

  // -- Subtract v*u' from scatter.
  void sub_outer_product(const vnl_vector_fixed<T,3> & u, const vnl_vector_fixed<T,3> & v);

  // -- Replace S with $(S+S^\top)/2$.
  void force_symmetric();

  // -- Compute the eigensystem of S.
  void compute_eigensystem();

  // -- Return the eigenvector corresponding to the smallest eigenvalue.
  vnl_vector_fixed<T,3> minimum_eigenvector() {
    if (!eigenvectors_currentp) compute_eigensystem();
    return vnl_vector_fixed<T,3>(V_(0,0), V_(1,0), V_(2,0));
  }

  // -- Return the column matrix of eigenvectors, sorted in increasing order
  // of eigenvalue.
  vnl_matrix_fixed<T,3,3>& V() {
    if (!eigenvectors_currentp) compute_eigensystem();
    return V_;
  }

protected:
  bool symmetricp;
  bool eigenvectors_currentp;
  vnl_matrix_fixed<T,3,3> V_;
  vnl_vector_fixed<T,3> D;
};


#endif // vnl_scatter_3x3_h_
