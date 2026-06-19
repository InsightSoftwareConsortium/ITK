// This is core/vnl/algo/vnl_scatter_3x3.h
#ifndef vnl_scatter_3x3_h_
#define vnl_scatter_3x3_h_

// ITK deprecation shim: vnl_scatter_3x3 is the only non-deprecated client of the
// netlib EISPACK symmetric eigensolver; it is deprecated alongside the
// vnl_*_eigensystem classes so the dead engine can be retired. Build a 3x3
// scatter matrix directly and use itk::SymmetricEigenDecomposition for its
// eigensystem. The guard is active only when itkConfigure.h is reachable (an ITK
// consumer), so ITK's own VXL build is unaffected.
#if __has_include(<itkConfigure.h>)
#  include <itkConfigure.h>
#  if defined(ITK_FUTURE_LEGACY_REMOVE)
#    error "vnl/algo/vnl_scatter_3x3.h is deprecated; build a 3x3 scatter matrix directly and use itk::SymmetricEigenDecomposition (itkSymmetricEigenDecomposition.h, Eigen-backed) for its eigensystem."
#  elif defined(ITK_LEGACY_REMOVE) && !defined(ITK_LEGACY_SILENT) && !defined(ITK_LEGACY_TEST)
#    if defined(_MSC_VER)
#      pragma message("vnl/algo/vnl_scatter_3x3.h is deprecated; use itk::SymmetricEigenDecomposition (itkSymmetricEigenDecomposition.h) for the eigensystem.")
#    else
#      warning "vnl/algo/vnl_scatter_3x3.h is deprecated; use itk::SymmetricEigenDecomposition (itkSymmetricEigenDecomposition.h) for the eigensystem."
#    endif
#  endif
#endif
//:
// \file
// \brief  3x3 scatter matrix
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   02 Oct 96
//
// \verbatim
//  Modifications
//   18 Feb 2000. fsm: templated.
//   4/4/01 LSB (Manchester) documentation tidied
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
//   13 Jan.2003 - Peter Vanroose - added missing implem. for sub_outer_product
// \endverbatim
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/algo/vnl_algo_export.h>

template <class T>
class vnl_scatter_3x3 : public vnl_matrix_fixed<T, 3, 3>
{
public:
  using base = vnl_matrix_fixed<T, 3, 3>;
  using vect = vnl_vector_fixed<T, 3>;

  //: Constructor.  Fills with zeros.
  vnl_scatter_3x3();

  //: Add v*v' to scatter.
  void
  add_outer_product(const vnl_vector_fixed<T, 3> & v);

  //: Add v*u' to scatter.
  void
  add_outer_product(const vnl_vector_fixed<T, 3> & u, const vnl_vector_fixed<T, 3> & v);

  //: Subtract v*v' from scatter.
  void
  sub_outer_product(const vnl_vector_fixed<T, 3> & v);

  //: Subtract v*u' from scatter.
  void
  sub_outer_product(const vnl_vector_fixed<T, 3> & u, const vnl_vector_fixed<T, 3> & v);

  //: Replace S with $(S+S^\top)/2$.
  void
  force_symmetric();

  //: Compute the eigensystem of S.
  void
  compute_eigensystem();

  //: Return the eigenvector corresponding to the smallest eigenvalue.
  vnl_vector_fixed<T, 3>
  minimum_eigenvector()
  {
    if (!eigenvectors_currentp)
      compute_eigensystem();
    return vnl_vector_fixed<T, 3>(V_(0, 0), V_(1, 0), V_(2, 0));
  }

  //: Return the column matrix of eigenvectors, sorted in increasing order of eigenvalue.
  vnl_matrix_fixed<T, 3, 3> &
  V()
  {
    if (!eigenvectors_currentp)
      compute_eigensystem();
    return V_;
  }

protected:
  bool symmetricp{ true };
  bool eigenvectors_currentp{ false };
  vnl_matrix_fixed<T, 3, 3> V_;
  vnl_vector_fixed<T, 3> D;
};


#endif // vnl_scatter_3x3_h_
