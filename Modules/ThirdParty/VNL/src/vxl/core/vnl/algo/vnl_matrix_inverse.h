// This is core/vnl/algo/vnl_matrix_inverse.h
#ifndef vnl_matrix_inverse_h_
#define vnl_matrix_inverse_h_

// ITK deprecation shim: vnl_matrix_inverse is a thin vnl_svd wrapper being
// retired; itk::bridge::Math::SVD (PseudoInverse/Solve, Eigen-backed) is the supported
// replacement. The guard is active only when itkConfigure.h is reachable (an
// ITK consumer), so ITK's own VXL build is unaffected.
#if __has_include(<itkConfigure.h>)
#  include <itkConfigure.h>
#  if defined(ITK_FUTURE_LEGACY_REMOVE) && !defined(ITK_LEGACY_TEST)
#    error "vnl/algo/vnl_matrix_inverse.h is deprecated; migrate to itk::bridge::Math::SVD (itkBridgeMathSVD.h, Eigen-backed)."
#  elif defined(ITK_LEGACY_REMOVE) && !defined(ITK_LEGACY_SILENT) && !defined(ITK_LEGACY_TEST)
#    if defined(_MSC_VER)
#      pragma message("vnl/algo/vnl_matrix_inverse.h is deprecated; migrate to itk::bridge::Math::SVD (itkBridgeMathSVD.h, Eigen-backed).")
#    else
#      warning "vnl/algo/vnl_matrix_inverse.h is deprecated; migrate to itk::bridge::Math::SVD (itkBridgeMathSVD.h, Eigen-backed)."
#    endif
#  endif
#endif
//:
// \file
// \brief Calculates inverse of a matrix (wrapper around vnl_svd<double>)
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   22 Nov 96
//
// \verbatim
//  Modifications
//   dac (Manchester) 28/03/2001: tidied up documentation
// \endverbatim

#include <vnl/algo/vnl_svd.h>
#include <vnl/algo/vnl_algo_export.h>

//: Calculates inverse of a matrix (wrapper around vnl_svd<double>)
//  vnl_matrix_inverse is a wrapper around vnl_svd<double> that allows
//  you to write
//  \code
//  x = vnl_matrix_inverse<double>(A) * b;
//  \endcode
//  This is exactly equivalent to
//  \code
//  x = vnl_svd<double>(A).solve(b);
//  \endcode
//  but is arguably clearer, and also allows for the vnl_matrix_inverse
//  class to be changed to use vnl_qr, say.

template <class T>
struct vnl_matrix_inverse : public vnl_svd<T>
{
  vnl_matrix_inverse(const vnl_matrix<T> & M)
    : vnl_svd<T>(M)
  {}
  ~vnl_matrix_inverse() override = default;

  vnl_matrix<T>
  as_matrix() const
  {
    return this->inverse();
  }

  explicit
  operator vnl_matrix<T>() const
  {
    return this->inverse();
  }
};

template <class T>
inline vnl_vector<T>
operator*(const vnl_matrix_inverse<T> & i, const vnl_vector<T> & B)
{
  return i.solve(B);
}

template <class T>
inline vnl_matrix<T>
operator*(const vnl_matrix_inverse<T> & i, const vnl_matrix<T> & B)
{
  return i.solve(B);
}

#endif // vnl_matrix_inverse_h_
