// This is core/vnl/algo/vnl_complex_generalized_schur.h
#ifndef vnl_complex_generalized_schur_h_
#define vnl_complex_generalized_schur_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief  Solves the generalized eigenproblem det(t A - s B) = 0.
// \author Peter Vanroose, ABIS Leuven
// \date   9 Jan 2011
// Adapted from vnl_generalized_schur.h/.cxx

#include <complex>
#include <algorithm>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_vector.h>
#include <vnl/algo/vnl_algo_export.h>


//:
// For a scalar type T, this function uses orthogonal matrices L, R
// over complex<T> to reduce the (square) matrices A, B to generalized
// (complex) Schur form. This means that A and B become upper triangular,
// A <-- L^* A R, and B <-- L^* B R.
// Of course, A and B should be of the same size.
//
// In addition, the function computes the (complex) generalized eigenvalues
// alpha(k) : beta(k) for k = 0, 1, 2,...
//
// To pass in scalar type T matrices A and B, you'll have to first convert them
// to complex matrices since they will be overwritten by they (complex) upper
// triangular decomposition.
template <class T>
bool vnl_generalized_schur(vnl_matrix<std::complex<T> > *A,
                           vnl_matrix<std::complex<T> > *B,
                           vnl_vector<std::complex<T> > *alpha,
                           vnl_vector<std::complex<T> > *beta,
                           vnl_matrix<std::complex<T> > *L,
                           vnl_matrix<std::complex<T> > *R);

template <>
VNL_ALGO_EXPORT bool vnl_generalized_schur(vnl_matrix<std::complex<double> > *A,
                           vnl_matrix<std::complex<double> > *B,
                           vnl_vector<std::complex<double> > *alpha,
                           vnl_vector<std::complex<double> > *beta,
                           vnl_matrix<std::complex<double> > *L,
                           vnl_matrix<std::complex<double> > *R);

#include <vcl_compiler.h>

template <class T>
std::complex<T> vnl_complex_generalized_schur_convert_cast(std::complex<double> a) { return static_cast<std::complex<T> >(a); }

template <class T>
inline bool vnl_generalized_schur(vnl_matrix<std::complex<T> > *A,
                                  vnl_matrix<std::complex<T> > *B,
                                  vnl_vector<std::complex<T> > *alpha,
                                  vnl_vector<std::complex<T> > *beta,
                                  vnl_matrix<std::complex<T> > *L,
                                  vnl_matrix<std::complex<T> > *R)
{
  vnl_matrix<std::complex<double> > A_(A->rows(), A->cols());
  vnl_matrix<std::complex<double> > B_(B->rows(), B->cols());
  std::copy(A->begin(), A->end(), A_.begin());
  std::copy(B->begin(), B->end(), B_.begin());

  vnl_vector<std::complex<double> > alpha_;
  vnl_vector<std::complex<double> > beta_;
  vnl_matrix<std::complex<double> > L_;
  vnl_matrix<std::complex<double> > R_;

  if (! vnl_generalized_schur/*<std::complex<double> >*/(&A_, &B_, &alpha_, &beta_, &L_, &R_))
    return false;

  std::transform(A_.begin(), A_.end(), A->begin(), vnl_complex_generalized_schur_convert_cast<T>);
  std::transform(B_.begin(), B_.end(), B->begin(), vnl_complex_generalized_schur_convert_cast<T>);

  alpha->set_size(alpha_.size());
  std::transform(alpha_.begin(), alpha_.end(), alpha->begin(), vnl_complex_generalized_schur_convert_cast<T>);

  beta->set_size(beta_.size());
  std::transform(beta_.begin(), beta_.end(), beta->begin(), vnl_complex_generalized_schur_convert_cast<T>);

  L->set_size(L_.rows(), L_.cols());
  std::transform(L_.begin(), L_.end(), L->begin(), vnl_complex_generalized_schur_convert_cast<T>);

  R->set_size(R_.rows(), R_.cols());
  std::transform(R_.begin(), R_.end(), R->begin(), vnl_complex_generalized_schur_convert_cast<T>);

  return true;
}

#endif // vnl_complex_generalized_schur_h_
