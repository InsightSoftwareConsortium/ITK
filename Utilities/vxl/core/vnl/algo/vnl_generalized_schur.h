// This is core/vnl/algo/vnl_generalized_schur.h
#ifndef vnl_generalized_schur_h_
#define vnl_generalized_schur_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief  Solves the generalized eigenproblem det(t A - s B) = 0.
// \author fsm, Oxford RRG
// \date   2 Oct 2001

#include <vnl/vnl_matrix.h>

//:
// For a *real* scalar type T, this function uses orthogonal
// matrices L, R to reduce the (square) matrices A, B to generalized
// (real) Schur form. This means that B is upper triangular and A is
// block upper triangular with blocks of size at most 2x2 such that
// the 2x2 blocks B corresponding to 2x2 blocks of A are diagonal.
// E.g.:
// \verbatim
//                [ * * * * * ]
//                [   * * * * ]
// A <- L^* A R = [   * * * * ]
//                [       * * ]
//                [       * * ]
//
//                [ * * * * * ]
//                [   *   * * ]
// B <- L^* B R = [     * * * ]
//                [       *   ]
//                [         * ]
// \endverbatim
//
// In addition, the function computes the generalized eigenvalues
// (alphar(k) + i alphai(k) : beta(k) for k = 0, 1, 2,...
template <class T>
bool vnl_generalized_schur(vnl_matrix<T> *A,
                           vnl_matrix<T> *B,
                           vnl_vector<T> *alphar,
                           vnl_vector<T> *alphai,
                           vnl_vector<T> *beta,
                           vnl_matrix<T> *L,
                           vnl_matrix<T> *R);

VCL_DEFINE_SPECIALIZATION
bool vnl_generalized_schur(vnl_matrix<double> *A,
                           vnl_matrix<double> *B,
                           vnl_vector<double> *alphar,
                           vnl_vector<double> *alphai,
                           vnl_vector<double> *beta,
                           vnl_matrix<double> *L,
                           vnl_matrix<double> *R);

#include <vcl_algorithm.h>

template <class T>
inline bool vnl_generalized_schur(vnl_matrix<T> *A,
                                  vnl_matrix<T> *B,
                                  vnl_vector<T> *alphar,
                                  vnl_vector<T> *alphai,
                                  vnl_vector<T> *beta,
                                  vnl_matrix<T> *L,
                                  vnl_matrix<T> *R)
{
  vnl_matrix<double> A_(A->rows(), A->cols());
  vnl_matrix<double> B_(B->rows(), B->cols());
  vcl_copy(A->begin(), A->end(), A_.begin());
  vcl_copy(B->begin(), B->end(), B_.begin());

  vnl_vector<double> alphar_;
  vnl_vector<double> alphai_;
  vnl_vector<double> beta_;
  vnl_matrix<double> L_;
  vnl_matrix<double> R_;

  if (! vnl_generalized_schur/*<double>*/(&A_, &B_, &alphar_, &alphai_, &beta_, &L_, &R_))
    return false;

  vcl_copy(A_.begin(), A_.end(), A->begin());
  vcl_copy(B_.begin(), B_.end(), B->begin());

  alphar->set_size(alphar_.size()); vcl_copy(alphar_.begin(), alphar_.end(), alphar->begin());
  alphai->set_size(alphai_.size()); vcl_copy(alphai_.begin(), alphai_.end(), alphai->begin());
  beta  ->set_size(beta_  .size()); vcl_copy(beta_  .begin(), beta_  .end(), beta  ->begin());
  L->set_size(L_.rows(), L_.cols()); vcl_copy(L_.begin(), L_.end(), L->begin());
  R->set_size(R_.rows(), R_.cols()); vcl_copy(R_.begin(), R_.end(), R->begin());

  return true;
}

#endif // vnl_generalized_schur_h_
