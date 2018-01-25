// This is core/vnl/algo/vnl_matrix_inverse.h
#ifndef vnl_matrix_inverse_h_
#define vnl_matrix_inverse_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
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
  vnl_matrix_inverse(vnl_matrix<T> const & M): vnl_svd<T>(M) { }
  ~vnl_matrix_inverse() {}

  operator vnl_matrix<T> () const { return this->inverse(); }
};

template <class T>
inline
vnl_vector<T> operator*(vnl_matrix_inverse<T> const & i,
                        vnl_vector<T> const & B)
{
  return i.solve(B);
}

template <class T>
inline
vnl_matrix<T> operator*(vnl_matrix_inverse<T> const & i,
                        vnl_matrix<T> const & B)
{
  return i.solve(B);
}

#endif // vnl_matrix_inverse_h_
