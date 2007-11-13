// This is core/vnl/vnl_matrix_exp.h
#ifndef vnl_matrix_exp_h_
#define vnl_matrix_exp_h_
//:
// \file
// \brief Compute the exponential of a square matrix
//
// Compute the exponential of a square matrix, by summing its
// exponential series $\exp(X) = \displaystyle\sum_{n \ge 0} X^n/n!$
// till a convergence requirement is met.
//
// Many improvements are possible.
//
//  \author fsm
//
// \verbatim
//  Modifications:
//   14-Jan-2007 Peter Vanroose - added vnl_matrix_fixed interface
// \endvarbatim

#include <vnl/vnl_matrix.h>

//: Compute the exponential of a square matrix - fiddly form
// \relates vnl_matrix
template <class T>
bool vnl_matrix_exp(vnl_matrix<T> const &X, vnl_matrix<T> &expX, double max_err);

//: Compute the exponential of a square matrix - easy form.
// \relates vnl_matrix
template <class T>
vnl_matrix<T> vnl_matrix_exp(vnl_matrix<T> const &X);

#include <vnl/vnl_matrix_fixed.h>

//: Compute the exponential of a square nxn matrix - easy form.
// \relates vnl_matrix_fixed
template <class T, unsigned int n>
vnl_matrix_fixed<T,n,n> vnl_matrix_exp(vnl_matrix_fixed<T,n,n> const& X);

//: Compute the exponential of a square nxn matrix - fiddly form
// \relates vnl_matrix_fixed
template <class T, unsigned int n>
bool vnl_matrix_exp(vnl_matrix_fixed<T,n,n> const &X, vnl_matrix_fixed<T,n,n> &expX, double max_err);

#endif // vnl_matrix_exp_h_
