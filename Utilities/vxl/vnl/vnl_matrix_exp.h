// This is vxl/vnl/vnl_matrix_exp.h
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

#include <vnl/vnl_matrix.h>

//: Compute the exponential of a square matrix - fiddly form
// \relates vnl_matrix
template <class T>
bool vnl_matrix_exp(vnl_matrix<T> const &X, vnl_matrix<T> &expX, double max_err);

//: Compute the exponential of a square matrix - easy form.
// \relates vnl_matrix
template <class T>
vnl_matrix<T> vnl_matrix_exp(vnl_matrix<T> const &X);

#endif // vnl_matrix_exp_h_
