#ifndef vnl_matrix_exp_h_
#define vnl_matrix_exp_h_
#ifdef __GNUC__
#pragma interface
#endif
/*
  fsm@robots.ox.ac.uk
*/

// .NAME	vnl_matrix_exp
// .LIBRARY	vnl
// .HEADER	vxl Package
// .INCLUDE	vnl/vnl_matrix_exp.h
// .FILE	vnl_matrix_exp.txx
// .SECTION Description
// Compute the exponential of a square matrix, by summing its
// exponential series $exp(X) = \sum_{n \ge 0} X^n/n!$ till a
// convergence requirement is met.
//
// Many improvements are possible.

#include <vnl/vnl_matrix.h>

//: fiddly form.
template <class T>
bool vnl_matrix_exp(vnl_matrix<T> const &X, vnl_matrix<T> &expX, double max_err);


//: easy form.
template <class T>
vnl_matrix<T> vnl_matrix_exp(vnl_matrix<T> const &X);

#endif // vnl_matrix_exp_h_
