// This is core/vnl/vnl_matrix_exp.h
#ifndef vnl_matrix_exp_h_
#define vnl_matrix_exp_h_

#include "vnl/vnl_export.h"

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
// \endverbatim

//: Compute the exponential of a square matrix - fiddly form
// \relatesalso vnl_matrix
// \relatesalso vnl_matrix_fixed
template <class SquareMatrix> VNL_TEMPLATE_EXPORT
bool vnl_matrix_exp(SquareMatrix const &X, SquareMatrix &expX, double max_err);

//: Compute the exponential of a square matrix - easy form.
// \relatesalso vnl_matrix
// \relatesalso vnl_matrix_fixed
template <class SquareMatrix> VNL_TEMPLATE_EXPORT
SquareMatrix vnl_matrix_exp(SquareMatrix const &X);


#endif // vnl_matrix_exp_h_
