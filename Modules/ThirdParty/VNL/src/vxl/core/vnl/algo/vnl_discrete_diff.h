#ifndef vnl_discrete_diff_h_
#define vnl_discrete_diff_h_
//:
// \file
// \brief Functions to compute jacobians of vnl_least_squares_functions
//
// Functions to compute jacobians of vnl_least_squares_functions
// by discrete differences.  They return false on failure and
// true on success.
//
// \verbatim
// name size    description
//
// lsf  ---     the function.
// h    1 or n  step size (scalar or a vector).
// x    n       point at which to evaluate the derivative of the function.
// y    m       value of the function at x.
// J    mxn     jacobian of the function at x.
// \endverbatim
//
// \author fsm
//
// \verbatim
//  Modifications
//   dac (Manchester) 28/03/2001: tidied up documentation
//   Peter Vanroose   27/05/2001: Corrected documentation
// \endverbatim

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/algo/vnl_algo_export.h>

class vnl_least_squares_function;

//: forward differences
//  \relatesalso vnl_least_squares_function
bool VNL_ALGO_EXPORT vnl_discrete_diff_fwd(vnl_least_squares_function *lsf,
                           double h,
                           vnl_vector<double> const &x,
                           vnl_matrix<double>       &J);

//: forward differences
//  \relatesalso vnl_least_squares_function
bool VNL_ALGO_EXPORT vnl_discrete_diff_fwd(vnl_least_squares_function *lsf,
                           vnl_vector<double> const &h,
                           vnl_vector<double> const &x,
                           vnl_matrix<double>       &J);

//: forward differences
//  \relatesalso vnl_least_squares_function
bool VNL_ALGO_EXPORT vnl_discrete_diff_fwd(vnl_least_squares_function *lsf,
                           vnl_vector<double> const &h,
                           vnl_vector<double> const &x,
                           vnl_vector<double> const &y,
                           vnl_matrix<double>       &J);

//: symmetric differences
//  \relatesalso vnl_least_squares_function
bool VNL_ALGO_EXPORT vnl_discrete_diff_sym(vnl_least_squares_function *lsf,
                           double h,
                           vnl_vector<double> const &x,
                           vnl_matrix<double>       &J);

//: symmetric differences
//  \relatesalso vnl_least_squares_function
bool VNL_ALGO_EXPORT vnl_discrete_diff_sym(vnl_least_squares_function *lsf,
                           vnl_vector<double> const &h,
                           vnl_vector<double> const &x,
                           vnl_matrix<double>       &J);

void vnl_discrete_diff_test_lsf(vnl_least_squares_function *lsf, vnl_vector<double> const &x);

#endif // vnl_discrete_diff_h_
