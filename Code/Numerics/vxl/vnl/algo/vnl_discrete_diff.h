#ifndef vnl_discrete_diff_h_
#define vnl_discrete_diff_h_

//:
//  \file
//  \brief Functions to compute jacobians of vnl_least_squares_functions
//  \author fsm@robots.ox.ac.uk
//
//  Modifications
//  dac (Manchester) 28/03/2001: tidied up documentation
//


#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
class vnl_least_squares_function;

//: Functions to compute jacobians of vnl_least_squares_functions 
//  by discrete differences.  They return false on failure and 
//  true on success.
//
// name size    description
//
// lsf  --      the function.
// h    1 or n  step size (scalar or a vector).
// x    n       point at which to evaluate the derivative of the function.
// y    m       value of the function at x.
// J    mxn     jacobian of the function at x.


//: forward differences

bool vnl_discrete_diff_fwd(vnl_least_squares_function *lsf,
                           double h,
                           vnl_vector<double> const &x,
                           vnl_matrix<double>       &J);

bool vnl_discrete_diff_fwd(vnl_least_squares_function *lsf,
                           vnl_vector<double> const &h,
                           vnl_vector<double> const &x,
                           vnl_matrix<double>       &J);

bool vnl_discrete_diff_fwd(vnl_least_squares_function *lsf,
                           vnl_vector<double> const &h,
                           vnl_vector<double> const &x,
                           vnl_vector<double> const &y,
                           vnl_matrix<double>       &J);

//: symmetric differences

bool vnl_discrete_diff_sym(vnl_least_squares_function *lsf,
                           double h,
                           vnl_vector<double> const &x,
                           vnl_matrix<double>       &J);

bool vnl_discrete_diff_sym(vnl_least_squares_function *lsf,
                           vnl_vector<double> const &h,
                           vnl_vector<double> const &x,
                           vnl_matrix<double>       &J);

#endif // vnl_discrete_diff_h_
