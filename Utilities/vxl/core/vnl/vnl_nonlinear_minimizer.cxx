// This is core/vnl/vnl_nonlinear_minimizer.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG, 22 Aug 99

#include "vnl_nonlinear_minimizer.h"
#include <vcl_iostream.h>
#include <vcl_iomanip.h>

//: Default ctor sets verbosity etc.
vnl_nonlinear_minimizer::vnl_nonlinear_minimizer()
{
  xtol = 1e-8;           // Termination tolerance on X (solution vector)
  maxfev = 2000; // Termination maximum number of iterations.
  ftol = xtol * 0.01;    // Termination tolerance on F (sum of squared residuals)
  gtol = 1e-5;           // Termination tolerance on Grad(F)' * F = 0
  epsfcn = xtol * 0.001; // Step length for FD Jacobian
  trace = false;
  verbose_ = false;
  check_derivatives_=0;
  failure_code_ = ERROR_FAILURE;
  reset();
}

vnl_nonlinear_minimizer::~vnl_nonlinear_minimizer()
{
}

vnl_matrix<double> const& vnl_nonlinear_minimizer::get_covariance()
{
  static vnl_matrix<double> null;
  return null;
}

void vnl_nonlinear_minimizer::reset()
{
  num_iterations_ = 0;
  num_evaluations_ = 0;
  start_error_ = 0;
}

//: Called by derived classes after each function evaluation.
void vnl_nonlinear_minimizer::report_eval(double f)
{
  if (num_evaluations_ == 0) {
    start_error_ = f;
    end_error_ = f;
  }
  if (f < end_error_)
    end_error_ = f;

  ++num_evaluations_;
}

//: Called by derived classes after each iteration
void vnl_nonlinear_minimizer::report_iter()
{
  ++num_iterations_;
  if (verbose_)
    vcl_cerr << "Iter " << vcl_setw(4) << num_iterations_ << ", Eval "
             << vcl_setw(4) << num_evaluations_ << ": Best F = "
             << vcl_setw(10) << end_error_ << '\n';
}

//: Return the name of the class
//  Used by polymorphic IO
vcl_string vnl_nonlinear_minimizer::is_a() const
{
  static const vcl_string class_name_="vnl_nonlinear_minimizer";
  return class_name_;
}

//: Return true if the name of the class matches the argument
//  Used by polymorphic IO
bool vnl_nonlinear_minimizer::is_class(vcl_string const& s) const
{
  return s==vnl_nonlinear_minimizer::is_a();
}

