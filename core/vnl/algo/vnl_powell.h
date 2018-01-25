// This is core/vnl/algo/vnl_powell.h
#ifndef vnl_powell_h_
#define vnl_powell_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Powell minimizer.
// \author awf@robots.ox.ac.uk
// \date   05 Dec 00
//
// \verbatim
//  Modifications
//   31 Oct 2008 - Hans Johnson - fixed errors caused by uninitialized var bx;
//                 (U. Iowa)      use vnl_brent_minimizer instead of vnl_brent
// \endverbatim

#include <vnl/vnl_cost_function.h>
#include <vnl/vnl_nonlinear_minimizer.h>
#include <vnl/algo/vnl_algo_export.h>

//: The ever-popular Powell minimizer.
// Derivative-free method which may be faster if your
// function is expensive to compute and many-dimensional.
// Implemented from scratch from NR.
class VNL_ALGO_EXPORT vnl_powell : public vnl_nonlinear_minimizer
{
 public:

  //: Initialize a powell with the given cost function
  vnl_powell(vnl_cost_function* functor)
    : functor_(functor), linmin_xtol_(1e-4), initial_step_(1.0) {}

  //: Run minimization, place result in x.
  ReturnCodes minimize(vnl_vector<double>& x);

  //: Set tolerance on line search parameter step
  //  Default value is 0.0001
  void set_linmin_xtol(double tol) { linmin_xtol_ = tol; }

  //: Set initial step when bracketing minima along a line
  //  Default value is 1.0
  void set_initial_step(double step) { initial_step_ = step; }

 protected:
  vnl_cost_function* functor_;

  friend class vnl_powell_1dfun;
  void pub_report_eval(double e) { report_eval(e); }

  //: Tolerance on line search parameter step
  double linmin_xtol_;

  //: Initial step when bracketing minima along a line
  double initial_step_;
};

#endif // vnl_powell_h_
