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

#include <vnl/vnl_cost_function.h>
#include <vnl/vnl_nonlinear_minimizer.h>

//: The ever-popular Powell minimizer.
// Derivative-free method which may be faster if your
// function is expensive to compute and many-dimensional.
// Implemented from scratch from NR.
class vnl_powell : public vnl_nonlinear_minimizer
{
 public:

  //: Initialize a powell with the given cost function
  vnl_powell(vnl_cost_function* functor);

  //: Run minimization, place result in x.
  ReturnCodes minimize(vnl_vector<double>& x);

  //: Set tolerance on line search parameter step
  //  Default value is 0.0001
  void set_linmin_xtol(double tol);

  //: Set initial step when bracketting minima along a line
  //  Default value is 1.0
  void set_initial_step(double step);

 protected:
  vnl_cost_function* functor_;

  friend class vnl_powell_1dfun;
  void pub_report_eval(double e) {
    report_eval(e);
  }

  //: Tolerance on line search parameter step
  double linmin_xtol_;

  //: Initial step when bracketting minima along a line
  double initial_step_;
};

#endif // vnl_powell_h_
