//-*- c++ -*-------------------------------------------------------------------
#ifndef vnl_powell_h_
#define vnl_powell_h_
#ifdef __GNUC__
#pragma interface
#endif
// Author: awf@robots.ox.ac.uk
// Created: 05 Dec 00

#include <vnl/vnl_cost_function.h>
#include <vnl/vnl_nonlinear_minimizer.h>

//: The ever-popular Powell minimizer.
// Derivative-free method which may be faster if your
// function is expensive to compute and many-dimensional.
// Implemented from scratch from NR.
class vnl_powell : public vnl_nonlinear_minimizer {
public:

  //: Initialize a powell with the given cost function
  vnl_powell(vnl_cost_function* functor);

  //: Run minimization, place result in x.
  ReturnCodes minimize(vnl_vector<double>& x);

protected:
  vnl_cost_function* functor_;

  friend class vnl_powell_1dfun;
  void pub_report_eval(double e) {
    report_eval(e);
  }
};

#endif // vnl_powell_h_
