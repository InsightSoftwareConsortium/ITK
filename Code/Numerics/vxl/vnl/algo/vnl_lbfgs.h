#ifndef vnl_lbfgs_h_
#define vnl_lbfgs_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME	vnl_lbfgs - Limited memory Broyden Fletcher Goldfarb Shannon minimization
// .LIBRARY	vnl-algo
// .HEADER	vxl Package
// .INCLUDE	vnl/algo/vnl_lbfgs.h
// .FILE	vnl_lbfgs.cxx
//
// .SECTION Description
//    vnl_lbfgs is a class that awf hasn't documented properly. FIXME
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 22 Aug 99
//
// .SECTION Modifications
//     990822 AWF Initial version.
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_cost_function.h>
#include <vnl/vnl_nonlinear_minimizer.h>

class vnl_lbfgs : public vnl_nonlinear_minimizer {
public:
  vnl_lbfgs();
  vnl_lbfgs(vnl_cost_function& f);

  bool minimize(vnl_vector<double>& x);

  // -- Number of direction vectors to keep.
  // Sensible values are from about 5 onwards.
  int memory;

  // -- Accuracy of line search.
  // If function evaluations are cheap wrt the actual minimization steps,
  // change to 0.1, from default of 0.9;
  double line_search_accuracy;

  // -- Default step length in line search.
  // If, on tracing, the STP is always 1, then you could try setting this to a
  // higher value to see how far along the gradient the minimum typically is.
  // Then set this to a number just below that to get maximally far with the
  // single evaluation.
  double default_step_length;

private:
  void init_parameters();
  vnl_cost_function* f_;
  //  vnl_lbfgs() {} // default constructor makes no sense
  // does too.  Can set values for parameters.
};

#endif // vnl_lbfgs_h_
