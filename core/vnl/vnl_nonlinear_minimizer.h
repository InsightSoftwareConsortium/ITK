// This is core/vnl/vnl_nonlinear_minimizer.h
#ifndef vnl_nonlinear_minimizer_h_
#define vnl_nonlinear_minimizer_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief  Base class for nonlinear optimization
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   22 Aug 1999
//
// \verbatim
//  Modifications
//   22 Mar.2001 - dac - added binary io and tidied documentation
//      Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
// \endverbatim

#include <string>
#include <vcl_compiler.h>
#include <vnl/vnl_matrix.h>
#include "vnl/vnl_export.h"

//: vnl_nonlinear_minimizer is a base class for nonlinear optimization.
// It defines a few common abilities such as get_num_evaluations.
// Known derived classes are:
// -  vnl_levenberg_marquardt
// -  vnl_lbfgs
// -  vnl_conjugate_gradient
// -  vnl_brent
// -  vnl_powell
class VNL_EXPORT vnl_nonlinear_minimizer
{
 public:
  vnl_nonlinear_minimizer();

  virtual ~vnl_nonlinear_minimizer();


  //: Set the convergence tolerance on F (sum of squared residuals).
  // When the differences in successive RMS errors is less than this, the
  // routine terminates.  So this is effectively the desired precision of your
  // minimization.  Setting it too low wastes time, too high might cause early
  // convergence.  The default of 1e-9 is on the safe side, but if speed is an
  // issue, you can try raising it.
  void set_f_tolerance(double v) { ftol = v; }
  double get_f_tolerance() const { return ftol; }

  //: Set the convergence tolerance on X.
  //  When the length of the steps taken in X are about this long, the routine
  // terminates.  The default is 1e-8, which should work for many problems,
  // but if you can get away with 1e-4, say, minimizations will be much quicker.
  void set_x_tolerance(double v) {
    xtol = v;
    epsfcn = xtol * 0.001;
  }
  double get_x_tolerance() const { return xtol; }

  //: Set the convergence tolerance on Grad(F)' * F.
  void set_g_tolerance(double v) { gtol = v; }
  double get_g_tolerance() const { return gtol; }

  //: Set the termination maximum number of iterations.
  void set_max_function_evals(int v) { maxfev = v; }
  int get_max_function_evals() const { return maxfev; }

  //: Set the step length for FD Jacobian.
  // Be aware that set_x_tolerance will reset this to xtol * 0.001.
  // The default is 1e-11.
  void set_epsilon_function(double v) { epsfcn = v; }
  double get_epsilon_function() const { return epsfcn; }

  //: Turn on per-iteration printouts.
  void set_trace(bool on) { trace = on; }
  bool get_trace() const { return trace; }

  //: Set verbose flag
  void set_verbose(bool verb) { verbose_ = verb; }
  bool get_verbose() const { return verbose_; }

  //: Set check_derivatives flag.  Negative values may mean fewer checks.
  void set_check_derivatives(int cd) { check_derivatives_ = cd; }
  int get_check_derivatives() const { return check_derivatives_; }

  //: Return the error of the function when it was evaluated at the start point of the last minimization.
  // For minimizers driven by a vnl_least_squares_function (Levenberg-Marquardt)
  // this is usually the RMS error.
  // For those driven by a vnl_cost_function (CG, LBFGS, Amoeba) it is simply the
  // value of the vnl_cost_function at the start (usually the sum of squared residuals).
  double get_start_error() const { return start_error_; }

  //:Return the best error that was achieved by the last minimization, corresponding to the returned x.
  double get_end_error() const { return end_error_; }

  //:Return the total number of times the function was evaluated by the last minimization.
  int get_num_evaluations() const { return num_evaluations_; }

  //:Return the number of {\em iterations} in the last minimization.
  // Each iteration may have comprised several function evaluations.
  int get_num_iterations() const { return num_iterations_; }

  //:Some generic return codes that apply to all minimizers.
  enum ReturnCodes {
    ERROR_FAILURE               =-1,
    ERROR_DODGY_INPUT           = 0,
    CONVERGED_FTOL              = 1,
    CONVERGED_XTOL              = 2,
    CONVERGED_XFTOL             = 3,
    CONVERGED_GTOL              = 4,
    FAILED_TOO_MANY_ITERATIONS  = 5,
    TOO_MANY_ITERATIONS         = FAILED_TOO_MANY_ITERATIONS,         // for backward-compatibility
    FAILED_FTOL_TOO_SMALL       = 6,
    FAILED_XTOL_TOO_SMALL       = 7,
    FAILED_GTOL_TOO_SMALL       = 8,
    FAILED_USER_REQUEST         = 9
  };

  //:Whether the error reduced in the last minimization
  bool obj_value_reduced() { return failure_code_ != ERROR_FAILURE && failure_code_ != ERROR_DODGY_INPUT && end_error_ < start_error_; }

  //:Return the covariance of the estimate at the end.
  virtual vnl_matrix<double> const& get_covariance();

  //: Return the name of the class.
  //  Used by polymorphic IO
  virtual std::string is_a() const;

  //: Return true if the name of the class matches the argument.
  //  Used by polymorphic IO
  virtual bool is_class(std::string const& s) const;

  //:Return the failure code of the last minimization
  ReturnCodes get_failure_code() const { return failure_code_; }

 protected:
  // Data Members--------------------------------------------------------------
  // Input variables
  double xtol;    //!< Termination tolerance on X (solution vector)
  long   maxfev;  //!< Termination maximum number of iterations
  double ftol;    //!< Termination tolerance on F (sum of squared residuals)
  double gtol;    //!< Termination tolerance on Grad(F)' * F = 0
  double epsfcn;  //!< Step length for FD Jacobian

  // Output variables
  unsigned num_iterations_;
  long    num_evaluations_;
  double start_error_;
  double end_error_;

  bool trace;

  // Verbose flag.
  bool verbose_;
  int check_derivatives_;
  ReturnCodes failure_code_;

  void reset();

  //: Called by derived classes after each function evaluation.
  void report_eval(double f);

  //: Called by derived classes after each iteration.
  //  When true is returned, minimizer should stop with code FAILED_USER_REQUEST.
  //  Derived classes can redefine this function to make the optimizer stop when a condition is satisfied.
  virtual bool report_iter();
};

#endif // vnl_nonlinear_minimizer_h_
