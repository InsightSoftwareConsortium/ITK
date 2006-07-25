// This is core/vnl/algo/vnl_conjugate_gradient.h
#ifndef vnl_conjugate_gradient_h_
#define vnl_conjugate_gradient_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief  real function minimization
//  \author Geoffrey Cross, Oxford RRG
//  \date   15 Feb 99
//
// \verbatim
// Modifications
// 990215 Geoff Initial version.
// 000628 David Capel - Major rewrite. Now derived from vnl_nonlinear_minimizer and operates on a vnl_cost_function.
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
// \endverbatim
//
//-----------------------------------------------------------------------------

#include <vcl_iosfwd.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_nonlinear_minimizer.h>

class vnl_cost_function;

//: real function minimization

class vnl_conjugate_gradient : public vnl_nonlinear_minimizer
{
 public:
  // Constructors/Destructors--------------------------------------------------

  //: Initialize with the function object that is to be minimized.
  vnl_conjugate_gradient(vnl_cost_function& f) { init( f); }

  //: Initialize as above, and then run minimization.
  vnl_conjugate_gradient(vnl_cost_function& f, vnl_vector<double>& x) {
    init(f);
    minimize(x);
  }

  //: Initialize all variables
  void init(vnl_cost_function &f);

  //: Destructor.
  ~vnl_conjugate_gradient();

  // Operations----------------------------------------------------------------

  void diagnose_outcome(vcl_ostream&) const;
  void diagnose_outcome(/*vcl_ostream& = vcl_cout*/) const;

  // Computations--------------------------------------------------------------

  //: Minimize the function supplied in the constructor until convergence or failure.
  // On return, x is such that f(x) is the lowest value achieved.
  // Returns true for convergence, false for failure.
  bool minimize(vnl_vector<double>& x);

 protected:
  // Data Members--------------------------------------------------------------

  vnl_cost_function *f_;
  double final_step_size_;

  // Helpers-------------------------------------------------------------------

#ifdef VCL_SUNPRO_CC
 public:
#endif
  static double valuecomputer_( double *x, void* userdata);
  static void gradientcomputer_( double *g, double *x, void* userdata);
  static void valueandgradientcomputer_( double *v, double *g, double *x, void* userdata);
  static void preconditioner_( double *out, double *in, void* userdata);

#if 0
 protected:
  void approximate_gradient( const vnl_vector<double> &x,
                             vnl_vector<double> &g, const double step);
  void approximate_hessian( const vnl_vector<double> &x,
                            vnl_matrix<double> &h, const double step);
#endif
};

#endif // vnl_conjugate_gradient_h_
