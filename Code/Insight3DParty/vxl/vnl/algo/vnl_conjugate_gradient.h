#ifndef vnl_conjugate_gradient_h_
#define vnl_conjugate_gradient_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME	vnl_conjugate_gradient - real function minimization
// .LIBRARY	vnl-algo
// .HEADER	vxl Package
// .INCLUDE	vnl/algo/vnl_conjugate_gradient.h
// .FILE	vnl_conjugate_gradient.cxx
//
// .SECTION Author
//     Geoffrey Cross, Oxford RRG, 15 Feb 99
//
// .SECTION Modifications
//     990215 Geoff Initial version.
//   000628 David Capel - Major rewrite. Now derived from vnl_nonlinear_minimizer
//                        and operates on a vnl_cost_function.
//
//-----------------------------------------------------------------------------

#include <vcl_iostream.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_nonlinear_minimizer.h>

class vnl_cost_function;

class vnl_conjugate_gradient : public vnl_nonlinear_minimizer {
public:
  // Constructors/Destructors--------------------------------------------------
  
  // -- Initialize with the function object that is to be minimized.
  vnl_conjugate_gradient(vnl_cost_function& f) { init( f); }

  // -- Initialize as above, and then run minimization.
  vnl_conjugate_gradient(vnl_cost_function& f, vnl_vector<double>& x) {
    init(f);
    minimize(x);
  }

  // -- Initialize all variables
  void init(vnl_cost_function &f);

  // -- Destructor.
  ~vnl_conjugate_gradient();

  // Operations----------------------------------------------------------------

  void diagnose_outcome(vcl_ostream& = vcl_cout) const;

  // Computations--------------------------------------------------------------

  // -- Minimize the function supplied in the constructor until convergence
  // or failure.  On return, x is such that f(x) is the lowest value achieved.
  // Returns true for convergence, false for failure.
  bool minimize(vnl_vector<double>& x);

protected:
  // Data Members--------------------------------------------------------------

  vnl_cost_function *f_;
  double final_step_size_;

  // Helpers-------------------------------------------------------------------

  friend class vnl_conjugate_gradient_Activate;
#ifdef VCL_SUNPRO_CC
public:
#endif
  static double valuecomputer_( double *x);
  static int gradientcomputer_( double *g, double *x);
  static int valueandgradientcomputer_( double *v, double *g, double *x);
  static int preconditioner_( double *out, double *in);
#ifdef VCL_SUNPRO_CC
protected:
#endif
//   void approximate_gradient( const vnl_vector<double> &x, vnl_vector<double> &g, const double step);
//   void approximate_hessian( const vnl_vector<double> &x, vnl_matrix<double> &h, const double step);
};

#endif // vnl_conjugate_gradient_h_
