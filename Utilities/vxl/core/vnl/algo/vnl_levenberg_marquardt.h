// This is core/vnl/algo/vnl_levenberg_marquardt.h
#ifndef vnl_levenberg_marquardt_h_
#define vnl_levenberg_marquardt_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Levenberg Marquardt nonlinear least squares
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   31 Aug 96
//
// \verbatim
// Modifications
//  AGAP 160701 Some comments. Changed minimize to call the correct minimization
//              routine.
//  RWMC 001097 Added verbose flag to get rid of all that blathering.
//  AWF  151197 Added trace flag to increase blather.
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
// \endverbatim
//

#include <vcl_iosfwd.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_nonlinear_minimizer.h>

class vnl_least_squares_function;

//: Levenberg Marquardt nonlinear least squares
//  vnl_levenberg_marquardt is an interface to the MINPACK routine lmdif,
//  and implements Levenberg Marquardt nonlinear fitting.  The function
//  to be minimized is passed as a vnl_least_squares_function object, which
//  may or may not wish to provide derivatives.  If derivatives are not
//  supplied, they are calculated by forward differencing, which costs
//  one function evaluation per dimension, but is perfectly accurate.
//  (See Hartley in ``Applications of Invariance in Computer Vision''
//  for example).

class vnl_levenberg_marquardt : public vnl_nonlinear_minimizer
{
 public:

  //: Initialize with the function object that is to be minimized.
  vnl_levenberg_marquardt(vnl_least_squares_function& f) { init(&f); }

#if 0
  //: Initialize as above, and then run minimization.
  //
  // obsolete, as virtuals in base class vnl_nonlinear_minimizer not valid...
  // i.e. if minimize() calls base::get_covariance(), it will call the
  // base version rather than any overridden here or in classes derived
  // from this.  This is an argument against computation in constructors.
  // You should replace code like
  // \code
  //    vnl_levenberg_marquardt lm(f, x);
  // \endcode
  // with
  // \code
  //    vnl_levenberg_marquardt lm(f);
  //    lm.minimize(x);
  // \endcode
  // Or
  // \code
  //    x = vnl_levenberg_marquardt_minimize(f, x);
  // \endcode

  vnl_levenberg_marquardt(vnl_least_squares_function& f,
                          vnl_vector<double>& x)
  {
    init(&f);
    minimize(x);
  }
#endif

  ~vnl_levenberg_marquardt();

  //: Minimize the function supplied in the constructor until convergence or failure.
  //  On return, x is such that f(x) is the lowest value achieved.
  //  Returns true for convergence, false for failure.
  //  Does not use the gradient even if the cost function provides one.
  bool minimize_without_gradient(vnl_vector<double>& x);

  //: Minimize the function supplied in the constructor until convergence or failure.
  //  On return, x is such that f(x) is the lowest value achieved.
  //  Returns true for convergence, false for failure.
  //  The cost function must provide a gradient.
  bool minimize_using_gradient  (vnl_vector<double>& x);

  //: Calls minimize_using_gradient() or minimize_without_gradient()
  // , depending on whether the cost function provides a gradient.
  bool minimize(vnl_vector<double>& x);
  bool minimize(vnl_vector_fixed<double,2>& x) { vnl_vector<double> y=x.extract(2); bool b=minimize(y); x=y; return b; }
  bool minimize(vnl_vector_fixed<double,3>& x) { vnl_vector<double> y=x.extract(3); bool b=minimize(y); x=y; return b; }
  bool minimize(vnl_vector_fixed<double,4>& x) { vnl_vector<double> y=x.extract(4); bool b=minimize(y); x=y; return b; }

  // Coping with failure-------------------------------------------------------

  //: Provide an ASCII diagnosis of the last minimization on vcl_ostream.
  void diagnose_outcome(/*vcl_cerr*/) const;
  void diagnose_outcome(vcl_ostream&) const;

  //: Return J'*J computed at last minimum.
  //  it is an approximation of inverse of covariance 
  vnl_matrix<double> const& get_JtJ();

 protected:

  vnl_least_squares_function* f_;
  vnl_matrix<double> fdjac_; // Computed during lmdif/lmder
  vnl_vector<int>    ipvt_;  // Also computed, both needed to get J'*J at end.

  vnl_matrix<double> inv_covar_;
  bool set_covariance_; // Set if covariance_ holds J'*J

  void init(vnl_least_squares_function* f);

  // Communication with callback
  friend class vnl_levenberg_marquardt_Activate;
  static void lmdif_lsqfun(int* m, int* n, double* x,
                           double* fx, int* iflag);
  static void lmder_lsqfun(int* m, int* n, double* x,
                           double* fx, double* fJ, int*, int* iflag);
};

//: Find minimum of "f", starting at "initial_estimate", and return.
vnl_vector<double> vnl_levenberg_marquardt_minimize(vnl_least_squares_function& f,
                                                    vnl_vector<double> const& initial_estimate);


#endif // vnl_levenberg_marquardt_h_
