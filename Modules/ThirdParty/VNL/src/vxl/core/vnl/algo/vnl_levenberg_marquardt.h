// This is core/vnl/algo/vnl_levenberg_marquardt.h
#ifndef vnl_levenberg_marquardt_h_
#define vnl_levenberg_marquardt_h_
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

#include <iosfwd>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include <vnl/vnl_vector.h>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_nonlinear_minimizer.h>

#include <vnl/algo/vnl_algo_export.h>

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

class VNL_ALGO_EXPORT vnl_levenberg_marquardt : public vnl_nonlinear_minimizer
{
 public:

  //: Initialize with the function object that is to be minimized.
  vnl_levenberg_marquardt(vnl_least_squares_function& f) { init(&f); }

  ~vnl_levenberg_marquardt() override;

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

  //: Calls minimize_using_gradient() or minimize_without_gradient(),
  // depending on whether the cost function provides a gradient.
  bool minimize(vnl_vector<double>& x);
  bool minimize(vnl_vector_fixed<double,1>& x) { vnl_vector<double> y=x.extract(1); bool b=minimize(y); x=y; return b; }
  bool minimize(vnl_vector_fixed<double,2>& x) { vnl_vector<double> y=x.extract(2); bool b=minimize(y); x=y; return b; }
  bool minimize(vnl_vector_fixed<double,3>& x) { vnl_vector<double> y=x.extract(3); bool b=minimize(y); x=y; return b; }
  bool minimize(vnl_vector_fixed<double,4>& x) { vnl_vector<double> y=x.extract(4); bool b=minimize(y); x=y; return b; }

  // Coping with failure-------------------------------------------------------

  //: Provide an ASCII diagnosis of the last minimization on std::ostream.
  void diagnose_outcome(/*std::cerr*/) const;
  void diagnose_outcome(std::ostream&) const;

  //: Return J'*J computed at last minimum.
  //  it is an approximation of inverse of covariance
  vnl_matrix<double> const& get_JtJ();

 protected:

  vnl_least_squares_function* f_;
  vnl_matrix<double> fdjac_; // Computed during lmdif/lmder
  vnl_vector<long>    ipvt_; // Also computed, both needed to get J'*J at end.

  vnl_matrix<double> inv_covar_;
  bool set_covariance_; // Set if covariance_ holds J'*J

  void init(vnl_least_squares_function* f);

  // Communication with callback
  static void lmdif_lsqfun(long* m, long* n, double* x,
                           double* fx, long* iflag, void* userdata);
  static void lmder_lsqfun(long* m, long* n, double* x,
                           double* fx, double* fJ, long*, long* iflag,
                           void* userdata);
};

//: Find minimum of "f", starting at "initial_estimate", and return.
vnl_vector<double> vnl_levenberg_marquardt_minimize(vnl_least_squares_function& f,
                                                    vnl_vector<double> const& initial_estimate);


#endif // vnl_levenberg_marquardt_h_
