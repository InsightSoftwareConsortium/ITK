#ifndef vnl_levenberg_marquardt_h_
#define vnl_levenberg_marquardt_h_
#ifdef __GNUC__
#pragma interface
#endif
// .NAME	vnl_levenberg_marquardt - Levenberg Marquardt nonlinear least squares
// .LIBRARY	vnl-algo
// .HEADER	vxl Package
// .INCLUDE	vnl/algo/vnl_levenberg_marquardt.h
// .FILE	vnl_levenberg_marquardt.cxx
// .EXAMPLE	../examples/vnl_rosenbrock.cxx
//
// .SECTION Description
//    vnl_levenberg_marquardt is an interface to the MINPACK routine lmdif,
//    and implements Levenberg Marquardt nonlinear fitting.  The function
//    to be minimized is passed as a vnl_least_squares_function object, which
//    may or may not wish to provide derivatives.  If derivatives are not
//    supplied, they are calculated by forward differencing, which costs
//    one function evaluation per dimension, but is perfectly accurate.
//    (See Hartley in ``Applications of Invariance in Computer Vision''
//    for example).
//
// .SECTION Author:
//    Andrew W. Fitzgibbon, Oxford RRG, 31 Aug 96
//
// .SECTION Modifications
//    RWMC 001097 Added verbose flag to get rid of all that blathering.
//    AWF  151197 Added trace flag to increase blather.

#include <vcl_iosfwd.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_nonlinear_minimizer.h>

class vnl_least_squares_function;

//: Levenberg Marquardt nonlinear least squares

class vnl_levenberg_marquardt : public vnl_nonlinear_minimizer {
public:

// -- Initialize with the function object that is to be minimized.
  vnl_levenberg_marquardt(vnl_least_squares_function& f) { init(&f); }

// -- Initialize as above, and then run minimization.
  vnl_levenberg_marquardt(vnl_least_squares_function& f, vnl_vector<double>& x) {
    init(&f);
    minimize(x);
  }

  ~vnl_levenberg_marquardt();

// -- Minimize the function supplied in the constructor until convergence
// or failure.  On return, x is such that f(x) is the lowest value achieved.
// Returns true for convergence, false for failure.
  bool minimize(vnl_vector<double>& x);
  bool minimize_using_gradient(vnl_vector<double>& x);

  // Coping with failure-------------------------------------------------------

// -- Provide an ASCII diagnosis of the last minimization on ostream.
  void diagnose_outcome(/*vcl_cerr*/) const;
  void diagnose_outcome(vcl_ostream&) const;

// -- Return J'*J computed at last minimum.
  vnl_matrix<double> const& get_JtJ();

protected:

  vnl_least_squares_function* f_;
  vnl_matrix<double>* fdjac_; // Computed during lmdif/lmder
  vnl_vector<int>*    ipvt_;  // Also computed, both needed to get J'*J at end.

  vnl_matrix<double>* covariance_;
  bool set_covariance_; // Set if covariance_ holds J'*J

  void init(vnl_least_squares_function* f);

  // Communication with callback
  friend class vnl_levenberg_marquardt_Activate;
  static int lmdif_lsqfun(int* m, int* n, const double* x, double* fx, int* iflag);
  static int lmder_lsqfun(int* m, int* n, const double* x, double* fx, double* fJ, int&, int* iflag);
};

#endif // vnl_levenberg_marquardt_h_
