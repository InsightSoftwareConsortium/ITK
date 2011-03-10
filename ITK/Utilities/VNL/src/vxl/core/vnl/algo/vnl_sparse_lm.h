// This is core/vnl/algo/vnl_sparse_lm.h
#ifndef vnl_sparse_lm_h_
#define vnl_sparse_lm_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Sparse Levenberg Marquardt nonlinear least squares
// \author Matt Leotta (Brown)
// \date   April 14, 2005
//
// \verbatim
//  Modifications
// \endverbatim
//

#include <vcl_iosfwd.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_nonlinear_minimizer.h>

class vnl_sparse_lst_sqr_function;

//: Sparse Levenberg Marquardt nonlinear least squares
//  Unlike vnl_levenberg_marquardt this does not use the MINPACK routines.
//  This class implements sparse Levenberg Marquardt as described in
//  the Hartley and Zisserman "Multiple View Geometry" book and further
//  described in a technical report on sparse bundle adjustment available
//  at http://www.ics.forth.gr/~lourakis/sba
class vnl_sparse_lm : public vnl_nonlinear_minimizer
{
 public:

  //: Initialize with the function object that is to be minimized.
  vnl_sparse_lm(vnl_sparse_lst_sqr_function& f) { init(&f); }

  //: Destructor
  ~vnl_sparse_lm();

  //: Minimize the function supplied in the constructor until convergence or failure.
  //  On return, a and b are such that f(a,b) is the lowest value achieved.
  //  Returns true for convergence, false for failure.
  //  if use_gradient is set to false, a finite difference approximation will be used,
  //  even if the Jacobian functions have been provided
  bool minimize(vnl_vector<double>& a, vnl_vector<double>& b, bool use_gradient = true);

  // Coping with failure-------------------------------------------------------

  //: Provide an ASCII diagnosis of the last minimization on vcl_ostream.
  void diagnose_outcome(/*vcl_cerr*/) const;
  void diagnose_outcome(vcl_ostream&) const;

  //: Return J'*J computed at last minimum.
  //  it is an approximation of inverse of covariance
  vnl_matrix<double> const& get_JtJ();

 protected:

  //: used to compute the initial damping
  double tau_;
  //: the function to minimize
  vnl_sparse_lst_sqr_function* f_;

  vnl_matrix<double> inv_covar_;
  bool set_covariance_; // Set if covariance_ holds J'*J

  void init(vnl_sparse_lst_sqr_function* f);
};


#endif // vnl_sparse_lm_h_
