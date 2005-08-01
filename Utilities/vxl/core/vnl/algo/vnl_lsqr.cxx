// This is core/vnl/algo/vnl_lsqr.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//
// vnl_lsqr
// Author: David Capel
// Created: July 2000
//
//-----------------------------------------------------------------------------

#include "vnl_lsqr.h"
#include <vcl_cstdlib.h>
#include <vcl_vector.h>
#include <vcl_iostream.h>
#include <vnl/vnl_vector_ref.h>

#include <vnl/algo/vnl_netlib.h> // lsqr_()

class vnl_lsqr_Activate
{
 public:
  static vnl_lsqr* current;

  vnl_lsqr_Activate(vnl_lsqr* minimizer) {
    if (current) {
      vcl_cerr << "vnl_lsqr: ERROR: Nested minimizations not supported.\n";
      vcl_abort();
      // This is a copy of what goes on in LevenbergMarquardt, so if awf decides to
      // fix that one, then maybe he could do the same here...
    }
    current = minimizer;
  }
  ~vnl_lsqr_Activate() {
    current = 0;
  }
};

vnl_lsqr *vnl_lsqr_Activate::current= 0;

vnl_lsqr::~vnl_lsqr()
{
}

// Requires number_of_residuals() of workspace in rw.
void vnl_lsqr::aprod_(int* mode, int* m, int* n, double* x, double* y, int* /*leniw*/, int* /*lenrw*/, int* /*iw*/, double* rw )
{
  vnl_lsqr* active = vnl_lsqr_Activate::current;

  //  If MODE = 1, compute  y = y + A*x.
  //  If MODE = 2, compute  x = x + A(transpose)*y.

  vnl_vector_ref<double> x_ref(*n,x);
  vnl_vector_ref<double> y_ref(*m,y);

  if (*mode == 1) {
    vnl_vector_ref<double> tmp(*m,rw);
    active->ls_->multiply(x_ref, tmp);
    y_ref += tmp;
  }
  else {
    vnl_vector_ref<double> tmp(*n,rw);
    active->ls_->transpose_multiply(y_ref, tmp);
    x_ref += tmp;
  }
}

int vnl_lsqr::minimize(vnl_vector<double>& result)
{
  int m = ls_->get_number_of_residuals();
  int n = ls_->get_number_of_unknowns();
  double damp = 0;
  int leniw = 1;
  int* iw = 0;
  int lenrw = m;
#ifdef __GNUC__
  double rw[m];
  double v[n];
  double w[n];
  double se[n];
#else
  vcl_vector<double> rw(m);
  vcl_vector<double> v(n);
  vcl_vector<double> w(n);
  vcl_vector<double> se(n);
#endif
  double atol = 0;
  double btol = 0;
  double conlim = 0;
  int nout = -1;
  double anorm, acond, rnorm, arnorm, xnorm;

  vnl_vector<double> rhs(m);
  ls_->get_rhs(rhs);

  vnl_lsqr_Activate activator(this); // This variable is not used, but the constructor must be called.

  lsqr_(&m, &n, aprod_, &damp, &leniw, &lenrw, iw, &rw[0],
        rhs.data_block(), &v[0], &w[0], result.data_block(), &se[0],
        &atol, &btol, &conlim, &max_iter_, &nout, &return_code_,
        &num_iter_, &anorm, &acond, &rnorm, &arnorm, &xnorm);

  resid_norm_estimate_ = rnorm;
  result_norm_estimate_ = xnorm;
  A_condition_estimate_ = acond;

#if 0
  vcl_cerr << "A Fro norm estimate      = " << anorm << vcl_endl
           << "A condition estimate     = " << acond << vcl_endl
           << "Residual norm estimate   = " << rnorm << vcl_endl
           << "A'(Ax - b) norm estimate = " << arnorm << vcl_endl
           << "x norm estimate          = " << xnorm << vcl_endl;
#endif

  return 0; // return value not used
}

void vnl_lsqr::diagnose_outcome(vcl_ostream& os) const
{
  translate_return_code(os, return_code_);
  os << __FILE__ " : residual norm estimate = " << resid_norm_estimate_ << vcl_endl
     << __FILE__ " : result norm estimate   = " << result_norm_estimate_ << vcl_endl
     << __FILE__ " : condition no. estimate = " << A_condition_estimate_ << vcl_endl
     << __FILE__ " : iterations             = " << num_iter_ << vcl_endl;
}

void vnl_lsqr::translate_return_code(vcl_ostream& os, int rc)
{
  const char* vnl_lsqr_reasons[] = {
   "x = 0  is the exact solution. No iterations were performed.",
   "The equations A*x = b are probably compatible.  "
       "Norm(A*x - b) is sufficiently small, given the "
       "values of ATOL and BTOL.",
   "The system A*x = b is probably not compatible.  "
       "A least-squares solution has been obtained that is "
       "sufficiently accurate, given the value of ATOL.",
   "An estimate of cond(Abar) has exceeded CONLIM.  "
       "The system A*x = b appears to be ill-conditioned.  "
       "Otherwise, there could be an error in subroutine APROD.",
   "The equations A*x = b are probably compatible.  "
       "Norm(A*x - b) is as small as seems reasonable on this machine.",
   "The system A*x = b is probably not compatible.  A least-squares "
       "solution has been obtained that is as accurate as seems "
       "reasonable on this machine.",
   "Cond(Abar) seems to be so large that there is no point in doing further "
       "iterations, given the precision of this machine. "
       "There could be an error in subroutine APROD.",
   "The iteration limit ITNLIM was reached."
  };

  if
    (rc < 0 || rc > 7) os << __FILE__ " : Illegal return code : " << rc << vcl_endl;
  else
    os << __FILE__ " : " << vnl_lsqr_reasons[rc] << vcl_endl;
}
