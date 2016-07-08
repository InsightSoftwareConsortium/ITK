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
#include <vcl_vector.h>
#include <vcl_iostream.h>
#include <vnl/vnl_vector_ref.h>

#include <vnl/algo/vnl_netlib.h> // lsqr_()

#include "lsqrBase.h"

class lsqrVNL : public lsqrBase
{
public:

  lsqrVNL()
    {
    this->ls_ = NULL;
    }

  virtual ~lsqrVNL()
    {
    }

  /**
   * computes y = y + A*x without altering x,
   * where A is a matrix of dimensions A[m][n].
   * The size of the vector x is n.
   * The size of the vector y is m.
   */
  void Aprod1(unsigned int m, unsigned int n, const double * x, double * y ) const
    {
    vnl_vector_ref<double> x_ref(n, const_cast<double*>(x) );
    vnl_vector_ref<double> y_ref(m,y);

    vnl_vector_ref<double> tmp(m,rw);
    this->ls_->multiply(x_ref, tmp);
    y_ref += tmp;
    }


  /**
   * computes x = x + A'*y without altering y,
   * where A is a matrix of dimensions A[m][n].
   * The size of the vector x is n.
   * The size of the vector y is m.
   */
  void Aprod2(unsigned int m, unsigned int n, double * x, const double * y ) const
    {
    vnl_vector_ref<double> x_ref(n,x);
    vnl_vector_ref<double> y_ref(m, const_cast<double*>( y ) );

    vnl_vector_ref<double> tmp(n,rw);
    this->ls_->transpose_multiply(y_ref, tmp);
    x_ref += tmp;
    }

  /** Set the linear system to be solved A*x = b. */
  void SetLinearSystem( vnl_linear_system * inls )
    {
    this->ls_ = inls;
    }

  void SetWorkingSpace( double * inrw )
    {
    this->rw = inrw;
    }

private:

  vnl_linear_system * ls_;

  double * rw;
};


vnl_lsqr::~vnl_lsqr()
{
}

// Requires number_of_residuals() of workspace in rw.
int vnl_lsqr::aprod_(long* mode, long* m, long* n, double* x, double* y, long* /*leniw*/, long* /*lenrw*/, long* /*iw*/, double* rw, void* userdata)
{
  //
  // THIS CODE IS DEPRECATED
  // THE FUNCTIONALITY HAS BEEN MOVED TO THE lsqrVNL class above.
  // THE FUNCTIONS IS CONSERVED HERE ONLY FOR BACKWARD COMPATIBILITY.
  //
  vnl_lsqr* self = static_cast<vnl_lsqr*>(userdata);

  //  If MODE = 1, compute  y = y + A*x.
  //  If MODE = 2, compute  x = x + A(transpose)*y.

  vnl_vector_ref<double> x_ref(*n,x);
  vnl_vector_ref<double> y_ref(*m,y);

  if (*mode == 1) {
    vnl_vector_ref<double> tmp(*m,rw);
    self->ls_->multiply(x_ref, tmp);
    y_ref += tmp;
  }
  else {
    vnl_vector_ref<double> tmp(*n,rw);
    self->ls_->transpose_multiply(y_ref, tmp);
    x_ref += tmp;
  }

  return 0;
}

int vnl_lsqr::minimize(vnl_vector<double>& result)
{
  long m = ls_->get_number_of_residuals();
  long n = ls_->get_number_of_unknowns();
  double damp = 0;
  long leniw = 1;
  long* iw = 0;
  long lenrw = m;
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
  long nout = -1;
  double anorm, acond, rnorm, arnorm, xnorm;

  vnl_vector<double> rhs(m);
  ls_->get_rhs(rhs);

  lsqrVNL solver;

  solver.SetDamp( damp );
  solver.SetLinearSystem( this->ls_ );
  solver.SetWorkingSpace( &rw[0] );
  solver.SetMaximumNumberOfIterations( max_iter_ );
  solver.SetStandardErrorEstimates( &se[0] );
  solver.SetToleranceA( atol );
  solver.SetToleranceB( btol );

  solver.Solve( m, n, rhs.data_block(), result.data_block() );

#ifdef THIS_CODE_IS_DISABLED_BECAUSE_THE_LSQR_CODE_FROM_NETLIB_WAS_COPYRIGHTED_BY_ACM
  v3p_netlib_lsqr_(
        &m, &n, aprod_, &damp, &leniw, &lenrw, iw, &rw[0],
        rhs.data_block(), &v[0], &w[0], result.data_block(), &se[0],
        &atol, &btol, &conlim, &max_iter_, &nout, &return_code_,
        &num_iter_, &anorm, &acond, &rnorm, &arnorm, &xnorm,
        this);
#endif

  resid_norm_estimate_ = solver.GetFinalEstimateOfNormRbar();
  result_norm_estimate_ = solver.GetFinalEstimateOfNormOfX();
  A_condition_estimate_ = solver.GetConditionNumberEstimateOfAbar();
  return_code_ = solver.GetStoppingReason();
  num_iter_ = solver.GetNumberOfIterationsPerformed();
  anorm = solver.GetFrobeniusNormEstimateOfAbar();
  arnorm = solver.GetFinalEstimateOfNormOfResiduals();

#if 0
  vcl_cerr << "A Fro norm estimate      = " << anorm << vcl_endl
           << "A condition estimate     = " << acond << vcl_endl
           << "Residual norm estimate   = " << rnorm << vcl_endl
           << "A'(Ax - b) norm estimate = " << arnorm << vcl_endl
           << "x norm estimate          = " << xnorm << vcl_endl;
#endif

  // We should return the return code, as translate_return_code is public and
  // it is very misleading that the return code from this function can't be fed
  // into translate_return_code. (Brian Amberg)
  return return_code_;
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
