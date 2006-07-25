// This is core/vnl/algo/vnl_powell.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
#include "vnl_powell.h"

#include <vcl_cassert.h>
#include <vnl/vnl_math.h>
#include <vnl/algo/vnl_brent.h>
#ifdef DEBUG
#include <vnl/vnl_matlab_print.h>
#include <vcl_iostream.h>
#endif

class vnl_powell_1dfun : public vnl_cost_function
{
 public:
  vnl_powell* powell_;
  vnl_cost_function* f_;
  unsigned int n_;
  vnl_vector<double> x0_;
  vnl_vector<double> dx_;
  vnl_vector<double> tmpx_;
  vnl_powell_1dfun(int n, vnl_cost_function* f, vnl_powell* p)
   : vnl_cost_function(1), powell_(p), f_(f), n_(n), x0_(n), dx_(n), tmpx_(n) {}

  void init(vnl_vector<double> const& x0, vnl_vector<double> const& dx)
  {
    x0_ = x0;
    dx_ = dx;
    assert(x0.size() == n_);
    assert(dx.size() == n_);
  }

  double f(const vnl_vector<double>& x)
  {
    uninit(x[0], tmpx_);
    double e = f_->f(tmpx_);
    powell_->pub_report_eval(e);
    return e;
  }

  void uninit(double lambda, vnl_vector<double>& out)
  {
    for (unsigned int i = 0; i < n_; ++i)
      out[i] = x0_[i] + lambda * dx_[i];
  }
};

vnl_nonlinear_minimizer::ReturnCodes
vnl_powell::minimize(vnl_vector<double>& p)
  //double p[], double **xi, int n
{
 // verbose_ = true;
  int n = p.size();
  vnl_powell_1dfun f1d(n, functor_, this);

  vnl_matrix<double> xi(n,n, vnl_matrix_identity);
  vnl_vector<double> ptt(n);
  vnl_vector<double> xit(n);
  double fret = functor_->f(p);
  report_eval(fret);
  vnl_vector<double> pt = p;
  while (num_iterations_ < unsigned(maxfev))
  {
    double fp = fret;
    int ibig=0;
    double del=0.0;

    for (int i=0;i<n;i++)
    {
      // xit = ith column of xi
      for (int j = 0; j < n; ++j)
        xit[j] = xi[j][i];
      double fptt = fret;

      // 1D minimization along xi
      f1d.init(p, xit);
      vnl_brent brent(&f1d);
      double ax = 0.0;
      double xx = initial_step_;
      double bx;
      brent.bracket_minimum(&ax, &xx, &bx);
      fret = brent.minimize_given_bounds(bx, xx, ax, linmin_xtol_, &xx);
      f1d.uninit(xx, p);
      // Now p is minimizer along xi

      if (vcl_fabs(fptt-fret) > del) {
        del = vcl_fabs(fptt-fret);
        ibig = i;
      }
    }

    if (2.0*vcl_fabs(fp-fret) <= ftol*(vcl_fabs(fp)+vcl_fabs(fret)))
    {
#ifdef DEBUG
      vnl_matlab_print(vcl_cerr, xi, "xi");
#endif
      return CONVERGED_FTOL;
    }

    if (num_iterations_ == unsigned(maxfev))
      return FAILED_TOO_MANY_ITERATIONS;

    for (int j=0;j<n;++j)
    {
      ptt[j]=2.0*p[j]-pt[j];
      xit[j]=p[j]-pt[j];
      pt[j]=p[j];
    }

    double fptt = functor_->f(ptt);
    report_eval(fret);
    if (fptt < fp)
    {
      double t=2.0*(fp-2.0*fret+fptt)*vnl_math_sqr(fp-fret-del)-del*vnl_math_sqr(fp-fptt);
      if (t < 0.0)
      {
        f1d.init(p, xit);
        vnl_brent brent(&f1d);
        double ax = 0.0;
        double xx = 1.0;
        double bx;
        brent.bracket_minimum(&ax, &xx, &bx);
        fret = brent.minimize_given_bounds(bx, xx, ax, linmin_xtol_, &xx);
        f1d.uninit(xx, p);

        for (int j=0;j<n;j++) {
          xi[j][ibig]=xi[j][n-1];
          xi[j][n-1]=xit[j];
        }
      }
    }
    report_iter();
  }
  return FAILED_TOO_MANY_ITERATIONS;
}
