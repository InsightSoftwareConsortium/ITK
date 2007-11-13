// This is core/vnl/algo/vnl_brent.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif

#include "vnl_brent.h"

#include <vcl_cmath.h>
#include <vcl_iostream.h>
#include <vcl_algorithm.h>

#include <vnl/vnl_math.h>
#include <vnl/vnl_vector.h>

static const int NRITERATIONS = 100;
static const double GLIMIT = 100.0;
static const double GOLDEN_RATIO = 1.618033988749894848; // = 0.5*(vcl_sqrt(5)-1);
static const double COMPL_GOLD   = 0.381966011250105152; // = 0.5*(3-vcl_sqrt(5));
static const double EPSILON = 1e-10;
static const double EPSQ = EPSILON * EPSILON;

struct vnl_brent_data
{
  vnl_vector<double> vx;
  vnl_cost_function* functor;

  double f(double x) {
    vx[0] = x;
    return functor->f(vx);
  }
};

vnl_brent::vnl_brent(vnl_cost_function* functor)
{
  data_ = new vnl_brent_data;
  data_->vx = vnl_vector<double>(1U);
  data_->functor = functor;
  set_f_tolerance(2e-4);
}

vnl_brent::~vnl_brent()
{
  delete data_;
}

static
void SHFT(double& a, double& b, double& c, double d)
{
  a = b;
  b = c;
  c = d;
}

double vnl_brent::minimize_given_bounds(double ax, double bx, double cx,
                                        double tol,
                                        double *xmin)
{
  double d=0.0,e=0.0,prev_e,fu,fv,fw,fx,p,q,r,tol1,tol2,u,v,w,x,xm;
  double minac=(ax < cx ? ax : cx);
  double maxac=(ax > cx ? ax : cx);
  x=w=v=bx;
  fw=fv=fx=data_->f(x);
  if (verbose_) vcl_cerr << "vnl_brent f("<<x<<")\t= "<<fx <<'\n';
  for (int iter=0; iter<NRITERATIONS; ++iter)
  {
    xm=0.5*(minac+maxac);
    tol1=tol*vcl_fabs(x)+EPSILON;
    tol2=2*tol1;
    if (vcl_fabs(x-xm) <= (tol2-0.5*(maxac-minac))) {
      *xmin=x;
      return fx;
    }
    if (vcl_fabs(e) > tol1)
    {
      r = (x-w)*(fx-fv);
      q = (x-v)*(fx-fw);
      p = (x-v)*q-(x-w)*r;
      q = 2*(q-r);
      if (q > 0.0) p *= -1;
      else         q *= -1;
      prev_e=e;
      e=d;
      if (vcl_fabs(p) >= vcl_fabs(0.5*q*prev_e) || p <= q*(minac-x) || p >= q*(maxac-x))
        e = (x >= xm ? minac-x : maxac-x), d = COMPL_GOLD*e;
      else {
        d=p/q;
        u=x+d;
        if (u-minac < tol2 || maxac-u < tol2)
          d=tol1 * vnl_math_sgn(xm-x);
      }
    }
    else
      e = (x >= xm ? minac-x : maxac-x), d = COMPL_GOLD*e;
    u=(vcl_fabs(d) >= tol1 ? x+d : x + tol1 * vnl_math_sgn(d));
    fu= data_->f(u);
    if (verbose_) vcl_cerr << "vnl_brent f("<<u<<")\t= "<<fu <<'\n';
    if (fu <= fx) {
      if (u >= x) minac=x; else maxac=x;
      SHFT(v,w,x,u);
      SHFT(fv,fw,fx,fu);
    } else {
      if (u < x) minac=u; else maxac=u;
      if (fu <= fw || w == x) {
        v=w;
        w=u;
        fv=fw;
        fw=fu;
      } else if (fu <= fv || v == x || v == w) {
        v=u;
        fv=fu;
      }
    }
  }
  vcl_cerr << "Too many iterations in brent\n";
  *xmin=x;
  return fx;
}

double vnl_brent::minimize_given_bounds_and_1st_f(double ax, double bx,
                                                  double fb, double cx,
                                                  double tol, double *xmin)
{
  double d=0.0,e=0.0,prev_e,fu,fv,fw,fx,p,q,r,tol1,tol2,u,v,w,x,xm;
  double minac=(ax < cx ? ax : cx);
  double maxac=(ax > cx ? ax : cx);
  x=w=v=bx;
  fw=fv=fx=fb;
  if (verbose_) vcl_cerr << "vnl_brent f("<<x<<")\t= "<<fx <<'\n';
  for (int iter=0; iter<NRITERATIONS; ++iter)
  {
    xm=0.5*(minac+maxac);
    tol1=tol*vcl_fabs(x)+EPSILON;
    tol2=2*(tol1);
    if (vcl_fabs(x-xm) <= (tol2-0.5*(maxac-minac))) {
      *xmin=x;
      return fx;
    }
    if (vcl_fabs(e) > tol1)
    {
      r = (x-w)*(fx-fv);
      q = (x-v)*(fx-fw);
      p = (x-v)*q-(x-w)*r;
      q = 2*(q-r);
      if (q > 0.0) p *= -1;
      else         q *= -1;
      prev_e=e;
      e=d;
      if (vcl_fabs(p) >= vcl_fabs(0.5*q*prev_e) || p <= q*(minac-x) || p >= q*(maxac-x))
        e = (x >= xm ? minac-x : maxac-x), d = COMPL_GOLD*e;
      else {
        d=p/q;
        u=x+d;
        if (u-minac < tol2 || maxac-u < tol2)
          d=tol1 * vnl_math_sgn(xm-x);
      }
    }
    else
      e = (x >= xm ? minac-x : maxac-x), d = COMPL_GOLD*e;
    u=(vcl_fabs(d) >= tol1 ? x+d : x + tol1 * vnl_math_sgn(d));
    fu= data_->f(u);
    if (verbose_) vcl_cerr << "vnl_brent f("<<u<<")\t= "<<fu <<'\n';
    if (fu <= fx) {
      if (u >= x) minac=x; else maxac=x;
      SHFT(v,w,x,u);
      SHFT(fv,fw,fx,fu);
    } else {
      if (u < x) minac=u; else maxac=u;
      if (fu <= fw || w == x) {
        v=w;
        w=u;
        fv=fw;
        fw=fu;
      } else if (fu <= fv || v == x || v == w) {
        v=u;
        fv=fu;
      }
    }
  }
  vcl_cerr << "Too many iterations in brent\n";
  *xmin=x;
  return fx;
}


void vnl_brent::bracket_minimum(double *ax, double *bx, double *cx)
{
  double fa, fb, fc;
  bracket_minimum(ax,bx,cx,&fa,&fb,&fc);
}

void vnl_brent::bracket_minimum(double *ax, double *bx, double *cx,
                                double *fa, double *fb, double *fc)
{
  double ulim,u,r,q,fu;

  *fa=data_->f(*ax);
  *fb=data_->f(*bx);
  if (*fb > *fa) {
    vcl_swap(*ax, *bx);
    vcl_swap(*fa, *fb);
  }
  *cx=(*bx)+GOLDEN_RATIO*(*bx-*ax);
  *fc=data_->f(*cx);
  while (*fb > *fc) {
    r=(*bx-*ax)*(*fb-*fc);
    q=(*bx-*cx)*(*fb-*fa);
    double dq = q-r;
    if (dq >= 0 && dq < EPSQ)
      dq = EPSQ;
    else if (dq < 0 && dq > -EPSQ)
      dq = -EPSQ;

    u=(*bx)-((*bx-*cx)*q-(*bx-*ax)*r)/(2*dq);
    ulim=(*bx)+GLIMIT*(*cx-*bx);
    if ((*bx-u)*(u-*cx) > 0.0)
    {
      fu=data_->f(u);
      if (fu < *fc) {
        *ax=(*bx);
        *bx=u;
        *fa=(*fb);
        *fb=fu;
        return;
      } else if (fu > *fb) {
        *cx=u;
        *fc=fu;
        return;
      }
      u=(*cx)+GOLDEN_RATIO*(*cx-*bx);
      fu=data_->f(u);
    }
    else if ((*cx-u)*(u-ulim) > 0.0) {
      fu=data_->f(u);
      if (fu < *fc) {
        SHFT(*bx,*cx,u,u+GOLDEN_RATIO*(u-*cx));
        SHFT(*fb,*fc,fu,data_->f(u));
      }
    }
    else if ((u-ulim)*(ulim-*cx) >= 0.0) {
      u=ulim;
      fu=data_->f(u);
    }
    else {
      u=(*cx)+GOLDEN_RATIO*(*cx-*bx);
      fu=data_->f(u);
    }
    SHFT(*ax,*bx,*cx,u);
    SHFT(*fa,*fb,*fc,fu);
  }
}


double vnl_brent::minimize(double x)
{
  double ax=x-1.0;
  double xx=x+1.0;
  double bx,fa,fx,fb;
  bracket_minimum(&ax,&xx,&bx,&fa,&fx,&fb);
  minimize_given_bounds(bx,xx,ax,ftol,&x);
  return x;
}
