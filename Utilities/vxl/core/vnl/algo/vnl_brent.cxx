// This is core/vnl/algo/vnl_brent.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif

#include "vnl_brent.h"

#include <vcl_cmath.h>
#include <vcl_iostream.h>
#include <vcl_algorithm.h>

#include <vnl/vnl_math.h>
#include <vnl/vnl_vector_fixed.h>

struct vnl_brent_data
{
  vnl_cost_function* functor;
  vnl_vector_fixed<double,1> vx;
  double tol;

  double f(double x) {
    vx[0] = x;
    return functor->f(vx.as_ref());
  }
};

vnl_brent::vnl_brent(vnl_cost_function* functor)
{
  p = new vnl_brent_data;
  p->functor = functor;
  p->tol = 2.0e-4;
}

vnl_brent::~vnl_brent()
{
  delete p;
}

static const int ITMAX = 100;
static const double CGOLD = 0.3819660;
static const double ZEPS = 1.0e-10;

static
void SHFT(double* a, double* b, double* c, double d)
{
  *a = *b;
  *b = *c;
  *c = d;
}

double vnl_brent::minimize_given_bounds(double ax, double bx, double cx,
                                        double tol,
                                        double *xmin)
{
  int iter;
  double a,b,d=0.0,etemp,fu,fv,fw,fx,p1,q,r,tol1,tol2,u,v,w,x,xm;
  double e=0.0;

  a=(ax < cx ? ax : cx);
  b=(ax > cx ? ax : cx);
  x=w=v=bx;
  fw=fv=fx=p->f(x);
  if (verbose_) vcl_cerr << "vnl_brent f("<<x<<") \t= "<<fx <<'\n';
  for (iter=1;iter<=ITMAX;iter++)
  {
    xm=0.5*(a+b);
    tol1=tol*vcl_fabs(x)+ZEPS;
    tol2=2.0*(tol1);
    if (vcl_fabs(x-xm) <= (tol2-0.5*(b-a))) {
      *xmin=x;
      return fx;
    }
    if (vcl_fabs(e) > tol1) {
      r=(x-w)*(fx-fv);
      q=(x-v)*(fx-fw);
      p1=(x-v)*q-(x-w)*r;
      q=2.0*(q-r);
      if (q > 0.0) p1 = -p1;
      q=vcl_fabs(q);
      etemp=e;
      e=d; // Warning: The variable d has not yet been assigned a value.
      if (vcl_fabs(p1) >= vcl_fabs(0.5*q*etemp) || p1 <= q*(a-x) || p1 >= q*(b-x))
        d=CGOLD*(e=(x >= xm ? a-x : b-x));
      else {
        d=p1/q;
        u=x+d;
        if (u-a < tol2 || b-u < tol2)
          d=tol1 * vnl_math_sgn(xm-x);
      }
    } else {
      d=CGOLD*(e=(x >= xm ? a-x : b-x));
    }
    u=(vcl_fabs(d) >= tol1 ? x+d : x + tol1 * vnl_math_sgn(d));
    fu= p->f(u);
    if (verbose_) vcl_cerr << "vnl_brent f("<<u<<") \t= "<<fu <<'\n';
    if (fu <= fx) {
      if (u >= x) a=x; else b=x;
      SHFT(&v,&w,&x,u);
      SHFT(&fv,&fw,&fx,fu);
    } else {
      if (u < x) a=u; else b=u;
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
  int iter;
  double a,b,d=0.0,etemp,fu,fv,fw,fx,p1,q,r,tol1,tol2,u,v,w,x,xm;
  double e=0.0;

  a=(ax < cx ? ax : cx);
  b=(ax > cx ? ax : cx);
  x=w=v=bx;
  fw=fv=fx=fb;
  for (iter=1;iter<=ITMAX;iter++)
  {
    xm=0.5*(a+b);
    tol1=tol*vcl_fabs(x)+ZEPS;
    tol2=2.0*(tol1);
    if (vcl_fabs(x-xm) <= (tol2-0.5*(b-a))) {
      *xmin=x;
      return fx;
    }
    if (vcl_fabs(e) > tol1) {
      r=(x-w)*(fx-fv);
      q=(x-v)*(fx-fw);
      p1=(x-v)*q-(x-w)*r;
      q=2.0*(q-r);
      if (q > 0.0) p1 = -p1;
      q=vcl_fabs(q);
      etemp=e;
      e=d; // Warning: The variable d has not yet been assigned a value.
      if (vcl_fabs(p1) >= vcl_fabs(0.5*q*etemp) || p1 <= q*(a-x) || p1 >= q*(b-x))
        d=CGOLD*(e=(x >= xm ? a-x : b-x));
      else {
        d=p1/q;
        u=x+d;
        if (u-a < tol2 || b-u < tol2)
          d=tol1 * vnl_math_sgn(xm-x);
      }
    } else {
      d=CGOLD*(e=(x >= xm ? a-x : b-x));
    }
    u=(vcl_fabs(d) >= tol1 ? x+d : x + tol1 * vnl_math_sgn(d));
    fu= p->f(u);
    if (verbose_) vcl_cerr << "vnl_brent f("<<u<<") \t= "<<fu <<'\n';
    if (fu <= fx) {
      if (u >= x) a=x; else b=x;
      SHFT(&v,&w,&x,u);
      SHFT(&fv,&fw,&fx,fu);
    } else {
      if (u < x) a=u; else b=u;
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

const double GOLD = 1.618034;
const double GLIMIT = 100.0;
const double TINY = 1.0e-20;

void vnl_brent::bracket_minimum(double *ax, double *bx, double *cx,
                                double *fa, double *fb, double *fc)
{
  double ulim,u,r,q,fu;

  *fa=p->f(*ax);
  *fb=p->f(*bx);
  if (*fb > *fa) {
    vcl_swap(*ax, *bx);
    vcl_swap(*fa, *fb);
  }
  *cx=(*bx)+GOLD*(*bx-*ax);
  *fc=p->f(*cx);
  while (*fb > *fc) {
    r=(*bx-*ax)*(*fb-*fc);
    q=(*bx-*cx)*(*fb-*fa);
    double dq = q-r;
    if (vcl_abs(dq) < TINY)
      dq = vnl_math_sgn(dq) * TINY;

    u=(*bx)-((*bx-*cx)*q-(*bx-*ax)*r)/(2.0*dq);
    ulim=(*bx)+GLIMIT*(*cx-*bx);
    if ((*bx-u)*(u-*cx) > 0.0) {
      fu=p->f(u);
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
      u=(*cx)+GOLD*(*cx-*bx);
      fu=p->f(u);
    } else if ((*cx-u)*(u-ulim) > 0.0) {
      fu=p->f(u);
      if (fu < *fc) {
        //SHFT(bx,cx,&u,*cx+GOLD*(*cx-*bx)); awf dumped -- c is useless
        SHFT(bx,cx,&u,u+GOLD*(u-*cx));
        SHFT(fb,fc,&fu,p->f(u));
      }
    } else if ((u-ulim)*(ulim-*cx) >= 0.0) {
      u=ulim;
      fu=p->f(u);
    } else {
      u=(*cx)+GOLD*(*cx-*bx);
      fu=p->f(u);
    }
    SHFT(ax,bx,cx,u);
    SHFT(fa,fb,fc,fu);
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
