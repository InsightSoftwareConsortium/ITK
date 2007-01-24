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
  //
  // This code has been removed after it was pointed out
  // to ITK developers that it was a copy of code from
  // Numerical Recipies. January 23 2007
  //
  // The distribution license of numerical recipies is not
  // compatible with the BSD-License used by ITK.
  // 
  // -----------------------------------------------------------
  //
  // The following implementation was based on the description
  // of the Brent's method presented in the Wikipedia:
  //
  //    http://en.wikipedia.org/wiki/Brent%27s_method
  //
  double a = ax;
  double b = cx;

  bool mflag;

  if( ax > cx )
    {
    a = cx;
    b = ax;
    }

  double x = bx;

  double fa = p->f(a);
  double fb = p->f(b);
  double fx = p->f(x);

  if (verbose_) 
    {
    vcl_cerr << "vnl_brent f("<<x<<") \t= "<<fx <<'\n';
    }

  if( fa * fb >= 0.0 )
    {
    vcl_cerr << "vnl_brent f("<<a<<") has same sign as f("<<b<<") \n";
    *xmin = x;
    return fx;
    }

  if( vcl_fabs(fa) < vcl_fabs(fb) )
    {
    const double t= a;
    a = b;
    b = t;
    const double ft = fa;
    fa = fb;
    fb = ft;
    }

  double c = a;
  double d = a;   // it is not clear how to initialize d
  double fc = fa;

  double s;

  for( unsigned int iteration = 1; iteration <= ITMAX; iteration++)
    {

    if (verbose_) 
      {
      vcl_cerr << "vnl_brent f("<<b<<") \t= "<<fb <<'\n';
      }

    if( vcl_fabs(fb) <= ZEPS || vcl_fabs( a - b ) <= ZEPS )
      {
      *xmin=b;
      return fb;
      }

    const double fac = fa - fc;
    const double fbc = fb - fc;
    const double fab = fa - fb;

    if( vcl_fabs( fac ) < ZEPS || vcl_fabs(fbc) < ZEPS )
      {
      // Apply secant rule
      s = b - fb * (b - a) / ( fb - fa );
      }
    else
      {
      // Inverse quadratic interpolation
      const double afbfc = ( a * fb * fc ) / ( fab * fac );
      const double bfafc = ( b * fa * fc ) / ( fab * fbc );
      const double cfafb = ( c * fa * fb ) / ( fac * fbc );
      s = afbfc - bfafc + cfafb;
      }

    if( !( s > ( 3 * a + b ) / 4.0 && s < b ) ||
         (  mflag && ( vcl_fabs( s - b ) >= vcl_fabs( b - c ) / 2.0 ) ) ||
         ( !mflag && ( vcl_fabs( s - b ) >= vcl_fabs( c - d ) / 2.0 ) )    )
      {
      s = ( a + b ) / 2;
      mflag = true;
      }
    else
      {
      mflag = false;
      }
       
    double fs = p->f(s);

    d = c;
    c = b;

    if( fa * fs < 0.0 )
      {
      b = s;
      fb = fs;
      }
    else
      {
      a = s;
      fa = fs;
      }

    if( vcl_fabs( fa ) < vcl_fabs( fb ) )
      {
      const double temp = a;
      a = b;
      b = temp;
      }
    
    }

  *xmin = b;
  return fb;
}

double vnl_brent::minimize_given_bounds_and_1st_f(double ax, double bx,
                                                  double fb, double cx,
                                                  double tol, double *xmin)
{
  //
  // This code has been removed after it was pointed out
  // to ITK developers that it was a copy of code from
  // Numerical Recipies. January 23 2007
  //
  // The distribution license of numerical recipies is not
  // compatible with the BSD-License used by ITK.
  // 
  vcl_cerr << "vnl_brent::minimize_given_bounds_and_1st_f TEMPORARILY DISABLED\n";
  return 0.0;
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
