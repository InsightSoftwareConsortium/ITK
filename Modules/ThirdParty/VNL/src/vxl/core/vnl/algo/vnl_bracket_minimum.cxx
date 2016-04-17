// This is core/vnl/algo/vnl_bracket_minimum.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \brief Function to bracket a minimum
// \author Tim Cootes
// \date   Feb 2007

#include <cmath>
#include <algorithm>
#include "vnl_bracket_minimum.h"
#include <vnl/algo/vnl_fit_parabola.h>
#include <vcl_compiler.h>
// not used? #include <iostream>

static const double GOLDEN_RATIO = 1.618033988749894848; // = 0.5*(std::sqrt(5)-1);
static const double EPS   = 1e-7;  // Loose tolerance
static const double EPSqr = 1e-14;
inline void swap(double& a, double& b)
{
  double x=a;
  a=b;
  b=x;
}

class vnl_bm_func
{
  vnl_vector<double> v;
  vnl_cost_function* f;
 public:
  vnl_bm_func(vnl_cost_function& fn) { f=&fn; v.set_size(1); }
  double operator()(double x) { v[0]=x; return f->f(v); }
};

//: Given initial values a and b, find bracket a<b<c s.t. f(a)>f(b)<f(c)
//  Final function values at a,b,c stored in fa,fb,fc
void vnl_bracket_minimum(vnl_cost_function& fn,
                         double& a, double& b, double& c,
                         double& fa, double& fb, double& fc)
{
  // Set up object to evaluate function
  // Note that fn takes a vector input - f converts a scalar to a vector
  vnl_bm_func f(fn);

  if (b==a) b=a+1.0;
  fa = f(a);
  fb = f(b);

  // Arrange that fb<=fa
  if (fb>fa)
  {
    swap(a,b); swap(fa,fb);
  }

  // Initial guess at c
  c = b+ GOLDEN_RATIO*(b-a);
  fc = f(c);

  while (fc<fb)  // Keep stepping until we go uphill again
  {
    // Use parabolic interpolation to estimate position of centre
    double p,q;
    vnl_fit_parabola(a,b,c,fa,fb,fc,p,q);

    // Ensure q not within EPSqr of zero
    if (q>=0 && q<EPSqr) q=EPSqr;
    else if (q<0 && q+EPSqr>0) q=-1.0*EPSqr;

    // Estimate of centre of parabolic fit - ie minimum
    // For true quadratic function, minima is at b+p/q
    double du = p/q;

    double tol = EPS*(1.0+std::max(std::fabs(b),std::fabs(c)));

    // Don't evaluate too close to b
    if (du>=0 && du<tol)       du=tol;
    else if (du<0 && du+tol>0) du=-1.0*tol;

    double u = b + du;

    // Don't evaluate too close to c
    if ((u-c)<tol && (u-c)>=0)        u+=tol;  // u>c by small amount
    else if ((c-u)<tol && (c-u)>=0)   u-=tol;  // u<c by small amount

    double u_limit = b + 100*(c-b);  // Some way along the line
    double fu=0.0;

    if ((u-b)*(c-u)>0.0)  // u in range (b,c), allowing for c<b
    {
      fu = f(u);
      if (fu<fc)
      {
        // Bracket is (b,u,c)
        a=b; fa=fb;  b=u; fb=fu;
        // Ensure a<b<c
        if (a>c) { swap(a,c); swap(fa,fc); }
        return;
      }
      else if (fu>fb)
      {
        // Bracket is (a,b,u)
        c=u; fc=fu;
        // Ensure a<b<c
        if (a>c) { swap(a,c); swap(fa,fc); }
        return;
      }
      // The predicted point is unhelpful, so try a default step
      u = c+GOLDEN_RATIO*(c-b);
      fu = f(u);
    }
    else if ((u-c)*(u_limit-u)>0.0)  // u in range (c,u_limit)
    {
      fu = f(u);
      if (fu>fc)
      {
        // Bracket is (b,c,u)
        a=b; fa=fb;  b=c; fb=fc; c=u; fc=fu;
        // Ensure a<b<c
        if (a>c) { swap(a,c); swap(fa,fc); }
        return;
      }
    }
    else if ((u_limit-c)*(u-u_limit)>=0) // u is beyond u_limit
    {
      u=u_limit;
      fu=f(u);
    }
    else  // u is somewhere else
    {
      // Next guess is at u
      u = c+GOLDEN_RATIO*(c-b);
      fu = f(u);
    }

    // Move bracket
    a=b;   b=c;    c=u;
    fa=fb; fb=fc; fc=fu;
  }

  // Ensure a<b<c
  if (a>c)
  {
    swap(a,c); swap(fa,fc);
  }
}

