// This is core/vnl/algo/vnl_brent_minimizer.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
#include <cmath>
#include <iostream>
#include <algorithm>
#include "vnl_brent_minimizer.h"

#include <vcl_compiler.h>
#include <vcl_cassert.h>

#include <vnl/vnl_vector.h>
#include <vnl/algo/vnl_bracket_minimum.h>

static const double GOLDEN_RATIO = 1.618033988749894848; // = 0.5*(std::sqrt(5)-1);
static const double COMPL_GOLD   = 0.381966011250105152; // = 0.5*(3-std::sqrt(5));
static const double EPS          = 1e-8;

// Wrapper to make it easy to evaluate the cost function
class vnl_brent_minimizer_func
{
  vnl_vector<double> v;
  vnl_cost_function* f;
 public:
  vnl_brent_minimizer_func(vnl_cost_function& fn)
    { f=&fn; v.set_size(1); }

  double operator()(double x) { v[0]=x; return f->f(v); }
};

vnl_brent_minimizer::vnl_brent_minimizer(vnl_cost_function& functor)
{
  f_=&functor;
  set_x_tolerance(1e-6);
}

vnl_brent_minimizer::~vnl_brent_minimizer()
{
}

//: Find the minimum value of f(x) within a<= x <= c.
// The minimum x is the return value.
// You need to provide a bracket for the minimum (a<b<c s.t. f(a)>f(b)<f(c).
// The tolerance can be set using prior call to set_x_tolerance(tol).
// Use f_at_last_minimum() to get function evaluation at the returned minima.
double vnl_brent_minimizer::minimize_given_bounds(double ax, double bx, double cx)
{
  vnl_brent_minimizer_func f(*f_);
  double fb = f(bx);

  return minimize_given_bounds_and_one_f(ax,bx,cx,fb);
}

//: Find the minimum value of f(x) within a<= x <= c.
// The minimum x is the return value.
// You need to provide a bracket for the minimum (a<b<c s.t. f(a)>f(b)<f(c),
// and the known value at b (fb=f(b)).
// The tolerance can be set using prior call to set_x_tolerance(tol).
// Use f_at_last_minimum() to get function evaluation at the returned minima.
double vnl_brent_minimizer::minimize_given_bounds_and_one_f(double ax, double bx, double cx,
                                                            double fb)
{
  // Check that the bracket is valid
  assert(ax<bx);
  assert(bx<cx);

  // Set up object to evaluate function as f(x)
  // Note that *f_ takes a vector input - f converts a scalar to a vector
  vnl_brent_minimizer_func f(*f_);

  double x=bx;  // Current best estimate of minimum
  double w=x;  // Next best point
  double v=w;  // Third best point
  double u;  // Most recently evaluated point

  // Function evaluations at these points
  double fx = fb;
  double fw = fx;
  double fv = fw;
  double fu;

  double m = 0.5*(ax+cx);  // Midpoint of (a,c)
  double tol = EPS*std::fabs(x)+xtol;  // Tolerance to use
  double tol2 = 2*tol;                // Twice the tolerance

  double d=0.0;  // Record of last p/q
  double e=0.0;  // Value of p/q in 2nd-to-last cycle of parabolic interp.

  // Loop until bracket sufficiently small
  while (std::fabs(x-m)>(tol2-0.5*(cx-ax)))
  {
    // Variables for parabolic interpolation
    double p=0.0,q=0.0,r=0.0;
    if (std::fabs(e)>tol)
    {
      // Fit a parabola
      r = (x-w)*(fx-fv);
      q = (x-v)*(fx-fw);
      p = (x-v)*q - (x-w)*r;
      q = 2*(q-r);
      if (q>0) p*=-1.0; else q*=-1.0;  // q always positive
      r = e; e = d;
    }

    if (std::fabs(p)<std::fabs(0.5*q*r) &&
        p>(q*(ax-x)) && p<(q*(cx-x)) )  // So that ax<x+p/q<cx
    {
      // Parabolic interpolation - set u to estimate of minimum
      d = p/q;
      u = x + d;

      // u must not be too close to ax or cx
      if (u-ax<tol2 || cx-u<tol2) d = (x<m?tol:-tol);
    }
    else
    {
      // Golden section step
      e = (x<m?cx:ax) - x;
      d = COMPL_GOLD * e;
    }

    // Do not evaluate too close to current x
    if (std::fabs(d)>=tol)
      u = x+d;
    else
    {
      if (d>0) u=x+tol;
      else     u=x-tol;
    }

    // Perform the function evaluation
    fu = f(u);

    // Update our current bounds
    if (fu<=fx)
    {
      if (u<x) cx=x; else ax=x;
      v=w; fv=fw;
      w=x; fw=fx;
      x=u; fx=fu;
    }
    else
    {
      if (u<x) ax=u; else cx=u;
      if (fu<=fw || w==x)
      {
        v=w; fv=fw;
        w=u; fw=fu;
      }
      else if (fu<=fv || v==x || v==w) { v=u; fv=fu; }
    }

    // Recompute mid-point and suitable tolerances
    m = 0.5*(ax+cx);  // Midpoint of (a,c)
    tol = EPS*std::fabs(x)+xtol;  // Tolerance to use
    tol2 = 2*tol;                // Twice the tolerance
  }

  f_at_last_minimum_ = fx;
  return x;
}

//: Find the minimum value of f(x) within a<= x <= c.
// The minimum x is the return value.
// You need to provide a bracket for the minimum (a<b<c s.t. f(a)>f(b)<f(c)),
// and the values fa=f(a), fb=f(b), fc=f(c). This avoids recalculating
// them if you have them already.
// The tolerance can be set using prior call to set_x_tolerance(tol).
// Use f_at_last_minimum() to get function evaluation at the returned minima.
double vnl_brent_minimizer::minimize_given_bounds_and_all_f(double ax, double bx, double cx,
                                                            double fa, double fb, double fc)
{
  // Check that the bracket is valid
  assert(ax<bx);
  assert(bx<cx);
  assert(fb<fa);
  assert(fb<fc);

  // Set up object to evaluate function as f(x)
  // Note that *f_ takes a vector input - f converts a scalar to a vector
  vnl_brent_minimizer_func f(*f_);

  double x=bx;  // Current best estimate of minimum
  double fx = fb;
  double w, fw;  // Next best point
  double v, fv;  // Third best point

  if (fa<fc) { w=ax; fw=fa;  v=cx; fv=fc; }
  else       { w=cx; fw=fc;  v=ax; fv=fa; }

  double u, fu;  // Most recently evaluated point and its value

  double m = 0.5*(ax+cx);  // Midpoint of (a,c)
  double tol = EPS*std::fabs(x)+xtol;  // Tolerance to use
  double tol2 = 2*tol;                // Twice the tolerance

  double d=std::min(bx-ax,cx-bx);  // Record of last p/q
  double e=std::max(bx-ax,cx-bx);  // Value of p/q in 2nd-to-last cycle of parabolic interp.

  // Loop until bracket sufficiently small
  while (std::fabs(x-m)>(tol2-0.5*(cx-ax)))
  {
    // Variables for parabolic interpolation
    double p=0.0,q=0.0,r=0.0;
    if (std::fabs(e)>tol)
    {
      // Fit a parabola
      r = (x-w)*(fx-fv);
      q = (x-v)*(fx-fw);
      p = (x-v)*q - (x-w)*r;
      q = 2*(q-r);
      if (q>0) p*=-1.0; else q*=-1.0;  // q always positive
      r = e; e = d;
    }

    if (std::fabs(p)<std::fabs(0.5*q*r) &&
        p>(q*(ax-x)) && p<(q*(cx-x)) )  // So that ax<x+p/q<cx
    {
      // Parabolic interpolation - set u to estimate of minimum
      d = p/q;
      u = x + d;

      // u must not be too close to ax or cx
      if (u-ax<tol2 || cx-u<tol2) d = (x<m?tol:-tol);
    }
    else
    {
      // Golden section step
      e = (x<m?cx:ax) - x;
      d = COMPL_GOLD * e;
    }

    // Do not evaluate too close to current x
    if (std::fabs(d)>=tol)
      u = x+d;
    else
    {
      if (d>0) u=x+tol;
      else     u=x-tol;
    }

    // Perform the function evaluation
    fu = f(u);

    // Update our current bounds
    if (fu<=fx)
    {
      if (u<x) cx=x; else ax=x;
      v=w; fv=fw;
      w=x; fw=fx;
      x=u; fx=fu;
    }
    else
    {
      if (u<x) ax=u; else cx=u;
      if (fu<=fw || w==x)
      {
        v=w; fv=fw;
        w=u; fw=fu;
      }
      else if (fu<=fv || v==x || v==w) { v=u; fv=fu; }
    }

    // Recompute mid-point and suitable tolerances
    m = 0.5*(ax+cx);  // Midpoint of (a,c)
    tol = EPS*std::fabs(x)+xtol;  // Tolerance to use
    tol2 = 2*tol;                // Twice the tolerance
  }

  f_at_last_minimum_ = fx;
  return x;
}

double vnl_brent_minimizer::minimize(double x)
{
  double ax=x-1.0;
  double cx=x+1.0;
  double fa,fx,fc;
  vnl_bracket_minimum(*f_, ax,x,cx, fa,fx,fc);

  return minimize_given_bounds_and_all_f(ax,x,cx,  fa,fx,fc);
}
