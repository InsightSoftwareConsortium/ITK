// This is core/vnl/algo/vnl_brent.h
#ifndef vnl_brent_h_
#define vnl_brent_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \author awf@robots.ox.ac.uk
// \date   07 Dec 00
//
// \verbatim
//  Modifications
//   31 May 2001 Ian Scott (Manchester). Added some documentation
//   31 May 2001 Ian Scott (Manchester). Added minimize_given_bounds_and_1st_f
// \endverbatim

#include <vnl/vnl_cost_function.h>
#include <vnl/algo/vnl_brent_minimizer.h>
#include <vnl/algo/vnl_algo_export.h>

//: Brent 1D minimizer (deprecated)
//
// Please use vnl_brent_minimizer instead.
//
// This routine used to contain copyrighted code, and is deprecated.
// It is now simply a wrapper around vnl_brent_minimizer.
class VNL_ALGO_EXPORT vnl_brent : public vnl_brent_minimizer
{
 public:
  vnl_brent(vnl_cost_function* functor);
 ~vnl_brent();

  //: Find a minimum of f(x) near to ax.
  double minimize(double ax);

  //: Find the minimum value of f(x) within a<= x <= c.
  // The minimum value is the return value, and *xmin the relevant value of x.
  // You need to provide a bracket for the minimum
  // Also returns fa = f(a), etc.
  double minimize_given_bounds(double ax, double bx, double cx,
                               double tol,
                               double *xmin);

  //: Save time over minimize_given_bounds() if you know f(b)
  // This function avoids a single computation of f, if you already know
  // it.
  double minimize_given_bounds_and_1st_f(double ax, double bx, double fb,
                                         double cx,  double tol, double *xmin);

  //: Given distinct points ax, and bx, find a bracket for the minimum.
  // Return a bracket ax > bx > cx, f(b) < f(a), f(b) < f(c) for minimum.
  // Also returns fa = f(a), etc.
  //
  // You should probably use vnl_bracket_minimum instead of this function.
  void bracket_minimum(double *ax, double *bx, double *cx,
                       double *fa, double *fb, double *fc);

  //: Given distinct points ax, and bx, find a bracket for the minimum.
  // Return a bracket ax > bx > cx, f(b) < f(a), f(b) < f(c) for minimum.
  //
  // You should probably use vnl_bracket_minimum instead of this function.
  void bracket_minimum(double *ax, double *bx, double *cx);
};

#endif // vnl_brent_h_
