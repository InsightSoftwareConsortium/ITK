// This is core/vnl/algo/vnl_brent_minimizer.h
#ifndef vnl_brent_minimizer_h_
#define vnl_brent_minimizer_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \author Tim Cootes
// \date   Feb 2007
//
// \verbatim
// Modifications
// \endverbatim

#include <vnl/vnl_cost_function.h>
#include <vnl/vnl_nonlinear_minimizer.h>

struct vnl_brent_data;

//: Brent 1D minimizer
// Minimizes a 1D function using a cunning combination of golden section
// and parabolic interpolation.  It does not require derivatives to be
// supplied. It is guaranteed to find a minimum, and generally works
// efficiently - ie using few function evaluations.
//
// This implementation is based on that described by R.P. Brent
// in Chapter 5 of "Algorithms for Minimization Without Derivatives", 1973.
// In particular, is a C++ translation of the ALGOL program given at
// the end of that chapter.
//
// Example usage:
// \verbatim
// // Create 1D cost function
// class my_cost_fn : public vnl_cost_function {
//   my_cost_fn() : vnl_cost_function(1) {}
//
//   double f(const vnl_vector<double>& x)
//   { return (2 - x[0]) * (2 - x[0]) + 10; }
// };
//
//   my_cost_fn f1;
//   vnl_brent_minimizer brent(f1);
//
//   double initial_x = 3.5;
//  // Find the position of the minimum
//   double x = brent.minimize(initial_x);
//   double min_f = brent.f_at_last_minimum();
// \endverbatim
class vnl_brent_minimizer : public vnl_nonlinear_minimizer
{
 protected:
  vnl_cost_function* f_;
     //: Function evaluation at value returned by minimize(x)
  double f_at_last_minimum_;
 public:
  vnl_brent_minimizer(vnl_cost_function& functor);
 ~vnl_brent_minimizer();

  //: Find a minimum of f(x) near to ax.
  //  The evaluation of f(x) at the returned value can be obtained
  //  by a call to f_at_last_minimum();
  double minimize(double ax);

   //: Function evaluation at value returned by minimize(x)
  double f_at_last_minimum() const { return f_at_last_minimum_; }

  //: Find the minimum x of f(x) within a<= x <= c using pure golden section
  // \retval The position,x, of the minimum x.
  // You need to provide a bracket for the minimum (a<b<c s.t. f(a)>f(b)<f(c).
  // The tolerance can be set using prior call to set_x_tolerance(tol).
  // Use f_at_last_minimum() to get function evaluation at the returned minima.
  double minimize_golden(double ax, double bx, double cx,
                         double fa, double fb, double fc);

  //: Find the minimum value of f(x) within a<= x <= c.
  // \retval The position,x, of the minimum x.
  // You need to provide a bracket for the minimum (a<b<c s.t. f(a)>f(b)<f(c).
  // The tolerance can be set using prior call to set_x_tolerance(tol).
  // Use f_at_last_minimum() to get function evaluation at the returned minima.
  double minimize_given_bounds(double ax, double bx, double cx);

  //: Find the minimum value of f(x) within a<= x <= c.
  // \retval The position,x, of the minimum x.
  // You need to provide a bracket for the minimum (a<b<c s.t. f(a)>f(b)<f(c),
  // and the known value at b (fb=f(b)).
  // The tolerance can be set using prior call to set_x_tolerance(tol).
  // Use f_at_last_minimum() to get function evaluation at the returned minima.
  double minimize_given_bounds_and_one_f(double ax, double bx, double cx,
                                         double fb);

  //: Find the minimum value of f(x) within a<= x <= c.
  // \retval The position,x, of the minimum x.
  // You need to provide a bracket for the minimum (a<b<c s.t. f(a)>f(b)<f(c)),
  // and the values fa=f(a), fb=f(b), fc=f(c). This avoids recalculating
  // them if you have them already.  If you don't have them, it is
  // probably better to use minimize_given_bounds(a,b,c).
  //
  // The tolerance can be set using prior call to set_x_tolerance(tol).
  // Use f_at_last_minimum() to get function evaluation at the returned minima.
  double minimize_given_bounds_and_all_f(double ax, double bx, double cx,
                                         double fa, double fb, double fc);
};

#endif // vnl_brent_minimizer_h_
