//-*- c++ -*-------------------------------------------------------------------
#ifndef vnl_brent_h_
#define vnl_brent_h_
#ifdef __GNUC__
#pragma interface
#endif
// Author: awf@robots.ox.ac.uk
// Created: 07 Dec 00

#include <vnl/vnl_cost_function.h>
#include <vnl/vnl_nonlinear_minimizer.h>

struct vnl_brent_data;

//: Brent 1D minimizer
class vnl_brent : public vnl_nonlinear_minimizer {
public:
  vnl_brent(vnl_cost_function* functor);
 ~vnl_brent();

  double minimize(double x);
  double minimize_given_bounds(double ax, double bx, double cx,
			       double tol, 
			       double *xmin);
  void bracket_minimum(double *ax, double *bx, double *cx, 
		       double *fa, double *fb, double *fc);

  void bracket_minimum(double *ax, double *bx, double *cx);

protected:
  vnl_brent_data *p;
};

#endif
