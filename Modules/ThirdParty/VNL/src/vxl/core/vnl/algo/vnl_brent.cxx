// This is core/vnl/algo/vnl_brent.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif

#include "vnl_brent.h"

#include <vcl_cassert.h>

#include <vnl/algo/vnl_bracket_minimum.h>


vnl_brent::vnl_brent(vnl_cost_function* functor)
  : vnl_brent_minimizer( *functor )
{
}

vnl_brent::~vnl_brent()
{
}

double vnl_brent::minimize_given_bounds(double ax, double bx, double cx,
                                        double tol,
                                        double *xmin)
{
  assert( xmin != NULL );
  this->set_x_tolerance( tol );
  *xmin = vnl_brent_minimizer::minimize_given_bounds( ax, bx, cx );
  return vnl_brent_minimizer::f_at_last_minimum();
}

double vnl_brent::minimize_given_bounds_and_1st_f(double ax, double bx,
                                                  double fb, double cx,
                                                  double tol, double *xmin)
{
  assert( xmin != NULL );
  this->set_x_tolerance( tol );
  *xmin = vnl_brent_minimizer::minimize_given_bounds_and_one_f( ax, bx, cx, fb );
  return vnl_brent_minimizer::f_at_last_minimum();
}


void vnl_brent::bracket_minimum(double *ax, double *bx, double *cx)
{
  double fa, fb, fc;
  bracket_minimum(ax,bx,cx,&fa,&fb,&fc);
}

void vnl_brent::bracket_minimum(double *ax, double *bx, double *cx,
                                double *fa, double *fb, double *fc)
{
  vnl_bracket_minimum( *f_, *cx, *bx, *ax, *fc, *fb, *fa );
}


double vnl_brent::minimize(double x)
{
  double ax=x-1.0;
  double xx=x+1.0;
  double bx = 0.0;
  double fa,fx,fb;
  bracket_minimum(&ax,&xx,&bx,&fa,&fx,&fb);
  minimize_given_bounds(bx,xx,ax,ftol,&x);
  return x;
}
