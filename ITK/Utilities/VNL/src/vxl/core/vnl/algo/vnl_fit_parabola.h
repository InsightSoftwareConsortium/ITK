// This is core/vnl/algo/vnl_fit_parabola.h
#ifndef vnl_fit_parabola_h_
#define vnl_fit_parabola_h_

//:
// \file
// \brief Function to fit a parabola to three point to predict the centre line
// \author Tim Cootes
// \date   Feb 2007
//
// \verbatim
//  Modifications
// \endverbatim

//: Fit a parabola so as to estimate the position of the centre line
//  The centre (maxima or minima) lies at xb + p/q.
//  If q is near zero, then the parabola is nearly flat
inline void vnl_fit_parabola(double xa, double xb, double xc,
                             double fa, double fb, double fc,
                             double& p, double& q)
{
  // Effectively shift origin to (xb,fb)
  // Parabola is then y=a*x*x+b*x
  // Centre is then at -b/2a = p/q in the following
  double x1=xa-xb, f1 = fa-fb;
  double x2=xc-xb, f2 = fc-fb;
  p = x2*x2*f1-x1*x1*f2;
  q = 2*(x2*f1-x1*f2);
}

#endif // vnl_fit_parabola_h_
