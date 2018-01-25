// This is core/vnl/vnl_gamma.h
#ifndef vnl_gamma_h_
#define vnl_gamma_h_
//:
//  \file
//  \brief Complete and incomplete gamma function approximations
//  \author Tim Cootes

#include <cmath>
#include <vcl_compiler.h>
#include "vnl/vnl_export.h"

//: Approximate log of gamma function
//  Uses 6 parameter Lanczos approximation as described by Toth
//  (http://www.rskey.org/gamma.htm)
//  Accurate to about one part in 3e-11.
VNL_EXPORT double vnl_log_gamma(double x);

//: Approximate gamma function
//  Uses 6 parameter Lanczos approximation as described by Toth
//  (http://www.rskey.org/gamma.htm)
//  Accurate to about one part in 3e-11.
inline double vnl_gamma(double x) { return std::exp(vnl_log_gamma(x)); }

//: Normalised Incomplete gamma function, P(a,x)
// $P(a,x)=\frac{1}{\Gamma(a)}\int_0^x e^{-t}t^{a-1}dt$
// Note the order of parameters - this is the normal maths order.
// MATLAB uses gammainc(x,a), ie the other way around
VNL_EXPORT double vnl_gamma_p(double a, double x);

//:Normalised Incomplete gamma function, Q(a,x)
// $Q(a,x)=\frac{1}{\Gamma(a)}\int_x^{\infty}e^{-t}t^{a-1}dt$
VNL_EXPORT double vnl_gamma_q(double a, double x);

//: P(chi<chi2)
// Calculates the probability that a value generated
// at random from a chi-square distribution with given
// degrees of freedom is less than the value chi2
// \param n_dof  Number of degrees of freedom
// \param chi2  Value of chi-squared
inline double vnl_cum_prob_chi2(int n_dof, double chi2)
{
  return vnl_gamma_p( n_dof*0.5 , chi2*0.5 );
}
//: approximate digamma function, dLog[gamma[z]]/dz
// Analytic derivative of the Lanczos approximation. Error < 10^-11  1<z<20.
VNL_EXPORT double vnl_digamma(double x);

#endif // vnl_gamma_h_
