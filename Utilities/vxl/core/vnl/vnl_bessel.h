// This is core/vnl/vnl_bessel.h
#ifndef vnl_bessel_h_
#define vnl_bessel_h_
//:
//  \file
//  \brief Bessel functions of the first kind
//  \author Tim Cootes

#include <vnl/vnl_vector.h>

//: Returns J_n(x), the value of the Bessel function of order n at x
//  Bessel function of the first kind of order n
double vnl_bessel(unsigned n, double x);

//: Returns J_0(x), the value of the Bessel function of order 0 at x
//  Bessel function of the first kind of order zero
double vnl_bessel0(double x);

//: Compute Bessel functions of first kind up to order n_max
//  On exit, J[i] = J_i(x) for i=0..n_max
void vnl_bessel(unsigned n_max, double x, vnl_vector<double>& J);

#endif // vnl_bessel_h_
