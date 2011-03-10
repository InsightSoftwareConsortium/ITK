// This is core/vnl/vnl_bessel.cxx
#include "vnl_bessel.h"
#include <vcl_algorithm.h>

//:
// \file
// \brief Bessel functions of the first kind
// \author Tim Cootes

//: Compute Bessel functions of first kind up to order n_max.
//  On exit, J[i] = J_i(x) for i=0..n_max
//
// Uses recurrence relation: J_(n-1)(x)+J_(n+1)=(2n/x)J_n(x)
// Thus J_n(x) = (2(n+1)/x)J_(n+1)(x)  - J_(n+2)(x)
// Start with arbitrary guess for high n and work backwards.
// Normalise suitably.
void vnl_bessel(unsigned n_max, double x, vnl_vector<double>& J)
{
  if (x==0.0)
  {
    J.set_size(1+n_max);
    J.fill(0.0);
    J[0]=1.0;
    return;
  }
  int nhi = 2*((vcl_max(int(n_max),int(x))+15)/2+1);
  vnl_vector<double> j(nhi+1);
  j[nhi]=0.0;
  j[nhi-1]=1.0;
  for (int m=nhi-2; m>=0; --m)
    j[m]=2*(m+1)*j[m+1]/x - j[m+2];

  // Normalise and return first (1+n_max) values
  double sum=j[0];
  for (int m=2;m<=nhi;m+=2) sum+=2*j[m];

  J.set_size(1+n_max);
  for (unsigned int m=0; m<=n_max; ++m) J[m]=j[m]/sum;
}

//: Returns J_0(x), the value of the Bessel function of order 0 at x.
//  Bessel function of the first kind of order zero.
//
// Uses recurrence relation: J_(n-1)(x)+J_(n+1)=(2n/x)J_n(x)
double vnl_bessel0(double x)
{
  if (x==0) return 1.0;
  int nhi = 2*((int(x)+15)/2);  // Even
  double j3=0.0;
  double j2=1.0;
  double j0=j2,j1;
  double even_sum=j2;
  for (int i=nhi;i>=0;i-=2)
  {
    // j0 is i-th term, j1 is i+1-th etc
    j1=2*(i+2)*j2/x - j3;
    j0=2*(i+1)*j1/x - j2;
    even_sum+=j0;
    j3=j1;
    j2=j0;
  }
  return j0/(2*even_sum-j0);
}

//: Returns J_n(x), the value of the Bessel function of order n at x.
//  Bessel function of the first kind of order zero.
//
// Uses recurrence relation: J_(n-1)(x)+J_(n+1)=(2n/x)J_n(x)
double vnl_bessel(unsigned n, double x)
{
  if (x==0)
  {
    if (n==0) return 1.0;
    else    return 0.0;
  }

  int nhi = 2*((vcl_max(int(n),int(x))+15)/2+1);
  double j3=0.0;
  double j2=1.0;
  double j0=j2,j1;
  double even_sum=j2;
  double jn=j0;
  for (int i=nhi; i>=0; i-=2)
  {
    // j0 is i-th term, j1 is i+1-th etc
    j1=2*(i+2)*j2/x - j3;
    j0=2*(i+1)*j1/x - j2;
    even_sum+=j0;
    j3=j1;
    j2=j0;
    if ((unsigned int)i==n) jn=j0;
    else if ((unsigned int)i+1==n) jn=j1;
  }
  return jn/(2*even_sum-j0);
}
