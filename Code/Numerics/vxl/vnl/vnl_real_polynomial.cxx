#ifdef __GNUC__
#pragma implementation
#endif
//
// Class: vnl_real_polynomial
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 23 Aug 96
//
//-----------------------------------------------------------------------------

#include "vnl_real_polynomial.h"
#include <vnl/vnl_complex.h>

// This is replacing a member template...
template <class T> 
T vnl_real_polynomial_evaluate(const double *a, int n, const T& x)
{
  --n;
  T acc = a[n];
  T xn = x;
  
  do {
    acc += a[--n] * xn;
    xn *= x;
  } while (n);
  
  return acc;  
}

#ifdef WIN32
#define SELECT(T) <T >
#else
#define SELECT(T) 
#endif

// Instantiate templates before use
template double         vnl_real_polynomial_evaluate SELECT(double        )(const double*, int, const double        &);
template vnl_double_complex vnl_real_polynomial_evaluate SELECT(vnl_double_complex)(const double*, int, const vnl_double_complex&);

// -- Evaluate polynomial at value x
double vnl_real_polynomial::evaluate(double x) const
{
  return vnl_real_polynomial_evaluate SELECT(double)(coeffs_.data_block(), coeffs_.size(), x);
}

// -- Evaluate polynomial at complex value x
vnl_double_complex vnl_real_polynomial::evaluate(const vnl_double_complex& x) const
{
  return vnl_real_polynomial_evaluate SELECT(vnl_double_complex)
     (coeffs_.data_block(), coeffs_.size(), x);
}

// -- Evaluate derivative at value x. Not implemented.
double vnl_real_polynomial::devaluate(double /*x*/) const
{
  return HUGE_VAL;
}

// -- Evaluate derivative at complex value x. Not implemented.
vnl_double_complex vnl_real_polynomial::devaluate(const vnl_double_complex& /*x*/) const
{
  return HUGE_VAL;
}
