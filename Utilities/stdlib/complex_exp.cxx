/*
 * Copyright (c) 1997-1998
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */ 

// exp, log, pow for complex<float>, complex<double>, and complex<long double>

#include <complex>
#include <numeric>
#include <math.h>

__STL_BEGIN_NAMESPACE


//----------------------------------------------------------------------
// exp

complex<float> exp(const complex<float>& z)
{
  float expx = ::expf(z._M_re);
  return complex<float>(expx * ::cosf(z._M_im),
                        expx * ::sinf(z._M_im));
}

complex<double> exp(const complex<double>& z)
{
  double expx = ::exp(z._M_re);
  return complex<double>(expx * ::cos(z._M_im),
                         expx * ::sin(z._M_im));
}

complex<long double> exp(const complex<long double>& z)
{
  long double expx = expl(z._M_re);
  return complex<long double>(expx * ::cosl(z._M_im),
                              expx * ::sinl(z._M_im));
}

//----------------------------------------------------------------------
// log10

complex<float> log10(const complex<float>& z)
{
  complex<float> r;
  static float ln10_inv = 1.f / ::logf(10.f);

  r._M_im = ::atan2f(z._M_im, z._M_re) * ln10_inv;
  r._M_re = ::log10f(::hypotf(z._M_re, z._M_im));
  return r;
}

complex<double> log10(const complex<double>& z)
{
  complex<double> r;
  static double ln10_inv = 1. / ::log(10.);

  r._M_im = ::atan2(z._M_im, z._M_re) * ln10_inv;
  r._M_re = ::log10(::hypot(z._M_re, z._M_im));
  return r;
}

complex<long double> log10(const complex<long double>& z)
{
  complex<long double> result;
  static long double ln10_inv = 1.l / ::logl(10.l);

  result._M_im = ::atan2l(z._M_im, z._M_re) * ln10_inv;
  result._M_re = ::log10l(::hypotl(z._M_re, z._M_im));
  return result;
}


//----------------------------------------------------------------------
// log

complex<float> log(const complex<float>& z)
{
  complex<float> r;

  r._M_im = ::atan2f(z._M_im, z._M_re);
  r._M_re = ::logf(::hypotf(z._M_re, z._M_im));
  return r;
}

complex<double> log(const complex<double>& z)
{
  complex<double> r;

  r._M_im = ::atan2(z._M_im, z._M_re);
  r._M_re = ::log(::hypot(z._M_re, z._M_im));
  return r;
}

complex<long double> log(const complex<long double>& z)
{
  complex<long double> result;

  result._M_im = ::atan2l(z._M_im, z._M_re);
  result._M_re = ::logl(::hypotl(z._M_re, z._M_im));
  return result;
}


//----------------------------------------------------------------------
// pow

complex<float> pow(const float& a, const complex<float>& b) {
  float logr = ::logf(a);
  float x = ::expf(logr*b._M_re);
  float y = logr*b._M_im;

  return complex<float>(x * ::cosf(y), x * ::sinf(y));
}

complex<float> pow(const complex<float>& z, int n) {
  if (n < 0)
    return 1.f / __power(z, n, multiplies< complex<float> >());
  else
    return __power(z, n, multiplies< complex<float> >());
}

complex<float> pow(const complex<float>& a, const float& b) {
  float logr = ::logf(::hypotf(a._M_re,a._M_im));
  float logi = ::atan2f(a._M_im, a._M_re);
  float x = ::expf(logr * b);
  float y = logi * b;

  return complex<float>(x * ::cosf(y), x * ::sinf(y));
}  

complex<float> pow(const complex<float>& a, const complex<float>& b) {
  float logr = logf(::hypotf(a._M_re,a._M_im));
  float logi = ::atan2f(a._M_im, a._M_re);
  float x = ::expf(logr*b._M_re - logi*b._M_im);
  float y = logr*b._M_im + logi*b._M_re;

  return complex<float>(x * ::cosf(y), x * ::sinf(y));
}


complex<double> pow(const double& a, const complex<double>& b) {
  double logr = ::log(a);
  double x = ::exp(logr*b._M_re);
  double y = logr*b._M_im;

  return complex<double>(x * ::cos(y), x * ::sin(y));
}

complex<double> pow(const complex<double>& z, int n) {
  if (n < 0)
    return 1. / __power(z, n, multiplies< complex<double> >());
  else
    return __power(z, n, multiplies< complex<double> >());
}

complex<double> pow(const complex<double>& a, const double& b) {
  double logr = ::log(::hypot(a._M_re,a._M_im));
  double logi = ::atan2(a._M_im, a._M_re);
  double x = ::exp(logr * b);
  double y = logi * b;

  return complex<double>(x * ::cos(y), x * ::sin(y));
}  

complex<double> pow(const complex<double>& a, const complex<double>& b) {
  double logr = ::log(::hypot(a._M_re,a._M_im));
  double logi = ::atan2(a._M_im, a._M_re);
  double x = ::exp(logr*b._M_re - logi*b._M_im);
  double y = logr*b._M_im + logi*b._M_re;

  return complex<double>(x * ::cos(y), x * ::sin(y));
}


complex<long double> pow(const long double& a,
                         const complex<long double>& b) {
  long double logr = ::logl(a);
  long double x = ::expl(logr*b._M_re);
  long double y = logr*b._M_im;

  return complex<long double>(x * ::cosl(y), x * ::sinl(y));
}

complex<long double> pow(const complex<long double>& z, int n) {
  if (n < 0)
    return 1.l / __power(z, n, multiplies< complex<long double> >());
  else
    return __power(z, n, multiplies< complex<long double> >());
}

complex<long double> pow(const complex<long double>& a,
                         const long double& b) {
  long double logr = ::logl(::hypotl(a._M_re,a._M_im));
  long double logi = ::atan2l(a._M_im, a._M_re);
  long double x = ::expl(logr * b);
  long double y = logi * b;

  return complex<long double>(x * ::cosl(y), x * ::sinl(y));
}  

complex<long double> pow(const complex<long double>& a,
                         const complex<long double>& b) {
  long double logr = ::logl(::hypotl(a._M_re,a._M_im));
  long double logi = ::atan2l(a._M_im, a._M_re);
  long double x = ::expl(logr*b._M_re - logi*b._M_im);
  long double y = logr*b._M_im + logi*b._M_re;

  return complex<long double>(x * ::cosl(y), x * ::sinl(y));
}

__STL_END_NAMESPACE
