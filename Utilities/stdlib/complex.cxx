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

#include <complex>
#include <math.h>

__STL_BEGIN_NAMESPACE

// Absolute value

float abs(const complex<float>& __z) 
{
  return ::hypotf(__z._M_re, __z._M_im);
}

double abs(const complex<double>& __z) 
{
  return ::hypot(__z._M_re, __z._M_im);
}

long double abs(const complex<long double>& __z) 
{
  return ::hypotl(__z._M_re, __z._M_im);
}

// Phase

float arg(const complex<float>& __z) 
{
  return ::atan2f(__z._M_im, __z._M_re);
}

double arg(const complex<double>& __z) 
{
  return ::atan2(__z._M_im, __z._M_re);
}

long double arg(const complex<long double>& __z) 
{
  return ::atan2l(__z._M_im, __z._M_re);
}

// Construct a complex number from polar representation

complex<float> polar(const float& __rho, const float& __phi) 
{
  return complex<float>(__rho * ::cosf(__phi), __rho * ::sinf(__phi));
}

complex<double> polar(const double& __rho, const double& __phi) 
{
  return complex<double>(__rho * ::cos(__phi), __rho * ::sin(__phi));
}

complex<long double> polar(const long double& __rho, const long double& __phi)
{
  return complex<long double>(__rho * ::cosl(__phi), __rho * ::sinl(__phi));
}

// Division

void complex<float>::_div(const float& __z1_r, const float& __z1_i,
                          const float& __z2_r, const float& __z2_i,
                          float& __res_r, float& __res_i) {
  float __ar = __z2_r >= 0 ? __z2_r : -__z2_r;
  float __ai = __z2_i >= 0 ? __z2_i : -__z2_i;

  if (__ar <= __ai) {
    float __ratio = __z2_r / __z2_i;
    float __denom = __z2_i * (1 + __ratio * __ratio);
    __res_r = (__z1_r * __ratio + __z1_i) / __denom;
    __res_i = (__z1_i * __ratio - __z1_r) / __denom;
  }
  else {
    float __ratio = __z2_i / __z2_r;
    float __denom = __z2_r * (1 + __ratio * __ratio);
    __res_r = (__z1_r + __z1_i * __ratio) / __denom;
    __res_i = (__z1_i - __z1_r * __ratio) / __denom;
  }
}

void complex<float>::_div(const float& __z1_r,
                          const float& __z2_r, const float& __z2_i,
                          float& __res_r, float& __res_i) {
  float __ar = __z2_r >= 0 ? __z2_r : -__z2_r;
  float __ai = __z2_i >= 0 ? __z2_i : -__z2_i;

  if (__ar <= __ai) {
    float __ratio = __z2_r / __z2_i;
    float __denom = __z2_i * (1 + __ratio * __ratio);
    __res_r = (__z1_r * __ratio) / __denom;
    __res_i = - __z1_r / __denom;
  }
  else {
    float __ratio = __z2_i / __z2_r;
    float __denom = __z2_r * (1 + __ratio * __ratio);
    __res_r = __z1_r / __denom;
    __res_i = - (__z1_r * __ratio) / __denom;
  }
}


void complex<double>::_div(const double& __z1_r, const double& __z1_i,
                           const double& __z2_r, const double& __z2_i,
                           double& __res_r, double& __res_i) {
  double __ar = __z2_r >= 0 ? __z2_r : -__z2_r;
  double __ai = __z2_i >= 0 ? __z2_i : -__z2_i;

  if (__ar <= __ai) {
    double __ratio = __z2_r / __z2_i;
    double __denom = __z2_i * (1 + __ratio * __ratio);
    __res_r = (__z1_r * __ratio + __z1_i) / __denom;
    __res_i = (__z1_i * __ratio - __z1_r) / __denom;
  }
  else {
    double __ratio = __z2_i / __z2_r;
    double __denom = __z2_r * (1 + __ratio * __ratio);
    __res_r = (__z1_r + __z1_i * __ratio) / __denom;
    __res_i = (__z1_i - __z1_r * __ratio) / __denom;
  }
}

void complex<double>::_div(const double& __z1_r,
                           const double& __z2_r, const double& __z2_i,
                           double& __res_r, double& __res_i) {
  double __ar = __z2_r >= 0 ? __z2_r : -__z2_r;
  double __ai = __z2_i >= 0 ? __z2_i : -__z2_i;

  if (__ar <= __ai) {
    double __ratio = __z2_r / __z2_i;
    double __denom = __z2_i * (1 + __ratio * __ratio);
    __res_r = (__z1_r * __ratio) / __denom;
    __res_i = - __z1_r / __denom;
  }
  else {
    double __ratio = __z2_i / __z2_r;
    double __denom = __z2_r * (1 + __ratio * __ratio);
    __res_r = __z1_r / __denom;
    __res_i = - (__z1_r * __ratio) / __denom;
  }
}


void complex<long double>
  ::_div(const long double& __z1_r, const long double& __z1_i,
         const long double& __z2_r, const long double& __z2_i,
         long double& __res_r, long double& __res_i) {
  long double __ar = __z2_r >= 0 ? __z2_r : -__z2_r;
  long double __ai = __z2_i >= 0 ? __z2_i : -__z2_i;

  if (__ar <= __ai) {
    long double __ratio = __z2_r / __z2_i;
    long double __denom = __z2_i * (1 + __ratio * __ratio);
    __res_r = (__z1_r * __ratio + __z1_i) / __denom;
    __res_i = (__z1_i * __ratio - __z1_r) / __denom;
  }
  else {
    long double __ratio = __z2_i / __z2_r;
    long double __denom = __z2_r * (1 + __ratio * __ratio);
    __res_r = (__z1_r + __z1_i * __ratio) / __denom;
    __res_i = (__z1_i - __z1_r * __ratio) / __denom;
  }
}


void complex<long double>
  ::_div(const long double& __z1_r,
         const long double& __z2_r, const long double& __z2_i,
         long double& __res_r, long double& __res_i) {
  long double __ar = __z2_r >= 0 ? __z2_r : -__z2_r;
  long double __ai = __z2_i >= 0 ? __z2_i : -__z2_i;

  if (__ar <= __ai) {
    long double __ratio = __z2_r / __z2_i;
    long double __denom = __z2_i * (1 + __ratio * __ratio);
    __res_r = (__z1_r * __ratio) / __denom;
    __res_i = - __z1_r / __denom;
  }
  else {
    long double __ratio = __z2_i / __z2_r;
    long double __denom = __z2_r * (1 + __ratio * __ratio);
    __res_r = __z1_r / __denom;
    __res_i = - (__z1_r * __ratio) / __denom;
  }
}

//----------------------------------------------------------------------
// Square root


complex<float> sqrt(const complex<float>& z) {
  float re = z._M_re;
  float im = z._M_im;
  float mag = ::hypotf(re, im);
  complex<float> result;

  if (mag == 0.) {
    result._M_re = result._M_im = 0.f;
  } else if (re > 0.f) {
    result._M_re = ::sqrtf(0.5f * (mag + re));
    result._M_im = im/result._M_re/2.f;
  } else {
    result._M_im = ::sqrtf(0.5f * (mag - re));
    if (im < 0.f)
      result._M_im = - result._M_im;
    result._M_re = im/result._M_im/2.f;
  }
  return result;
}


complex<double> sqrt(const complex<double>& z) {
  double re = z._M_re;
  double im = z._M_im;
  double mag = ::hypot(re, im);
  complex<double> result;

  if (mag == 0.) {
    result._M_re = result._M_im = 0.;
  } else if (re > 0.) {
    result._M_re = ::sqrt(0.5 * (mag + re));
    result._M_im = im/result._M_re/2;
  } else {
    result._M_im = ::sqrt(0.5 * (mag - re));
    if (im < 0.)
      result._M_im = - result._M_im;
    result._M_re = im/result._M_im/2;
  }
  return result;
}


complex<long double> sqrt(const complex<long double>& z) {
  long double re = z._M_re;
  long double im = z._M_im;
  long double mag = ::hypotl(re, im);
  complex<long double> result;

  if (mag == 0.L) {
    result._M_re = result._M_im = 0.L;
  } else if (re > 0.L) {
    result._M_re = ::sqrtl(0.5L * (mag + re));
    result._M_im = (im/result._M_re) * .5L;
  } else {
    result._M_im = ::sqrtl(0.5L * (mag - re));
    if (im < 0.L)
      result._M_im = - result._M_im;
    result._M_re = (im/result._M_im) * .5L;
  }
  return result;
}

__STL_END_NAMESPACE
