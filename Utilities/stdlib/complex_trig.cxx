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

// Trigonometric and hyperbolic functions for complex<float>, 
// complex<double>, and complex<long double>

#include <complex>
#include <math.h>
#include <float.h>

__STL_BEGIN_NAMESPACE

//----------------------------------------------------------------------
// sin

complex<float> sin(const complex<float>& z) {
  return complex<float>(::sinf(z._M_re) * ::coshf(z._M_im),
                        ::cosf(z._M_re) * ::sinhf(z._M_im));
}

complex<double> sin(const complex<double>& z) {
  return complex<double>(::sin(z._M_re) * ::cosh(z._M_im),
                         ::cos(z._M_re) * ::sinh(z._M_im));
}

complex<long double> sin(const complex<long double>& z) {
  return complex<long double>(::sinl(z._M_re) * ::coshl(z._M_im),
                              ::cosl(z._M_re) * ::sinhl(z._M_im));
}


//----------------------------------------------------------------------
// cos

complex<float> cos(const complex<float>& z) {
  return complex<float>(::cosf(z._M_re) * ::coshf(z._M_im),
                        -::sinf(z._M_re) * ::sinhf(z._M_im));
}

complex<double> cos(const complex<double>& z) {
  return complex<double>(::cos(z._M_re) * ::cosh(z._M_im),
                        -::sin(z._M_re) * ::sinh(z._M_im));
}

complex<long double> cos(const complex<long double>& z) {
  return complex<long double>(::cosl(z._M_re) * ::coshl(z._M_im),
                              -::sinl(z._M_re) * ::sinhl(z._M_im));
}


//----------------------------------------------------------------------
// tan

complex<float> tan(const complex<float>& z) {
  float re2 = 2.f * z._M_re;
  float im2 = 2.f * z._M_im;

#ifdef __sgi
  const union { unsigned int i; float f; } ulimit = { 0x42b2d4fc };
  const float limit = ulimit.f;
#else
  const float limit = logf(FLT_MAX);
#endif

  if (fabsf(im2) > limit)
    return complex<float>(0.f, (im2 > 0 ? 1.f : -1.f));
  else {
    float den = ::cosf(re2) + ::coshf(im2);
    return complex<float>(::sinf(re2) / den, ::sinhf(im2) / den);
  }
}

complex<double> tan(const complex<double>& z) {
  double re2 = 2. * z._M_re;
  double im2 = 2. * z._M_im;

#ifdef __sgi
  union {
    struct { unsigned int h; unsigned int l; } w;
    double d;
  } ulimit = { 0x408633ce, 0x8fb9f87d };
  const double limit = ulimit.d;
#else
  const double limit = ::log(DBL_MAX);
#endif

  if (fabs(im2) > limit)
    return complex<double>(0., (im2 > 0 ? 1. : -1.));
  else {
    double den = ::cos(re2) + ::cosh(im2);
    return complex<double>(::sin(re2) / den, ::sinh(im2) / den);
  }
}

complex<long double> tan(const complex<long double>& z) {
  long double re2 = 2.l * z._M_re;
  long double im2 = 2.l * z._M_im;

#ifdef __sgi
  union {
    struct { unsigned int h[2]; unsigned int l[2]; } w;
    long double ld;
  } ulimit = {0x408633ce, 0x8fb9f87e, 0xbd23b659, 0x4e9bd8b1};
  const long double limit = ulimit.ld;
#else
  const long double limit = logl(LDBL_MAX);
#endif

  if (fabsl(im2) > limit)
    return complex<long double>(0.l, (im2 > 0 ? 1.l : -1.l));
  else {
    long double den = ::cosl(re2) + ::coshl(im2);
    return complex<long double>(::sinl(re2) / den, ::sinhl(im2) / den);
  }
}


//----------------------------------------------------------------------
// sinh

complex<float> sinh(const complex<float>& z) {
  return complex<float>(::sinhf(z._M_re) * ::cosf(z._M_im),
                        ::coshf(z._M_re) * ::sinf(z._M_im));
}

complex<double> sinh(const complex<double>& z) {
  return complex<double>(::sinh(z._M_re) * ::cos(z._M_im),
                         ::cosh(z._M_re) * ::sin(z._M_im));
}

complex<long double> sinh(const complex<long double>& z) {
  return complex<long double>(::sinhl(z._M_re) * ::cosl(z._M_im),
                              ::coshl(z._M_re) * ::sinl(z._M_im));
}


//----------------------------------------------------------------------
// cosh

complex<float> cosh(const complex<float>& z) {
  return complex<float>(::coshf(z._M_re) * ::cosf(z._M_im),
                        ::sinhf(z._M_re) * ::sinf(z._M_im));
}

complex<double> cosh(const complex<double>& z) {
  return complex<double>(::cosh(z._M_re) * ::cos(z._M_im),
                         ::sinh(z._M_re) * ::sin(z._M_im));
}

complex<long double> cosh(const complex<long double>& z) {
  return complex<long double>(::coshl(z._M_re) * ::cosl(z._M_im),
                              ::sinhl(z._M_re) * ::sinl(z._M_im));
}


//----------------------------------------------------------------------
// tanh

complex<float> tanh(const complex<float>& z) {
  float re2 = 2.f * z._M_re;
  float im2 = 2.f * z._M_im;

#ifdef __sgi
  const union { unsigned int i; float f; } ulimit = { 0x42b2d4fc };
  const float limit = ulimit.f;
#else
  const float limit = logf(FLT_MAX);
#endif

  if (fabsf(re2) > limit)
    return complex<float>((re2 > 0 ? 1.f : -1.f), 0.f);
  else {
    float den = ::coshf(re2) + ::cosf(im2);
    return complex<float>(::sinhf(re2) / den, ::sinf(im2) / den);
  }
}

complex<double> tanh(const complex<double>& z) {
  double re2 = 2. * z._M_re;
  double im2 = 2. * z._M_im;

#ifdef __sgi
  union {
    struct { unsigned int h; unsigned int l; } w;
    double d;
  } ulimit = { 0x408633ce, 0x8fb9f87d };
  const double limit = ulimit.d;
#else
  const double limit = ::log(DBL_MAX);
#endif
  
  if (fabs(re2) > limit)
    return complex<double>((re2 > 0 ? 1. : -1.), 0.);
  else {
    double den = ::cosh(re2) + ::cos(im2);
    return complex<double>(::sinh(re2) / den, ::sin(im2) / den);
  }
}

complex<long double> tanh(const complex<long double>& z) {
  long double re2 = 2.l * z._M_re;
  long double im2 = 2.l * z._M_im;

#ifdef __sgi
  union {
    struct { unsigned int h[2]; unsigned int l[2]; } w;
    long double ld;
  } ulimit = {0x408633ce, 0x8fb9f87e, 0xbd23b659, 0x4e9bd8b1};
  const long double limit = ulimit.ld;
#else
  const long double limit = logl(LDBL_MAX);
#endif

  if (fabsl(re2) > limit)
    return complex<long double>((re2 > 0 ? 1.l : -1.l), 0.l);
  else {
    long double den = ::coshl(re2) + ::cosl(im2);
    return complex<long double>(::sinhl(re2) / den, ::sinl(im2) / den);
  }
}

__STL_END_NAMESPACE
