// This is vxl/vnl/vnl_complex.h

//-*- c++ -*-------------------------------------------------------------------
#ifndef vnl_complex_h_
#define vnl_complex_h_
#ifdef __GNUC__
#pragma interface
#endif

//: \file
//  \brief Complex additions to vnl_math.
//
//     We don't want everyone to pay for complex when they don't need it, as
//     its ratio of expense to frequency of use is high. So we define those
//     functions from vnl_math which use complex here instead.
//     In a sense, vnl_math should be a namespace, and this file adds to that
//     namespace.

// Modifications
// LSB (Manchester) 26/3/01 Tidied documentation

#include <vcl_cmath.h>
#include <vcl_complex.h>
#include <vnl/vnl_math.h>

// these function could have been templated, if not for the
// broken overload resolution of SGI CC 7.2.x -- fsm

#define macro(T) \
inline bool vnl_math_isnan(vcl_complex<T> const& z) { return vnl_math_isnan(z.real()) || vnl_math_isnan(z.imag()); } \
inline bool vnl_math_isfinite(vcl_complex<T> const& z) { return vnl_math_isfinite(z.real()) && vnl_math_isfinite(z.imag()); } \
inline T vnl_math_abs(vcl_complex<T> const& z) { return vcl_abs(z); } \
inline vcl_complex<T> vnl_math_sqr(vcl_complex<T> const& z) { return z*z; } \
inline T vnl_math_squared_magnitude(vcl_complex<T> const& z) { return vcl_norm(z); }
macro(float)
macro(double)
macro(long double)
#undef macro

// // isinf
// template <class T> inline
// bool vnl_math_isinf(const vcl_complex<T>& z)
// {
//   reutrn vnl_math_isinf(z.real()) || vnl_math_isinf(z.imag());
// }

#endif // vnl_complex_h_
