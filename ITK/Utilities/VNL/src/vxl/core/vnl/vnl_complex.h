// This is core/vnl/vnl_complex.h
#ifndef vnl_complex_h_
#define vnl_complex_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Complex additions to vnl_math.
//
// We don't want everyone to pay for complex when they don't need it, as
// its ratio of expense to frequency of use is high. So we define those
// functions from vnl_math which use complex here instead.
// In a sense, vnl_math should be a namespace, and this file adds to that
// namespace.
//
// \verbatim
//  Modifications
//   LSB (Manchester) 26/3/01 Tidied documentation
// \endverbatim

#include <vcl_cmath.h> // for sqrt(double)
#include <vcl_complex.h>
#include <vcl_iosfwd.h>
#include <vnl/vnl_math.h>

// these function could have been templated, if not for the
// broken overload resolution of SGI CC 7.2.x -- fsm

#define macro(T) \
inline bool vnl_math_isnan(vcl_complex<T >const& z){return vnl_math_isnan(vcl_real(z)) || vnl_math_isnan(vcl_imag(z));} \
inline bool vnl_math_isfinite(vcl_complex<T >const& z){return vnl_math_isfinite(vcl_real(z)) && vnl_math_isfinite(vcl_imag(z));} \
inline T vnl_math_abs(vcl_complex<T > const& z) { return vcl_abs(z); } \
inline vcl_complex<T > vnl_math_sqr(vcl_complex<T > const& z) { return z*z; } \
inline T vnl_math_squared_magnitude(vcl_complex<T > const& z) { return vcl_norm(z); }
macro(float)
macro(double)
macro(long double)
#undef macro

#if 0
// isinf
template <class T> inline
bool vnl_math_isinf(const vcl_complex<T>& z)
{
  return vnl_math_isinf(vcl_real(z)) || vnl_math_isinf(vcl_imag(z));
}
#endif

#ifdef NEED_COMPLEX_BIGNUM // should never be defined ;-)

#include <vnl/vnl_bignum.h>

inline bool vnl_math_isnan(vcl_complex<vnl_bignum> const& ) { return false; }
inline bool vnl_math_isfinite(vcl_complex<vnl_bignum> const&) { return true; }
inline vnl_bignum vnl_math_squared_magnitude(vcl_complex<vnl_bignum> const& z) { return vcl_norm(z); }
inline vnl_bignum vnl_math_abs(vcl_complex<vnl_bignum> const& z) { return vcl_sqrt(double(vcl_norm(z))); }
inline vcl_complex<vnl_bignum> vnl_math_sqr(vcl_complex<vnl_bignum> const& z) { return z*z; }
inline vcl_ostream& operator<<(vcl_ostream& s, vcl_complex<vnl_bignum> const& z)
{ return s << '(' << z.real() << ',' << z.imag() << ')'; }
inline vcl_istream& operator>>(vcl_istream& s, vcl_complex<vnl_bignum>& z)
{ vnl_bignum r, i; s >> r >> i; z=vcl_complex<vnl_bignum>(r,i); return s; }

#endif // NEED_COMPLEX_BIGNUM

#ifdef NEED_COMPLEX_RATIONAL // should probably not be defined ;-)

#include <vnl/vnl_rational.h>

inline bool vnl_math_isnan(vcl_complex<vnl_rational> const& z)
{ return vnl_math_isnan(vcl_real(z)) || vnl_math_isnan(vcl_imag(z)); }
inline bool vnl_math_isfinite(vcl_complex<vnl_rational> const& z)
{ return vnl_math_isfinite(vcl_real(z)) && vnl_math_isfinite(vcl_imag(z)); }
inline vnl_rational vnl_math_squared_magnitude(vcl_complex<vnl_rational> const& z) { return vcl_norm(z); }
inline vnl_rational vnl_math_abs(vcl_complex<vnl_rational> const& z) { return vcl_sqrt(double(vcl_norm(z))); }
inline vcl_complex<vnl_rational> vnl_math_sqr(vcl_complex<vnl_rational> const& z) { return z*z; }
inline vcl_ostream& operator<< (vcl_ostream& s, vcl_complex<vnl_rational> const& z)
{ return s << '(' << z.real() << ',' << z.imag() << ')'; }
inline vcl_istream& operator>> (vcl_istream& s, vcl_complex<vnl_rational>& z)
{ vnl_rational r, i; s >> r >> i; z=vcl_complex<vnl_rational>(r,i); return s; }

#endif // NEED_COMPLEX_RATIONAL

#endif // vnl_complex_h_
