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
// So this file adds to the vnl_math namespace.
//
// \verbatim
//  Modifications
//   LSB (Manchester) 26/03/2001 Tidied documentation
//   Peter Vanroose   24/10/2012 Now adds to the vnl_math namespace
// \endverbatim

#include <vcl_cmath.h> // for sqrt(double)
#include <vcl_complex.h>
#include <vcl_iosfwd.h>
#include <vnl/vnl_math.h>

namespace vnl_math
{
 // should consider making these function templated,
#define type_macro(T) \
 inline bool isnan(vcl_complex<T >const& z) { return isnan(vcl_real(z)) || isnan(vcl_imag(z)); } \
 inline bool isfinite(vcl_complex<T >const& z) { return isfinite(vcl_real(z)) && isfinite(vcl_imag(z)); } \
 inline T abs(vcl_complex<T > const& z) { return vcl_abs(z); } \
 inline vcl_complex<T > sqr(vcl_complex<T > const& z) { return z*z; } \
 inline T squared_magnitude(vcl_complex<T > const& z) { return vcl_norm(z); }
 type_macro(float)
 type_macro(double)
 type_macro(long double)
#undef type_macro

#ifdef NEED_COMPLEX_BIGNUM // should never be defined ;-)

#include <vnl/vnl_bignum.h>

 inline bool isnan(vcl_complex<vnl_bignum> const& ) { return false; }
 inline bool isfinite(vcl_complex<vnl_bignum> const&) { return true; }
 inline vnl_bignum squared_magnitude(vcl_complex<vnl_bignum> const& z) { return vcl_norm(z); }
 inline vnl_bignum abs(vcl_complex<vnl_bignum> const& z) { return vcl_sqrt(double(vcl_norm(z))); }
 inline vcl_complex<vnl_bignum> sqr(vcl_complex<vnl_bignum> const& z) { return z*z; }
 inline vcl_ostream& operator<<(vcl_ostream& s, vcl_complex<vnl_bignum> const& z)
 { return s << '(' << z.real() << ',' << z.imag() << ')'; }
 inline vcl_istream& operator>>(vcl_istream& s, vcl_complex<vnl_bignum>& z)
 { vnl_bignum r, i; s >> r >> i; z=vcl_complex<vnl_bignum>(r,i); return s; }

#endif // NEED_COMPLEX_BIGNUM

#ifdef NEED_COMPLEX_RATIONAL // should probably not be defined ;-)

#include <vnl/vnl_rational.h>

 inline bool isnan(vcl_complex<vnl_rational> const& z)
 { return isnan(vcl_real(z)) || isnan(vcl_imag(z)); }
 inline bool isfinite(vcl_complex<vnl_rational> const& z)
 { return isfinite(vcl_real(z)) && isfinite(vcl_imag(z)); }
 inline vnl_rational squared_magnitude(vcl_complex<vnl_rational> const& z) { return vcl_norm(z); }
 inline vnl_rational abs(vcl_complex<vnl_rational> const& z) { return vcl_sqrt(double(vcl_norm(z))); }
 inline vcl_complex<vnl_rational> sqr(vcl_complex<vnl_rational> const& z) { return z*z; }
 inline vcl_ostream& operator<< (vcl_ostream& s, vcl_complex<vnl_rational> const& z)
 { return s << '(' << z.real() << ',' << z.imag() << ')'; }
 inline vcl_istream& operator>> (vcl_istream& s, vcl_complex<vnl_rational>& z)
 { vnl_rational r, i; s >> r >> i; z=vcl_complex<vnl_rational>(r,i); return s; }

#endif // NEED_COMPLEX_RATIONAL

} // end namespace vnl_math

#endif // vnl_complex_h_
