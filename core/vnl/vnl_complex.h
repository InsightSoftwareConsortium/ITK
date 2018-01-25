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

#include <cmath>
#include <complex>
#include <iosfwd>
#include <vcl_compiler.h>
#include <vnl/vnl_math.h>
#include "vnl/vnl_export.h"

namespace vnl_math
{
 // should consider making these function templated,
#define type_macro(T) \
 inline bool isnan(std::complex<T >const& z) { return isnan(std::real(z)) || isnan(std::imag(z)); } \
 inline bool isfinite(std::complex<T >const& z) { return isfinite(std::real(z)) && isfinite(std::imag(z)); } \
 inline T abs(std::complex<T > const& z) { return std::abs(z); } \
 inline std::complex<T > sqr(std::complex<T > const& z) { return z*z; } \
 inline T squared_magnitude(std::complex<T > const& z) { return std::norm(z); }
 type_macro(float)
 type_macro(double)
 type_macro(long double)
#undef type_macro

#ifdef NEED_COMPLEX_BIGNUM // should never be defined ;-)

#include <vnl/vnl_bignum.h>

 inline bool isnan(std::complex<vnl_bignum> const& ) { return false; }
 inline bool isfinite(std::complex<vnl_bignum> const&) { return true; }
 inline vnl_bignum squared_magnitude(std::complex<vnl_bignum> const& z) { return std::norm(z); }
 inline vnl_bignum abs(std::complex<vnl_bignum> const& z) { return std::sqrt(double(std::norm(z))); }
 inline std::complex<vnl_bignum> sqr(std::complex<vnl_bignum> const& z) { return z*z; }
 inline std::ostream& operator<<(std::ostream& s, std::complex<vnl_bignum> const& z)
 { return s << '(' << z.real() << ',' << z.imag() << ')'; }
 inline std::istream& operator>>(std::istream& s, std::complex<vnl_bignum>& z)
 { vnl_bignum r, i; s >> r >> i; z=std::complex<vnl_bignum>(r,i); return s; }

#endif // NEED_COMPLEX_BIGNUM

#ifdef NEED_COMPLEX_RATIONAL // should probably not be defined ;-)

#include <vnl/vnl_rational.h>

 inline bool isnan(std::complex<vnl_rational> const& z)
 { return isnan(std::real(z)) || isnan(std::imag(z)); }
 inline bool isfinite(std::complex<vnl_rational> const& z)
 { return isfinite(std::real(z)) && isfinite(std::imag(z)); }
 inline vnl_rational squared_magnitude(std::complex<vnl_rational> const& z) { return std::norm(z); }
 inline vnl_rational abs(std::complex<vnl_rational> const& z) { return std::sqrt(double(std::norm(z))); }
 inline std::complex<vnl_rational> sqr(std::complex<vnl_rational> const& z) { return z*z; }
 inline std::ostream& operator<< (std::ostream& s, std::complex<vnl_rational> const& z)
 { return s << '(' << z.real() << ',' << z.imag() << ')'; }
 inline std::istream& operator>> (std::istream& s, std::complex<vnl_rational>& z)
 { vnl_rational r, i; s >> r >> i; z=std::complex<vnl_rational>(r,i); return s; }

#endif // NEED_COMPLEX_RATIONAL

} // end namespace vnl_math

#endif // vnl_complex_h_
