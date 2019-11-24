// This is core/vnl/vnl_complex.h
#ifndef vnl_complex_h_
#define vnl_complex_h_
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
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include "vnl_math.h"
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

} // end namespace vnl_math

#endif // vnl_complex_h_
