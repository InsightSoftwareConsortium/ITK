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
namespace numeric_predicates
{
#define type_macro(T)                                                                                 \
  inline bool isnan(std::complex<T> const & z) { return isnan(std::real(z)) || isnan(std::imag(z)); } \
  inline bool isfinite(std::complex<T> const & z) { return isfinite(std::real(z)) && isfinite(std::imag(z)); }
type_macro(float) type_macro(double) type_macro(long double)
#undef type_macro
} // namespace numeric_predicates

// Deprecated public spellings; the non-template overloads divert complex calls
// away from the deprecated vnl_math:: forwarder templates in vnl_math.h.
#if VNL_MATH_DEPRECATE_PREDICATES
#  define VNL_MATH_CPLX_PREDICATE_DEPRECATED \
    [[deprecated("vnl_math:: classification functions are deprecated; use std::isnan/isinf/isfinite/isnormal or itk::Math")]]
#else
#  define VNL_MATH_CPLX_PREDICATE_DEPRECATED
#endif
#define type_macro(T)                                                                            \
  VNL_MATH_CPLX_PREDICATE_DEPRECATED inline bool isnan(std::complex<T> const & z) { return numeric_predicates::isnan(z); } \
  VNL_MATH_CPLX_PREDICATE_DEPRECATED inline bool isfinite(std::complex<T> const & z) { return numeric_predicates::isfinite(z); }
type_macro(float) type_macro(double) type_macro(long double)
#undef type_macro
#undef VNL_MATH_CPLX_PREDICATE_DEPRECATED

namespace detail // unstable; not part of the public API
{
#define type_macro(T)                                                    \
  inline std::complex<T> sqr(std::complex<T> const & z) { return z * z; } \
  inline T squared_magnitude(std::complex<T> const & z) { return std::norm(z); }
type_macro(float) type_macro(double) type_macro(long double)
#undef type_macro
} // namespace detail
// vnl_math::abs(std::complex) resolves through detail's `using std::abs`
// (std::abs(std::complex) from <complex>); no separate overload is needed.

// Deprecated public spellings; the non-template overloads divert complex calls
// away from the deprecated vnl_math:: forwarder templates in vnl_math.h.
#if VNL_MATH_DEPRECATE_FUNCTIONS
#  define VNL_MATH_CPLX_FUNCTION_DEPRECATED \
    [[deprecated("this vnl_math:: function is deprecated; use the std:: equivalent or itk::Math")]]
#else
#  define VNL_MATH_CPLX_FUNCTION_DEPRECATED
#endif
#define type_macro(T)                                                                                               \
  VNL_MATH_CPLX_FUNCTION_DEPRECATED inline std::complex<T> sqr(std::complex<T> const & z) { return detail::sqr(z); } \
  VNL_MATH_CPLX_FUNCTION_DEPRECATED inline T squared_magnitude(std::complex<T> const & z)                            \
  {                                                                                                                  \
    return detail::squared_magnitude(z);                                                                            \
  }
type_macro(float) type_macro(double) type_macro(long double)
#undef type_macro
#undef VNL_MATH_CPLX_FUNCTION_DEPRECATED

} // end namespace vnl_math

#endif // vnl_complex_h_
