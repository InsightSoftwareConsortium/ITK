// This is core/vnl/vnl_math.h
#ifndef vnl_math_h_
#define vnl_math_h_
//:
// \file
// \brief Namespace with standard math functions
//
//  The vnl_math namespace provides a standard set of the simple mathematical
//  functions (min, max, sqr, sgn, rnd, abs), and some predefined constants
//  such as pi and e, which are not defined by the ANSI C++ standard.
//
//  There are complex versions defined in vnl_complex.h
//
//  That's right, M_PI is nonstandard!
//
//  Aside from e, pi and their associates the namespace also defines eps,
//  the IEEE double machine precision.  This is the smallest number
//  eps such that 1+eps != 1.
//
//  The operations are overloaded for int, float and double arguments,
//  which in combination with inlining can make them  more efficient than
//  their counterparts in the standard C library.
//
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   July 13, 1996
//
// \verbatim
//  Modifications
//   21 May 1998 AWF Removed conditional VCL_IMPLEMENT_STATIC_CONSTS, sometimes gcc needs them.
//   LSB (Modifications) 23 Jan 2001 Documentation tidied
//   Peter Vanroose - 7 Sep 2002 - maxdouble etc. replaced by vnl_numeric_traits<T>::maxval
//   Amitha Perera - 13 Sep 2002 - make constant initialization standards compliant.
//   Peter Vanroose -22 Oct 2012 - was a class, now is a namespace
//                                 also renamed functions vnl_math_isnan etc. to vnl_math::isnan
// \endverbatim

#include <cmath>
#include <algorithm>
#include <complex>
#include <limits>
#include <utility>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include "dll.h"
#include <vxl_config.h>
#include <vnl/vnl_export.h>

//: Type-accessible infinities for use in templates.
// Define VNL_MATH_DEPRECATE_HUGE_VAL=0 to silence during migration.
#ifndef VNL_MATH_DEPRECATE_HUGE_VAL
#  define VNL_MATH_DEPRECATE_HUGE_VAL 1
#endif
#if VNL_MATH_DEPRECATE_HUGE_VAL
#  define VNL_HUGE_VAL_DEPRECATED                                                                       \
    [[deprecated("vnl_huge_val is deprecated; use std::numeric_limits<T>::infinity() for "              \
                 "floating-point T, or std::numeric_limits<T>::max() for integral T")]]
#else
#  define VNL_HUGE_VAL_DEPRECATED
#endif
template <class T>
VNL_HUGE_VAL_DEPRECATED VNL_EXPORT T
vnl_huge_val(T);
VNL_HUGE_VAL_DEPRECATED extern VNL_EXPORT long double
vnl_huge_val(long double);
VNL_HUGE_VAL_DEPRECATED extern VNL_EXPORT double
vnl_huge_val(double);
VNL_HUGE_VAL_DEPRECATED extern VNL_EXPORT float
vnl_huge_val(float);
VNL_HUGE_VAL_DEPRECATED extern VNL_EXPORT long int
vnl_huge_val(long int);
VNL_HUGE_VAL_DEPRECATED extern VNL_EXPORT int
vnl_huge_val(int);
VNL_HUGE_VAL_DEPRECATED extern VNL_EXPORT short
vnl_huge_val(short);
VNL_HUGE_VAL_DEPRECATED extern VNL_EXPORT char
vnl_huge_val(char);
#undef VNL_HUGE_VAL_DEPRECATED

//: real numerical constants
// Declared 'inline constexpr' so every translation unit that includes this
// header sees the same object at the same address, rather than the per-TU
// internal-linkage copies produced by 'static constexpr'.
namespace vnl_math
{
//: Undeprecated numeric constants for vnl's own internal use.
// vnl source and headers reference these (e.g. vnl_math::detail::pi)
// so they are unaffected by the deprecation of the public vnl_math:: aliases
// below, even when vnl headers are instantiated in downstream translation units.
//
// NOT part of the public API: this namespace and its members are an
// implementation detail, carry no stability guarantee, and may be renamed or
// removed at any time. Downstream code must use std::numbers (C++20) or its
// toolkit's constants (e.g. itk::Math), never these.
namespace detail // unstable; not part of the public API
{
//: pi, e and all that.  Constants are rounded to the shown precision.
inline constexpr double e = 2.71828182845904523536;                // http://oeis.org/A001113
inline constexpr double log2e = 1.44269504088896340736;            // http://oeis.org/A007525
inline constexpr double log10e = 0.43429448190325182765;           // http://oeis.org/A002285
inline constexpr double ln2 = 0.69314718055994530942;              // http://oeis.org/A002162
inline constexpr double ln10 = 2.30258509299404568402;             // http://oeis.org/A002392
inline constexpr double pi = 3.14159265358979323846;               // http://oeis.org/A000796
inline constexpr double twopi = 6.28318530717958647693;            // http://oeis.org/A019692
inline constexpr double pi_over_2 = 1.57079632679489661923;        // http://oeis.org/A019669
inline constexpr double pi_over_4 = 0.78539816339744830962;        // http://oeis.org/A003881
inline constexpr double pi_over_180 = 0.01745329251994329577;      // http://oeis.org/A019685
inline constexpr double one_over_pi = 0.31830988618379067154;      // http://oeis.org/A049541
inline constexpr double two_over_pi = 0.63661977236758134308;      // http://oeis.org/A060294
inline constexpr double deg_per_rad = 57.2957795130823208768;      // http://oeis.org/A072097
inline constexpr double sqrt2pi = 2.50662827463100050242;          // http://oeis.org/A019727
inline constexpr double two_over_sqrtpi = 1.12837916709551257390;  // http://oeis.org/A190732
inline constexpr double one_over_sqrt2pi = 0.39894228040143267794; // http://oeis.org/A231863
inline constexpr double sqrt2 = 1.41421356237309504880;            // http://oeis.org/A002193
inline constexpr double sqrt1_2 = 0.70710678118654752440;          // http://oeis.org/A010503
inline constexpr double sqrt1_3 = 0.57735026918962576451;          // http://oeis.org/A020760
inline constexpr double euler = 0.57721566490153286061;            // http://oeis.org/A001620
//: IEEE double machine precision
inline constexpr double eps = std::numeric_limits<double>::epsilon();
inline constexpr double sqrteps = 0x1p-26; // sqrt(eps) = sqrt(2^-52) = 2^-26, exactly representable
//: IEEE single machine precision
inline constexpr float float_eps = std::numeric_limits<float>::epsilon();
inline constexpr float float_sqrteps = 3.4526698300e-4f;
} // namespace detail

// Deprecated public aliases. Downstream consumers that reference vnl_math::pi
// (etc.) get a compile-time deprecation warning and should migrate to
// std::numbers (C++20) or their toolkit's constants (e.g. itk::Math in ITK).
// Define VNL_MATH_DEPRECATE_CONSTANTS=0 to silence during migration.
#ifndef VNL_MATH_DEPRECATE_CONSTANTS
#  define VNL_MATH_DEPRECATE_CONSTANTS 1
#endif
#if VNL_MATH_DEPRECATE_CONSTANTS
#  define VNL_MATH_CONSTANT_DEPRECATED \
    [[deprecated("vnl_math:: numeric constants are deprecated; use std::numbers (C++20) or itk::Math (ITK)")]]
#else
#  define VNL_MATH_CONSTANT_DEPRECATED
#endif
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double e = detail::e;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double log2e = detail::log2e;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double log10e = detail::log10e;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double ln2 = detail::ln2;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double ln10 = detail::ln10;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double pi = detail::pi;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double twopi = detail::twopi;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double pi_over_2 = detail::pi_over_2;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double pi_over_4 = detail::pi_over_4;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double pi_over_180 = detail::pi_over_180;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double one_over_pi = detail::one_over_pi;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double two_over_pi = detail::two_over_pi;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double deg_per_rad = detail::deg_per_rad;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double sqrt2pi = detail::sqrt2pi;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double two_over_sqrtpi = detail::two_over_sqrtpi;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double one_over_sqrt2pi = detail::one_over_sqrt2pi;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double sqrt2 = detail::sqrt2;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double sqrt1_2 = detail::sqrt1_2;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double sqrt1_3 = detail::sqrt1_3;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double euler = detail::euler;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double eps = detail::eps;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr double sqrteps = detail::sqrteps;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr float float_eps = detail::float_eps;
VNL_MATH_CONSTANT_DEPRECATED inline constexpr float float_sqrteps = detail::float_sqrteps;
#undef VNL_MATH_CONSTANT_DEPRECATED

namespace detail // unstable; not part of the public API
{
//: Convert an angle to [0, 2Pi) range
VNL_EXPORT double
angle_0_to_2pi(double angle);
//: Convert an angle to [-Pi, Pi) range
VNL_EXPORT double
angle_minuspi_to_pi(double angle);
} // namespace detail
} // namespace vnl_math

namespace vnl_math
{
namespace numeric_predicates
{
// Wrap the <cmath> classification functions to guarantee a bool return type;
// some standard libraries returned a signed integer from these.
template <typename TArg>
inline bool
isinf(TArg arg)
{
  return bool(std::isinf(arg));
}
template <typename TArg>
inline bool
isnan(TArg arg)
{
  return bool(std::isnan(arg));
}
template <typename TArg>
inline bool
isfinite(TArg arg)
{
  return bool(std::isfinite(arg));
}
template <typename TArg>
inline bool
isnormal(TArg arg)
{
  return bool(std::isnormal(arg));
}
} // namespace numeric_predicates

// Deprecated public spellings; vnl-internal code uses numeric_predicates::.
// Define VNL_MATH_DEPRECATE_PREDICATES=0 to silence during migration.
#ifndef VNL_MATH_DEPRECATE_PREDICATES
#  define VNL_MATH_DEPRECATE_PREDICATES 1
#endif
#if VNL_MATH_DEPRECATE_PREDICATES
#  define VNL_MATH_PREDICATE_DEPRECATED \
    [[deprecated("vnl_math:: classification functions are deprecated; use std::isnan/isinf/isfinite/isnormal or itk::Math")]]
#else
#  define VNL_MATH_PREDICATE_DEPRECATED
#endif
template <typename TArg>
VNL_MATH_PREDICATE_DEPRECATED inline bool
isinf(TArg arg)
{
  return numeric_predicates::isinf(arg);
}
template <typename TArg>
VNL_MATH_PREDICATE_DEPRECATED inline bool
isnan(TArg arg)
{
  return numeric_predicates::isnan(arg);
}
template <typename TArg>
VNL_MATH_PREDICATE_DEPRECATED inline bool
isfinite(TArg arg)
{
  return numeric_predicates::isfinite(arg);
}
template <typename TArg>
VNL_MATH_PREDICATE_DEPRECATED inline bool
isnormal(TArg arg)
{
  return numeric_predicates::isnormal(arg);
}
#undef VNL_MATH_PREDICATE_DEPRECATED
// Deprecated re-exports of the std:: equivalents; use std:: directly.
// Define VNL_MATH_DEPRECATE_STD_REEXPORTS=0 to silence during migration.
#ifndef VNL_MATH_DEPRECATE_STD_REEXPORTS
#  define VNL_MATH_DEPRECATE_STD_REEXPORTS 1
#endif
#if VNL_MATH_DEPRECATE_STD_REEXPORTS
#  define VNL_MATH_STD_REEXPORT_DEPRECATED(fn) [[deprecated("vnl_math::" #fn " is deprecated; use std::" #fn)]]
#else
#  define VNL_MATH_STD_REEXPORT_DEPRECATED(fn)
#endif
#define VNL_MATH_DEPRECATED_STD_FORWARD(fn)                     \
  template <typename... Args>                                   \
  VNL_MATH_STD_REEXPORT_DEPRECATED(fn) inline auto fn(Args &&... args) \
    ->decltype(std::fn(std::forward<Args>(args)...))            \
  {                                                             \
    return std::fn(std::forward<Args>(args)...);                \
  }
VNL_MATH_DEPRECATED_STD_FORWARD(max)
VNL_MATH_DEPRECATED_STD_FORWARD(min)
VNL_MATH_DEPRECATED_STD_FORWARD(cbrt)
VNL_MATH_DEPRECATED_STD_FORWARD(hypot)
#undef VNL_MATH_DEPRECATED_STD_FORWARD
#undef VNL_MATH_STD_REEXPORT_DEPRECATED

namespace detail // unstable; not part of the public API
{
// rnd_halfinttoeven  -- round towards nearest integer
//         halfway cases are rounded towards the nearest even integer, e.g.
//         rnd_halfinttoeven( 1.5) ==  2
//         rnd_halfinttoeven(-1.5) == -2
//         rnd_halfinttoeven( 2.5) ==  2
//         rnd_halfinttoeven( 3.5) ==  4
//
// std::lrint rounds to the nearest integer using the current rounding mode,
// which is round-to-nearest-even by default. Modern compilers lower it to a
// single hardware instruction (cvtsd2si on x86-64, fcvtns on AArch64).
inline int
rnd_halfinttoeven(float x)
{
  return static_cast<int>(std::lrint(x));
}

inline int
rnd_halfinttoeven(double x)
{
  return static_cast<int>(std::lrint(x));
}


// rnd_halfintup  -- round towards nearest integer
//         halfway cases are rounded upward, e.g.
//         rnd_halfintup( 1.5) ==  2
//         rnd_halfintup(-1.5) == -1
//         rnd_halfintup( 2.5) ==  3
//
// Halfway-up has no standard-library equivalent (std::round is ties-away),
// so it is derived from the half-to-even primitive: rounding 2x+0.5 to even
// and halving shifts the tie upward.
// Be careful: argument absolute value must be less than INT_MAX/2
// for rnd_halfintup to be guaranteed to work.
inline int
rnd_halfintup(float x)
{
  return rnd_halfinttoeven(2 * x + 0.5f) >> 1;
}
inline int
rnd_halfintup(double x)
{
  return rnd_halfinttoeven(2 * x + 0.5) >> 1;
}

// rnd  -- round towards nearest integer
//         halfway cases such as 0.5 may be rounded either up or down
//         so as to maximize the efficiency, e.g.
//         rnd( 1.5) ==  1 or  2
//         rnd(-1.5) == -2 or -1
//         rnd( 2.5) ==  2 or  3
//         rnd( 3.5) ==  3 or  4
//
// The contract permits either tie direction, so the half-to-even std::lrint
// (round-to-nearest in the default mode) is used.
inline int
rnd(float x)
{
  return static_cast<int>(std::lrint(x));
}
inline int
rnd(double x)
{
  return static_cast<int>(std::lrint(x));
}

// floor -- round towards minus infinity
inline int
floor(float x)
{
  return static_cast<int>(std::floor(x));
}

inline int
floor(double x)
{
  return static_cast<int>(std::floor(x));
}


// ceil -- round towards plus infinity
inline int
ceil(float x)
{
  return static_cast<int>(std::ceil(x));
}

inline int
ceil(double x)
{
  return static_cast<int>(std::ceil(x));
}

// abs
inline bool
abs(bool x)
{
  return x;
}
inline unsigned char
abs(unsigned char x)
{
  return x;
}
inline unsigned char
abs(signed char x)
{
  return x < 0 ? static_cast<unsigned char>(-x) : x;
}
inline unsigned char
abs(char x)
{
  return static_cast<unsigned char>(x);
}
inline unsigned short
abs(short x)
{
  return x < 0 ? static_cast<unsigned short>(-x) : x;
}
inline unsigned short
abs(unsigned short x)
{
  return x;
}
inline unsigned int
abs(unsigned int x)
{
  return x;
}
inline unsigned long
abs(unsigned long x)
{
  return x;
}
////long long - target type will have width of at least 64 bits. (since C++11)
inline unsigned long long
abs(unsigned long long x)
{
  return x;
}
//
using std::abs; // (covers int, long, long long, float, double, long double

// sqr (square)
inline bool
sqr(bool x)
{
  return x;
}
inline int
sqr(int x)
{
  return x * x;
}
inline unsigned int
sqr(unsigned int x)
{
  return x * x;
}
inline long
sqr(long x)
{
  return x * x;
}
inline unsigned long
sqr(unsigned long x)
{
  return x * x;
}
// long long - target type will have width of at least 64 bits. (since C++11)
inline long long
sqr(long long x)
{
  return x * x;
}
inline unsigned long long
sqr(unsigned long long x)
{
  return x * x;
}

inline float
sqr(float x)
{
  return x * x;
}
inline double
sqr(double x)
{
  return x * x;
}

// cube
inline bool
cube(bool x)
{
  return x;
}
inline int
cube(int x)
{
  return x * x * x;
}
inline unsigned int
cube(unsigned int x)
{
  return x * x * x;
}
inline long
cube(long x)
{
  return x * x * x;
}
inline unsigned long
cube(unsigned long x)
{
  return x * x * x;
}
// long long - target type will have width of at least 64 bits. (since C++11)
inline long long
cube(long long x)
{
  return x * x * x;
}
inline unsigned long long
cube(unsigned long long x)
{
  return x * x * x;
}

inline float
cube(float x)
{
  return x * x * x;
}
inline double
cube(double x)
{
  return x * x * x;
}

// sgn (sign in -1, 0, +1)
inline int
sgn(int x)
{
  return x ? ((x > 0) ? 1 : -1) : 0;
}
inline int
sgn(long x)
{
  return x ? ((x > 0) ? 1 : -1) : 0;
}
// long long - target type will have width of at least 64 bits. (since C++11)
inline int
sgn(long long x)
{
  return x ? ((x > 0) ? 1 : -1) : 0;
}

inline int
sgn(float x)
{
  return (x != 0) ? ((x > 0) ? 1 : -1) : 0;
}
inline int
sgn(double x)
{
  return (x != 0) ? ((x > 0) ? 1 : -1) : 0;
}

// sgn0 (sign in -1, +1 only, useful for reals)
inline int
sgn0(int x)
{
  return (x >= 0) ? 1 : -1;
}
inline int
sgn0(long x)
{
  return (x >= 0) ? 1 : -1;
}
// long long - target type will have width of at least 64 bits. (since C++11)
inline int
sgn0(long long x)
{
  return (x >= 0) ? 1 : -1;
}

inline int
sgn0(float x)
{
  return (x >= 0) ? 1 : -1;
}
inline int
sgn0(double x)
{
  return (x >= 0) ? 1 : -1;
}

// squared_magnitude
inline unsigned int
squared_magnitude(char x)
{
  return int(x) * int(x);
}
inline unsigned int
squared_magnitude(unsigned char x)
{
  return int(x) * int(x);
}
inline unsigned int
squared_magnitude(int x)
{
  return x * x;
}
inline unsigned int
squared_magnitude(unsigned int x)
{
  return x * x;
}
inline unsigned long
squared_magnitude(long x)
{
  return x * x;
}
inline unsigned long
squared_magnitude(unsigned long x)
{
  return x * x;
}
// long long - target type will have width of at least 64 bits. (since C++11)
inline unsigned long long
squared_magnitude(long long x)
{
  return x * x;
}
inline unsigned long long
squared_magnitude(unsigned long long x)
{
  return x * x;
}

inline float
squared_magnitude(float x)
{
  return x * x;
}
inline double
squared_magnitude(double x)
{
  return x * x;
}
inline long double
squared_magnitude(long double x)
{
  return x * x;
}


// truncated remainder
inline int
remainder_truncated(int x, int y)
{
  return x % y;
}
inline unsigned int
remainder_truncated(unsigned int x, unsigned int y)
{
  return x % y;
}
inline long
remainder_truncated(long x, long y)
{
  return x % y;
}
inline unsigned long
remainder_truncated(unsigned long x, unsigned long y)
{
  return x % y;
}
inline long long
remainder_truncated(long long x, long long y)
{
  return x % y;
}
inline unsigned long long
remainder_truncated(unsigned long long x, unsigned long long y)
{
  return x % y;
}
inline float
remainder_truncated(float x, float y)
{
  return fmodf(x, y);
}
inline double
remainder_truncated(double x, double y)
{
  return fmod(x, y);
}
inline long double
remainder_truncated(long double x, long double y)
{
  return fmodl(x, y);
}

// floored remainder
inline int
remainder_floored(int x, int y)
{
  return ((x % y) + y) % y;
}
inline unsigned int
remainder_floored(unsigned int x, unsigned int y)
{
  return x % y;
}
inline long
remainder_floored(long x, long y)
{
  return ((x % y) + y) % y;
}
inline unsigned long
remainder_floored(unsigned long x, unsigned long y)
{
  return x % y;
}
inline long long
remainder_floored(long long x, long long y)
{
  return ((x % y) + y) % y;
}
inline unsigned long long
remainder_floored(unsigned long long x, unsigned long long y)
{
  return x % y;
}
inline float
remainder_floored(float x, float y)
{
  return fmodf(fmodf(x, y) + y, y);
}
inline double
remainder_floored(double x, double y)
{
  return std::fmod(std::fmod(x, y) + y, y);
}
inline long double
remainder_floored(long double x, long double y)
{
  return fmodl(fmodl(x, y) + y, y);
}
} // namespace detail

// Deprecated public spellings; vnl-internal code uses vnl_math::detail::.
// Each forwarder perfectly forwards to detail::, preserving the overload set
// (scalar, integral and complex) and return type of the original.
// Define VNL_MATH_DEPRECATE_FUNCTIONS=0 to silence during migration.
#ifndef VNL_MATH_DEPRECATE_FUNCTIONS
#  define VNL_MATH_DEPRECATE_FUNCTIONS 1
#endif
#if VNL_MATH_DEPRECATE_FUNCTIONS
#  define VNL_MATH_FUNCTION_DEPRECATED \
    [[deprecated("this vnl_math:: function is deprecated; use the std:: equivalent or itk::Math")]]
#else
#  define VNL_MATH_FUNCTION_DEPRECATED
#endif
#define VNL_MATH_DEPRECATED_FORWARD(fn)                       \
  template <typename... Args>                                 \
  VNL_MATH_FUNCTION_DEPRECATED inline auto fn(Args &&... args) \
    ->decltype(detail::fn(std::forward<Args>(args)...))       \
  {                                                           \
    return detail::fn(std::forward<Args>(args)...);           \
  }
VNL_MATH_DEPRECATED_FORWARD(angle_0_to_2pi)
VNL_MATH_DEPRECATED_FORWARD(angle_minuspi_to_pi)
VNL_MATH_DEPRECATED_FORWARD(rnd_halfinttoeven)
VNL_MATH_DEPRECATED_FORWARD(rnd_halfintup)
VNL_MATH_DEPRECATED_FORWARD(rnd)
VNL_MATH_DEPRECATED_FORWARD(floor)
VNL_MATH_DEPRECATED_FORWARD(ceil)
VNL_MATH_DEPRECATED_FORWARD(sgn)
VNL_MATH_DEPRECATED_FORWARD(sgn0)
VNL_MATH_DEPRECATED_FORWARD(remainder_truncated)
VNL_MATH_DEPRECATED_FORWARD(remainder_floored)
VNL_MATH_DEPRECATED_FORWARD(sqr)
VNL_MATH_DEPRECATED_FORWARD(cube)
VNL_MATH_DEPRECATED_FORWARD(squared_magnitude)
#undef VNL_MATH_DEPRECATED_FORWARD
#undef VNL_MATH_FUNCTION_DEPRECATED

// abs migrates to itk::Math::Absolute(), which (unlike std::abs) preserves the
// unsigned-returning integral semantics and supports INT_MIN and bool.
#if VNL_MATH_DEPRECATE_FUNCTIONS
#  define VNL_MATH_ABS_DEPRECATED \
    [[deprecated("vnl_math::abs is deprecated; use itk::Math::Absolute() (ITK) or std::abs")]]
#else
#  define VNL_MATH_ABS_DEPRECATED
#endif
template <typename... Args>
VNL_MATH_ABS_DEPRECATED inline auto
abs(Args &&... args) -> decltype(detail::abs(std::forward<Args>(args)...))
{
  return detail::abs(std::forward<Args>(args)...);
}
#undef VNL_MATH_ABS_DEPRECATED

} // end of namespace vnl_math
#endif // vnl_math_h_
