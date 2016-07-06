// This is core/vnl/vnl_math.h
#ifndef vnl_math_h_
#define vnl_math_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
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
//   Peter Vanroose -15 Nov 2012 - the deprecated vnl_math_* #defines are now only available when VNL_CONFIG_LEGACY_METHODS==1
// \endverbatim

#include <cmath>
#include <algorithm>
#include <vcl_compiler.h>
#include "dll.h"
#include <vxl_config.h>
#include <vnl/vnl_config.h> // for VNL_CONFIG_ENABLE_SSE2_ROUNDING
#include <vcl_config_compiler.h> //for VXL_CONSTEXPR_VAR definition
#include "vnl/vnl_export.h"
#ifdef VNL_CHECK_FPU_ROUNDING_MODE
# include <vcl_cassert.h>
#endif
#if VNL_CONFIG_LEGACY_METHODS
# include <vcl_deprecated.h>
#endif

// Figure out when the fast implementation can be used
#if VNL_CONFIG_ENABLE_SSE2_ROUNDING && defined(__SSE2__)
# if !VXL_HAS_EMMINTRIN_H
#   error "Required file emmintrin.h for SSE2 not found"
# else
#   include <emmintrin.h> // sse 2 intrinsics
#   define USE_SSE2_IMPL 1
# endif
#else
# define USE_SSE2_IMPL 0
#endif

// Turn on fast impl when using GCC on Intel-based machines with the following exception:
#if defined(__GNUC__) && ((defined(__i386__) || defined(__i386) || defined(__x86_64__) || defined(__x86_64)))
# define GCC_USE_FAST_IMPL 1
#else
# define GCC_USE_FAST_IMPL 0
#endif
// Turn on fast impl when using msvc on 32 bits windows
#if defined(VCL_VC) && !defined(_WIN64)
# define VC_USE_FAST_IMPL 1
#else
# define VC_USE_FAST_IMPL 0
#endif



//: Type-accessible infinities for use in templates.
template <class T> VNL_TEMPLATE_EXPORT T vnl_huge_val(T);
extern VNL_EXPORT long double   vnl_huge_val(long double);
extern VNL_EXPORT double   vnl_huge_val(double);
extern VNL_EXPORT float    vnl_huge_val(float);
extern VNL_EXPORT long int vnl_huge_val(long int);
extern VNL_EXPORT int      vnl_huge_val(int);
extern VNL_EXPORT short    vnl_huge_val(short);
extern VNL_EXPORT char     vnl_huge_val(char);

//: real numerical constants
// Strictly speaking, the static declaration of the constant variables is
// redundant with the implicit behavior in C++ of objects declared const
// as defined at:
//  "C++98 7.1.1/6: ...Objects declared const and not explicitly declared
//   extern have internal linkage."
//
// Explicit use of the static keyword is used to make the code easier to
// understand.
namespace vnl_math
{
  //: pi, e and all that
  static VXL_CONSTEXPR_VAR double e                = 2.71828182845904523536;
  static VXL_CONSTEXPR_VAR double log2e            = 1.44269504088896340736;
  static VXL_CONSTEXPR_VAR double log10e           = 0.43429448190325182765;
  static VXL_CONSTEXPR_VAR double ln2              = 0.69314718055994530942;
  static VXL_CONSTEXPR_VAR double ln10             = 2.30258509299404568402;
  static VXL_CONSTEXPR_VAR double pi               = 3.14159265358979323846;
  static VXL_CONSTEXPR_VAR double twopi            = 6.28318530717958647692;
  static VXL_CONSTEXPR_VAR double pi_over_2        = 1.57079632679489661923;
  static VXL_CONSTEXPR_VAR double pi_over_4        = 0.78539816339744830962;
  static VXL_CONSTEXPR_VAR double pi_over_180      = 0.01745329251994329577;
  static VXL_CONSTEXPR_VAR double one_over_pi      = 0.31830988618379067154;
  static VXL_CONSTEXPR_VAR double two_over_pi      = 0.63661977236758134308;
  static VXL_CONSTEXPR_VAR double deg_per_rad      = 57.2957795130823208772;
  static VXL_CONSTEXPR_VAR double sqrt2pi          = 2.50662827463100024161;
  static VXL_CONSTEXPR_VAR double two_over_sqrtpi  = 1.12837916709551257390;
  static VXL_CONSTEXPR_VAR double one_over_sqrt2pi = 0.39894228040143267794;
  static VXL_CONSTEXPR_VAR double sqrt2            = 1.41421356237309504880;
  static VXL_CONSTEXPR_VAR double sqrt1_2          = 0.70710678118654752440;
  static VXL_CONSTEXPR_VAR double sqrt1_3          = 0.57735026918962573106;
  static VXL_CONSTEXPR_VAR double euler            = 0.57721566490153286061;

  //: IEEE double machine precision
  static VXL_CONSTEXPR_VAR double eps              = 2.2204460492503131e-16;
  static VXL_CONSTEXPR_VAR double sqrteps          = 1.490116119384766e-08;
  //: IEEE single machine precision
  static VXL_CONSTEXPR_VAR float  float_eps        = 1.192092896e-07f;
  static VXL_CONSTEXPR_VAR float  float_sqrteps    = 3.4526698307e-4f;

  //: Convert an angle to [0, 2Pi) range
  VNL_EXPORT double angle_0_to_2pi(double angle);
  //: Convert an angle to [-Pi, Pi) range
  VNL_EXPORT double angle_minuspi_to_pi(double angle);
}

// We do not want to make assumptions about unknown types that happen
// to have conversions to one of the fundamental types.  The templated
// versions of isnan, isinf, and isfinite below serve as catch-alls to
// cause linker errors if these functions are invoked with an unknown
// type.  However, due to compiler bugs, the templates sometimes match
// too often (see documentation of VCL_TEMPLATE_MATCHES_TOO_OFTEN) and
// are selected over reference-binding overloads like those in
// vnl_rational.h.  We add the catch-all templates only if the
// compiler does not have this bug. -- Brad King

// Note that the three template functions below should not be declared "inline"
// since that would override the non-inline specialisations. - PVr.
//

namespace vnl_math
{
#if VXL_FULLCXX11SUPPORT
  // Prefer to use perfect forwarding to the std library if C++11 features are available.
  //http://stackoverflow.com/questions/9864125/c11-how-to-alias-a-function
  template <typename... Args>
    auto isnan(Args&&... args) -> decltype(std::isnan(std::forward<Args>(args)...)) {
      return std::isnan(std::forward<Args>(args)...);
    }
  template <typename... Args>
    auto isinf(Args&&... args) -> decltype(std::isinf(std::forward<Args>(args)...)) {
      return std::isinf(std::forward<Args>(args)...);
    }
  template <typename... Args>
    auto isfinite(Args&&... args) -> decltype(std::isfinite(std::forward<Args>(args)...)) {
      return std::isfinite(std::forward<Args>(args)...);
    }
  template <typename... Args>
    auto isnormal(Args&&... args) -> decltype(std::isnormal(std::forward<Args>(args)...)) {
      return std::isnormal(std::forward<Args>(args)...);
    }
  template <typename... Args>
    auto max(Args&&... args) -> decltype(std::max(std::forward<Args>(args)...)) {
      return std::max(std::forward<Args>(args)...);
    }
  template <typename... Args>
    auto min(Args&&... args) -> decltype(std::min(std::forward<Args>(args)...)) {
      return std::min(std::forward<Args>(args)...);
    }
  //cbrt is defined in C++11
  template <typename... Args>
    auto cuberoot(Args&&... args) -> decltype(std::cbrt(std::forward<Args>(args)...)) {
      return std::cbrt(std::forward<Args>(args)...);
    }
  template <typename... Args>
    auto hypot(Args&&... args) -> decltype(std::hypot(std::forward<Args>(args)...)) {
      return std::hypot(std::forward<Args>(args)...);
    }
#else
 // isnan
 inline bool isnan(char)               { return false; }
 inline bool isnan(short)              { return false; }
 inline bool isnan(int)                { return false; }
 inline bool isnan(long)               { return false; }
 inline bool isnan(signed char)        { return false; }
 inline bool isnan(unsigned char)      { return false; }
 inline bool isnan(unsigned short)     { return false; }
 inline bool isnan(unsigned int)       { return false; }
 inline bool isnan(unsigned long)      { return false; }
#if VCL_HAS_LONG_LONG
 inline bool isnan(long long)          { return false; }
 inline bool isnan(unsigned long long) { return false; }
#endif
 VNL_EXPORT bool isnan(float);
 VNL_EXPORT bool isnan(double);
 VNL_EXPORT bool isnan(long double);
#if !VCL_TEMPLATE_MATCHES_TOO_OFTEN
 template <class T> VNL_TEMPLATE_EXPORT bool isnan(T);
#endif


 // isinf
 inline bool isinf(char)               { return false; }
 inline bool isinf(short)              { return false; }
 inline bool isinf(int)                { return false; }
 inline bool isinf(long)               { return false; }
 inline bool isinf(signed char)        { return false; }
 inline bool isinf(unsigned char)      { return false; }
 inline bool isinf(unsigned short)     { return false; }
 inline bool isinf(unsigned int)       { return false; }
 inline bool isinf(unsigned long)      { return false; }
#if VCL_HAS_LONG_LONG
 inline bool isinf(long long)          { return false; }
 inline bool isinf(unsigned long long) { return false; }
#endif
 VNL_EXPORT bool isinf(float);
 VNL_EXPORT bool isinf(double);
 VNL_EXPORT bool isinf(long double);
#if !VCL_TEMPLATE_MATCHES_TOO_OFTEN
 template <class T> VNL_TEMPLATE_EXPORT bool isinf(T);
#endif

 // isfinite
 inline bool isfinite(char)               { return true; }
 inline bool isfinite(short)              { return true; }
 inline bool isfinite(int)                { return true; }
 inline bool isfinite(long)               { return true; }
 inline bool isfinite(signed char)        { return true; }
 inline bool isfinite(unsigned char)      { return true; }
 inline bool isfinite(unsigned short)     { return true; }
 inline bool isfinite(unsigned int)       { return true; }
 inline bool isfinite(unsigned long)      { return true; }
#if VCL_HAS_LONG_LONG
 inline bool isfinite(long long)          { return true; }
 inline bool isfinite(unsigned long long) { return true; }
#endif
 VNL_EXPORT bool isfinite(float);
 VNL_EXPORT bool isfinite(double);
 VNL_EXPORT bool isfinite(long double);
#if !VCL_TEMPLATE_MATCHES_TOO_OFTEN
 template <class T> VNL_TEMPLATE_EXPORT bool isfinite(T);
#endif

// If we must use windows.h, we should at least sanitise it first
#ifndef NOMINMAX
  #define NOMINMAX
#endif
#ifdef max
  #undef max
#endif

#ifdef min
  #undef min
#endif

// max
template<class T> VNL_TEMPLATE_EXPORT
const T& max( const T& x, const T& y) { return std::max(x,y); }

template<class T> VNL_TEMPLATE_EXPORT
const T& min( const T& x, const T& y) { return std::min(x,y); }

// cuberoot
inline float  cuberoot(const float  &a) { return float((a<0) ? -std::exp(std::log(-a)/3) : std::exp(std::log(a)/3)); }
inline double cuberoot(const double &a) { return       (a<0) ? -std::exp(std::log(-a)/3) : std::exp(std::log(a)/3); }

// hypotenuse
extern VNL_EXPORT int         hypot(int         x, int         y);
extern VNL_EXPORT float       hypot(float       x, float       y);
extern VNL_EXPORT double      hypot(double      x, double      y);
extern VNL_EXPORT long double hypot(long double x, long double y);

#endif //If not C++11 features

#if USE_SSE2_IMPL // Fast sse2 implementation

// rnd_halfinttoeven  -- round towards nearest integer
//         halfway cases are rounded towards the nearest even integer, e.g.
//         rnd_halfinttoeven( 1.5) ==  2
//         rnd_halfinttoeven(-1.5) == -2
//         rnd_halfinttoeven( 2.5) ==  2
//         rnd_halfinttoeven( 3.5) ==  4
//
// We assume that the rounding mode is not changed from the default
// one (or at least that it is always restored to the default one).
inline int rnd_halfinttoeven(float  x)
{
# if defined(VNL_CHECK_FPU_ROUNDING_MODE) && defined(__GNUC__)
  assert(fegetround()==FE_TONEAREST);
# endif
  return _mm_cvtss_si32(_mm_set_ss(x));
}

inline int rnd_halfinttoeven(double  x)
{
# if defined(VNL_CHECK_FPU_ROUNDING_MODE) && defined(__GNUC__)
  assert(fegetround()==FE_TONEAREST);
# endif
  return _mm_cvtsd_si32(_mm_set_sd(x));
}

#elif GCC_USE_FAST_IMPL // Fast gcc asm implementation

inline int rnd_halfinttoeven(float  x)
{
# ifdef VNL_CHECK_FPU_ROUNDING_MODE
  assert(fegetround()==FE_TONEAREST);
# endif
  int r;
  __asm__ __volatile__ ("fistpl %0" : "=m"(r) : "t"(x) : "st");
  return r;
}

inline int rnd_halfinttoeven(double  x)
{
# ifdef VNL_CHECK_FPU_ROUNDING_MODE
  assert(fegetround()==FE_TONEAREST);
# endif
  int r;
  __asm__ __volatile__ ("fistpl %0" : "=m"(r) : "t"(x) : "st");
  return r;
}

#elif VC_USE_FAST_IMPL // Fast msvc asm implementation

inline int rnd_halfinttoeven(float  x)
{
  int r;
  __asm {
    fld x
    fistp r
  }
  return r;
}

inline int rnd_halfinttoeven(double  x)
{
  int r;
  __asm {
    fld x
    fistp r
  }
  return r;
}

#else // Vanilla implementation

inline int rnd_halfinttoeven(float  x)
{
  if (x>=0.f)
  {
     x+=0.5f;
     const int r = static_cast<int>(x);
     if ( x != static_cast<float>(r) ) return r;
     return 2*(r/2);
  }
  else
  {
     x-=0.5f;
     const int r = static_cast<int>(x);
     if ( x != static_cast<float>(r) ) return r;
     return 2*(r/2);
  }
}

inline int rnd_halfinttoeven(double x)
{
  if (x>=0.)
  {
     x+=0.5;
     const int r = static_cast<int>(x);
     if ( x != static_cast<double>(r) ) return r;
     return 2*(r/2);
  }
  else
  {
     x-=0.5;
     const int r = static_cast<int>(x);
     if ( x != static_cast<double>(r) ) return r;
     return 2*(r/2);
  }
}

#endif


#if USE_SSE2_IMPL || GCC_USE_FAST_IMPL || VC_USE_FAST_IMPL

// rnd_halfintup  -- round towards nearest integer
//         halfway cases are rounded upward, e.g.
//         rnd_halfintup( 1.5) ==  2
//         rnd_halfintup(-1.5) == -1
//         rnd_halfintup( 2.5) ==  3
//
// Be careful: argument absolute value must be less than INT_MAX/2
// for rnd_halfintup to be guaranteed to work.
// We also assume that the rounding mode is not changed from the default
// one (or at least that it is always restored to the default one).

inline int rnd_halfintup(float  x) { return rnd_halfinttoeven(2*x+0.5f)>>1; }
inline int rnd_halfintup(double  x) { return rnd_halfinttoeven(2*x+0.5)>>1; }

#else // Vanilla implementation

inline int rnd_halfintup(float  x)
{
  x+=0.5f;
  return static_cast<int>(x>=0.f?x:(x==static_cast<int>(x)?x:x-1.f));
}

inline int rnd_halfintup(double x)
{
  x+=0.5;
  return static_cast<int>(x>=0.?x:(x==static_cast<int>(x)?x:x-1.));
}

#endif

#if  USE_SSE2_IMPL || GCC_USE_FAST_IMPL || VC_USE_FAST_IMPL
// rnd  -- round towards nearest integer
//         halfway cases such as 0.5 may be rounded either up or down
//         so as to maximize the efficiency, e.g.
//         rnd_halfinttoeven( 1.5) ==  1 or  2
//         rnd_halfinttoeven(-1.5) == -2 or -1
//         rnd_halfinttoeven( 2.5) ==  2 or  3
//         rnd_halfinttoeven( 3.5) ==  3 or  4
//
// We assume that the rounding mode is not changed from the default
// one (or at least that it is always restored to the default one).
inline int rnd(float  x) { return rnd_halfinttoeven(x); }
inline int rnd(double  x) { return rnd_halfinttoeven(x); }

#else // Vanilla implementation

inline int rnd(float  x) { return x>=0.f?static_cast<int>(x+.5f):static_cast<int>(x-.5f); }
inline int rnd(double x) { return x>=0.0?static_cast<int>(x+0.5):static_cast<int>(x-0.5); }

#endif

#if  USE_SSE2_IMPL // Fast sse2 implementation
// floor -- round towards minus infinity
//
// Be careful: argument absolute value must be less than INT_MAX/2
// for floor to be guaranteed to work.
// We also assume that the rounding mode is not changed from the default
// one (or at least that it is always restored to the default one).

inline int floor(float  x)
{
# if defined(VNL_CHECK_FPU_ROUNDING_MODE) && defined(__GNUC__)
  assert(fegetround()==FE_TONEAREST);
# endif
   return _mm_cvtss_si32(_mm_set_ss(2*x-.5f))>>1;
}

inline int floor(double  x)
{
# if defined(VNL_CHECK_FPU_ROUNDING_MODE) && defined(__GNUC__)
  assert(fegetround()==FE_TONEAREST);
# endif
   return _mm_cvtsd_si32(_mm_set_sd(2*x-.5))>>1;
}

#elif GCC_USE_FAST_IMPL // Fast gcc asm implementation

inline int floor(float  x)
{
# ifdef VNL_CHECK_FPU_ROUNDING_MODE
  assert(fegetround()==FE_TONEAREST);
# endif
  int r;
  x = 2*x-.5f;
  __asm__ __volatile__ ("fistpl %0" : "=m"(r) : "t"(x) : "st");
  return r>>1;
}

inline int floor(double  x)
{
# ifdef VNL_CHECK_FPU_ROUNDING_MODE
  assert(fegetround()==FE_TONEAREST);
# endif
  int r;
  x = 2*x-.5;
  __asm__ __volatile__ ("fistpl %0" : "=m"(r) : "t"(x) : "st");
  return r>>1;
}

#elif VC_USE_FAST_IMPL // Fast msvc asm implementation

inline int floor(float  x)
{
  int r;
  x = 2*x-.5f;
  __asm {
    fld x
    fistp r
  }
  return r>>1;
}

inline int floor(double  x)
{
  int r;
  x = 2*x-.5;
  __asm {
    fld x
    fistp r
  }
  return r>>1;
}

#else // Vanilla implementation

inline int floor(float  x)
{
  return static_cast<int>(x>=0.f?x:(x==static_cast<int>(x)?x:x-1.f));
}

inline int floor(double x)
{
  return static_cast<int>(x>=0.0?x:(x==static_cast<int>(x)?x:x-1.0));
}

#endif


#if  USE_SSE2_IMPL // Fast sse2 implementation
// ceil -- round towards plus infinity
//
// Be careful: argument absolute value must be less than INT_MAX/2
// for ceil to be guaranteed to work.
// We also assume that the rounding mode is not changed from the default
// one (or at least that it is always restored to the default one).

inline int ceil(float  x)
{
# if defined(VNL_CHECK_FPU_ROUNDING_MODE) && defined(__GNUC__)
  assert(fegetround()==FE_TONEAREST);
# endif
   return -(_mm_cvtss_si32(_mm_set_ss(-.5f-2*x))>>1);
}

inline int ceil(double  x)
{
# if defined(VNL_CHECK_FPU_ROUNDING_MODE) && defined(__GNUC__)
  assert(fegetround()==FE_TONEAREST);
# endif
   return -(_mm_cvtsd_si32(_mm_set_sd(-.5-2*x))>>1);
}

#elif GCC_USE_FAST_IMPL // Fast gcc asm implementation

inline int ceil(float  x)
{
# ifdef VNL_CHECK_FPU_ROUNDING_MODE
  assert(fegetround()==FE_TONEAREST);
# endif
  int r;
  x = -.5f-2*x;
  __asm__ __volatile__ ("fistpl %0" : "=m"(r) : "t"(x) : "st");
  return -(r>>1);
}

inline int ceil(double  x)
{
# ifdef VNL_CHECK_FPU_ROUNDING_MODE
  assert(fegetround()==FE_TONEAREST);
# endif
  int r;
  x = -.5-2*x;
  __asm__ __volatile__ ("fistpl %0" : "=m"(r) : "t"(x) : "st");
  return -(r>>1);
}

#elif VC_USE_FAST_IMPL // Fast msvc asm implementation

inline int ceil(float  x)
{
  int r;
  x = -.5f-2*x;
  __asm {
    fld x
    fistp r
  }
  return -(r>>1);
}

inline int ceil(double  x)
{
  int r;
  x = -.5-2*x;
  __asm {
    fld x
    fistp r
  }
  return -(r>>1);
}

#else // Vanilla implementation

inline int ceil(float  x)
{
  return static_cast<int>(x<0.f?x:(x==static_cast<int>(x)?x:x+1.f));
}

inline int ceil(double x)
{
  return static_cast<int>(x<0.0?x:(x==static_cast<int>(x)?x:x+1.0));
}

#endif

// abs
inline bool               abs(bool x)               { return x; }
inline unsigned char      abs(unsigned char x)      { return x; }
inline unsigned char      abs(signed char x)        { return x < 0 ? static_cast<unsigned char>(-x) : x; }
inline unsigned char      abs(char x)               { return static_cast<unsigned char>(x); }
inline unsigned short     abs(short x)              { return x < 0 ? static_cast<unsigned short>(-x) : x; }
inline unsigned short     abs(unsigned short x)     { return x; }
inline unsigned int       abs(int x)                { return x < 0 ? -x : x; }
inline unsigned int       abs(unsigned int x)       { return x; }
inline unsigned long      abs(long x)               { return x < 0L ? -x : x; }
inline unsigned long      abs(unsigned long x)      { return x; }
#if VCL_HAS_LONG_LONG
inline unsigned long long abs(long long x)          { return x < 0LL ? -x : x; }
inline unsigned long long abs(unsigned long long x) { return x; }
#endif
inline float              abs(float x)              { return x < 0.0f ? -x : x; }
inline double             abs(double x)             { return x < 0.0 ? -x : x; }
inline long double        abs(long double x)        { return x < 0.0 ? -x : x; }

// sqr (square)
inline bool               sqr(bool x)               { return x; }
inline int                sqr(int x)                { return x*x; }
inline unsigned int       sqr(unsigned int x)       { return x*x; }
inline long               sqr(long x)               { return x*x; }
inline unsigned long      sqr(unsigned long x)      { return x*x; }
#if VCL_HAS_LONG_LONG
inline long long          sqr(long long x)          { return x*x; }
inline unsigned long long sqr(unsigned long long x) { return x*x; }
#endif
inline float              sqr(float x)              { return x*x; }
inline double             sqr(double x)             { return x*x; }

// cube
inline bool               cube(bool x)               { return x; }
inline int                cube(int x)                { return x*x*x; }
inline unsigned int       cube(unsigned int x)       { return x*x*x; }
inline long               cube(long x)               { return x*x*x; }
inline unsigned long      cube(unsigned long x)      { return x*x*x; }
#if VCL_HAS_LONG_LONG
inline long long          cube(long long x)          { return x*x*x; }
inline unsigned long long cube(unsigned long long x) { return x*x*x; }
#endif
inline float              cube(float x)              { return x*x*x; }
inline double             cube(double x)             { return x*x*x; }

// sgn (sign in -1, 0, +1)
inline int sgn(int x)       { return x?((x>0)?1:-1):0; }
inline int sgn(long x)      { return x?((x>0)?1:-1):0; }
#if VCL_HAS_LONG_LONG
inline int sgn(long long x) { return x?((x>0)?1:-1):0; }
#endif
inline int sgn(float x)     { return (x != 0)?((x>0)?1:-1):0; }
inline int sgn(double x)    { return (x != 0)?((x>0)?1:-1):0; }

// sgn0 (sign in -1, +1 only, useful for reals)
inline int sgn0(int x)         { return (x>=0)?1:-1; }
inline int sgn0(long x)        { return (x>=0)?1:-1; }
#if VCL_HAS_LONG_LONG
inline int sgn0(long long x)   { return (x>=0)?1:-1; }
#endif
inline int sgn0(float x)       { return (x>=0)?1:-1; }
inline int sgn0(double x)      { return (x>=0)?1:-1; }

// squared_magnitude
inline unsigned int       squared_magnitude(char               x) { return int(x)*int(x); }
inline unsigned int       squared_magnitude(unsigned char      x) { return int(x)*int(x); }
inline unsigned int       squared_magnitude(int                x) { return x*x; }
inline unsigned int       squared_magnitude(unsigned int       x) { return x*x; }
inline unsigned long      squared_magnitude(long               x) { return x*x; }
inline unsigned long      squared_magnitude(unsigned long      x) { return x*x; }
#if VCL_HAS_LONG_LONG
inline unsigned long long squared_magnitude(long long          x) { return x*x; }
inline unsigned long long squared_magnitude(unsigned long long x) { return x*x; }
#endif
inline float              squared_magnitude(float              x) { return x*x; }
inline double             squared_magnitude(double             x) { return x*x; }
inline long double        squared_magnitude(long double        x) { return x*x; }


// truncated remainder
inline int                remainder_truncated(int x, int y)                               { return x % y; }
inline unsigned int       remainder_truncated(unsigned int x, unsigned int y)             { return x % y; }
inline long               remainder_truncated(long x, long y)                             { return x % y; }
inline unsigned long      remainder_truncated(unsigned long x, unsigned long y)           { return x % y; }
inline long long          remainder_truncated(long long x, long long y)                   { return x % y; }
inline unsigned long long remainder_truncated(unsigned long long x, unsigned long long y) { return x % y; }
inline float              remainder_truncated(float x, float y)                           { return fmod(x,y); }
inline double             remainder_truncated(double x, double y)                         { return fmod(x,y); }
inline long double        remainder_truncated(long double x, long double y)               { return fmod(x,y); }

// floored remainder
inline int                remainder_floored(int x, int y)                               { return ((x % y) + y) % y; }
inline unsigned int       remainder_floored(unsigned int x, unsigned int y)             { return x % y; }
inline long               remainder_floored(long x, long y)                             { return ((x % y) + y) % y; }
inline unsigned long      remainder_floored(unsigned long x, unsigned long y)           { return x % y; }
inline long long          remainder_floored(long long x, long long y)                   { return ((x % y) + y) % y; }
inline unsigned long long remainder_floored(unsigned long long x, unsigned long long y) { return x % y; }
inline float              remainder_floored(float x, float y)                           { return fmod(fmod(x,y)+y,y); }
inline double             remainder_floored(double x, double y)                         { return fmod(fmod(x,y)+y,y); }
inline long double        remainder_floored(long double x, long double y)               { return fmod(fmod(x,y)+y,y); }

} // end of namespace vnl_math

#if VNL_CONFIG_LEGACY_METHODS // Legacy definitions, for backward compatibility; deprecated!
#define vnl_math_isnan vnl_math::isnan
#define vnl_math_isinf vnl_math::isinf
#define vnl_math_isfinite vnl_math::isfinite
#define vnl_math_rnd_halfinttoeven vnl_math::rnd_halfinttoeven
#define vnl_math_rnd_halfintup vnl_math::rnd_halfintup
#define vnl_math_rnd vnl_math::rnd
#define vnl_math_floor vnl_math::floor
#define vnl_math_ceil vnl_math::ceil
#define vnl_math_abs vnl_math::abs
#define vnl_math_max vnl_math::max
#define vnl_math_min vnl_math::min
#define vnl_math_sqr vnl_math::sqr
#define vnl_math_cube vnl_math::cube
#define vnl_math_sgn vnl_math::sgn
#define vnl_math_sgn0 vnl_math::sgn0
#define vnl_math_squared_magnitude vnl_math::squared_magnitude
#define vnl_math_cuberoot vnl_math::cuberoot
#define vnl_math_hypot vnl_math::hypot
#endif // VNL_CONFIG_LEGACY_METHODS

#endif // vnl_math_h_
