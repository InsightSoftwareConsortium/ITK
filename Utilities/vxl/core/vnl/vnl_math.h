// This is core/vnl/vnl_math.h
#ifndef vnl_math_h_
#define vnl_math_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief Namespace with standard math functions
//
//    The vnl_math namespace provides a standard set of the simple mathematical
//    functions (min, max, sqr, sgn, rnd, abs), and some predefined constants
//    such as pi and e, which are not defined by the ANSI C++ standard.
//
//    There are complex versions defined in vnl_complex.h
//
//    That's right, M_PI is nonstandard!
//
//    Aside from e, pi and their associates the class also defines eps,
//    the IEEE double machine precision.  This is the smallest number
//    eps such that 1+eps != 1.
//
//    The operations are overloaded for int, float and double arguments,
//    which in combination with inlining can make them  more efficient than
//    their counterparts in the standard C library.
//
//  \author Andrew W. Fitzgibbon, Oxford RRG
//  \date   July 13, 1996
//
// \verbatim
// Modifications
//  210598 AWF Removed conditional VCL_IMPLEMENT_STATIC_CONSTS, sometimes gcc needs them.
//  LSB (Modifications) 23/1/01 Documentation tidied
//  Peter Vanroose - 7 Sept. 2002 - maxdouble etc. replaced by vnl_numeric_traits<T>::maxval
//  Amitha Perera - 13 Sep 2002 - make constant initialization standards compliant.
// \endverbatim

#include <vcl_cmath.h>
#include "dll.h"

//: Type-accessible infinities for use in templates.
template <class T> T vnl_huge_val(T);
double   vnl_huge_val(double);
float    vnl_huge_val(float);
long int vnl_huge_val(long int);
int      vnl_huge_val(int);
short    vnl_huge_val(short);
char     vnl_huge_val(char);

//: real numerical constants
class vnl_math
{
 public:
  //: pi, e and all that
  static VNL_DLL_DATA const double e               VCL_STATIC_CONST_INIT_FLOAT_DECL(2.7182818284590452354);
  static VNL_DLL_DATA const double log2e           VCL_STATIC_CONST_INIT_FLOAT_DECL(1.4426950408889634074);
  static VNL_DLL_DATA const double log10e          VCL_STATIC_CONST_INIT_FLOAT_DECL(0.43429448190325182765);
  static VNL_DLL_DATA const double ln2             VCL_STATIC_CONST_INIT_FLOAT_DECL(0.69314718055994530942);
  static VNL_DLL_DATA const double ln10            VCL_STATIC_CONST_INIT_FLOAT_DECL(2.30258509299404568402);
  static VNL_DLL_DATA const double pi              VCL_STATIC_CONST_INIT_FLOAT_DECL(3.14159265358979323846);
  static VNL_DLL_DATA const double pi_over_2       VCL_STATIC_CONST_INIT_FLOAT_DECL(1.57079632679489661923);
  static VNL_DLL_DATA const double pi_over_4       VCL_STATIC_CONST_INIT_FLOAT_DECL(0.78539816339744830962);
  static VNL_DLL_DATA const double one_over_pi     VCL_STATIC_CONST_INIT_FLOAT_DECL(0.31830988618379067154);
  static VNL_DLL_DATA const double two_over_pi     VCL_STATIC_CONST_INIT_FLOAT_DECL(0.63661977236758134308);
  static VNL_DLL_DATA const double two_over_sqrtpi VCL_STATIC_CONST_INIT_FLOAT_DECL(1.12837916709551257390);
  static VNL_DLL_DATA const double sqrt2           VCL_STATIC_CONST_INIT_FLOAT_DECL(1.41421356237309504880);
  static VNL_DLL_DATA const double sqrt1_2         VCL_STATIC_CONST_INIT_FLOAT_DECL(0.70710678118654752440);

  //: IEEE double machine precision
  static VNL_DLL_DATA const double eps             VCL_STATIC_CONST_INIT_FLOAT_DECL(2.2204460492503131e-16);
  static VNL_DLL_DATA const double sqrteps         VCL_STATIC_CONST_INIT_FLOAT_DECL(1.490116119384766e-08);
  //: IEEE single machine precision
  static VNL_DLL_DATA const float float_eps        VCL_STATIC_CONST_INIT_FLOAT_DECL(1.192092896e-07f);
  static VNL_DLL_DATA const float float_sqrteps    VCL_STATIC_CONST_INIT_FLOAT_DECL(3.4526698307e-4f);
};

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

// isnan
inline bool vnl_math_isnan(char) { return false; }
inline bool vnl_math_isnan(short) { return false; }
inline bool vnl_math_isnan(int) { return false; }
inline bool vnl_math_isnan(long) { return false; }
inline bool vnl_math_isnan(signed char) { return false; }
inline bool vnl_math_isnan(unsigned char) { return false; }
inline bool vnl_math_isnan(unsigned short) { return false; }
inline bool vnl_math_isnan(unsigned int) { return false; }
inline bool vnl_math_isnan(unsigned long) { return false; }
bool vnl_math_isnan(float);
bool vnl_math_isnan(double);
bool vnl_math_isnan(long double);
#if !VCL_TEMPLATE_MATCHES_TOO_OFTEN
template <class T> bool vnl_math_isnan(T);
#endif

// isinf
inline bool vnl_math_isinf(char) { return false; }
inline bool vnl_math_isinf(short) { return false; }
inline bool vnl_math_isinf(int) { return false; }
inline bool vnl_math_isinf(long) { return false; }
inline bool vnl_math_isinf(signed char) { return false; }
inline bool vnl_math_isinf(unsigned char) { return false; }
inline bool vnl_math_isinf(unsigned short) { return false; }
inline bool vnl_math_isinf(unsigned int) { return false; }
inline bool vnl_math_isinf(unsigned long) { return false; }
bool vnl_math_isinf(float);
bool vnl_math_isinf(double);
bool vnl_math_isinf(long double);
#if !VCL_TEMPLATE_MATCHES_TOO_OFTEN
template <class T> bool vnl_math_isinf(T);
#endif

// isfinite
inline bool vnl_math_isfinite(char) { return true; }
inline bool vnl_math_isfinite(short) { return true; }
inline bool vnl_math_isfinite(int) { return true; }
inline bool vnl_math_isfinite(long) { return true; }
inline bool vnl_math_isfinite(signed char) { return true; }
inline bool vnl_math_isfinite(unsigned char) { return true; }
inline bool vnl_math_isfinite(unsigned short) { return true; }
inline bool vnl_math_isfinite(unsigned int) { return true; }
inline bool vnl_math_isfinite(unsigned long) { return true; }
bool vnl_math_isfinite(float);
bool vnl_math_isfinite(double);
bool vnl_math_isfinite(long double);
#if !VCL_TEMPLATE_MATCHES_TOO_OFTEN
template <class T> bool vnl_math_isfinite(T);
#endif

// rnd (rounding; 0.5 rounds up)
inline int vnl_math_rnd(float  x) { return (x>=0.0)?(int)(x + 0.5):(int)(x - 0.5); }
inline int vnl_math_rnd(double x) { return (x>=0.0)?(int)(x + 0.5):(int)(x - 0.5); }

// abs
inline bool           vnl_math_abs(bool x) { return x; }
inline unsigned char  vnl_math_abs(unsigned char x) { return x; }
inline unsigned char  vnl_math_abs(signed char x) { return x < 0 ? -x : x; }
inline unsigned char  vnl_math_abs(char x) { return (unsigned char)x; }
inline unsigned short vnl_math_abs(short x) { return x < 0 ? -x : x; }
inline unsigned short vnl_math_abs(unsigned short x) { return x; }
inline int            vnl_math_abs(int x) { return x < 0 ? -x : x; }
inline unsigned int   vnl_math_abs(unsigned int x) { return x; }
inline long           vnl_math_abs(long x) { return x < 0 ? -x : x; }
inline unsigned long  vnl_math_abs(unsigned long x) { return x; }
inline float          vnl_math_abs(float x) { return x < 0.0f ? -x : x; }
inline double         vnl_math_abs(double x) { return x < 0.0 ? -x : x; }
inline long double    vnl_math_abs(long double x) { return x < 0.0 ? -x : x; }

// max
inline int    vnl_math_max(int x, int y) { return (x > y) ? x : y; }
inline unsigned int vnl_math_max(unsigned int x, unsigned int y) { return (x > y) ? x : y; }
inline long   vnl_math_max(long x, long y) { return (x > y) ? x : y; }
inline unsigned long vnl_math_max(unsigned long x, unsigned long y) { return (x > y) ? x : y;}
inline float  vnl_math_max(float x, float y) { return (x < y) ? y : x; }
inline double vnl_math_max(double x, double y) { return (x < y) ? y : x; }

// min
inline int    vnl_math_min(int x, int y) { return (x < y) ? x : y; }
inline unsigned int vnl_math_min(unsigned int x, unsigned int y) { return (x < y) ? x : y; }
inline long   vnl_math_min(long x, long y) { return (x < y) ? x : y; }
inline unsigned long vnl_math_min(unsigned long x, unsigned long y) { return (x < y) ? x : y;}
inline float  vnl_math_min(float x, float y) { return (x > y) ? y : x; }
inline double vnl_math_min(double x, double y) { return (x > y) ? y : x; }

// sqr (square)
inline bool         vnl_math_sqr(bool x) { return x; }
inline int          vnl_math_sqr(int x) { return x*x; }
inline unsigned int vnl_math_sqr(unsigned int x) { return x*x; }
inline long         vnl_math_sqr(long x) { return x*x; }
inline float        vnl_math_sqr(float x) { return x*x; }
inline double       vnl_math_sqr(double x) { return x*x; }

// cube
inline bool         vnl_math_cube(bool x) { return x; }
inline int          vnl_math_cube(int x) { return x*x*x; }
inline unsigned int vnl_math_cube(unsigned int x) { return x*x*x; }
inline long         vnl_math_cube(long x) { return x*x*x; }
inline float        vnl_math_cube(float x) { return x*x*x; }
inline double       vnl_math_cube(double x) { return x*x*x; }

// sgn (sign in -1, 0, +1)
inline int vnl_math_sgn(int x) { return x?((x>0)?1:-1):0; }
inline int vnl_math_sgn(long x) { return x?((x>0)?1:-1):0; }
inline int vnl_math_sgn(float x) { return (x != 0)?((x>0)?1:-1):0; }
inline int vnl_math_sgn(double x) { return (x != 0)?((x>0)?1:-1):0; }

// sgn0 (sign in -1, +1 only, useful for reals)
inline int vnl_math_sgn0(int x) { return (x>=0)?1:-1; }
inline int vnl_math_sgn0(long x) { return (x>=0)?1:-1; }
inline int vnl_math_sgn0(float x) { return (x>=0)?1:-1; }
inline int vnl_math_sgn0(double x) { return (x>=0)?1:-1; }

// squared_magnitude
inline unsigned int  vnl_math_squared_magnitude(char x) { return int(x)*int(x); }
inline unsigned int  vnl_math_squared_magnitude(unsigned char x) { return int(x)*int(x); }
inline unsigned int  vnl_math_squared_magnitude(int x) { return x*x; }
inline unsigned int  vnl_math_squared_magnitude(unsigned int x) { return x*x; }
inline long          vnl_math_squared_magnitude(long x) { return x*x; }
inline unsigned long vnl_math_squared_magnitude(unsigned long x) { return x*x; }
inline float         vnl_math_squared_magnitude(float x) { return x*x; }
inline double        vnl_math_squared_magnitude(double x) { return x*x; }
inline long double   vnl_math_squared_magnitude(long double x) { return x*x; }

// cuberoot
inline float  vnl_math_cuberoot(float a) { return float((a<0) ? -vcl_exp(vcl_log(-a)/3) : vcl_exp(vcl_log(a)/3)); }
inline double vnl_math_cuberoot(double a) { return (a<0) ? -vcl_exp(vcl_log(-a)/3) : vcl_exp(vcl_log(a)/3); }

// hypotenuse
inline double vnl_math_hypot(int x, int y) { return vcl_sqrt(double(x*x + y*y)); }
inline float  vnl_math_hypot(float x, float y) { return float( vcl_sqrt(double(x*x + y*y)) ); }
inline double vnl_math_hypot(double x, double y) { return vcl_sqrt(x*x + y*y); }
inline long double vnl_math_hypot(long double x, long double y) { return vcl_sqrt(x*x + y*y); }

#endif // vnl_math_h_
