#ifndef vnl_math_h_
#define vnl_math_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME	vnl_math - namespace with standard math functions
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_math.h
// .FILE	vnl_math.cxx
//
// .SECTION Description
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
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, July 13, 1996
//
// .SECTION Modifications
//     210598 AWF Removed conditional VCL_IMPLEMENT_STATIC_CONSTS, sometimes gcc needs them.
//

#include <vcl_cmath.h>
#include "dll.h"

// Type-accessible infinities for use in templates.
template <class T> T vnl_huge_val(T);
double   vnl_huge_val(double);
float    vnl_huge_val(float);
long int vnl_huge_val(long int);
int      vnl_huge_val(int);
short    vnl_huge_val(short);
char     vnl_huge_val(char);

//: real numerical constants
class vnl_math {
public:
  // pi, e and all that
  static VNL_DLL_DATA double const e               VCL_STATIC_CONST_INIT_FLOAT(2.7182818284590452354);
  static VNL_DLL_DATA double const log2e           VCL_STATIC_CONST_INIT_FLOAT(1.4426950408889634074);
  static VNL_DLL_DATA double const log10e          VCL_STATIC_CONST_INIT_FLOAT(0.43429448190325182765);
  static VNL_DLL_DATA double const ln2             VCL_STATIC_CONST_INIT_FLOAT(0.69314718055994530942);
  static VNL_DLL_DATA double const ln10            VCL_STATIC_CONST_INIT_FLOAT(2.30258509299404568402);
  static VNL_DLL_DATA double const pi              VCL_STATIC_CONST_INIT_FLOAT(3.14159265358979323846);
  static VNL_DLL_DATA double const pi_over_2       VCL_STATIC_CONST_INIT_FLOAT(1.57079632679489661923);
  static VNL_DLL_DATA double const pi_over_4       VCL_STATIC_CONST_INIT_FLOAT(0.78539816339744830962);
  static VNL_DLL_DATA double const one_over_pi     VCL_STATIC_CONST_INIT_FLOAT(0.31830988618379067154);
  static VNL_DLL_DATA double const two_over_pi     VCL_STATIC_CONST_INIT_FLOAT(0.63661977236758134308);
  static VNL_DLL_DATA double const two_over_sqrtpi VCL_STATIC_CONST_INIT_FLOAT(1.12837916709551257390);
  static VNL_DLL_DATA double const sqrt2           VCL_STATIC_CONST_INIT_FLOAT(1.41421356237309504880);
  static VNL_DLL_DATA double const sqrt1_2         VCL_STATIC_CONST_INIT_FLOAT(0.70710678118654752440);

  // IEEE double machine precision
  static VNL_DLL_DATA double const eps             VCL_STATIC_CONST_INIT_FLOAT(2.2204460492503131e-16);
  static VNL_DLL_DATA double const sqrteps         VCL_STATIC_CONST_INIT_FLOAT(1.490116119384766e-08);

  // MAX* constants.
  // Supplied until compilers accept the templated numeric_traits.
  // These are lowercase to avoid conflict with OS-defined macros.
  static VNL_DLL_DATA int const      maxint;
  static VNL_DLL_DATA long int const maxlong;
  static VNL_DLL_DATA double const   maxdouble;
  static VNL_DLL_DATA float const    maxfloat;
};

// isnan
//bool vnl_math_isnan(float);
bool vnl_math_isnan(double);

// isinf
//bool vnl_math_isinf(float);
bool vnl_math_isinf(double);

// isfinite
//bool vnl_math_isfinite(float);
bool vnl_math_isfinite(double);

// rnd (rounding; 0.5 rounds up)
inline long vnl_math_rnd(float x) { return (x>=0.0)?(int)(x + 0.5):(int)(x - 0.5); }
inline int  vnl_math_rnd(double x) { return (x>=0.0)?(int)(x + 0.5):(int)(x - 0.5); }

// abs
inline int      vnl_math_abs(int x) { return x < 0 ? -x : x; }
inline unsigned vnl_math_abs(unsigned x) { return x; } // to avoid SunPro4.2 float/double conflict
inline long     vnl_math_abs(long x) { return x < 0 ? -x : x; }
inline unsigned long vnl_math_abs(unsigned long x) { return x; }
inline float    vnl_math_abs(float x) { return x < 0.0 ? -x : x; }
inline double   vnl_math_abs(double x) { return x < 0.0 ? -x : x; }

// max
inline int      vnl_math_max(int x, int y) { return (x > y) ? x : y; }
inline unsigned vnl_math_max(unsigned x, unsigned y) { return (x > y) ? x : y; }
inline long     vnl_math_max(long x, long y) { return (x > y) ? x : y; }
inline float    vnl_math_max(float x, float y) { return (x < y) ? y : x; }
inline double   vnl_math_max(double x, double y) { return (x < y) ? y : x; }

// min
inline int      vnl_math_min(int x, int y) { return (x < y) ? x : y; }
inline unsigned vnl_math_min(unsigned x, unsigned y) { return (x < y) ? x : y; }
inline long     vnl_math_min(long x, long y) { return (x < y) ? x : y; }
inline float    vnl_math_min(float x, float y) { return (x > y) ? y : x; }
inline double   vnl_math_min(double x, double y) { return (x > y) ? y : x; }

// sqr (square)
inline int                 vnl_math_sqr(int x) { return x*x; }
inline unsigned            vnl_math_sqr(unsigned x) { return x*x; }
inline long                vnl_math_sqr(long x) { return x*x; }
inline float               vnl_math_sqr(float x) { return x*x; }
inline double              vnl_math_sqr(double x) { return x*x; }

// sgn (sign in -1, 0, +1)
inline int vnl_math_sgn(int x) { return x?((x>0)?1:-1):0; }
inline int vnl_math_sgn(long x) { return x?((x>0)?1:-1):0; }
inline int vnl_math_sgn(float x) { return (x != 0)?((x>0)?1:-1):0; }
inline int vnl_math_sgn(double x) { return (x != 0)?((x>0)?1:-1):0; }

// sng0 (sign inn -1, +1 only, useful for reals)
inline int vnl_math_sgn0(int x) { return (x>=0)?1:-1; }
inline int vnl_math_sgn0(long x) { return (x>=0)?1:-1; }
inline int vnl_math_sgn0(float x) { return (x>=0)?1:-1; }
inline int vnl_math_sgn0(double x) { return (x>=0)?1:-1; }

// squared_magnitude
inline int      vnl_math_squared_magnitude(int x) { return x*x; }
inline unsigned vnl_math_squared_magnitude(unsigned x) { return x*x; }
inline long     vnl_math_squared_magnitude(long x) { return x*x; }
inline unsigned long vnl_math_squared_magnitude(unsigned long x) { return x*x; }
inline float    vnl_math_squared_magnitude(float x) { return x*x; }
inline double   vnl_math_squared_magnitude(double x) { return x*x; }

// squareroot
inline float  vnl_math_sqrt(float x) { return float( vcl_sqrt(double(x))); }
inline double vnl_math_sqrt(double x) { return       vcl_sqrt(double(x)) ; }

// cuberoot
inline float  vnl_math_cuberoot(float a) { return float((a<0) ? -vcl_exp(vcl_log(-a)/3) : vcl_exp(vcl_log(a)/3)); }
inline double vnl_math_cuberoot(double a) { return (a<0) ? -vcl_exp(vcl_log(-a)/3) : vcl_exp(vcl_log(a)/3); }

// hypotenuse
inline double vnl_math_hypot(int x, int y) { return vcl_sqrt(double(x*x + y*y)); }
inline float  vnl_math_hypot(float x, float y) { return float( vcl_sqrt(double(x*x + y*y)) ); }
inline double vnl_math_hypot(double x, double y) { return vcl_sqrt(x*x + y*y); }

#endif // vnl_math_h_
