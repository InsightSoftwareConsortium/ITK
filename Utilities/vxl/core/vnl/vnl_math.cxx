// This is core/vnl/vnl_math.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file

#include "vnl_math.h"
#include <vxl_config.h>

#if defined(VCL_VC)
// I don't think we need this, because <ieeefp.h> is available -- fsm
# include <Float.h> // for 'isnan' and 'finite'
// # define isnan _isnan
# define finite _finite
# define finitef _finite
# define isnan _isnan
#elif VXL_IEEEFP_HAS_FINITE
# include <ieeefp.h>
# ifndef finitef
#  define finitef finite
# endif

#elif VXL_C_MATH_HAS_FINITE
# include <math.h>  // dont_vxl_filter: this is *not* supposed to be <cmath>
# ifndef __alpha__ // on Alpha, finitef() must be used for float args instead of finite()
#  define finitef finite
# endif

#elif defined(SYSV) && !defined(hppa)
// needed on platforms with finite() declared in strange places, e.g. on alpha
extern "C" int finite(double);
# ifdef __alpha__ // on Alpha, finitef() must be used for float args instead of finite()
extern "C" int finitef(float);
# else
#  define finitef finite
# endif

#elif defined(VCL_BORLAND) 
# include <math.h>
# include <float.h>
#else
# warning finite() is not declared on this platform
# define VNL_HAS_NO_FINITE
#endif

#ifdef VCL_SUNPRO_CC_50
# include <math.h> // dont_vxl_filter: no HUGE_VAL or isnan() in <cmath>
#endif

#if defined(__APPLE__)
# include <math.h>
# define isnan __isnan
#endif

//--------------------------------------------------------------------------------

#if !VCL_STATIC_CONST_INIT_FLOAT_NO_DEFN

// constants
const double vnl_math::e               VCL_STATIC_CONST_INIT_FLOAT_DEFN( 2.7182818284590452354  );
const double vnl_math::log2e           VCL_STATIC_CONST_INIT_FLOAT_DEFN( 1.4426950408889634074  );
const double vnl_math::log10e          VCL_STATIC_CONST_INIT_FLOAT_DEFN( 0.43429448190325182765 );
const double vnl_math::ln2             VCL_STATIC_CONST_INIT_FLOAT_DEFN( 0.69314718055994530942 );
const double vnl_math::ln10            VCL_STATIC_CONST_INIT_FLOAT_DEFN( 2.30258509299404568402 );
const double vnl_math::pi              VCL_STATIC_CONST_INIT_FLOAT_DEFN( 3.14159265358979323846 );
const double vnl_math::pi_over_2       VCL_STATIC_CONST_INIT_FLOAT_DEFN( 1.57079632679489661923 );
const double vnl_math::pi_over_4       VCL_STATIC_CONST_INIT_FLOAT_DEFN( 0.78539816339744830962 );
const double vnl_math::one_over_pi     VCL_STATIC_CONST_INIT_FLOAT_DEFN( 0.31830988618379067154 );
const double vnl_math::two_over_pi     VCL_STATIC_CONST_INIT_FLOAT_DEFN( 0.63661977236758134308 );
const double vnl_math::two_over_sqrtpi VCL_STATIC_CONST_INIT_FLOAT_DEFN( 1.12837916709551257390 );
const double vnl_math::sqrt2           VCL_STATIC_CONST_INIT_FLOAT_DEFN( 1.41421356237309504880 );
const double vnl_math::sqrt1_2         VCL_STATIC_CONST_INIT_FLOAT_DEFN( 0.70710678118654752440 );

// IEEE double machine precision
const double vnl_math::eps             VCL_STATIC_CONST_INIT_FLOAT_DEFN( 2.2204460492503131e-16 );
const double vnl_math::sqrteps         VCL_STATIC_CONST_INIT_FLOAT_DEFN( 1.490116119384766e-08  );

  //: IEEE single machine precision
const float vnl_math::float_eps        VCL_STATIC_CONST_INIT_FLOAT_DEFN( 1.192092896e-07f );
const float vnl_math::float_sqrteps    VCL_STATIC_CONST_INIT_FLOAT_DEFN( 3.4526698307e-4f );

#endif

//--------------------------------------------------------------------------------
#if defined(VCL_ICC)
#include <mathimf.h> // defines isnanf, isnan, and isnanl
//: Return true iff x is "Not a Number"
bool vnl_math_isnan(float x) { return isnanf(x); }
//: Return true iff x is "Not a Number"
bool vnl_math_isnan(double x) { return isnan(x); }
//: Return true iff x is "Not a Number"
bool vnl_math_isnan(long double x) { return isnanl(x); }
#elif defined(VCL_BORLAND)
//: Return true iff x is "Not a Number"
bool vnl_math_isnan(float x) { return _isnan(x); }
//: Return true iff x is "Not a Number"
bool vnl_math_isnan(double x) { return _isnan(x); }
//: Return true iff x is "Not a Number"
bool vnl_math_isnan(long double x) { return _isnanl(x); }
#elif !defined(VNL_HAS_NO_FINITE) && !defined(VCL_SGI_CC_7) && !defined(__alpha__) && !defined(VCL_WIN32)
//: Return true iff x is "Not a Number"
bool vnl_math_isnan(float x) { return x != x; } // causes "floating exception" on alpha & sgi
//: Return true iff x is "Not a Number"
bool vnl_math_isnan(double x) { return x != x; }
//: Return true iff x is "Not a Number"
bool vnl_math_isnan(long double x) { return x != x; }
#else
// Auxiliary function to simplify notation
# ifndef DEBUG
static inline unsigned int bMp(void*x,unsigned int y,int p=0){return ((((unsigned int*)x)[p])&y);}
static inline bool bMe(void*x,unsigned int y,int p=0){return ((((unsigned int*)x)[p])&y)==y;}
# else
# include <vcl_iostream.h>
static inline unsigned int bMp(void* x, unsigned int y, int p=0) {
  unsigned char* v=(unsigned char*)x;
  vcl_cout<<int(v[4*p])<<' '<<int(v[4*p+1])<<' '<<int(v[4*p+2])<<' '<<int(v[4*p+3])<<" & ";
  v=(unsigned char*)(&y);
  vcl_cout<<int(v[0])<<' '<<int(v[1])<<' '<<int(v[2])<<' '<<int(v[3])<<" = ";
  unsigned int z = ((((unsigned int*)x)[p]) & y);
  v=(unsigned char*)(&z);
  vcl_cout<<int(v[0])<<' '<<int(v[1])<<' '<<int(v[2])<<' '<<int(v[3]);
  if (z == y) vcl_cout<<" ==";
  vcl_cout << '\n';
  return z;
}
static inline bool bMe(void* x, unsigned int y, int p=0) { return bMp(x,y,p) == y; }
# endif
# if VXL_BIG_ENDIAN
static const int sz_f = 0;
static const int sz_d = 0;
static const int sz_l = 0;
# else
static const int sz_f = sizeof(float)/sizeof(int) -1;
static const int sz_d = sizeof(double)/sizeof(int) -1;
static const int sz_l = sizeof(long double)/sizeof(int) -1;
# endif
// Assume IEEE floating point number representation
bool vnl_math_isnan( float x){return bMe(&x,0x7f800000L,sz_f)&&bMp(&x,0x007fffffL,sz_f);}
bool vnl_math_isnan(double x){return bMe(&x,0x7ff00000L,sz_d)&&bMp(&x,0x000fffffL,sz_d);}
bool vnl_math_isnan(long double x) {
  if (sizeof(long double) == 8) return bMe(&x,0x7ff00000L,sz_l) && bMp(&x,0x000fffffL,sz_l);
  else if (sizeof(long double) <= 12) return bMe(&x,0x4001ffffL,sz_l) && bMp(&x,0x40000000,sz_l-4);
  else return bMe(&x,0x7ff70000L,sz_l) && bMp(&x,0x0008ffffL,sz_l);
}
#endif

// fsm
// On linux noshared builds, with optimisation on, calling 'finite' within the
// scope of vnl_math causes vnl_math_isinf to be called. This blows the stack.
// Plausible theory : 'finite' is a preprocessor macro, defined in terms of a
// macro called 'isinf'.
#if defined(isinf)
# if defined(__GNUC__) || defined(VCL_METRO_WERKS)
// I do not know if MW accepts #warning. Comment out the #undef if not.
#  warning macro isinf is defined
#  undef isinf
# else
// do not fail silently
#  error macro isinf is defined
# endif
#endif

#if defined(VCL_BORLAND)
//: Return true if x is neither NaN nor Inf.
bool vnl_math_isfinite(float x) { return _finite(x) != 0; }
//: Return true if x is neither NaN nor Inf.
bool vnl_math_isfinite(double x) { return _finite(x) != 0; }
//: Return true if x is neither NaN nor Inf.
bool vnl_math_isfinite(long double x) { return _finitel(x) != 0 && !_isnanl(x); }
#elif !defined(VNL_HAS_NO_FINITE)
//: Return true if x is neither NaN nor Inf.
bool vnl_math_isfinite(float x) { return finitef(x) != 0; }
//: Return true if x is neither NaN nor Inf.
bool vnl_math_isfinite(double x) { return finite(x) != 0; }
//: Return true if x is neither NaN nor Inf.
bool vnl_math_isfinite(long double x) { return finite(x) != 0; }
#else
// Assume IEEE floating point number representation
bool vnl_math_isfinite(float x) { return !bMe(&x,0x7f800000L,sz_f) && bMp(&x,0x7fffffffL,sz_f) != 0x7f7fffffL; }
bool vnl_math_isfinite(double x) { return !bMe(&x,0x7ff00000L,sz_d); }
bool vnl_math_isfinite(long double x) {
  if (sizeof(long double) == 8) return !bMe(&x,0x7ff00000L,sz_l);
  else if (sizeof(long double) <= 12) return !bMe(&x,0xbfff7fffL,sz_l) && !bMe(&x,0x4001ffffL,sz_l);
  else return !bMe(&x,0x7ff70000L,sz_l);
}
#endif


#if defined(VCL_BORLAND)
//: Return true if x is inf
bool vnl_math_isinf(float x) { return !_finite(x) && !_isnan(x); }
//: Return true if x is inf
bool vnl_math_isinf(double x) { return !_finite(x) && !_isnan(x); }
//: Return true if x is inf
bool vnl_math_isinf(long double x) { return !_finitel(x) && !_isnanl(x); }
#elif !defined(VNL_HAS_NO_FINITE)
//: Return true if x is inf
bool vnl_math_isinf(float x) { return !finitef(x) && !isnan(x); }
//: Return true if x is inf
bool vnl_math_isinf(double x) { return !finite(x) && !isnan(x); }
//: Return true if x is inf
bool vnl_math_isinf(long double x) { return !finite(x) && !isnan(x); }
#else
// Assume IEEE floating point number representation
bool vnl_math_isinf(float x) {return(bMe(&x,0x7f800000L,sz_f)&&!bMp(&x,0x007fffffL,sz_f))||bMp(&x,0x7fffffffL,sz_f)==0x7f7fffffL;}
bool vnl_math_isinf(double x) { return bMe(&x,0x7ff00000L,sz_d) && !bMp(&x,0x000fffffL,sz_d); }
bool vnl_math_isinf(long double x) {
  if (sizeof(long double) == 8) return bMe(&x,0x7ff00000L,sz_l) && !bMp(&x,0x000fffffL,sz_l);
  else if (sizeof(long double) <= 12) return (bMe(&x,0xbfff7fffL,sz_l)||bMe(&x,0x4001ffffL,sz_l))&&!bMp(&x,0x40000000,sz_l-4);
  else return bMe(&x,0x7ff70000L,sz_l) && !bMp(&x,0x0008ffffL,sz_l);
}
#endif

//----------------------------------------------------------------------

//: Type-accessible infinities for use in templates.
template <class T> T vnl_huge_val(T);
double vnl_huge_val(double) { return HUGE_VAL; }
float  vnl_huge_val(float)  { return (float)HUGE_VAL; }
#ifdef _INT_64BIT_
long int vnl_huge_val(long int) { return 0x7fffffffffffffffL; }
int    vnl_huge_val(int)    { return 0x7fffffffffffffffL; }
#else
int    vnl_huge_val(int)    { return 0x7fffffff; }
#endif
short  vnl_huge_val(short)  { return 0x7fff; }
char   vnl_huge_val(char)   { return 0x7f; }

//----------------------------------------------------------------------
