// This is core/vnl/vnl_math.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file

#include "vnl_math.h"
#include <vxl_config.h>

#if defined(VCL_VC) || defined(__MINGW32__)
// I don't think we need this, because <ieeefp.h> is available -- fsm
# include <float.h> // for 'isnan' and 'finite'
// # define isnan _isnan
# define finite _finite
# define finitef _finite
#ifndef finitel
# define finitel _finite
#endif
# define isnan _isnan
#elif VXL_IEEEFP_HAS_FINITE
# include <ieeefp.h>
# ifndef finitef
#  define finitef finite
# endif
# ifndef finitel
#  define finitel finite
# endif

#elif VXL_C_MATH_HAS_FINITE
# include <math.h> // dont_vxl_filter: this is *not* supposed to be <cmath>
# if !VXL_C_MATH_HAS_FINITEF
#  define finitef finite
# endif
# if !VXL_C_MATH_HAS_FINITEL
#  define finitel finite
# endif

#elif defined(__hpux)
# include <math.h> // dont_vxl_filter: this is *not* supposed to be <cmath>
# define finite _Isfinite
# define finitef _Isfinitef
# define finitel _Isfinite

#elif defined(SYSV)
// needed on platforms with finite() declared in strange places
extern "C" int finite(double);
# define finitef finite
# define finitel finite

#elif defined(VCL_BORLAND)
# include <math.h> // dont_vxl_filter: this is *not* supposed to be <cmath>
# include <float.h>

#else
# warning finite() is not declared on this platform
# define VNL_HAS_NO_FINITE
#endif

#ifdef VCL_SUNPRO_CC_5
# include <math.h> // dont_vxl_filter: no HUGE_VAL or isnan() in <cmath>
#endif

// On OSX Tiger in C++ the math.h header defines an inline __isnan
// that gets compiled here into an internal-linkage symbol.  Then at
// link time the relocation entry from libm.dylib confuses the linker
// because it thinks the entry applies to the static version of the
// symbol.  We need to avoid use of the inline version by never
// calling __isnan in C++ code.
#if defined(__APPLE__)
# include <math.h> // dont_vxl_filter: this is *not* supposed to be <cmath>
# if VXL_APPLE_HAS_ISNAND
#  define isnan(x) __isnand((double)x)
# else
#  define isnan(x) __inline_isnand((double)x)
# endif
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
static inline unsigned int bMp(void* x, unsigned int y, int p=0)
{
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
bool vnl_math_isnan(double x){return bMe(&x,0x7ff00000L,sz_d)&&(bMp(&x,0x000fffffL,sz_d)||bMp(&x,0xffffffffL,1-sz_d));}
bool vnl_math_isnan(long double x)
{
  if (sizeof(long double) == 8) return bMe(&x,0x7ff00000L,sz_l) && (bMp(&x,0x000fffffL,sz_l)||bMp(&x,0xffffffffL,1-sz_d));
  else if (sizeof(long double) <= 12) // This code doesn't properly check the less significant
                                      // bytes for non-zero ness to distinguish inf from nan
                                      // see http://babbage.cs.qc.cuny.edu/IEEE-754/References.xhtml#tables
# if defined LDBL_MANT_DIG && LDBL_MANT_DIG<=53
    return bMe(&x,0x4001ffffL,sz_l) && bMp(&x,0x40000000,sz_l-4);
# else
    return bMe(&x,0x7ff00000L,sz_l) && bMp(&x,0x000fffffL,sz_l-4);
# endif
  else return bMe(&x,0x7ff00000L,sz_l) && bMp(&x,0x0000ffffL,sz_l);
}
#endif

// fsm
// On linux noshared builds, with optimisation on, calling 'finite' within the
// scope of vnl_math causes vnl_math_isinf to be called. This blows the stack.
// Plausible theory : 'finite' is a preprocessor macro, defined in terms of a
// macro called 'isinf'.
// Doesn't seem to be an issue with ICC 8
#if defined(isinf) && !defined(VCL_ICC_8)
# if defined(__GNUC__) || defined(VCL_METRO_WERKS) || defined(__INTEL_COMPILER)
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
bool vnl_math_isfinite(long double x) { return finitel(x) != 0; }
#else
// Assume IEEE floating point number representation
bool vnl_math_isfinite(float x) { return !bMe(&x,0x7f800000L,sz_f) && bMp(&x,0x7fffffffL,sz_f) != 0x7f7fffffL; }
bool vnl_math_isfinite(double x) { return !bMe(&x,0x7ff00000L,sz_d); }
bool vnl_math_isfinite(long double x)
{
  if (sizeof(long double) == 8) return !bMe(&x,0x7ff00000L,sz_l);
  else if (sizeof(long double) <= 12) return !bMe(&x,0xbfff7fffL,sz_l) && !bMe(&x,0x4001ffffL,sz_l);
  else return !bMe(&x,0x7ff70000L,sz_l);
}
#endif


#if defined(VCL_BORLAND)
//: Return true if x is inf
bool vnl_math_isinf(float x) { return !_finite(x) && !vnl_math_isnan(x); }
//: Return true if x is inf
bool vnl_math_isinf(double x) { return !_finite(x) && !vnl_math_isnan(x); }
//: Return true if x is inf
bool vnl_math_isinf(long double x) { return !_finitel(x) && !vnl_math_isnan(x); }
#elif !defined(VNL_HAS_NO_FINITE)
//: Return true if x is inf
bool vnl_math_isinf(float x) { return !finitef(x) && !vnl_math_isnan(x); }
//: Return true if x is inf
bool vnl_math_isinf(double x) { return !finite(x) && !vnl_math_isnan(x); }
//: Return true if x is inf
bool vnl_math_isinf(long double x) { return !finitel(x) && !vnl_math_isnan(x); }
#else
// Assume IEEE floating point number representation
bool vnl_math_isinf(float x) {return(bMe(&x,0x7f800000L,sz_f)&&!bMp(&x,0x007fffffL,sz_f))||bMp(&x,0x7fffffffL,sz_f)==0x7f7fffffL;}
bool vnl_math_isinf(double x) { return bMe(&x,0x7ff00000L,sz_d) && !bMp(&x,0x000fffffL,sz_d); }
bool vnl_math_isinf(long double x)
{
  if (sizeof(long double) == 8) return bMe(&x,0x7ff00000L,sz_l) && !bMp(&x,0x000fffffL,sz_l);
  else if (sizeof(long double) <= 12) return (bMe(&x,0xbfff7fffL,sz_l)||bMe(&x,0x4001ffffL,sz_l))&&!bMp(&x,0x40000000,sz_l-4);
  else return bMe(&x,0x7ff70000L,sz_l) && !bMp(&x,0x0008ffffL,sz_l);
}
#endif

//----------------------------------------------------------------------

//: Type-accessible infinities for use in templates.
template <class T> T vnl_huge_val(T);
#ifndef VCL_ICC_81
double vnl_huge_val(double) { return HUGE_VAL; }
float  vnl_huge_val(float)  { return (float)HUGE_VAL; }
#else
// workaround ICC warning that 0x1.0p2047 cannot be represented exactly.
double vnl_huge_val(double) { return // 2^2047
16158503035655503650357438344334975980222051334857742016065172713762\
32756943394544659860070576145673184435898046094900974705977957524546\
05475440761932241415603154386836504980458750988751948260533980288191\
92033784138396109321309878080919047169238085235290822926018152521443\
78794577053290430377619956196519276095716669483417121034248739328228\
47474280880176631610290389028296655130963542301570751292964320885583\
62971801859230928678799175576150822952201848806616643615613562842355\
41010486257855086346566173483927129032834896752299863417649931910776\
25831947186677718010677166148023226592393024760740967779268055297981\
15328.0; }
float  vnl_huge_val(float)  { return // 2^255
57896044618658097711785492504343953926634992332820282019728792003956\
564819968.0f; }
#endif
#ifdef _INT_64BIT_
long int vnl_huge_val(long int) { return 0x7fffffffffffffffL; }
int    vnl_huge_val(int)    { return 0x7fffffffffffffffL; }
#else
int    vnl_huge_val(int)    { return 0x7fffffff; }
#endif
short  vnl_huge_val(short)  { return 0x7fff; }
char   vnl_huge_val(char)   { return 0x7f; }


namespace
{

template <typename T> T vnl_math_hypot(T x, T y)
{
    T ret_val, d1, d2, d3;
    T p, r, s, t, u;

    d1 = vnl_math_abs(x), d2 = vnl_math_abs(y);
    p = vnl_math_max(d1,d2);
    if (p == 0)
        return 0.0;

    d2 = vnl_math_abs(x), d3 = vnl_math_abs(y);
    d1 = vnl_math_min(d2,d3) / p;
    r = d1 * d1;

    t = r + 4.0;
    while (t != 4.0)
    {
        s = r / t;
        u = s * 2. + 1.;
        p = u * p;
        d1 = s / u;
        r = d1 * d1 * r;

        t = r + 4.0;
    }

    ret_val = p;
    return ret_val;
}

}

int vnl_math_hypot(int x, int y)
{
  return vnl_math_hypot<double>(x,y);
}

float vnl_math_hypot(float x, float y)
{
  return vnl_math_hypot<double>(x,y);
}

double vnl_math_hypot(double x, double y)
{
  return vnl_math_hypot<double>(x,y);
}

long double vnl_math_hypot(long double x, long double y)
{
  return vnl_math_hypot<long double>(x,y);
}



//----------------------------------------------------------------------
namespace vnl_math
{
double angle_0_to_2pi(const double angle)
  {
  const double twopi= vnl_math::pi*2.0;
  double a;
  if ( angle >= twopi )
    a = vcl_fmod( angle, twopi );
  else if ( angle < 0 )
    a = twopi + vcl_fmod(angle,twopi);
  else
    a = angle;

  // added by Nhon: these two lines of code are to fix the bug when
  // angle = -1.1721201390607859e-016
  // then after all the computation, we get
  // a = 6.2831853071795862 == twopi !!!!!!!
  // this situation can happen when a is very close to zero.

  if ( ! ( ( a >= 0 ) && ( a < twopi ) ) ) {
    a = 0;
  }
  return a;
  }
}
