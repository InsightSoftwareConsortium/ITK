// This is core/vnl/vnl_math.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file

#include <limits>
#include "vnl_math.h"
#include <vxl_config.h>
#include <vcl_compiler.h>

#if VXL_FULLCXX11SUPPORT
// Nothing to do for isnan,isfinite,isinf,isnormal
// or hypot here
#else

#if defined(VCL_VC) || defined(__MINGW32__)
// I don't think we need this, because <ieeefp.h> is available -- fsm
# include <float.h> // for 'isnan' and 'finite'
// # define isnan _isnan
# define finite _finite
# define finitef _finite
#ifndef finitel
# define finitel _finite
#endif

#elif VXL_HAS_STD_ISFINITE || VXL_HAS_STD_ISNAN ||  VXL_HAS_STD_ISNORMAL
# include<math.h>
# if VXL_HAS_STD_ISFINITE
#    define finite  std::isfinite
#    define finitef std::isfinite
#    define finitel std::isfinite
#  endif

// # define isnan _isnan // PVr commented out: we don't want to redefine vnl::isnan ...
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

#else
# warning finite() is not declared on this platform
# define VNL_HAS_NO_FINITE
#endif

//--------------------------------------------------------------------------------
namespace vnl_math
{
 //--------------------------------------------------------------------------------
 //: Return true iff x is "Not a Number"
#if VXL_FULLCXX11SUPPORT || VXL_HAS_STD_ISNAN
 //: Return true iff x is "Not a Number"
 bool isnan(float x) { return std::isnan(x); }
 //: Return true iff x is "Not a Number"
 bool isnan(double x) { return std::isnan(x); }
 //: Return true iff x is "Not a Number"
 bool isnan(long double x) { return std::isnan(x); }
#elif !defined(VNL_HAS_NO_FINITE) && !defined(VCL_WIN32)
//: Return true iff x is "Not a Number"
bool isnan(float x) { return x != x; } // causes "floating exception" on alpha
//: Return true iff x is "Not a Number"
bool isnan(double x) { return x != x; }
//: Return true iff x is "Not a Number"
bool isnan(long double x) { return x != x; }
#else
// Auxiliary function to simplify notation
# ifndef DEBUG
static inline unsigned int bMp(void*x,unsigned int y,int p=0) {return ((((unsigned int*)x)[p])&y);}
static inline bool bMe(void*x,unsigned int y,int p=0) {return ((((unsigned int*)x)[p])&y)==y;}
# else
# include <std::iostream.h>
static inline unsigned int bMp(void* x, unsigned int y, int p=0)
{
  unsigned char* v=(unsigned char*)x;
  std::cout<<int(v[4*p])<<' '<<int(v[4*p+1])<<' '<<int(v[4*p+2])<<' '<<int(v[4*p+3])<<" & ";
  v=(unsigned char*)(&y);
  std::cout<<int(v[0])<<' '<<int(v[1])<<' '<<int(v[2])<<' '<<int(v[3])<<" = ";
  unsigned int z = ((((unsigned int*)x)[p]) & y);
  v=(unsigned char*)(&z);
  std::cout<<int(v[0])<<' '<<int(v[1])<<' '<<int(v[2])<<' '<<int(v[3]);
  if (z == y) std::cout<<" ==";
  std::cout << '\n';
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
bool isnan( float x) {return bMe(&x,0x7f800000L,sz_f)&&bMp(&x,0x007fffffL,sz_f);}
bool isnan(double x) {return bMe(&x,0x7ff00000L,sz_d)&&(bMp(&x,0x000fffffL,sz_d)||bMp(&x,0xffffffffL,1-sz_d));}
bool isnan(long double x)
{
  if (sizeof(long double) == 8) return bMe(&x,0x7ff00000L,sz_l) && (bMp(&x,0x000fffffL,sz_l)||bMp(&x,0xffffffffL,1-sz_d));
  else if (sizeof(long double) <= 12) // This code doesn't properly check the less significant
                                      // bytes for non-zero-ness to distinguish inf from nan
                                      // see http://babbage.cs.qc.cuny.edu/IEEE-754/References.xhtml#tables
# if defined LDBL_MANT_DIG && LDBL_MANT_DIG<=53
    return bMe(&x,0x4001ffffL,sz_l) && bMp(&x,0x40000000,sz_l-4);
# else
    return bMe(&x,0x7ff00000L,sz_l) && bMp(&x,0x000fffffL,sz_l-4);
# endif
  else return bMe(&x,0x7ff00000L,sz_l) && bMp(&x,0x0000ffffL,sz_l);
}
#endif

#if VXL_FULLCXX11SUPPORT || VXL_HAS_STD_ISFINITE
//: Return true if x is neither NaN nor Inf.
bool isfinite(float x) { return std::isfinite(x); }
//: Return true if x is neither NaN nor Inf.
bool isfinite(double x) { return std::isfinite(x); }
//: Return true if x is neither NaN nor Inf.
bool isfinite(long double x) { return std::isfinite(x); }
#elif !defined(VNL_HAS_NO_FINITE)
//: Return true if x is neither NaN nor Inf.
bool isfinite(float x) { return finitef(x) != 0; }
//: Return true if x is neither NaN nor Inf.
bool isfinite(double x) { return finite(x) != 0; }
//: Return true if x is neither NaN nor Inf.
bool isfinite(long double x) { return finitel(x) != 0; }
#else
// Assume IEEE floating point number representation
bool isfinite(float x) { return !bMe(&x,0x7f800000L,sz_f) && bMp(&x,0x7fffffffL,sz_f) != 0x7f7fffffL; }
bool isfinite(double x) { return !bMe(&x,0x7ff00000L,sz_d); }
bool isfinite(long double x)
{
  if (sizeof(long double) == 8) return !bMe(&x,0x7ff00000L,sz_l);
  else if (sizeof(long double) <= 12) return !bMe(&x,0xbfff7fffL,sz_l) && !bMe(&x,0x4001ffffL,sz_l);
  else return !bMe(&x,0x7ff70000L,sz_l);
}
#endif


#if VXL_FULLCXX11SUPPORT || VXL_HAS_STD_ISINF
//: Return true if x is inf
bool isinf(float x) { return std::isinf(x); }
//: Return true if x is inf
bool isinf(double x) { return std::isinf(x); }
//: Return true if x is inf
bool isinf(long double x) { return std::isinf(x); }
#elif !defined(VNL_HAS_NO_FINITE)
//: Return true if x is inf
bool isinf(float x) { return ! vnl_math::isfinite(x) && ! vnl_math::isnan(x); }
//: Return true if x is inf
bool isinf(double x) { return ! vnl_math::isfinite(x) && ! vnl_math::isnan(x); }
//: Return true if x is inf
bool isinf(long double x) { return ! vnl_math::isfinite(x) && ! vnl_math::isnan(x); }
#else
// Assume IEEE floating point number representation
bool isinf(float x) {return(bMe(&x,0x7f800000L,sz_f)&&!bMp(&x,0x007fffffL,sz_f))||bMp(&x,0x7fffffffL,sz_f)==0x7f7fffffL;}
bool isinf(double x) { return bMe(&x,0x7ff00000L,sz_d) && !bMp(&x,0x000fffffL,sz_d); }
bool isinf(long double x)
{
  if (sizeof(long double) == 8) return bMe(&x,0x7ff00000L,sz_l) && !bMp(&x,0x000fffffL,sz_l);
  else if (sizeof(long double) <= 12) return (bMe(&x,0xbfff7fffL,sz_l)||bMe(&x,0x4001ffffL,sz_l))&&!bMp(&x,0x40000000,sz_l-4);
  else return bMe(&x,0x7ff70000L,sz_l) && !bMp(&x,0x0008ffffL,sz_l);
}
#endif

#if VXL_FULLCXX11SUPPORT || VXL_HAS_STD_ISNORMAL
//: Return true if x is inf
bool isnormal(float x) { return std::isnormal(x); }
//: Return true if x is inf
bool isnormal(double x) { return std::isnormal(x); }
//: Return true if x is inf
bool isnormal(long double x) { return std::isnormal(x); }
#else
//: Return true if x is inf
bool isnormal(float x) { return vnl_math::isfinite(x) && ( x != 0.0 ); }
//: Return true if x is inf
bool isnormal(double x) { return vnl_math::isfinite(x) && ( x != 0.0 ); }
//: Return true if x is inf
bool isnormal(long double x) { return vnl_math::isfinite(x) && (x != 0.0 ); }
#endif

} // end namespace vnl_math

//----------------------------------------------------------------------
namespace vnl_math
{

namespace
{
template <typename OutType, typename T> OutType hypot(const T x, const T y)
{
    T ret_val;
    {
    const T d1 = vnl_math::abs(x);
    const T d2 = vnl_math::abs(y);
    ret_val = vnl_math::max(d1,d2);
    if (ret_val == static_cast<T>(0))
       {
       return static_cast<OutType>(0);
       }
    }
    {
    const T d2 = vnl_math::abs(x);
    const T d3 = vnl_math::abs(y);
    T d1 = vnl_math::min(d2,d3) / ret_val;

    T r = d1 * d1;
    T t = r + 4.0;
    while (t != 4.0)
      {
      const T s = r / t;
      const T u = s * 2. + 1.;
      ret_val = u * ret_val;
      d1 = s / u;
      r = d1 * d1 * r;
      t = r + 4.0;
      }
    }
    return static_cast<OutType>(ret_val);
}
}

int hypot(int x, int y)
{
  return hypot<int,double>(x,y);
}

float hypot(float x, float y)
{
  return hypot<float,double>(x,y);
}

double hypot(double x, double y)
{
  return hypot<double,double>(x,y);
}

long double hypot(long double x, long double y)
{
  return hypot<long double,long double>(x,y);
}
} // end namespace vnl_math

#endif

//----------------------------------------------------------------------

//: Type-accessible infinities for use in templates.
template <class T> T vnl_huge_val(T);
float  vnl_huge_val(float)  { return std::numeric_limits<float>::infinity(); }
double  vnl_huge_val(double)  { return std::numeric_limits<double>::infinity(); }
long double vnl_huge_val(long double) { return std::numeric_limits<long double>::infinity(); }

#ifdef _INT_64BIT_
long int vnl_huge_val(long int) { return 0x7fffffffffffffffL; }
int    vnl_huge_val(int)    { return 0x7fffffffffffffffL; }
#else
int    vnl_huge_val(int)    { return 0x7fffffff; }
#endif
short  vnl_huge_val(short)  { return 0x7fff; }
char   vnl_huge_val(char)   { return 0x7f; }


//----------------------------------------------------------------------
namespace vnl_math
{
double angle_0_to_2pi(double angle)
{
  angle = std::fmod(angle, vnl_math::twopi);
  if (angle >= 0) return angle;
  double a = angle + vnl_math::twopi;
  if (a > 0 && a < vnl_math::twopi) return a;
  // added by Nhon: this fixes a bug when angle >= -1.1721201390607859e-016 :
  // then after the above computation we get 6.2831853071795864769 == twopi
  // while this function guarantees that it returns values < twopi !!!
  if (angle < 0) return 6.28318530717958575;
  else return angle;
}

double angle_minuspi_to_pi(double angle)
{
  angle = std::fmod(angle, vnl_math::twopi);
  if (angle> vnl_math::pi) angle -= vnl_math::twopi;
  if (angle<-vnl_math::pi) angle += vnl_math::twopi;
  return angle;
}
}; // end namespace vnl_math
