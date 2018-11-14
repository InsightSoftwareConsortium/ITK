// This is core/vnl/vnl_math.cxx
//:
// \file

#include <limits>
#include <cmath>
#include "vnl_math.h"

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
