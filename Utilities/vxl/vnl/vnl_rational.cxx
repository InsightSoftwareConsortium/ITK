// This is vxl/vnl/vnl_rational.cxx
#include "vnl_rational.h"
//:
// \file

//: Creates a rational from a double.
//  This is done by computing the continued fraction approximation for d.
vnl_rational::vnl_rational(double d)
{
  bool sign = d<0;
  if (sign) d = -d;

  // Continued fraction approximation of abs(d): recursively determined
  long den=0L, num=1L, prev_den=1L, prev_num=0L;

  while (d*num < 1e9 && d*den < 1e9) {
    long a = (long)d; // integral part of d
    d -= a; // certainly >= 0
    long temp = num; num = a*num + prev_num; prev_num = temp;
         temp = den; den = a*den + prev_den; prev_den = temp;
    if (d < 1e-6) break;
    d = 1/d;
  }
  num_ = num; den_ = den;
  if (sign) num_ = -num_;
  // no need to normalize() since prev_num and prev_den have guaranteed a gcd=1
}
