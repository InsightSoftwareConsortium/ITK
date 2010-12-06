// This is core/vnl/vnl_rational.cxx
#include "vnl_rational.h"
//:
// \file

#include <vnl/vnl_numeric_traits.h> // for vnl_numeric_traits<long>::maxval
#include <vcl_cassert.h>

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

//: Multiply/assign: replace lhs by lhs * rhs
//  Note that 0 * Inf and Inf * 0 are undefined.
//  Also note that there could be integer overflow during this calculation!
//  In that case, an approximate result will be returned.
vnl_rational& vnl_rational::operator*=(vnl_rational const& r)
{
  assert(num_!=0 || den_ != 0); // 0 * Inf is undefined
  long a = vnl_rational::gcd(r.numerator(),den_),
       b = vnl_rational::gcd(r.denominator(),num_);
  num_ /= b; den_ /= a;
  a = r.numerator()/a; b = r.denominator()/b;
  // find out whether overflow would occur; in that case, return approximate result
  double n = double(a) * double(num_),
         d = double(b) * double(den_);
  if (n < vnl_numeric_traits<long>::maxval && d < vnl_numeric_traits<long>::maxval)
  { num_ *= a; den_ *= b; normalize(); return *this; }
  else
    return *this = vnl_rational(n/d);
}

//: Multiply/assign: replace lhs by lhs * rhs
//  Note that there could be integer overflow during this calculation!
//  In that case, an approximate result will be returned.
vnl_rational& vnl_rational::operator*=(long r)
{
  long a = vnl_rational::gcd(r,den_);
  den_ /= a; r /= a;
  // find out whether overflow would occur; in that case, return approximate result
  double n = double(r) * double(num_);
  if (n < vnl_numeric_traits<long>::maxval)
  { num_ *= r; normalize(); return *this; }
  else
    return *this = vnl_rational(n/double(den_));
}

//: Divide/assign: replace lhs by lhs / rhs
//  Note that 0 / 0 and Inf / Inf are undefined.
//  Also note that there could be integer overflow during this calculation!
//  In that case, an approximate result will be returned.
vnl_rational& vnl_rational::operator/=(vnl_rational const& r)
{
  assert(num_!=0 || den_ != 0); // 0/0, Inf/Inf undefined
  long a = vnl_rational::gcd(r.numerator(),num_),
       b = vnl_rational::gcd(r.denominator(),den_);
  num_ /= a; den_ /= b;
  a = r.numerator()/a; b = r.denominator()/b;
  // find out whether overflow would occur; in that case, return approximate result
  double n = double(b) * double(num_),
         d = double(a) * double(den_);
  if (n < vnl_numeric_traits<long>::maxval && d < vnl_numeric_traits<long>::maxval)
  { num_ *= b; den_ *= a; normalize(); return *this; }
  else
    return *this = vnl_rational(n/d);
}

//: Divide/assign: replace lhs by lhs / rhs
//  Note that 0 / 0 is undefined.
//  Also note that there could be integer overflow during this calculation!
//  In that case, an approximate result will be returned.
vnl_rational& vnl_rational::operator/=(long r)
{
  assert(num_!=0 || r != 0); // 0/0 undefined
  long a = vnl_rational::gcd(r,num_);
  num_ /= a; r /= a;
  // find out whether overflow would occur; in that case, return approximate result
  double d = double(r) * double(den_);
  if (d < vnl_numeric_traits<long>::maxval)
  { den_ *= r; normalize(); return *this; }
  else
    return *this = vnl_rational(double(num_)/d);
}
