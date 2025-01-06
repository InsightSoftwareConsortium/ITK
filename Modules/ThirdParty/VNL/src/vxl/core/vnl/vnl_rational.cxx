// This is core/vnl/vnl_rational.cxx
#include <cassert>
#include "vnl_rational.h"
//:
// \file

#include "vnl/vnl_numeric_traits.h" // for vnl_numeric_traits<long>::maxval

using int_type = vnl_rational::int_type;

static constexpr double maxint_as_double = static_cast<double>(vnl_numeric_traits<int_type>::maxval);

template <typename FloatingType>
inline void
makeNumDen(FloatingType d, int_type & num_, int_type & den_)
{
  bool sign = d < 0;
  if (sign)
    d = -d;

  // Continued fraction approximation of abs(d): recursively determined
  int_type den = 0L, num = 1L, prev_den = 1L, prev_num = 0L;

  while (d * num < 1e9 && d * den < 1e9)
  {
    int_type a = static_cast<int_type>(d); // integral part of d
    d -= a;                                // certainly >= 0
    int_type temp = num;
    num = a * num + prev_num;
    prev_num = temp;
    temp = den;
    den = a * den + prev_den;
    prev_den = temp;
    if (d < 1e-6)
      break;
    d = 1 / d;
  }
  num_ = num;
  den_ = den;
  if (sign)
    num_ = -num_;
  // no need to normalize() since prev_num and prev_den have guaranteed a gcd=1
}

//: Creates a rational from a double.
//  This is done by computing the continued fraction approximation for d.
vnl_rational::vnl_rational(double d)
{
  makeNumDen<double>(d, num_, den_);
}

//: Creates a rational from a double.
//  This is done by computing the continued fraction approximation for d.
vnl_rational::vnl_rational(float f)
{
  makeNumDen<double>(f, num_, den_);
}

//: Multiply/assign: replace lhs by lhs * rhs
//  Note that 0 * Inf and Inf * 0 are undefined.
//  Also note that there could be integer overflow during this calculation!
//  In that case, an approximate result will be returned.
vnl_rational &
vnl_rational::operator*=(vnl_rational const & r)
{
  assert(num_ != 0 || den_ != 0); // 0 * Inf is undefined
  int_type a = vnl_rational::gcd(r.numerator(), den_), b = vnl_rational::gcd(r.denominator(), num_);
  num_ /= b;
  den_ /= a;
  a = r.numerator() / a;
  b = r.denominator() / b;
  // find out whether overflow would occur; in that case, return approximate result
  double n = double(a) * double(num_);
  double d = double(b) * double(den_);
  if (n < maxint_as_double && d < maxint_as_double)
  {
    num_ *= a;
    den_ *= b;
    normalize();
    return *this;
  }
  else
    return *this = vnl_rational(n / d);
}

//: Multiply/assign: replace lhs by lhs * rhs
//  Note that there could be integer overflow during this calculation!
//  In that case, an approximate result will be returned.
vnl_rational &
vnl_rational::operator*=(int_type r)
{
  int_type a = vnl_rational::gcd(r, den_);
  den_ /= a;
  r /= a;
  // find out whether overflow would occur; in that case, return approximate result
  double n = double(r) * double(num_);
  if (n < maxint_as_double)
  {
    num_ *= r;
    normalize();
    return *this;
  }
  else
    return *this = vnl_rational(n / double(den_));
}

//: Divide/assign: replace lhs by lhs / rhs
//  Note that 0 / 0 and Inf / Inf are undefined.
//  Also note that there could be integer overflow during this calculation!
//  In that case, an approximate result will be returned.
vnl_rational &
vnl_rational::operator/=(vnl_rational const & r)
{
  assert(num_ != 0 || den_ != 0); // 0/0, Inf/Inf undefined
  int_type a = vnl_rational::gcd(r.numerator(), num_), b = vnl_rational::gcd(r.denominator(), den_);
  num_ /= a;
  den_ /= b;
  a = r.numerator() / a;
  b = r.denominator() / b;
  // find out whether overflow would occur; in that case, return approximate result
  double n = double(b) * double(num_), d = double(a) * double(den_);
  if (n < maxint_as_double && d < maxint_as_double)
  {
    num_ *= b;
    den_ *= a;
    normalize();
    return *this;
  }
  else
    return *this = vnl_rational(n / d);
}

//: Divide/assign: replace lhs by lhs / rhs
//  Note that 0 / 0 is undefined.
//  Also note that there could be integer overflow during this calculation!
//  In that case, an approximate result will be returned.
vnl_rational &
vnl_rational::operator/=(int_type r)
{
  assert(num_ != 0 || r != 0); // 0/0 undefined
  int_type a = vnl_rational::gcd(r, num_);
  num_ /= a;
  r /= a;
  // find out whether overflow would occur; in that case, return approximate result
  double d = double(r) * double(den_);
  if (d < maxint_as_double)
  {
    den_ *= r;
    normalize();
    return *this;
  }
  else
    return *this = vnl_rational(double(num_) / d);
}
