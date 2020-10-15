// This is core/vnl/vnl_rational.h
#ifndef vnl_rational_h_
#define vnl_rational_h_
//:
// \file
// \brief High-precision rational numbers
//
// The  vnl_rational  class  provides  high-precision rational numbers and
// arithmetic, using the built-in type long, for the numerator and denominator.
// Implicit conversion to the system defined signed integral types, float, and
// double is supported by  overloaded  operator member functions.  Although the
// rational class makes judicious use of inline  functions and  deals only with
// integral values, the user  is warned that  the rational  integer  arithmetic
// class is still considerably slower than the built-in  integer data types. If
// the range  of values  anticipated will  fit into a  built-in  type, use that
// instead.
//
// In  addition  to  the  original  COOL Rational class, vnl_rational is able to
// represent plus and minus infinity.  An  other  interesting  addition  is  the
// possibility  to construct a rational from a double.  This allows for lossless
// conversion from e.g. double 1.0/3.0 to the rational number 1/3, hence no more
// rounding errors.  This is implemented with continued fraction approximations.
//
// \author
// Copyright (C) 1991 Texas Instruments Incorporated.
//
// Permission is granted to any individual or institution to use, copy, modify,
// and distribute this software, provided that this complete copyright and
// permission notice is maintained, intact, in all copies and supporting
// documentation.
//
// Texas Instruments Incorporated provides this software "as is" without
// express or implied warranty.
//
// \verbatim
// Modifications
//  Peter Vanroose, 13 July 2001: Added continued fraction cnstrctr from double
//  Peter Vanroose, 10 July 2001: corrected operator%=()
//  Peter Vanroose, 10 July 2001: corrected ceil() and floor() for negative args
//  Peter Vanroose, 10 July 2001: extended operability range of += by using gcd
//  Peter Vanroose, 10 July 2001: added abs().
//  Peter Vanroose, 10 July 2001: removed state data member and added Inf repres
//  Peter Vanroose,  9 July 2001: ported to vnl from COOL
//  Peter Vanroose, 11 June 2009: made "*" and "/" robust against int overflow
//                                (actually a full re-implementation, using gcd)
// \endverbatim

#include <iostream>
#include <type_traits> // For enable_if, is_signed, and is_integral.
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include <cassert>
#include <cstdint>
#include "vnl/vnl_export.h"

//: High-precision rational numbers
//
// The  vnl_rational  class  provides  high-precision rational numbers and
// arithmetic, using the built-in type long, for the numerator and denominator.
// Implicit conversion to the system defined signed integral types, float, and
// double is supported by  overloaded  operator member functions.  Although the
// rational class makes judicious use of inline  functions and  deals only with
// integral values, the user  is warned that  the rational  integer  arithmetic
// class is still considerably slower than the built-in  integer data types. If
// the range  of values  anticipated will  fit into a  built-in  type, use that
// instead.
//
// In  addition  to  the  original  COOL Rational class, vnl_rational is able to
// represent plus and minus infinity.  An  other  interesting  addition  is  the
// possibility  to construct a rational from a double.  This allows for lossless
// conversion from e.g. double 1.0/3.0 to the rational number 1/3, hence no more
// rounding errors.  This is implemented with continued fraction approximations.
//
class VNL_EXPORT vnl_rational
{
 public:
  //! The integer type of the numerator and the denominator.
  using int_type = std::int64_t;

 private:
   int_type num_{0L}; //!< Numerator portion
   int_type den_{1L}; //!< Denominator portion

 public:
   // Helper type alias to be used for SFINAE.
   template <typename T>
   using enable_if_signed_integral_type = typename std::enable_if<
     std::is_signed<T>::value && std::is_integral<T>::value>::type;

  //: Creates a rational with given numerator and denominator.
  //  Default constructor gives 0.
  //  Also serves as automatic cast from int_type to vnl_rational.
  //  The only input which is not allowed is (0,0);
  //  the denominator is allowed to be 0, to represent +Inf or -Inf.

   inline vnl_rational() { normalize(); }

   inline vnl_rational(int_type num) : num_(num) {
     assert(num != 0 || den_ != 0);
     normalize();
   }
  inline vnl_rational(int_type num, int_type den)
    : num_(num), den_(den) { assert(num!=0||den!=0); normalize(); }

  //: Implicitly converts a signed or unsigned integer to a rational.
  template <typename T, typename SFINAE = typename std::enable_if<
                            std::is_integral<T>::value>::type>
  inline vnl_rational(const T num) : num_{static_cast<int_type>(num)} {}

  //: Creates a rational from a double.
  //  This is done by computing the continued fraction approximation for d.
  //  Note that this is explicitly \e not an automatic type conversion.
  vnl_rational(double d);
  vnl_rational(float d);
  //  Copy constructor
  inline vnl_rational(vnl_rational const& from)
    : num_(from.numerator()), den_(from.denominator()) {}
  //  Destructor
  inline ~vnl_rational() = default;
  //  Assignment: overwrite an existing vnl_rational
  inline void set(int_type num, int_type den) { assert(num!=0||den!=0); num_=num; den_=den; normalize(); }

  //: Return the numerator of the (simplified) rational number representation
  inline int_type numerator() const { return num_; }
  //: Return the denominator of the (simplified) rational number representation
  inline int_type denominator() const { return den_; }

  //: Copies the contents and state of rhs rational over to the lhs
  inline vnl_rational& operator=(vnl_rational const& rhs) {
    num_ = rhs.numerator(); den_ = rhs.denominator(); return *this; }

  //: Returns true if the two rationals have the same representation
  inline bool operator==(vnl_rational const& rhs) const {
    return num_ == rhs.numerator() && den_ == rhs.denominator(); }
  inline bool operator!=(vnl_rational const& rhs) const { return !operator==(rhs); }
  inline bool operator==(int_type rhs) const { return num_ == rhs && den_ == 1; }
  inline bool operator!=(int_type rhs) const { return !operator==(rhs); }
  template <typename T, typename SFINAE = enable_if_signed_integral_type<T>>
  inline bool operator==(const T rhs) const { return num_ == rhs && den_ == 1; }
  template <typename T, typename SFINAE = enable_if_signed_integral_type<T>>
  inline bool operator!=(const T rhs) const { return !operator==(rhs); }

  //: Unary minus - returns the negation of the current rational.
  inline vnl_rational operator-() const { return vnl_rational(-num_, den_); }
  //: Unary plus - returns the current rational.
  inline vnl_rational operator+() const { return *this; }
  //: Unary not - returns true if rational is equal to zero.
  inline bool operator!() const { return num_ == 0L; }
  //: Returns the absolute value of the current rational.
  inline vnl_rational abs() const { return vnl_rational(num_<0?-num_:num_, den_); }
  //: Replaces rational with 1/rational and returns it.
  //  Inverting 0 gives +Inf, inverting +-Inf gives 0.
  vnl_rational& invert() {
    int_type t = num_; num_ = den_; den_ = t; normalize(); return *this; }

  //: Plus/assign: replace lhs by lhs + rhs
  //  Note that +Inf + -Inf and -Inf + +Inf are undefined.
  inline vnl_rational& operator+=(vnl_rational const& r) {
    if (den_ == r.denominator()) num_ += r.numerator();
    else { int_type c = vnl_rational::gcd(den_,r.denominator()); if (c==0) c=1;
           num_ = num_*(r.denominator()/c) + (den_/c)*r.numerator();
           den_ *= r.denominator()/c; }
    assert(num_!=0 || den_ != 0); // +Inf + -Inf is undefined
    normalize(); return *this;
  }
  inline vnl_rational& operator+=(int_type r) { num_ += den_*r; return *this; }
  //: Minus/assign: replace lhs by lhs - rhs
  //  Note that +Inf - +Inf and -Inf - -Inf are undefined.
  inline vnl_rational& operator-=(vnl_rational const& r) {
    if (den_ == r.denominator()) num_ -= r.num_;
    else { int_type c = vnl_rational::gcd(den_,r.denominator()); if (c==0) c=1;
           num_ = num_*(r.denominator()/c) - (den_/c)*r.numerator();
           den_ *= r.denominator()/c; }
    assert(num_!=0 || den_ != 0); // +Inf - +Inf is undefined
    normalize(); return *this;
  }
  inline vnl_rational& operator-=(int_type r) { num_ -= den_*r; return *this; }
  //: Multiply/assign: replace lhs by lhs * rhs
  //  Note that 0 * Inf and Inf * 0 are undefined.
  //  Also note that there could be integer overflow during this calculation!
  //  In that case, an approximate result will be returned.
  vnl_rational& operator*=(vnl_rational const& r);
  //: Multiply/assign: replace lhs by lhs * rhs
  //  Note that there could be integer overflow during this calculation!
  //  In that case, an approximate result will be returned.
  vnl_rational& operator*=(int_type r);
  //: Divide/assign: replace lhs by lhs / rhs
  //  Note that 0 / 0 and Inf / Inf are undefined.
  //  Also note that there could be integer overflow during this calculation!
  //  In that case, an approximate result will be returned.
  vnl_rational& operator/=(vnl_rational const& r);
  //: Divide/assign: replace lhs by lhs / rhs
  //  Note that 0 / 0 is undefined.
  //  Also note that there could be integer overflow during this calculation!
  //  In that case, an approximate result will be returned.
  vnl_rational& operator/=(int_type r);
  //: Modulus/assign: replace lhs by lhs % rhs
  //  Note that r % Inf is r, and that r % 0 and Inf % r are undefined.
  inline vnl_rational& operator%=(vnl_rational const& r) {
    assert(r.numerator() != 0);
    if (den_ == r.denominator()) num_ %= r.numerator();
    else { int_type c = vnl_rational::gcd(den_,r.denominator()); if (c==0) c=1;
           num_ *= r.denominator()/c;
           num_ %= (den_/c)*r.numerator();
           den_ *= r.denominator()/c; }
    normalize(); return *this;
  }
  inline vnl_rational& operator%=(int_type r) {assert(r);num_%=den_*r;normalize();return *this;}

  //: Pre-increment (++r).  No-op when +-Inf.
  inline vnl_rational& operator++() { num_ += den_; return *this; }
  //: Pre-decrement (--r).  No-op when +-Inf.
  inline vnl_rational& operator--() { num_ -= den_; return *this; }
  //: Post-increment (r++).  No-op when +-Inf.
  inline vnl_rational operator++(int) {vnl_rational b=*this;num_+=den_;return b;}
  //: Post-decrement (r--).  No-op when +-Inf.
  inline vnl_rational operator--(int) {vnl_rational b=*this;num_-=den_;return b;}

  inline bool operator<(vnl_rational const& rhs) const {
    if (den_ == rhs.denominator())   // If same denominator
      return num_ < rhs.numerator(); // includes the case -Inf < +Inf
    // note that denominator is always >= 0:
    else
      return num_ * rhs.denominator() < den_ * rhs.numerator();
  }
  inline bool operator>(vnl_rational const& r) const { return r < *this; }
  inline bool operator<=(vnl_rational const& r) const { return !operator>(r); }
  inline bool operator>=(vnl_rational const& r) const { return !operator<(r); }
  inline bool operator<(int_type r) const { return num_ < den_ * r; }
  inline bool operator>(int_type r) const { return num_ > den_ * r; }
  inline bool operator<=(int_type r) const { return !operator>(r); }
  inline bool operator>=(int_type r) const { return !operator<(r); }
  template <typename T, typename SFINAE = enable_if_signed_integral_type<T>>
  inline bool operator<(const T r) const { return num_ < den_ * r; }
  template <typename T, typename SFINAE = enable_if_signed_integral_type<T>>
  inline bool operator>(const T r) const { return num_ > den_ * r; }
  template <typename T, typename SFINAE = enable_if_signed_integral_type<T>>
  inline bool operator<=(const T r) const { return !operator>(r); }
  template <typename T, typename SFINAE = enable_if_signed_integral_type<T>>
  inline bool operator>=(const T r) const { return !operator<(r); }

  inline bool operator<(double r) const { return num_ < den_ * r; }
  inline bool operator>(double r) const { return num_ > den_ * r; }
  inline bool operator<=(double r) const { return !operator>(r); }
  inline bool operator>=(double r) const { return !operator<(r); }

  //: Converts rational value to integer by truncating towards zero.
  inline int_type truncate() const { assert(den_ != 0);  return num_/den_; }
  //: Converts rational value to integer by truncating towards negative infinity.
  inline int_type floor() const { int_type t = truncate();
    return num_<0L && (num_%den_) != 0 ? t-1 : t; }
  //: Converts rational value to integer by truncating towards positive infinity.
  inline int_type ceil() const { int_type t = truncate();
    return num_>0L && (num_%den_) != 0 ? t+1 : t; }
  //: Rounds rational to nearest integer.
  inline int_type round() const { int_type t = truncate();
    if (num_ < 0) return ((-num_)%den_) >= 0.5*den_ ? t-1 : t;
    else          return   (num_ %den_) >= 0.5*den_ ? t+1 : t;
  }

  // Implicit conversions
  template <typename T, typename SFINAE = enable_if_signed_integral_type<T>>
  inline operator T() const {
    int_type t = truncate(); auto r = static_cast<T>(t);
    assert(r == t); // abort on underflow or overflow
    return r;
  }
  inline operator int_type() const { return truncate(); }
  inline operator float() const { return ((float)num_)/((float)den_); }
  inline operator double() const { return ((double)num_)/((double)den_); }

  //: Calculate greatest common divisor of two integers.
  //  Used to simplify rational number.
  static inline int_type gcd (int_type l1, int_type l2) {
    while (l2!=0) { int_type t = l2; l2 = l1 % l2; l1 = t; }
    return l1<0 ? (-l1) : l1;
  }

 private:
  //: Private function to normalize numerator/denominator of rational number.
  //  If num_ and den_ are both nonzero, their gcd is made 1 and den_ made positive.
  //  Otherwise, the nonzero den_ is set to 1 or the nonzero num_ to +1 or -1.
  inline void normalize() {
    if (num_ == 0) { den_ = 1; return; } // zero
    if (den_ == 0) { num_ = (num_>0) ? 1 : -1; return; } // +-Inf
    if (num_ != 1 && num_ != -1 && den_ != 1) {
      int_type common = vnl_rational::gcd(num_, den_);
      if (common != 1) { num_ /= common; den_ /= common; }
    }
    // if negative, put sign in numerator:
    if (den_ < 0) { num_ *= -1; den_ *= -1; }
  }
};

//: formatted output
// \relatesalso vnl_rational
inline std::ostream& operator<<(std::ostream& s, vnl_rational const& r)
{
  return s << r.numerator() << '/' << r.denominator();
}

//: simple input
// \relatesalso vnl_rational
inline std::istream& operator>>(std::istream& s, vnl_rational& r)
{
  vnl_rational::int_type n, d; s >> n >> d;
  r.set(n,d); return s;
}

//: Returns the sum of two rational numbers.
// \relatesalso vnl_rational
inline vnl_rational operator+(vnl_rational const& r1, vnl_rational const& r2)
{
  vnl_rational result(r1); return result += r2;
}

inline vnl_rational operator+(vnl_rational const& r1, vnl_rational::int_type r2)
{
  vnl_rational result(r1); return result += r2;
}

template <typename T, typename SFINAE = vnl_rational::enable_if_signed_integral_type<T>>
inline vnl_rational operator+(vnl_rational const& r1, const T r2)
{
  vnl_rational result(r1); return result += vnl_rational::int_type{r2};
}

inline vnl_rational operator+(vnl_rational::int_type r2, vnl_rational const& r1)
{
  vnl_rational result(r1); return result += r2;
}

template <typename T, typename SFINAE = vnl_rational::enable_if_signed_integral_type<T>>
inline vnl_rational operator+(const T r2, vnl_rational const& r1)
{
  vnl_rational result(r1); return result += vnl_rational::int_type{r2};
}

//: Returns the difference of two rational numbers.
// \relatesalso vnl_rational
inline vnl_rational operator-(vnl_rational const& r1, vnl_rational const& r2)
{
  vnl_rational result(r1); return result -= r2;
}

inline vnl_rational operator-(vnl_rational const& r1, vnl_rational::int_type r2)
{
  vnl_rational result(r1); return result -= r2;
}

template <typename T, typename SFINAE = vnl_rational::enable_if_signed_integral_type<T>>
inline vnl_rational operator-(vnl_rational const& r1, const T r2)
{
  vnl_rational result(r1); return result -= vnl_rational::int_type{r2};
}

inline vnl_rational operator-(vnl_rational::int_type r2, vnl_rational const& r1)
{
  vnl_rational result(-r1); return result += r2;
}

template <typename T, typename SFINAE = vnl_rational::enable_if_signed_integral_type<T>>
inline vnl_rational operator-(const T r2, vnl_rational const& r1)
{
  vnl_rational result(-r1); return result += vnl_rational::int_type{r2};
}

//: Returns the product of two rational numbers.
// \relatesalso vnl_rational
inline vnl_rational operator*(vnl_rational const& r1, vnl_rational const& r2)
{
  vnl_rational result(r1); return result *= r2;
}

inline vnl_rational operator*(vnl_rational const& r1, vnl_rational::int_type r2)
{
  vnl_rational result(r1); return result *= r2;
}

template <typename T, typename SFINAE = vnl_rational::enable_if_signed_integral_type<T>>
inline vnl_rational operator*(vnl_rational const& r1, const T r2)
{
  vnl_rational result(r1); return result *= vnl_rational::int_type{r2};
}

inline vnl_rational operator*(vnl_rational::int_type r2, vnl_rational const& r1)
{
  vnl_rational result(r1); return result *= r2;
}

template <typename T, typename SFINAE = vnl_rational::enable_if_signed_integral_type<T>>
inline vnl_rational operator*(const T r2, vnl_rational const& r1)
{
  vnl_rational result(r1); return result *= vnl_rational::int_type{r2};
}

//: Returns the quotient of two rational numbers.
// \relatesalso vnl_rational
inline vnl_rational operator/(vnl_rational const& r1, vnl_rational const& r2)
{
  vnl_rational result(r1); return result /= r2;
}

inline vnl_rational operator/(vnl_rational const& r1, vnl_rational::int_type r2)
{
  vnl_rational result(r1); return result /= r2;
}

template <typename T, typename SFINAE = vnl_rational::enable_if_signed_integral_type<T>>
inline vnl_rational operator/(vnl_rational const& r1, const T r2)
{
  vnl_rational result(r1); return result /= vnl_rational::int_type{r2};
}

inline vnl_rational operator/(vnl_rational::int_type r1, vnl_rational const& r2)
{
  vnl_rational result(r1); return result /= r2;
}

template <typename T, typename SFINAE = vnl_rational::enable_if_signed_integral_type<T>>
inline vnl_rational operator/(const T r1, vnl_rational const& r2)
{
  vnl_rational result(vnl_rational::int_type{r1}); return result /= r2;
}

//: Returns the remainder of r1 divided by r2.
// \relatesalso vnl_rational
inline vnl_rational operator%(vnl_rational const& r1, vnl_rational const& r2)
{
  vnl_rational result(r1); return result %= r2;
}

inline vnl_rational operator%(vnl_rational const& r1, vnl_rational::int_type r2)
{
  vnl_rational result(r1); return result %= r2;
}

template <typename T, typename SFINAE = vnl_rational::enable_if_signed_integral_type<T>>
inline vnl_rational operator%(vnl_rational const& r1, const T r2)
{
  vnl_rational result(r1); return result %= vnl_rational::int_type{r2};
}

inline vnl_rational operator%(vnl_rational::int_type r1, vnl_rational const& r2)
{
  vnl_rational result(r1); return result %= r2;
}

template <typename T, typename SFINAE = vnl_rational::enable_if_signed_integral_type<T>>
inline vnl_rational operator%(const T r1, vnl_rational const& r2)
{
  vnl_rational result(vnl_rational::int_type{r1}); return result %= r2;
}

template <typename T, typename SFINAE = vnl_rational::enable_if_signed_integral_type<T>>
inline bool operator==(const T r1, vnl_rational const& r2) { return r2.operator==(r1); }
inline bool operator==(vnl_rational::int_type r1, vnl_rational const& r2) { return r2==r1; }
template <typename T, typename SFINAE = vnl_rational::enable_if_signed_integral_type<T>>
inline bool operator!=(const T r1, vnl_rational const& r2) { return r2 != r1; }
inline bool operator!=(vnl_rational::int_type r1, vnl_rational const& r2) { return r2!=r1; }
template <typename T, typename SFINAE = vnl_rational::enable_if_signed_integral_type<T>>
inline bool operator< (const T r1, vnl_rational const& r2) { return r2 > r1; }
inline bool operator< (vnl_rational::int_type r1, vnl_rational const& r2) { return r2> r1; }
template <typename T, typename SFINAE = vnl_rational::enable_if_signed_integral_type<T>>
inline bool operator> (const T r1, vnl_rational const& r2) { return r2 < r1; }
inline bool operator> (vnl_rational::int_type r1, vnl_rational const& r2) { return r2< r1; }
template <typename T, typename SFINAE = vnl_rational::enable_if_signed_integral_type<T>>
inline bool operator<=(const T r1, vnl_rational const& r2) { return r2>=r1; }
inline bool operator<=(vnl_rational::int_type r1, vnl_rational const& r2) { return r2>=r1; }
template <typename T, typename SFINAE = vnl_rational::enable_if_signed_integral_type<T>>
inline bool operator>=(const T r1, vnl_rational const& r2) { return r2 <= r1; }
inline bool operator>=(vnl_rational::int_type r1, vnl_rational const& r2) { return r2<=r1; }

inline vnl_rational::int_type truncate(vnl_rational const& r) { return r.truncate(); }
inline vnl_rational::int_type floor(vnl_rational const& r) { return r.floor(); }
inline vnl_rational::int_type ceil(vnl_rational const& r) { return r.ceil(); }
inline vnl_rational::int_type round(vnl_rational const& r) { return r.round(); }

namespace vnl_math
{
  inline vnl_rational abs(vnl_rational const& x) { return x<0L ? -x : x; }
  inline vnl_rational squared_magnitude(vnl_rational const& x) { return x*x; }
  inline vnl_rational sqr(vnl_rational const& x) { return x*x; }
  inline bool isnan(vnl_rational const& ) {return false;}
  inline bool isfinite(vnl_rational const& x) {return x.denominator() != 0L;}
}

#endif // vnl_rational_h_
