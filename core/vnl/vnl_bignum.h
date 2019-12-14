// This is core/vnl/vnl_bignum.h
#ifndef vnl_bignum_h_
#define vnl_bignum_h_
//:
// \file
// \brief Infinite precision integers
//
// The vnl_bignum class implements near-infinite precision integers
// and arithmetic by using a dynamic bit vector. A
// vnl_bignum object will grow in size as necessary to hold its
// integer value.  Implicit conversion to the system defined
// types: short, int, long, float, double and long double
// is supported by overloaded operator member functions.
// Addition and subtraction operators are performed by
// simple bitwise addition and subtraction on
// unsigned short boundaries with checks for carry flag propagation.
// The multiplication, division, and remainder operations
// utilize the algorithms from Knuth's Volume 2 of "The
// Art of Computer Programming". However, despite the use of
// these algorithms and inline member functions, arithmetic
// operations on vnl_bignum objects are considerably slower than
// the built-in integer types that use hardware integer arithmetic
// capabilities.
//
// The vnl_bignum class supports the parsing of character string
// representations of all the literal number formats, PLUS the
// strings "Infinity", "+Infinity" and "-Infinity".  The following
// table shows an example of a character string
// representation on the left and a brief description of the
// interpreted meaning on the right:
//
// Character String  Interpreted Meaning
// 1234              1234
// 1234l             1234
// 1234L             1234
// 1234u             1234
// 1234U             1234
// 1234ul            1234
// 1234UL            1234
// 01234             1234 in octal (leading 0)
// 0x1234            1234 in hexadecimal (leading 0x)
// 0X1234            1234 in hexadecimal (leading 0X)
// 123.4             123 (value truncated)
// 1.234e2           123 (exponent expanded/truncated)
// 1.234e-5          0 (truncated value less than 1)
// Infinity          +Inf ("maxval", obeying all conventional arithmetic)
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
//  Peter Vanroose, 24 January 2002: ported to vnl from COOL
//  Peter Vanroose, 7 September 2002: added "Infinity" (incl. all arithmetic)
//  Ian Scott, 23 March 2004: made ++ and -- much more efficient.
//  Peter Vanroose, March 2008: try to fix divide bug: partially succeeded
//  Peter Vanroose, June 2009: finally fixed this long standing divide bug
// \endverbatim

#include <cassert>
#include <iostream>
#include <limits>
#include <string>
#include  <algorithm>
#include <type_traits> // For enable_if and is_unsigned.
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include "vnl/vnl_export.h"

class vnl_bignum;

// These are all auxiliary functions:

VNL_EXPORT int magnitude_cmp(const vnl_bignum&, const vnl_bignum&);
VNL_EXPORT void add(const vnl_bignum&, const vnl_bignum&, vnl_bignum&);
VNL_EXPORT void subtract(const vnl_bignum&, const vnl_bignum&, vnl_bignum&);
VNL_EXPORT void multiply_aux(const vnl_bignum&, unsigned short d, vnl_bignum&, unsigned short i);
VNL_EXPORT unsigned short normalize(const vnl_bignum&, const vnl_bignum&, vnl_bignum&, vnl_bignum&);
VNL_EXPORT void divide_aux(const vnl_bignum&, unsigned short, vnl_bignum&, unsigned short&);
VNL_EXPORT unsigned short estimate_q_hat(const vnl_bignum&, const vnl_bignum&, unsigned short);
VNL_EXPORT unsigned short multiply_subtract(vnl_bignum&, const vnl_bignum&, unsigned short, unsigned short);
VNL_EXPORT void divide(const vnl_bignum&, const vnl_bignum&, vnl_bignum&, vnl_bignum&);
VNL_EXPORT vnl_bignum left_shift(const vnl_bignum& b1, int l);
VNL_EXPORT vnl_bignum right_shift(const vnl_bignum& b1, int l);
VNL_EXPORT void decrement (vnl_bignum& bnum);
VNL_EXPORT void increment (vnl_bignum& bnum);

//: formatted output
// \relatesalso vnl_bignum
VNL_EXPORT std::ostream& operator<<(std::ostream& s, vnl_bignum const& r);

//: simple input
// \relatesalso vnl_bignum
VNL_EXPORT std::istream& operator>>(std::istream& s, vnl_bignum& r);

//: Infinite precision integers
//
// The vnl_bignum class implements near-infinite precision integers
// and arithmetic by using a dynamic bit vector. A
// vnl_bignum object will grow in size as necessary to hold its
// integer value.  Implicit conversion to the system defined
// types: short, int, long, float, double and long double
// is supported by overloaded operator member functions.
// Addition and subtraction operators are performed by
// simple bitwise addition and subtraction on
// unsigned short boundaries with checks for carry flag propagation.
// The multiplication, division, and remainder operations
// utilize the algorithms from Knuth's Volume 2 of "The
// Art of Computer Programming". However, despite the use of
// these algorithms and inline member functions, arithmetic
// operations on vnl_bignum objects are considerably slower than
// the built-in integer types that use hardware integer arithmetic
// capabilities.
//
// The vnl_bignum class supports the parsing of character string
// representations of all the literal number formats, PLUS the
// strings "Infinity", "+Infinity" and "-Infinity".  The following
// table shows an example of a character string
// representation on the left and a brief description of the
// interpreted meaning on the right:
//
// Character String  Interpreted Meaning
// 1234              1234
// 1234l             1234
// 1234L             1234
// 1234u             1234
// 1234U             1234
// 1234ul            1234
// 1234UL            1234
// 01234             1234 in octal (leading 0)
// 0x1234            1234 in hexadecimal (leading 0x)
// 0X1234            1234 in hexadecimal (leading 0X)
// 123.4             123 (value truncated)
// 1.234e2           123 (exponent expanded/truncated)
// 1.234e-5          0 (truncated value less than 1)
// Infinity          +Inf ("maxval", obeying all conventional arithmetic)
//
class VNL_EXPORT vnl_bignum
{
 private:
  using Counter = unsigned short;
  using Data = unsigned short;
  unsigned short count; // Number of data elements (never 0 except for "0")
  int sign;             // Sign of vnl_bignum (+1 or -1, nothing else!!)
  unsigned short* data; // Pointer to data value

  // Provides the absolute value and the sign of the constructor argument.
  template <typename T, bool is_signed = std::is_signed<T>::value>
  struct constructor_helper
  {
    using unsigned_type = typename std::make_unsigned<T>::type;
    unsigned_type absolute_value;
    int sign;

    explicit constructor_helper(const T arg)
    {
      if (arg < 0)
      {
        // Special case for the lowest signed value, as `-lowest`
        // might cause integer overflow (undefined behavior).
        constexpr auto lowest = std::numeric_limits<T>::lowest();
        constexpr auto absolute_value_of_lowest =
          static_cast<unsigned_type>(
            static_cast<unsigned_type>(-(lowest + 1)) + 1U);

        absolute_value = (arg == lowest) ?
          absolute_value_of_lowest :
          static_cast<unsigned_type>(-arg);
        sign = -1;
      }
      else
      {
        absolute_value = arg;
        sign = 1;
      }
    }
  };

  // Specialization for an unsigned type T.
  template <typename T>
  struct constructor_helper<T, false>
  {
    T absolute_value;
    static constexpr int sign = 1;

    explicit constructor_helper(const T arg)
      :
      absolute_value{ arg }
    {
    }
  };

public:
  vnl_bignum();                        // Void constructor

  // Converting constructor template for any integer type.
  template <typename T, typename SFINAE =
    typename std::enable_if<std::is_integral<T>::value>::type>
  vnl_bignum(const T arg)
    : count(0), data(nullptr)
  {
    const constructor_helper<T> helper{arg};
    auto l = helper.absolute_value;
    this->sign = helper.sign;

    Data buf[sizeof(l)];          // Temp buffer to store l in
    Counter i = 0;                // buffer index
    while (l) {                   // While more bits in l
      assert(i < sizeof(l));      // no more buffer space
      buf[i] = Data(l);           // Peel off lower order bits

      //NOTE: shifting by more than number of bits results in all zeros
      constexpr int maxbits = sizeof(l)*8;
      l >>= std::min(16,maxbits); // Shift next bits into place
      i++;
    }
    if (i > 0)
      this->data = new Data[this->count=i]; // Allocate permanent data

    while (i--)     // Save buffer into perm. data
      this->data[i] = buf[i];
  }

  vnl_bignum(float);                   // Float constructor
  vnl_bignum(double);                  // Double constructor
  vnl_bignum(long double);             // Long Double constructor
  vnl_bignum(vnl_bignum const&);       // Copy constructor
  vnl_bignum(const char*);             // String constructor
  ~vnl_bignum();                       // Destructor

  operator short() const;              // Implicit type conversion
  operator int() const;                // Implicit type conversion
  operator long() const;               // Implicit type conversion
  operator float() const;              // Implicit type conversion
  operator double() const;             // Implicit type conversion
  operator long double() const;        // Implicit type conversion
  inline operator short() { return ((const vnl_bignum*)this)->operator short(); }
  inline operator int() { return ((const vnl_bignum*)this)->operator int(); }
  inline operator long() { return ((const vnl_bignum*)this)->operator long(); }
  inline operator float() { return ((const vnl_bignum*)this)->operator float(); }
  inline operator double() { return ((const vnl_bignum*)this)->operator double(); }
  inline operator long double() { return ((const vnl_bignum*)this)->operator long double(); }

  vnl_bignum operator-() const;        // Unary minus operator
  inline vnl_bignum operator+() const { return *this; } // Unary plus operator

  vnl_bignum& operator=(const vnl_bignum&); // Assignment operator

  vnl_bignum operator<<(int l) const;  // Bit shift
  vnl_bignum operator>>(int l) const;  // Bit shift
  vnl_bignum operator+(vnl_bignum const& r) const;
  inline vnl_bignum& operator+=(vnl_bignum const& r) { return *this = operator+(r); }
  inline vnl_bignum& operator-=(vnl_bignum const& r) { return *this = operator+(-r); }
  vnl_bignum& operator*=(vnl_bignum const& r);
  vnl_bignum& operator/=(vnl_bignum const& r);
  vnl_bignum& operator%=(vnl_bignum const& r);
  inline vnl_bignum& operator<<=(int l) { return *this = *this << l; }
  inline vnl_bignum& operator>>=(int l) { return *this = *this >> l; }

  //: prefix increment (++b)
  vnl_bignum& operator++();
  //: decrement
  vnl_bignum& operator--();
  //: postfix increment (b++)
  inline vnl_bignum operator++(int) { vnl_bignum b=(*this); operator++(); return b; }
  //: decrement
  inline vnl_bignum operator--(int) { vnl_bignum b=(*this); operator--(); return b; }

  bool operator==(vnl_bignum const&) const; // equality
  bool operator< (vnl_bignum const&) const; // less than
  inline bool operator!=(vnl_bignum const& r) const { return !operator==(r); }
  inline bool operator> (vnl_bignum const& r) const { return r<(*this); }
  inline bool operator<=(vnl_bignum const& r) const { return !operator>(r); }
  inline bool operator>=(vnl_bignum const& r) const { return !operator<(r); }
  inline bool operator==(long r) const { return operator==(vnl_bignum(r)); }
  inline bool operator!=(long r) const { return !operator==(vnl_bignum(r)); }
  inline bool operator< (long r) const { return operator<(vnl_bignum(r)); }
  inline bool operator> (long r) const { return vnl_bignum(r) < (*this); }
  inline bool operator<=(long r) const { return !operator>(vnl_bignum(r)); }
  inline bool operator>=(long r) const { return !operator<(vnl_bignum(r)); }
  inline bool operator==(int r) const { return operator==(long(r)); }
  inline bool operator!=(int r) const { return !operator==(long(r)); }
  inline bool operator< (int r) const { return operator<(long(r)); }
  inline bool operator> (int r) const { return vnl_bignum(long(r)) < (*this); }
  inline bool operator<=(int r) const { return !operator>(long(r)); }
  inline bool operator>=(int r) const { return !operator<(long(r)); }
  inline bool operator==(double r) const { return r == this->operator double(); }
  inline bool operator!=(double r) const { return r != this->operator double(); }
  inline bool operator< (double r) const { return r > this->operator double(); }
  inline bool operator> (double r) const { return r < this->operator double(); }
  inline bool operator<=(double r) const { return r >= this->operator double(); }
  inline bool operator>=(double r) const { return r <= this->operator double(); }
  inline bool operator==(long double r) const { return r == this->operator long double(); }
  inline bool operator!=(long double r) const { return r != this->operator long double(); }
  inline bool operator< (long double r) const { return r > this->operator long double(); }
  inline bool operator> (long double r) const { return r < this->operator long double(); }
  inline bool operator<=(long double r) const { return r >= this->operator long double(); }
  inline bool operator>=(long double r) const { return r <= this->operator long double(); }

  inline vnl_bignum abs() const { return operator<(0L) ? operator-() : *this; }

  // "+/-Inf" is represented as: count=1, data[0]=0, sign=+/-1 :
  inline bool is_infinity() const { return count==1 && data && data[0]==0; }
  inline bool is_plus_infinity() const { return is_infinity() && sign==1; }
  inline bool is_minus_infinity() const { return is_infinity() && sign==-1; }

  void dump(std::ostream& = std::cout) const;     // Dump contents of vnl_bignum

  friend int magnitude_cmp(const vnl_bignum&, const vnl_bignum&);
  friend void add(const vnl_bignum&, const vnl_bignum&, vnl_bignum&);
  friend void subtract(const vnl_bignum&, const vnl_bignum&, vnl_bignum&);
  friend void increment (vnl_bignum& bnum);
  friend void decrement (vnl_bignum& bnum);
  friend void multiply_aux(const vnl_bignum&, unsigned short, vnl_bignum&, unsigned short);
  friend unsigned short normalize(const vnl_bignum&, const vnl_bignum&, vnl_bignum&, vnl_bignum&);
  friend void divide_aux(const vnl_bignum&, unsigned short, vnl_bignum&, unsigned short&);
  friend unsigned short estimate_q_hat(const vnl_bignum&, const vnl_bignum&, unsigned short);
  friend unsigned short multiply_subtract(vnl_bignum&, const vnl_bignum&, unsigned short, unsigned short);
  friend void divide(const vnl_bignum&, const vnl_bignum&, vnl_bignum&, vnl_bignum&);
  friend vnl_bignum left_shift(const vnl_bignum& b1, int l);
  friend vnl_bignum right_shift(const vnl_bignum& b1, int l);
  friend std::ostream& operator<< (std::ostream&, const vnl_bignum&);
  friend std::istream& operator>> (std::istream&, vnl_bignum&);
  friend VNL_EXPORT std::string& vnl_bignum_to_string (std::string& s, const vnl_bignum& b);
  friend VNL_EXPORT vnl_bignum& vnl_bignum_from_string (vnl_bignum& b, const std::string& s);

 private:
  void xtoBigNum(const char *s);       // convert hex to vnl_bignum
  int  dtoBigNum(const char *s);       // convert decimal to vnl_bignum
  void otoBigNum(const char *s);       // convert octal to vnl_bignum
  void exptoBigNum(const char *s);     // convert exponential to vnl_bignum

  void resize(short);                  // Resize vnl_bignum data
  vnl_bignum& trim();                  // Trim vnl_bignum data
};


//: Convert the number to a decimal representation in a string.
// \relatesalso vnl_bignum
VNL_EXPORT std::string& vnl_bignum_to_string (std::string& s, const vnl_bignum& b);

//: Convert the number from a decimal representation in a string.
// \relatesalso vnl_bignum
VNL_EXPORT vnl_bignum& vnl_bignum_from_string (vnl_bignum& b, const std::string& s);

//: Returns the sum of two bignum numbers.
// \relatesalso vnl_bignum
inline vnl_bignum operator+(vnl_bignum const& r1, long r2) { return r1+vnl_bignum(r2); }
inline vnl_bignum operator+(vnl_bignum const& r1, int r2) { return r1+long(r2); }
inline vnl_bignum operator+(vnl_bignum const& r1, double r2) { return r1+vnl_bignum(r2); }
inline vnl_bignum operator+(vnl_bignum const& r1, long double r2) { return r1+vnl_bignum(r2); }
inline vnl_bignum operator+(long r2, vnl_bignum const& r1) { return r1 + r2; }
inline vnl_bignum operator+(int r2, vnl_bignum const& r1) { return r1 + r2; }
inline vnl_bignum operator+(double r2, vnl_bignum const& r1) { return r1 + r2; }
inline vnl_bignum operator+(long double r2, vnl_bignum const& r1) { return r1 + r2; }

//: Returns the difference of two bignum numbers.
// \relatesalso vnl_bignum
inline vnl_bignum operator-(vnl_bignum const& r1, vnl_bignum const& r2) { return r1 + (-r2); }
inline vnl_bignum operator-(vnl_bignum const& r1, long r2) { return r1 + (-r2); }
inline vnl_bignum operator-(vnl_bignum const& r1, int r2) { return r1 + (-r2); }
inline vnl_bignum operator-(vnl_bignum const& r1, double r2) { return r1 + (-r2); }
inline vnl_bignum operator-(vnl_bignum const& r1, long double r2) { return r1 + (-r2); }
inline vnl_bignum operator-(long r2, vnl_bignum const& r1) { return -(r1 + (-r2)); }
inline vnl_bignum operator-(int r2, vnl_bignum const& r1) { return -(r1 + (-r2)); }
inline vnl_bignum operator-(double r2, vnl_bignum const& r1) { return -(r1 + (-r2)); }
inline vnl_bignum operator-(long double r2, vnl_bignum const& r1) { return -(r1 + (-r2)); }

//: Returns the product of two bignum numbers.
// \relatesalso vnl_bignum
inline vnl_bignum operator*(vnl_bignum const& r1, vnl_bignum const& r2)
{
  vnl_bignum result(r1); return result *= r2;
}

inline vnl_bignum operator*(vnl_bignum const& r1, long r2)
{
  vnl_bignum result(r1); return result *= vnl_bignum(r2);
}

inline vnl_bignum operator*(vnl_bignum const& r1, int r2)
{
  vnl_bignum result(r1); return result *= (long)r2;
}

inline vnl_bignum operator*(vnl_bignum const& r1, double r2)
{
  vnl_bignum result(r1); return result *= (vnl_bignum)r2;
}

inline vnl_bignum operator*(vnl_bignum const& r1, long double r2)
{
  vnl_bignum result(r1); return result *= (vnl_bignum)r2;
}

inline vnl_bignum operator*(long r2, vnl_bignum const& r1)
{
  vnl_bignum result(r1); return result *= r2;
}

inline vnl_bignum operator*(int r2, vnl_bignum const& r1)
{
  vnl_bignum result(r1); return result *= (long)r2;
}

inline vnl_bignum operator*(double r2, vnl_bignum const& r1)
{
  vnl_bignum result(r1); return result *= (vnl_bignum)r2;
}

inline vnl_bignum operator*(long double r2, vnl_bignum const& r1)
{
  vnl_bignum result(r1); return result *= (vnl_bignum)r2;
}

//: Returns the division of two bignum numbers.
// \relatesalso vnl_bignum
inline vnl_bignum operator/(vnl_bignum const& r1, vnl_bignum const& r2)
{
  vnl_bignum result(r1); return result /= r2;
}

inline vnl_bignum operator/(vnl_bignum const& r1, long r2)
{
  vnl_bignum result(r1); return result /= r2;
}

inline vnl_bignum operator/(vnl_bignum const& r1, int r2)
{
  vnl_bignum result(r1); return result /= (long)r2;
}

inline vnl_bignum operator/(vnl_bignum const& r1, double r2)
{
  vnl_bignum result(r1); return result /= (vnl_bignum)r2;
}

inline vnl_bignum operator/(vnl_bignum const& r1, long double r2)
{
  vnl_bignum result(r1); return result /= (vnl_bignum)r2;
}

inline vnl_bignum operator/(long r1, vnl_bignum const& r2)
{
  vnl_bignum result(r1); return result /= r2;
}

inline vnl_bignum operator/(int r1, vnl_bignum const& r2)
{
  vnl_bignum result((long)r1); return result /= r2;
}

inline vnl_bignum operator/(double r1, vnl_bignum const& r2)
{
  vnl_bignum result(r1); return result /= r2;
}

inline vnl_bignum operator/(long double r1, vnl_bignum const& r2)
{
  vnl_bignum result(r1); return result /= r2;
}

//: Returns the remainder of r1 divided by r2.
// \relatesalso vnl_bignum
inline vnl_bignum operator%(vnl_bignum const& r1, vnl_bignum const& r2)
{
  vnl_bignum result(r1); return result %= r2;
}

inline vnl_bignum operator%(vnl_bignum const& r1, long r2)
{
  vnl_bignum result(r1); return result %= vnl_bignum(r2);
}

inline vnl_bignum operator%(vnl_bignum const& r1, int r2)
{
  vnl_bignum result(r1); return result %= vnl_bignum((long)r2);
}

inline vnl_bignum operator%(long r1, vnl_bignum const& r2)
{
  vnl_bignum result(r1); return result %= r2;
}

inline vnl_bignum operator%(int r1, vnl_bignum const& r2)
{
  vnl_bignum result((long)r1); return result %= r2;
}

// Miscellaneous operators and functions

inline bool operator==(long r1, vnl_bignum const& r2) { return r2==r1; }
inline bool operator!=(long r1, vnl_bignum const& r2) { return r2!=r1; }
inline bool operator< (long r1, vnl_bignum const& r2) { return r2> r1; }
inline bool operator> (long r1, vnl_bignum const& r2) { return r2< r1; }
inline bool operator<=(long r1, vnl_bignum const& r2) { return r2>=r1; }
inline bool operator>=(long r1, vnl_bignum const& r2) { return r2<=r1; }

namespace vnl_math
{
  inline vnl_bignum abs(vnl_bignum const& x) { return x.abs(); }
  inline vnl_bignum squared_magnitude(vnl_bignum const& x) { return x*x; }
  inline vnl_bignum sqr(vnl_bignum const& x) { return x*x; }
  inline bool isnan(vnl_bignum const& ) { return false; }
  inline bool isfinite(vnl_bignum const& x) { return ! x.is_infinity(); }
}

#endif // vnl_bignum_h_
