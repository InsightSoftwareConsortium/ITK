// This is core/vnl/vnl_bignum.cxx
#include <cstdlib>
#include <cstring>
#include <cmath>
#include <cassert>
#include <algorithm>
#include <vector>
#include <iostream>
#include "vnl_bignum.h"
//:
// \file

#include <vnl/vnl_math.h> // for vnl_math::isfinite(double)

typedef unsigned short Counter;
typedef unsigned short Data;

//: Creates a zero vnl_bignum.

vnl_bignum::vnl_bignum()
: count(0), sign(1), data(nullptr)
{
}

//: Creates a vnl_bignum from a long integer.

vnl_bignum::vnl_bignum(long l)
: count(0), sign(1), data(nullptr)
{
  if (l < 0) {                  // Get correct sign
    l = -l;                     // Get absolute value of l
    this->sign = -1;
  }
  Data buf[sizeof(l)];          // Temp buffer to store l in
  Counter i = 0;                // buffer index
  while (l) {                   // While more bits in l
    assert(i < sizeof(l));      // no more buffer space
    buf[i] = Data(l);           // Peel off lower order bits
    l >>= 16;   // Shift next bits into place
    ++i;
  }
  if (i > 0)
    this->data = new Data[this->count=i]; // Allocate permanent data

  while (i--)     // Save buffer into perm. data
    this->data[i] = buf[i];
}

//: Creates a vnl_bignum from an integer.

vnl_bignum::vnl_bignum(int l)
: count(0), sign(1), data(nullptr)
{
  if (l < 0) {                  // Get correct sign
    l = -l;                     // Get absolute value of l
    this->sign = -1;
  }
  Data buf[sizeof(l)];          // Temp buffer to store l in
  Counter i = 0;                // buffer index
  while (l) {                   // While more bits in l
    assert(i < sizeof(l));      // no more buffer space
    buf[i] = Data(l);           // Peel off lower order bits
    l >>= 16;                   // Shift next bits into place
    i++;
  }
  if (i > 0)
    this->data = new Data[this->count=i]; // Allocate permanent data

  while (i--)     // Save buffer into perm. data
    this->data[i] = buf[i];
}

//: Creates a vnl_bignum from an unsigned long integer.

vnl_bignum::vnl_bignum(unsigned long l)
: count(0), sign(1), data(nullptr)
{
  Data buf[sizeof(l)];          // Temp buffer to store l in
  Counter i = 0;                // buffer index
  while (l) {                   // While more bits in l
    assert(i < sizeof(l));      // no more buffer space
    buf[i] = Data(l);           // Peel off lower order bits
    l >>= 16;   // Shift next bits into place
    i++;
  }
  if (i > 0)
    this->data = new Data[this->count=i]; // Allocate permanent data

  while (i--)     // Save buffer into perm. data
    this->data[i] = buf[i];
}

//: Creates a vnl_bignum from an unsigned integer.

vnl_bignum::vnl_bignum(unsigned int l)
: count(0), sign(1), data(nullptr)
{
  Data buf[sizeof(l)];          // Temp buffer to store l in
  Counter i = 0;                // buffer index
  while (l) {                   // While more bits in l
    assert(i < sizeof(l));      // no more buffer space
    buf[i] = Data(l);           // Peel off lower order bits
    l >>= 16;   // Shift next bits into place
    i++;
  }
  if (i > 0)
    this->data = new Data[this->count=i]; // Allocate permanent data

  while (i--)     // Save buffer into perm. data
    this->data[i] = buf[i];
}

//: Creates a vnl_bignum from a single-precision floating point number.

vnl_bignum::vnl_bignum(float f)
: count(0), sign(1), data(nullptr)
{
  double d = f;
  if (d < 0.0) {                // Get sign of d
    d = -d;                     // Get absolute value of d
    this->sign = -1;
  }

  if (!vnl_math::isfinite(d)) {
    // Infinity is represented as: count=1, data[0]=0.
    // This is an otherwise unused representation, since 0 is represented as count=0.
    this->count = 1;
    this->data = new Data[1];
    this->data[0] = 0;
  }
  else if (d >= 1.0) {
    // Note: 0x10000L == 1 >> 16: the (assumed) size of unsigned short is 16 bits.
    std::vector<Data> buf;
    while (d >= 1.0) {
      buf.push_back( Data(std::fmod(d,0x10000L)) );  // Get next data "digit" from d
      d /= 0x10000L;                                // Shift d right 1 data "digit"
    }
    // Allocate and copy into permanent buffer
    this->data = buf.size()>0 ? new Data[buf.size()] : nullptr;
    this->count = (unsigned short)(buf.size());
    std::copy( buf.begin(), buf.end(), data );
  }
}

//: Creates a vnl_bignum from a double floating point number.

vnl_bignum::vnl_bignum(double d)
: count(0), sign(1), data(nullptr)
{
  if (d < 0.0) {                // Get sign of d
    d = -d;                     // Get absolute value of d
    this->sign = -1;
  }

  if (!vnl_math::isfinite(d)) {
    // Infinity is represented as: count=1, data[0]=0.
    // This is an otherwise unused representation, since 0 is represented as count=0.
    this->count = 1;
    this->data = new Data[1];
    this->data[0] = 0;
  }
  else if (d >= 1.0) {
    // Note: 0x10000L == 1 >> 16: the (assumed) size of unsigned short is 16 bits.
    std::vector<Data> buf;
    while (d >= 1.0) {
      buf.push_back( Data(std::fmod(d,0x10000L)) );  // Get next data "digit" from d
      d /= 0x10000L;                                // Shift d right 1 data "digit"
    }
    // Allocate and copy into permanent buffer
    this->data = buf.size()>0 ? new Data[buf.size()] : nullptr;
    this->count = (unsigned short)(buf.size());
    std::copy( buf.begin(), buf.end(), data );
  }
}

//: Creates a vnl_bignum from a "long double" floating point number.

vnl_bignum::vnl_bignum(long double d)
: count(0), sign(1), data(nullptr)
{
  if (d < 0.0) {                // Get sign of d
    d = -d;                     // Get absolute value of d
    this->sign = -1;
  }

  if (!vnl_math::isfinite(d)) {
    // Infinity is represented as: count=1, data[0]=0.
    // This is an otherwise unused representation, since 0 is represented as count=0.
    this->count = 1;
    this->data = new Data[1];
    this->data[0] = 0;
  }
  else if (d >= 1.0) {
    // Note: 0x10000L == 1 >> 16: the (assumed) size of unsigned short is 16 bits.
    std::vector<Data> buf;
    while (d >= 1.0) {
      buf.push_back( Data(std::fmod(d,0x10000L)) );  // Get next data "digit" from d
      d /= 0x10000L;                                // Shift d right 1 data "digit"
    }
    // Allocate and copy into permanent buffer
    this->count = (unsigned short)(buf.size());
    if (this->count > 0) {
      this->data = new Data[this->count];
      std::copy( buf.begin(), buf.end(), data );
    }
    else {
      this->data = nullptr;
    }
  }
}

static char rt[4096];
static int rt_pos = 0;

static char next(const char*& s, std::istream** is)
{
  if (!is || *s) { char c = *s; if (c) ++rt_pos, ++s; return c; }
  if (rt_pos == 4096) return '\0';
  (*is)->get(rt[rt_pos]); // read a single byte from istream
  if (*s) ++s; // in case s == rt+rt_pos
  rt[++rt_pos] = '\0'; return rt[rt_pos-1];
}

static bool is_decimal(const char* s, std::istream** is = nullptr)
{
  rt_pos = 0;
  char c = next(s,is);
  while (c == ' ' || c == '\t' || c == '\n' || c == '\r') c = next(s,is);
  if (c == '+' || c == '-') c = next(s,is);
  if (c < '1' || c > '9') return false;
  while (c >= '0' && c <= '9') c = next(s,is);
  if (c == 'l' || c == 'L') c = next(s,is);
  if (rt_pos > 0) rt[++rt_pos] = '\0';
  return is ? true : c == '\0';
}

static bool is_exponential(const char* s, std::istream** is = nullptr)
{
  rt_pos = 0;
  char c = next(s,is);
  while (c == ' ' || c == '\t' || c == '\n' || c == '\r') c = next(s,is);
  if (c == '+' || c == '-') c = next(s,is);
  if (c < '1' || c > '9') return false;
  while (c >= '0' && c <= '9') c = next(s,is);
  if (c != 'e' && c != 'E') return false;
  c = next(s,is);
  if (c == '+') c = next(s,is); // no negative exponent!
  if (c < '0' || c > '9') return false;
  while (c >= '0' && c <= '9') c = next(s,is);
  if (rt_pos > 0) rt[++rt_pos] = '\0';
  return is ? true : c == '\0';
}

static bool is_hexadecimal(const char* s, std::istream** is = nullptr)
{
  rt_pos = 0;
  char c = next(s,is);
  while (c == ' ' || c == '\t' || c == '\n' || c == '\r') c = next(s,is);
  if (c == '+' || c == '-') c = next(s,is);
  if (c != '0') return false;
  c = next(s,is);
  if (c != 'x' && c != 'X') return false;
  c = next(s,is);
  if ((c < '0' || c > '9') &&
      (c < 'a' || c > 'f') &&
      (c < 'A' || c > 'F')) return false;
  while ((c >= '0' && c <= '9') ||
         (c >= 'a' && c <= 'f') ||
         (c >= 'A' && c <= 'F')) c = next(s,is);
  if (c == 'l' || c == 'L') c = next(s,is);
  if (rt_pos > 0) rt[++rt_pos] = '\0';
  return is ? true : c == '\0';
}

static bool is_octal(const char* s, std::istream** is = nullptr)
{
  rt_pos = 0;
  char c = next(s,is);
  while (c == ' ' || c == '\t' || c == '\n' || c == '\r') c = next(s,is);
  if (c == '+' || c == '-') c = next(s,is);
  if (c != '0') return false;
  while (c >= '0' && c <= '7') c = next(s,is);
  if (c == 'l' || c == 'L') c = next(s,is);
  if (rt_pos > 0) rt[++rt_pos] = '\0';
  return is ? true : c == '\0';
}

static bool is_plus_inf(const char* s, std::istream** is = nullptr)
{
  rt_pos = 0;
  char c = next(s,is);
  while (c == ' ' || c == '\t' || c == '\n' || c == '\r') c = next(s,is);
  if (c == '+') c = next(s,is);
  if (c != 'I') return false; c = next(s,is);
  if (c != 'n') return false; c = next(s,is);
  if (c != 'f') return false; c = next(s,is);
  if (c == 'i') c = next(s,is);
  if (c == 'n') c = next(s,is);
  if (c == 'i') c = next(s,is);
  if (c == 't') c = next(s,is);
  if (c == 'y') c = next(s,is);
  if (rt_pos > 0) rt[++rt_pos] = '\0';
  return is ? true : c == '\0';
}

static bool is_minus_inf(const char* s, std::istream** is = nullptr)
{
  rt_pos = 0;
  char c = next(s,is);
  while (c == ' ' || c == '\t' || c == '\n' || c == '\r') c = next(s,is);
  if (c != '-') return false; c = next(s,is);
  if (c != 'I') return false; c = next(s,is);
  if (c != 'n') return false; c = next(s,is);
  if (c != 'f') return false; c = next(s,is);
  if (c == 'i') c = next(s,is);
  if (c == 'n') c = next(s,is);
  if (c == 'i') c = next(s,is);
  if (c == 't') c = next(s,is);
  if (c == 'y') c = next(s,is);
  if (rt_pos > 0) rt[++rt_pos] = '\0';
  return is ? true : c == '\0';
}

//: Creates a vnl_bignum from the character string representation.

vnl_bignum::vnl_bignum(const char *s)
: count(0), sign(1), data(nullptr)
{
  // decimal:     "^ *[-+]?[1-9][0-9]*$"
  // exponential: "^ *[-+]?[1-9][0-9]*[eE][+]?[0-9]+$"
  // hexadecimal: "^ *[-+]?0[xX][0-9a-fA-F]+$"
  // octal:       "^ *[-+]?0[0-7]*$"
  // infinity:    "^ *[-+]?Inf(inity)?$"

  if (is_plus_inf(s))
    count=1,data=new Data[1],data[0]=0;
  else if (is_minus_inf(s))
    sign=-1,count=1,data=new Data[1],data[0]=0;
  else if (is_decimal(s))               // If string is decimal
    this->dtoBigNum(s);                 // convert decimal to vnl_bignum
  else if (is_exponential(s))           // If string is exponential
    this->exptoBigNum(s);               // convert exp. to vnl_bignum
  else if (is_hexadecimal(s))           // If string is hex,
    this->xtoBigNum(s);                 // convert hex to vnl_bignum
  else if (is_octal(s))                 // If string is octal
    this->otoBigNum(s);                 // convert octal to vnl_bignum
  else {                                // Otherwise
    std::cerr << "Cannot convert string " << s << " to vnl_bignum\n";
  }
}

//: Reads a vnl_bignum from a stream

std::istream& operator>>(std::istream& is, vnl_bignum& x)
{
  // decimal:     "^ *[-+]?[1-9][0-9]*$"
  // exponential: "^ *[-+]?[1-9][0-9]*[eE][+]?[0-9]+$"
  // hexadecimal: "^ *[-+]?0[xX][0-9a-fA-F]+$"
  // octal:       "^ *[-+]?0[0-7]*$"
  std::istream* isp = &is;
  rt[0] = '\0';

  x = 0L;
  if (is_plus_inf(rt,&isp))
    x.sign=1,x.count=1,x.data=new Data[1],x.data[0]=0;
  else if (is_minus_inf(rt,&isp))
    x.sign=-1,x.count=1,x.data=new Data[1],x.data[0]=0;
  else if (is_exponential(rt,&isp))     // If input stream string is exponential
    x.exptoBigNum(rt);                  // convert exp. to vnl_bignum
  else if (is_decimal(rt,&isp))         // If string is decimal
    x.dtoBigNum(rt);                    // convert decimal to vnl_bignum
  else if (is_hexadecimal(rt,&isp))     // If string is hex,
    x.xtoBigNum(rt);                    // convert hex to vnl_bignum
  else if (is_octal(rt,&isp))           // If string is octal
    x.otoBigNum(rt);                    // convert octal to vnl_bignum
  else {                                // Otherwise
    std::cerr << "Cannot convert string " << rt << " to vnl_bignum\n";
  }
  return is; // FIXME - should probably push back read characters to istream
}

//: Copies the contents of vnl_bignum b.

vnl_bignum::vnl_bignum(const vnl_bignum& b)
: count(b.count), sign(b.sign)
{
  if (b.data) {
    this->data = new Data[b.count];            // Allocate data
    for (Counter i = 0; i < this->count; ++i)  // Copy b data
      this->data[i] = b.data[i];
  }
  else {
    this->data = nullptr;
  }
}

//: Frees space for vnl_bignum.

vnl_bignum::~vnl_bignum()
{
  delete [] this->data; this->count = 0;        // Delete any allocated data
}

//: Copies rhs vnl_bignum to lhs vnl_bignum.

vnl_bignum& vnl_bignum::operator=(const vnl_bignum& rhs)
{
  if (this != &rhs) {                           // Avoid self-assignment
    delete[] this->data;                        // Delete existing data
    this->count = rhs.count;                    // Copy rhs's count
    if (rhs.data) {
      this->data = new Data[rhs.count];         // Allocate data if necessary
      for (Counter i = 0; i < rhs.count; ++i)   // Copy rhs's data
        this->data[i] = rhs.data[i];
    }
    else {
      this->data = nullptr;
    }
    this->sign = rhs.sign;                      // Copy rhs's sign
  }
  return *this;                                 // Return reference
}

//: Returns the negation of a vnl_bignum.

vnl_bignum vnl_bignum::operator-() const
{
  vnl_bignum neg(*this);
  if (neg.count)                // So long as this is non-zero
    neg.sign = -neg.sign;       // Flip its sign
  return neg;
}

//: Prefix increment. Increments a vnl_bignum by 1, and returns it.

vnl_bignum& vnl_bignum::operator++()
{
  if (this->is_infinity()) return *this;
  if (this->count==0)
  {
    this->resize(1);
    this->data[0] = 1;
    this->sign = +1;
    return *this;
  }

  if (this->sign > 0) increment(*this);
  else decrement(*this);

  return *this;
}

//: Prefix decrement. Decrements a vnl_bignum by 1, and returns it.

vnl_bignum& vnl_bignum::operator--()
{
  if (this->is_infinity()) return *this;
  if (this->count==0)
  {
    this->resize(1);
    this->data[0] = 1;
    this->sign = -1;
    return *this;
  }

  if (this->sign < 0) increment(*this);
  else decrement(*this);

  return *this;
}

//: Adds two vnl_bignums, and returns new sum.

vnl_bignum vnl_bignum::operator+(const vnl_bignum& b) const
{
  // Infinity arithmetic:
  assert (! b.is_minus_infinity() || ! this->is_plus_infinity() ); // +Inf-Inf
  assert (! b.is_plus_infinity() || ! this->is_minus_infinity() ); // -Inf+Inf
  if (b.is_infinity()) { return b; }
  if (this->is_infinity()) { return *this; }

  vnl_bignum sum;                       // Init sum to zero
  if (this->sign == b.sign) {           // If both have same sign
    add(*this,b,sum);                   //   Do simple addition
    sum.sign = this->sign;              // Attach proper sign
  }
  else {                                // Else different signs
    int mag = magnitude_cmp(*this,b);   // Determine relative sizes
    if (mag > 0) {                      // If abs(*this) > abs(b)
      subtract(*this,b,sum);            //   sum = *this - b
      sum.sign = this->sign;            // Sign of sum follows *this
    }
    else if (mag < 0) {                 // Else if abs(*this) < abs(b)
      subtract(b,*this,sum);            //   sum = b - *this
      sum.sign = b.sign;                // Sign of sum follows b
    }                                   // (Else abs(*this) == abs(b)
  }                                     //   so sum must be zero)
  return sum;                           // shallow swap on return
}

//: Multiplies this with a vnl_bignum

vnl_bignum& vnl_bignum::operator*=(const vnl_bignum& b)
{
  // Infinity arithmetic:
  assert (! b.is_infinity() || this->count != 0 ); // multiplication 0*Inf
  assert (! this->is_infinity() || b.count != 0 ); // multiplication Inf*0
  if (b.is_infinity()) return (*this) = (this->sign<0 ? -b : b);
  if (this->is_infinity()) return (*this) = (b.sign<0 ? -(*this) : *this);

  if (b.count == 0 || this->count == 0)
    return (*this)=0L;
  vnl_bignum prod;
  prod.resize(this->count + b.count);           //   allocate data for product
  for (Counter i = 0; i < b.count; i++)         //   multiply each b "digit"
    multiply_aux(*this, b.data[i], prod, i);    //   times b1 and add to total
  prod.sign = this->sign * b.sign;              //   determine correct sign
  prod.trim();                                  //   trim excess data and ret.
  return (*this)=prod;
}

//: Divides this by a vnl_bignum

vnl_bignum& vnl_bignum::operator/=(const vnl_bignum& b)
{
  // Infinity arithmetic:
  assert (! b.is_infinity() || ! this->is_infinity() ); // division Inf/Inf
  if (b.is_infinity()) return (*this)=0L;
  if (this->is_infinity()) return (*this) = (b.sign<0 ? -(*this) : *this);
  assert (b.count!=0 || this->count != 0); // division 0/0
  if (b.count == 0)
    return (*this) = (this->sign < 0 ? vnl_bignum("-Inf") : vnl_bignum("+Inf"));

  vnl_bignum quot, r;          // Quotient and remainder
  divide(*this,b,quot,r);      // Call divide fn
  return (*this) = quot;
}

//: Divides this by a vnl_bignum and replaces this by remainder.

vnl_bignum& vnl_bignum::operator%=(const vnl_bignum& b)
{
  // Infinity arithmetic:
  assert (! b.is_infinity() || ! this->is_infinity() ); // division Inf/Inf
  if (b.is_infinity()) return *this;                    // remainder of x/Inf is x.
  if (this->is_infinity()) return (*this) = 0L;         // convention: remainder is 0
  assert (b.count!=0 || this->count != 0);              // division 0/0
  if (b.count == 0) return (*this) = 0L;                // convention: remainder is 0

  vnl_bignum remain, q;        // Quotient and remainder
  divide(*this,b,q,remain);    // divide by b and save remainder
  return (*this) = remain;     // shallow swap on return
}

//: Shifts bignum to the left l digits.

vnl_bignum vnl_bignum::operator<<(int l) const
{
  // Infinity arithmetic:
  if (this->is_infinity()) return *this;

  if (l == 0 || *this == 0L)            // if either arg is zero
    return *this;
  if (l < 0)                            // if shift amt is negative
    return right_shift(*this,-l);       //   do an actual right shift
  else                                  // otherwise
    return left_shift(*this,l);         //   do a left shift
}

//: Shifts bignum to the right l digits.

vnl_bignum vnl_bignum::operator>>(int l) const
{
  // Infinity arithmetic:
  if (this->is_infinity()) return *this;

  if (l == 0 || *this == 0L)            // if either arg is zero
    return *this;
  if (l < 0)                            // if shift amt is negative
    return left_shift(*this,-l);        //   do an actual left shift
  else                                  // else
    return right_shift(*this,l);        //   do a right shift
}

//: Two vnl_bignums are equal if and only if they have the same integer representation.

bool vnl_bignum::operator==(const vnl_bignum& rhs) const
{
  if (this != &rhs) {                           // Check address
    if (this->sign != rhs.sign) return false;   // Different sign implies !=
    if (this->count != rhs.count) return false; // Different size implies !=
    for (Counter i = 0; i < this->count; i++)   // Each data element the same?
      {
      if ( ( ! this->data ) || ( ! rhs.data ) || (this->data[i] != rhs.data[i]))
        {
        return false; // No. Return !=
        }
      }
  }
  return true;                                    // Yes. Return ==
}

//: Compares two vnl_bignums.

bool vnl_bignum::operator<(const vnl_bignum& rhs) const
{
  if (this->sign < rhs.sign) return true;       // Different signs?
  if (this->sign > rhs.sign) return false;
  if (this->sign == 1)                          // Both signs == 1
    return magnitude_cmp(*this,rhs) < 0;        // this must be smaller
  else                                          // Both signs == -1
    return magnitude_cmp(*this,rhs) > 0;        // this must be larger
}

//: Formatted output for bignum.

std::ostream& operator<<(std::ostream& os, const vnl_bignum& b)
{
  vnl_bignum d = b;                     // Copy the input vnl_bignum
  if (d.sign == -1) {                   // If it's negative
    os << '-';                          //   Output leading minus sign
    d.sign = 1;                         //   Make d positive for divide
  }
  if (d.is_infinity()) return os<<"Inf";
  vnl_bignum q,r;                       // Temp quotient and remainder
  char *cbuf = new char[5 * (b.count+1)];   // Temp character buffer
  Counter i = 0;
  do {                                  // repeat:
    divide(d,10L,q,r);                  //   Divide vnl_bignum by ten
    cbuf[i++] = char(long(r) + '0');    //   Get one's digit
    d = q;                              //   Then discard one's digit
    q = r = 0L;                         //   Prep for next divide
  } while (d != 0L);                    // until no more one's digits
  do {                                  // repeat;
    os << cbuf[--i];                    //   output char buf in reverse
  } while (i);                          // until no more chars
  delete [] cbuf;                       // delete temp char buf
  return os;                            // return output stream
}

//: Convert the number to a decimal representation in a string.

std::string& vnl_bignum_to_string(std::string& s, const vnl_bignum& b)
{
  s.erase();
  std::string::size_type insert_point = 0; // keep record of location of first number.

  vnl_bignum d = b;                     // Copy the input vnl_bignum
  if (d.sign == -1) {                   // If it's negative
    s.insert(insert_point,"-");         //   Output leading minus sign
    d.sign = 1;                         //   Make d positive for divide
    ++insert_point;                     // keep record of location of first number.
  }
  if (d.is_infinity()) return s+="Inf";
  vnl_bignum q,r;                       // Temp quotient and remainder
  do {                                  // repeat:
    divide(d,10L,q,r);                  //   Divide vnl_bignum by ten
    s.insert(insert_point, 1, char('0'+long(r))); //   Get one's digit, and insert it at head.
    d = q;                              //   Then discard one's digit
    q = r = 0L;                         //   Prep for next divide
  } while (d != 0L);                    // until no more one's digits
  return s;
}

//: Convert the number from a decimal representation in a string.

vnl_bignum& vnl_bignum_from_string(vnl_bignum& b, const std::string& s)
{
  // decimal:     "^ *[-+]?[1-9][0-9]*$"
  // Infinity:    "^ *[-+]?Inf(inity)?$"

  if (is_plus_inf(s.c_str()))
    b=vnl_bignum("+Inf");
  else if (is_minus_inf(s.c_str()))
    b=vnl_bignum("-Inf");
  else
    b.dtoBigNum(s.c_str());             // convert decimal to vnl_bignum
  return b;
}

//: Implicit conversion from a vnl_bignum to a short.

vnl_bignum::operator short() const
{
  int j = this->operator int();
  return (short)j;
}

//: Implicit conversion from a vnl_bignum to an int.

vnl_bignum::operator int() const
{
  int j = 0;
  for (Counter i = this->count; i > 0; )
    j = int(j*0x10000 + this->data[--i]);
  return (this->sign < 0) ? -j : j;
}

//: Implicit conversion from a vnl_bignum to a long.

vnl_bignum::operator long() const
{
  long l = 0;
  for (Counter i = this->count; i > 0; )
    l = l*0x10000L + this->data[--i];
  return (this->sign < 0) ? -l : l;
}

//: Implicit conversion from a vnl_bignum to a float.

vnl_bignum::operator float() const
{
  float f = 0.0f;
  for (Counter i = this->count; i > 0; )
    f = f*0x10000 + this->data[--i];
  if (this->is_infinity()) f = std::numeric_limits<float>::infinity();
  return (this->sign < 0) ? -f : f;
}

//: Implicit conversion from a vnl_bignum to a double.

vnl_bignum::operator double() const
{
  double d = 0.0;
  for (Counter i = this->count; i > 0; )
    d = d*0x10000 + this->data[--i];
  if (this->is_infinity()) d = std::numeric_limits<double>::infinity();
  return (this->sign < 0) ? -d : d;
}

//: Implicit conversion from a vnl_bignum to a long double.

vnl_bignum::operator long double() const
{
  long double d = 0.0;
  for (Counter i = this->count; i > 0; )
    d = d*0x10000 + this->data[--i];
  if (this->is_infinity()) d = std::numeric_limits<long double>::infinity();
  return (this->sign < 0) ? -d : d;
}

//: dump the contents of a vnl_bignum to a stream, default cout.

void vnl_bignum::dump(std::ostream& os) const
{
  os << "{count=" << this->count       // output count field
     << ", sign=" << this->sign        // output sign field
     << ", data=" << this->data        // output data pointer
     << ", value=" << *this
     << ", {";
  // format string == "%04X%s" or "%02X%s", etc.
  //  static char format_str[10] =
  //    {'%','0',char(2*2 + '0'),'X','%','s'};
  //  format_str[2] = char(2*2 + '0');
  if (this->count > 0) { // output data array
    os << std::hex << (this->data[this->count-1]);
    for (Counter i = this->count - 1; i > 0; --i) {
      os  << ',';
      if (this->data[i-1] < 0x10) os << '0';
      if (this->data[i-1] < 0x100) os << '0';
      if (this->data[i-1] < 0x1000) os << '0';
      os  << this->data[i-1];
    }
    os << std::dec;
  }
  os << "}}\n";                         // close brackets
}

//: Converts decimal string to a vnl_bignum.

int vnl_bignum::dtoBigNum(const char *s)
{
  this->resize(0); this->sign = 1;      // Reset number to 0.
  Counter len = 0;                      // No chars converted yet
  vnl_bignum sum;
  while (*s == ' ' || *s == '\t' || *s == '\n' || *s == '\r') ++s; // skip whitespace
  if (*s == '-' || *s == '+') ++len;    // Skip over leading +,-
  while (s[len]>='0' && s[len]<='9') {  // While current char is digit
    *this *= vnl_bignum(10L),           // Shift vnl_bignum left a decimal
    add(*this,vnl_bignum(long(s[len++]-'0')),sum), // digit and add new digit
    *this = sum;
  }
  if (*s == '-') this->sign = -1;       // If s had leading -, note it
  return len;                           // Return # of chars processed
}

//: convert exponential string to a vnl_bignum

void vnl_bignum::exptoBigNum(const char *s)
{
  while (*s == ' ' || *s == '\t' || *s == '\n' || *s == '\r') ++s; // skip whitespace
  auto pos = Counter(this->dtoBigNum(s) + 1); // Convert the base, skip [eE]
  long pow = std::atol(s + pos);         // Convert the exponent to long
  while (pow-- > 0)                     // Raise vnl_bignum to the given
    *this = (*this) * 10L;              // power
}

//: convert hex character to integer hex value (ASCII or EBCDIC)
// - Inputs:  character representation of a hex number
// - Outputs: integer value of the hex number

unsigned int ctox(int c)
{
  if ('0' <= c && c <= '9')
    return c - '0';
  if ('a' <= c && c <= 'f')
    return c - 'a' + 10;
  return c - 'A' + 10;
}

//: convert hex string to vnl_bignum

void vnl_bignum::xtoBigNum(const char *s)
{
  this->resize(0); sign = 1;            // Reset number to 0.
  while (*s == ' ' || *s == '\t' || *s == '\n' || *s == '\r') ++s; // skip whitespace
  auto size = Counter(std::strlen(s));
  Counter len = 2;                      // skip leading "0x"
  while (len < size) {                  // While there are more chars
    (*this) = ((*this) * 16L) +         // Shift vnl_bignum left one hex
      vnl_bignum(long(ctox(s[len++]))); //   digit and add next digit
  }
}

//: convert octal string to vnl_bignum

void vnl_bignum::otoBigNum(const char *s)
{
  this->resize(0); sign = 1;           // Reset number to 0.
  while (*s == ' ' || *s == '\t' || *s == '\n' || *s == '\r') ++s; // skip whitespace
  auto size = Counter(std::strlen(s));
  Counter len = 0;                      // No chars converted yet
  while (len < size) {                  // While there are more chars
    (*this) = ((*this) * 8L) +          // Shift vnl_bignum left 1 oct dig.
      vnl_bignum(long(s[len++] - '0')); // Add next character value
  }
}

//: change the data allotment for a vnl_bignum

void vnl_bignum::resize(short new_count)
{
  assert(new_count >= 0);
  if (new_count == this->count) return;
  Data *new_data = (new_count > 0 ? new Data[new_count] : nullptr); // Allocate data if necessary

  if (this->count <= new_count) {       // Copy old data into new
    short i = 0;
    if( this->data && new_data )
      {
      for (; i < this->count; i++)
        new_data[i] = this->data[i];
      }
    for (; i < new_count; i++)
      new_data[i] = 0;
  }
  else {
    for (short i = 0; i < new_count; i++)
      new_data[i] = this->data[i];
  }

  delete [] this->data;                 // Get rid of old data
  this->data = new_data;                // Point to new data
  this->count = new_count;              // Save new count
}

//: trim non-infinite vnl_bignum of excess data allotment

vnl_bignum& vnl_bignum::trim()
{
  Counter i = this->count;
  for (; i > 0; i--)                    // Skip over high-order words
    if (this->data[i - 1] != 0) break;  //   that are zero
  if (i < this->count) {                // If there are some such words
    this->count = i;                    // Update the count
    Data *new_data = (i > 0 ? new Data[i] : nullptr); // Allocate data if necessary
    for (; i > 0; i--)                  // Copy old data into new
      new_data[i - 1] = this->data[i - 1];
    delete [] this->data;               // Delete old data
    this->data = new_data;              // Point to new data
  }
  return *this;                         // return reference to vnl_bignum
}

//: add two non-infinite vnl_bignum values and save their sum

void add(const vnl_bignum& b1, const vnl_bignum& b2, vnl_bignum& sum)
{
  const vnl_bignum *bmax, *bmin;        // Determine which of the two
  if (b1.count >= b2.count) {           //   addends has the most
    bmax = &b1;                         //   data.
    bmin = &b2;
  }
  else {
    bmax = &b2;
    bmin = &b1;
  }
  sum.resize(bmax->count);              // Allocate data for their sum
  unsigned long temp, carry = 0;
  Counter i = 0;
  if( b1.data )
    {
    while (i < bmin->count) {             // Add, element by element.
      // Add both elements and carry
      temp = (unsigned long)b1.data[i] + (unsigned long)b2.data[i] + carry;
      carry = temp/0x10000L;              // keep track of the carry
      sum.data[i] = Data(temp);           // store sum
      i++;                                // go to next element
    }
    }
  if( bmax->data )
    {
    while (i < bmax->count ) {             // bmin has no more elements
      temp = bmax->data[i] + carry;       // propagate the carry through
      carry = temp/0x10000L;              // the rest of bmax's elements
      sum.data[i] = Data(temp);           // store sum
      i++;
    }
    }
  if (carry) {                          // if carry left over
    sum.resize(bmax->count + 1);        //   allocate another word
    sum.data[bmax->count] = 1;          //   save the carry in it
  }
}

//: Add 1 to bnum (unsigned, non-infinite)

void increment(vnl_bignum& bnum)
{
  Counter i = 0;
  unsigned long carry = 1;
  while (i < bnum.count && carry) {             // increment, element by element.
    unsigned long temp = (unsigned long)bnum.data[i] + carry;
    carry = temp/0x10000L;
    bnum.data[i] = (Data)temp;
    ++i;
  }
  if (carry)
  {
    bnum.resize(bnum.count+1);
    bnum.data[bnum.count-1] = 1;
  }
}

//: subtract bmin from bmax (unsigned, non-infinite), result in diff

void subtract(const vnl_bignum& bmax, const vnl_bignum& bmin, vnl_bignum& diff)
{
  diff.resize(bmax.count);                      // Allocate data for difference
  unsigned long temp;
  int borrow = 0;
  Counter i = 0;
  for (; i < bmin.count; i++) {                 // Subtract word by word.
    temp = (unsigned long)bmax.data[i] + 0x10000L - borrow; // Add radix to bmax's data
    temp -= (unsigned long)bmin.data[i];        // Subtract off bmin's data
    borrow = (temp/0x10000L == 0);              // Did we have to borrow?
    diff.data[i] = (Data) temp;                 // Reduce modulo radix and save
  }
  for (; i < bmax.count; i++) {                 // No more data for bmin
    temp = (unsigned long)bmax.data[i] + 0x10000L - borrow; // Propagate the borrow through
    borrow = (temp/0x10000L == 0);              //   rest of bmax's data
    diff.data[i] = (Data) temp;
  }
  diff.trim();                                  // Done. Now trim excess data
}

//: Subtract 1 from bnum (unsigned, non-infinite, non-zero)

void decrement(vnl_bignum& bnum)
{
  Counter i = 0;
  unsigned long borrow = 1;
  while (i < bnum.count && borrow) {            // decrement, element by element.
    unsigned long temp = (unsigned long)bnum.data[i] + 0x10000L - borrow;
    borrow = (temp/0x10000L == 0);              // Did we have to borrow?
    bnum.data[i] = (Data)temp;                  // Reduce modulo radix and save
    ++i;
  }
  bnum.trim();                                  // Done. Now trim excess data
  if (bnum.count==0) bnum.sign=+1;              // Re-establish sign invariant
}

//: compare absolute values of two vnl_bignums
// Outputs:  result of comparison:  -1 if abs(b1) < abs(b2)
//                                   0 if abs(b1) == abs(b2)
//                                  +1 if abs(b1) > abs(b2)

int magnitude_cmp(const vnl_bignum& b1, const vnl_bignum& b2)
{
  if (b1.is_infinity()) return b2.is_infinity() ? 0 : 1;
  if (b2.is_infinity()) return -1;
  if (b1.count > b2.count) return 1;    // If one has more data than
  if (b2.count > b1.count) return -1;   //   the other, it wins
  Counter i = b1.count;                 // Else same number of elements
  while (i > 0) {                       // Do lexicographic comparison
    if (b1.data[i - 1] > b2.data[i - 1])
      return 1;
    else if (b1.data[i - 1] < b2.data[i - 1])
      return -1;
    i--;
  }                                     // No data, or all elements same
  return 0;                             //  so must be equal
}

//: multiply a non-infinite vnl_bignum by a "single digit"
// - Inputs:  vnl_bignum reference, single word multiplier, reference to the product,
//            and index of starting storage location to use in product

void multiply_aux(const vnl_bignum& b, Data d, vnl_bignum& prod, Counter i)
{
  // this function works just like normal multiplication by hand, in that the
  // top number is multiplied by the first digit of the bottom number, then the
  // second digit, and so on.  The only difference is that instead of doing all
  // of the multiplication before adding the rows, addition is done
  // concurrently.
  if (i == 0) {                         // if index is zero
    Counter j = 0;                      //   then zero out all of
    while (j < prod.count)              //   prod's data elements
      prod.data[j++] = 0;
  }
  if (d != 0) {                         // if d == 0, nothing to do
    unsigned long temp;
    Data carry = 0;

    Counter j = 0;
    for (; j < b.count; j++) {
      // for each of b's data elements, multiply times d and add running product
      temp = (unsigned long)b.data[j] * (unsigned long)d
           + (unsigned long)prod.data[i + j] + carry;
      prod.data[i + j] = Data(temp % 0x10000L); //   store result in product
      carry = Data(temp/0x10000L);              //   keep track of carry
    }
    if (i+j < prod.count)
      prod.data[i + j] = carry;                 // Done.  Store the final carry
  }
}

//: normalize two vnl_bignums
// (Refer to Knuth, V.2, Section 4.3.1, Algorithm D for details.
//  A digit here is one data element in the radix 2**2.)
// - Inputs:  references to two vnl_bignums b1, b2
// - Outputs: their normalized counterparts u and v,
//            and the integral normalization factor used

Data normalize(const vnl_bignum& b1, const vnl_bignum& b2, vnl_bignum& u, vnl_bignum& v)
{
  Data d = Data(0x10000L/((unsigned long)(b2.data[b2.count - 1]) + 1L)); // Calculate normalization factor.
  u.resize(b1.count + 1);                       // Get data for u (plus extra)
  v.resize(b2.count);                           // Get data for v
  u.data[b1.count] = 0;                         // Set u's leading digit to 0
  multiply_aux(b1,d,u,0);                       // u = b1 * d
  multiply_aux(b2,d,v,0);                       // v = b2 * d
  return d;                                     // return normalization factor
}

//: divide a vnl_bignum by a "single digit"
// (Refer to Knuth, V.2, Section 4.3.2, exercise 16 for details.
//  A digit here is one data element in the radix 2**2.)
// - Inputs:  reference to vnl_bignum dividend, single digit divisor d, vnl_bignum
//            quotient, and single digit remainder r

void divide_aux(const vnl_bignum& b1, Data d, vnl_bignum& q, Data& r)
{
  r = 0;                                        // init remainder to zero
  unsigned long temp;
  for (Counter j = b1.count; j > 0; j--) {
    temp = (unsigned long)r*0x10000L + (unsigned long)b1.data[j - 1]; // get remainder, append next
    if (j < 1 + q.count)
      q.data[j - 1] = Data(temp/d);             //   digit, then divide
    r = Data(temp % d);                         // calculate new remainder
  }
}

//: estimate next dividend
// (Refer to Knuth, V.2, Section 4.3.1, Algorithm D for details.
//  This function estimates how many times v goes into u, starting at u's
//  jth digit.  A digit here is actually a data element, thought of as
//  being in the radix 2**2.)
// - Inputs:  reference to vnl_bignum dividend and divisor and starting digit j
// - Outputs: estimated number of times v goes into u

Data estimate_q_hat(const vnl_bignum& u, const vnl_bignum& v, Counter j)
{
  Data q_hat,
       v1 = v.data[v.count - 1],                // localize frequent data
       v2 = v.data[v.count - 2],
       u0 = u.data[u.count - 1 - j],
       u1 = u.data[u.count - 2 - j],
       u2 = u.data[u.count - 3 - j];

  // Initial Knuth estimate, usually correct
  q_hat = (u0 == v1 ? Data(0xffff) : Data(((unsigned long)u0 * 0x10000L + (unsigned long)u1) / (unsigned long)v1));

  // high speed test to determine most of the cases in which initial
  // estimate is too large.  Eliminates most cases in which q_hat is one too
  // large.  Eliminates all cases in which q_hat is two too large.  The test
  // looks hairy because we have to watch out for overflow.  In the book, this
  // test is the simple inequality:
  //     v2*q_hat > (u0*0x10000L + u1 - q_hat*v1)*0x10000L + u2.
  // If the inequality is true, decrease q_hat by 1.  If inequality is still
  // true, decrease q_hat again.
  unsigned long lhs, rhs;               // lhs, rhs of Knuth inequality
  for (Counter i = 0; i < 2; i++) {     // loop at most twice
    lhs = (unsigned long)v2 * (unsigned long)q_hat;       // Calculate left-hand side of ineq.
    rhs = (unsigned long)u0 * 0x10000L +(unsigned long)u1;// Calculate part of right-hand side
    rhs -= ((unsigned long)q_hat * (unsigned long)v1);    // Now subtract off part

    if ( rhs >= 0x10000L )              // if multiplication with 0x10000L causes overflow
      break;                            //   then rhs > lhs, so test fails
    rhs *= 0x10000L;                    // No overflow:  ok to multiply

    if (rhs > rhs + (unsigned long)u2)  // if addition yields overflow
      break;                            //   then rhs > lhs, so test fails
    rhs += u2;                          // No overflow: ok to add.
    if (lhs <= rhs)                     // if lhs <= rhs
      break;                            //   test fails
    q_hat--;                            // Test passes:  decrement q_hat
  }                                     // Loop again
  return q_hat;                         // Return estimate
}

//: calculate u - v*q_hat
// (Refer to Knuth, V. 2, Section 4.3.1, Algorithm D for details.
//  A digit here is a data element, thought of as being in the radix 2**2.)
// - Inputs:  reference to vnl_bignum dividend, divisor, estimated result, and index
//            into jth digit of dividend
// - Outputs: true number of times v goes into u

Data multiply_subtract(vnl_bignum& u, const vnl_bignum& v, Data q_hat, Counter j)
{
  // At this point it has been estimated that v goes into the jth and higher
  // digits of u about q_hat times, and in fact that q_hat is either the
  // correct number of times or one too large.

  if (q_hat == 0) return q_hat;         // if q_hat 0, nothing to do
  vnl_bignum rslt;                      // create a temporary vnl_bignum
  Counter tmpcnt;
  rslt.resize(v.count + 1u);            // allocate data for it

  // simultaneous computation of u - v*q_hat
  unsigned long prod, diff;
  Data carry = 0, borrow = 0;
  Counter i = 0;
  for (; i < v.count; ++i) {
    // for each digit of v, multiply it by q_hat and subtract the result
    prod = (unsigned long)v.data[i] * (unsigned long)q_hat + carry;
    diff = (unsigned long)u.data[u.count - v.count - 1 - j + i] + (0x10000L - (unsigned long)borrow);
    diff -= (unsigned long)Data(prod);  //   form proper digit of u
    rslt.data[i] = Data(diff);          //   save the result
    borrow = (diff/0x10000L == 0) ? 1 : 0; //   keep track of any borrows
    carry = Data(prod/0x10000L);        //   keep track of carries
  }
  tmpcnt = Counter(u.count - v.count + i - j - 1);
  diff = (unsigned long)u.data[tmpcnt]  //  special case for the last digit
         + (0x10000L - (unsigned long)borrow);
  diff -= (unsigned long)carry;
  rslt.data[i] = Data(diff);
  borrow = (diff/0x10000L == 0) ? 1 : 0;

  // A leftover borrow indicates that u - v*q_hat is negative, i.e., that
  // q_hat was one too large.  So to get correct result, decrement q_hat and
  // add back one multiple of v
  if (borrow) {
    q_hat--;
    carry = 0;
    unsigned long sum;
    for (i = 0; i < v.count; ++i) {
      sum = (unsigned long)rslt.data[i] + (unsigned long)v.data[i] + carry;
      carry = Data(sum/0x10000L);
      u.data[u.count - v.count + i - 1 - j] = Data(sum);
    }
    u.data[u.count - v.count + i - 1 - j] = rslt.data[i] + carry;
  }
  else {                                // otherwise, result is ok
    for (i = 0; i < rslt.count; ++i)    // store result back into u
      u.data[u.count - v.count + i - 1 - j] = rslt.data[i];
  }
  return q_hat;                         // return corrected q_hat
}

//: divide b2 into b1, getting quotient q and remainder r.
// (Refer to Knuth, V.2, Section 4.3.1, Algorithm D for details.
//  This function implements Algorithm D.)
// - Inputs: references to a vnl_bignum dividend b1, divisor b2, quotient q, and
//           remainder r.

void divide(const vnl_bignum& b1, const vnl_bignum& b2, vnl_bignum& q, vnl_bignum& r)
{
  // Note that q or r may *not* be identical to either b1 or b2 !
  assert(&b1 != &q && &b2 != &q && &b1 != &r && &b2 != &r);
  q = r = 0L;
  if (b1 == 0L)                      // If divisor is zero
    return;                          //   return zero quotient and remainder
  int mag = magnitude_cmp(b1,b2);    // Compare magnitudes
  if (mag < 0)                       // if abs(b1) < abs(b2)
    r = b1;                          //   return zero quotient, b1 remainder
  else if (mag == 0)                 // if abs(b1) == abs(b2)
    q = 1L;                          //   quotient is 1, remainder is 0
  else {                             // otherwise abs(b1) > abs(b2), so divide
    q.resize(b1.count + 1u - b2.count); // Allocate quotient
    r.resize(b2.count);              // Allocate remainder
    if (b2.count == 1) {                        // Single digit divisor?
      divide_aux(b1,b2.data[0],q,r.data[0]);    // Do single digit divide
    }
    else {                                      // Else full-blown divide
      vnl_bignum u,v;
#ifdef DEBUG
      std::cerr << "\nvnl_bignum::divide: b1 ="; b1.dump(std::cerr);
      std::cerr << "vnl_bignum::divide: b2 ="; b2.dump(std::cerr);
#endif
      Data d = normalize(b1,b2,u,v);            // Set u = b1*d, v = b2*d
#ifdef DEBUG
      std::cerr << "vnl_bignum::divide: d = " << d << std::hex << " (0x" << d << ")\n" << std::dec;
      std::cerr << "vnl_bignum::divide: u = "; u.dump(std::cerr);
      std::cerr << "vnl_bignum::divide: v = "; v.dump(std::cerr);
#endif
      Counter j = 0;
      while (j <= b1.count - b2.count) {        // Main division loop
        Data q_hat = estimate_q_hat(u,v,j);     // Estimate # times v divides u
        q.data[q.count - 1 - j] =               // Do division, get true answ.
          multiply_subtract(u,v,q_hat,j);
        j++;
#ifdef DEBUG
        std::cerr << "vnl_bignum::divide: q_hat = " << q_hat << std::hex << " (0x" << q_hat << ")\n" << std::dec;
        std::cerr << "vnl_bignum::divide: u = "; u.dump(std::cerr);
#endif
      }
      static Data dufus;                // dummy variable
      divide_aux(u,d,r,dufus);          // Unnormalize u for remainder

#ifdef DEBUG
      std::cerr << "vnl_bignum::divide: q = "; q.dump(std::cerr);
      std::cerr << "vnl_bignum::divide: r = "; r.dump(std::cerr);
#endif
    }
    q.trim();                           // Trim leading zeros of quot.
    r.trim();                           // Trim leading zeros of rem.
  }
  q.sign = r.sign = b1.sign * b2.sign;  // Calculate signs
}

//: left shift (arithmetic) non-infinite vnl_bignum by positive number.
// - Inputs:  reference to vnl_bignum, positive shift value
// - Outputs: vnl_bignum, multiplied by the corresponding power of two

vnl_bignum left_shift(const vnl_bignum& b1, int l)
{
  // to carry out this arithmetic left shift, we cheat.  Instead of physically
  // shifting the data array l bits to the left, we shift just enough to get
  // the correct word alignment, and then pad the array on the right with as
  // many zeros as we need.
  vnl_bignum rslt;                      // result of shift
  rslt.sign = b1.sign;                  // result follows sign of input
  auto growth = Counter(l / 16);     // # of words rslt will grow by
  Data shift = Data(l % 16);            // amount to actually shift
  Data rshift = Data(16 - shift);       // amount to shift next word by
  Data carry = Data(                    // value that will be shifted
    b1.data[b1.count - 1] >> (16 - shift));// out end of current array
  rslt.resize(b1.count + growth + (carry ? 1u : 0u)); // allocate new data array
  Counter i = 0;
  while (i < growth)                            // zero out padded elements
    rslt.data[i++] = 0;
  rslt.data[i++] = b1.data[0] << shift;         // shift first non-zero element
  while (i < rslt.count - 1) {                  // for remaining data words
    rslt.data[i] = (b1.data[i - growth] << shift) + // shift current data word
      (b1.data[i - 1 - growth] >> rshift);          // propagate adjacent
    i++;                                        // carry into current word
  }
  if (i < rslt.count) {
    if (carry)                                  // if last word had overflow
      rslt.data[i] = carry;                     //   store it new data
    else                                        // otherwise,
      rslt.data[i] = (b1.data[i - growth] << shift) // do like the rest
                   + (b1.data[i - 1 - growth] >> rshift);
  }
  vnl_bignum& result = *((vnl_bignum*) &rslt);// same physical object
  return result;                              // shallow swap on return
}

//: right shift (arithmetic) non-infinite vnl_bignum by positive number.
// - Inputs:  reference to vnl_bignum, positive shift value
// - Outputs: vnl_bignum, divided by the corresponding power of two

vnl_bignum right_shift(const vnl_bignum& b1, int l)
{
  vnl_bignum rslt;                              // result of shift
  auto shrinkage = Counter(l / 16);          // # of words rslt will shrink
  Data shift = Data(l % 16);                    // amount to actually shift
  Data dregs = Data(b1.data[b1.count-1] >> shift);// high end data to save
  if (shrinkage + (dregs == 0) < b1.count) {    // if not all data shifted out
    rslt.sign = b1.sign;                        // rslt follows sign of input
    rslt.resize(b1.count - shrinkage - (dregs == 0 ? 1 : 0)); // allocate new data
    Data lshift = Data(16 - shift);             // amount to shift high word
    Counter i = 0;
    while (i < rslt.count - 1) {                // shift current word
      rslt.data[i] = (b1.data[i + shrinkage] >> shift) + // propagate adjacent
        (b1.data[i + shrinkage + 1u] << lshift); // word into current word
      i++;
    }
    if (dregs)                                  // don't lose dregs
      rslt.data[i] = dregs;
    else {
      rslt.data[i] = (b1.data[i + shrinkage] >> shift) +
        (b1.data[i + shrinkage + 1u] << lshift);
    }
  }
  vnl_bignum& result = *((vnl_bignum*) &rslt);  // same physical object
  return result;                                // shallow swap on return
}
