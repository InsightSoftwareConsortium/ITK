// This is core/vnl/vnl_decnum.cxx
#include <sstream>
#include "vnl_decnum.h"
#include <vcl_cassert.h>
#include <vcl_compiler.h>

// constructor from (decimal) string.
// Parses the input into (in that order)
// * optional blanks,
// * the sign (could be "+" or "-" or nothing at all),
// * the mantissa, consisting of just decimal digits (at least one),
// * and the exponent (optional, starts with "e", then optionally "+" or "-", then an integer)
// * If the mantissa contains a decimal point, it is ignored (but the exponent is adapted accordingly).
// Alternatively, the input might also be "NaN", "Inf", "+Inf", or "-Inf".
// See also operator>>(std::istream& s, vnl_decnum& r).
vnl_decnum::vnl_decnum(std::string const& r)
: sign_('+'), data_(""), exp_(0L)
{
  long exp = 0L;
  char const* p = r.c_str();
  while (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r') ++p;
  if (*p == '-') sign_ = '-', ++p;
  else if (*p == '+') ++p;
  if (*p == 'I' && *++p == 'n' && *++p == 'f') { data_ = "Inf"; }
  else if (*p == 'N' && *++p == 'a' && *++p == 'N') { data_ = "NaN"; }
  else {
    while (*p == '0') ++p;
    while (*p >= '0' && *p <= '9') data_.push_back(*p++);
    if (*p == '.') {
      ++p;
      while (*p >= '0' && *p <= '9') { data_.push_back(*p++); --exp; }
    }
    if (data_ == "") sign_ = ' ';
    else if (*p == 'e') {
      ++p;
      char sign = '+';
      if (*p == '-') sign = '-', ++p;
      else if (*p == '+') ++p;
      while (*p == '0') ++p;
      while (*p >= '0' && *p <= '9') exp_ *= 10L, exp_ += (*p-'0'), ++p;
      if (sign == '-') exp_ = -exp_;
    }
    exp_ += exp;
    if (sign_ == ' ') exp_ = 0L;
  }
#ifdef DEBUG
  std::cerr << "Leaving vnl_decnum::vnl_decnum(\"" << r << "\") with " << sign_ << data_ << 'e' << exp_ << '\n';
#endif
}

// constructor from an unsigned long
vnl_decnum::vnl_decnum(unsigned long r)
: sign_('+'), data_(""), exp_(0L)
{
  if (r == 0) sign_ = ' ';
  else {
    while (r) { data_.insert(data_.begin(), '0'+(r%10)); r/=10; }
  }
}

// constructor from a double
vnl_decnum::vnl_decnum(double r)
{
#ifdef DEBUG
  std::cerr << "vnl_decnum::vnl_decnum(double " << r << ")\n";
#endif
  std::ostringstream os; os << r;
  *this = vnl_decnum(os.str());
}

// Implicit type conversion to a decimal string
// Also used for ostream output
vnl_decnum::operator std::string() const
{
  if (data_=="NaN") return "NaN";
  if (sign_==' ') return "0"; // even if the exponent would be nonzero
  std::string r=data_; if (sign_=='-') r.insert(r.begin(), sign_);
  if (exp_ == 0) return r;
  // if not a plain integer: also print out the exponent:
  r.push_back('e');
  long exp=exp_; if (exp < 0) { exp = -exp; r.push_back('-'); }
  std::string e="";
  while (exp) {e.insert(e.begin(), '0'+(exp%10)); exp /=10; }
  return r+e;
}

vnl_decnum::operator unsigned long() const
{
  if (data_ == "NaN") return 0L;
  if (data_ == "Inf") return 0xffffffffu;
  unsigned long l = 0L;
  for (long i=0; i<long(data_.length())+exp_; ++i) { l *= 10; if (i<long(data_.length())) l += (data_.c_str()[i]-'0'); } // might overflow!!!
  return l; // forget the sign
}

vnl_decnum::operator long() const
{
  if (data_ == "NaN") return 0L;
  if (data_ == "Inf" && sign_ == '+') return 0x7fffffff;
  else if (data_ == "Inf") return -0x7fffffff - 1;
  long l = 0L;
  for (long i=0; i<long(data_.length())+exp_; ++i) { l *= 10; if (i<long(data_.length())) l += (data_.c_str()[i]-'0'); } // might overflow!!!
  return sign_=='-' ? -l : l;
}

vnl_decnum::operator unsigned int() const
{
  if (data_ == "NaN") return 0L;
  if (data_ == "Inf") return 0xffffffffu;
  unsigned int l = 0;
  for (long i=0; i<long(data_.length())+exp_; ++i) { l *= 10; if (i<long(data_.length())) l += (data_.c_str()[i]-'0'); } // might overflow!!!
  return l; // forget the sign
}

vnl_decnum::operator int() const
{
  if (data_ == "NaN") return 0L;
  if (data_ == "Inf" && sign_ == '+') return 0x7fffffff;
  else if (data_ == "Inf") return -0x7fffffff - 1;
  int l = 0;
  for (long i=0; i<long(data_.length())+exp_; ++i) { l *= 10; if (i<long(data_.length())) l += (data_.c_str()[i]-'0'); } // might overflow!!!
  return sign_=='-' ? -l : l;
}

bool vnl_decnum::operator==(vnl_decnum const& r) const
{
  // quick return if exponents are identical or signs are different:
  if (sign_!=r.sign()) return false;
  else if (data_ == "NaN" || r.data() == "NaN") return false; // NaN equals nothing!
  else if (data_ == "Inf" && r.data() == "Inf") return true; // of the same sign, of course
  else if (sign_ == ' ') return true; // both are zero
  else if (exp_ == r.exp()) return data_==r.data();
  else if (exp_ > r.exp()) {
    // by adding zeros to data_ while decreasing exp_ until it equals r.exp(),
    // both mantissas become comparable:
    return add_zeros(data_,exp_-r.exp()) == r.data();
  }
  else {
    // similarly in the other direction:
    return add_zeros(r.data(),r.exp()-exp_) == data_;
  }
}

// This is "operator<" for strings.
// The arguments should consist of digits only (interpreted as mantissas with the same exponent).
// The shorter of the two arguments is implicitly zero-padded.
bool vnl_decnum::comp(std::string const& a, std::string const& b)
{
#ifdef DEBUG
  std::cerr << "Entering vnl_decnum::comp with " << a << " and " << b << '\n';
#endif
  int i, na = int(a.length()), nb = int(b.length()), nc = na < nb ? na : nb;
  for (i = 0; i < nc; ++i) {
    if (a.c_str()[i] < b.c_str()[i]) return true;
    else if (a.c_str()[i] > b.c_str()[i]) return false;
  }
  for (; i < nb; ++i) { // in case b is longer than a
    if ('0' < b.c_str()[i]) return true;
  }
  return false; // a longer string "a" cannot be strictly smaller than "b"
}

bool vnl_decnum::operator< (vnl_decnum const& r) const
{
#ifdef DEBUG
  std::cerr << "Entering vnl_decnum::operator< with " << data_ << " and " << r.data() << '\n';
#endif
  std::string rs = r.data();
  if (data_ == "NaN" || rs == "NaN") return false; // NaN compares to nothing!
  else if (operator==(r)) return false;
  else if (data_ == "Inf") return sign_ == '-';
  else if (rs == "Inf") return r.sign() == '+';

  if (sign_=='-' && r.sign() == '-') return -r < operator-();
  else if (sign_=='-') return true;
  else if (r.sign() == '-') return false;
  else if (sign_==' ') return true;
  else if (r.sign() == ' ') return false;
  else if (data_.length()+exp_ < rs.length()+r.exp()) return true;
  else if (data_.length()+exp_ > rs.length()+r.exp()) return false;
  else // at this point, the orders of magnitude are the same
    return comp(data_,rs);
}

// Returns the sum of the two first arguments (interpreted as mantissas with the same exponent).
// Both arguments should consist of digits only.
// The third argument will be the exponent of the result.
vnl_decnum vnl_decnum::plus(std::string const& a, std::string const& b, long exp)
{
#ifdef DEBUG
  std::cerr << "Entering vnl_decnum::plus with " << a << " and " << b << '\n';
#endif
  std::string result = "";
  int na=int(a.length()), nb=int(b.length()), carry=0;
  for (--na,--nb; na>=0&&nb>=0; --na,--nb) {
    char c = a.c_str()[na] + (b.c_str()[nb] - '0') + carry;
    if (c > '9') c-=10, carry=1; else carry=0;
    result.insert(result.begin(), c);
  }
  for (; na>=0&&nb<0; --na) {
    char c = a.c_str()[na] + carry;
    if (c > '9') c-=10, carry=1; else carry=0;
    result.insert(result.begin(), c);
  }
  for (; nb>=0&&na<0; --nb) {
    char c = b.c_str()[nb] + carry;
    if (c > '9') c-=10, carry=1; else carry=0;
    result.insert(result.begin(), c);
  }
  if (carry) result.insert(result.begin(), '1');
  return vnl_decnum('+',result,exp);
}

// Returns the difference of the two first arguments (interpreted as mantissas with the same exponent).
// Both arguments should consist of digits only
// and the first one should be numerically larger than the second one.
// The third argument will be the exponent of the result.
vnl_decnum vnl_decnum::minus(std::string const& a, std::string const& b, long exp)
{
#ifdef DEBUG
  std::cerr << "Entering vnl_decnum::minus with " << a << " and " << b << '\n';
#endif
  std::string result = "";
  int na=int(a.length()), nb=int(b.length()), carry=0;
  assert(na>=nb);
  for (--na,--nb; na>=0&&nb>=0; --na,--nb) {
    char c = a.c_str()[na] - (b.c_str()[nb] - '0') - carry;
    if (c < '0') c+=10, carry=1; else carry=0;
    result.insert(result.begin(), c);
  }
  for (; na>=0&&nb<0; --na) {
    char c = a.c_str()[na] - carry;
    if (c < '0') c+=10, carry=1; else carry=0;
    result.insert(result.begin(), c);
  }
  for (na=0; result.c_str()[na]=='0'; ++na) ;
  if (na) result.erase(0, na);
  assert(carry==0);
  return vnl_decnum('+',result,exp);
}

vnl_decnum vnl_decnum::operator+(vnl_decnum const& r) const
{
#ifdef DEBUG
  std::cerr << "Entering vnl_decnum::operator+ with "
           << sign_ << data_ << 'e' << exp_ << " and "
           << r.sign() << r.data() << 'e' << r.exp() << '\n';
#endif
  if (data_ == "NaN") return *this;
  else if (r.data() == "NaN") return r;
  else if (data_ == "Inf" && r.data() == "Inf") return sign_ == r.sign() ? *this : vnl_decnum("NaN");
  else if (data_ == "Inf") return *this;
  else if (r.data() == "Inf") return r;

  if (sign_ == ' ') return r;
  else if (r.sign() == ' ') return *this;
  else if (operator==(-r)) return vnl_decnum(0L);
  // by adding zeros to r.data() while decreasing r.exp() until it equals exp_, both mantissas become comparable:
  else if (exp_ < r.exp()) { return operator+(vnl_decnum(r.sign(), add_zeros(r.data(),r.exp()-exp_), exp_)); }
  else if (exp_ > r.exp()) { return r.operator+(*this); }
  else if (sign_ == '-' && r.sign() == '-') return - plus(data_, r.data(), exp_);
  else if (sign_ == '-' && operator<(-r)) return - minus(data_, r.data(), exp_);
  else if (sign_ == '-') return minus(r.data(), data_, exp_);
  else if (r.sign() == '-' && operator>(-r)) return minus(data_, r.data(), exp_);
  else if (r.sign() == '-') return - minus(r.data(), data_, exp_);
  else return plus(data_, r.data(), exp_);
}

// Returns the product of the two arguments.
// The first argument should consist of digits only;
// the second argument should be a single digit.
std::string vnl_decnum::mult(std::string const& a, char b)
{
#ifdef DEBUG
  std::cerr << "Entering vnl_decnum::mult with " << a << " and " << b << '\n';
#endif
  std::string result = "";
  int na=int(a.length()), carry=0, bb = b-'0';
  assert(bb >= 0 && bb <= 9);
  for (--na; na>=0; --na) {
    int c = (a.c_str()[na]-'0') * bb + carry;
    assert(c >= 0 && c <= 99);
    carry = c/10; c%=10;
    result.insert(result.begin(), '0'+c);
  }
  if (carry) result.insert(result.begin(), '0'+carry);
  return result;
}

vnl_decnum vnl_decnum::operator*(vnl_decnum const& r) const
{
#ifdef DEBUG
  std::cerr << "Entering vnl_decnum::operator* with "
           << sign_ << data_ << 'e' << exp_ << " and "
           << r.sign() << r.data() << 'e' << r.exp() << '\n';
#endif
  if (data_ == "NaN") return *this;
  else if (r.data() == "NaN") return r;
  else if (data_ == "Inf" || r.data() == "Inf")
    return sign_ == r.sign()             ? vnl_decnum("+Inf")
         : (sign_==' ' || r.sign()==' ') ? vnl_decnum("NaN")
         :                                 vnl_decnum("-Inf");

  int sign = (sign_==' '?0:sign_=='-'?-1:1) * (r.sign()==' '?0:r.sign()=='-'?-1:1);
  vnl_decnum result(0L);
  if (sign == 0) return result;
  std::string zeros = "";
  int na=int(data_.length());
  for (--na; na>=0; --na) {
    result += vnl_decnum(mult(r.data(), data_.c_str()[na]) + zeros);
    zeros.push_back('0');
  }
  result <<= (exp_ + r.exp());
  return (sign==-1) ? -result : result;
}

// Returns the largest one-significant-digit divisor of the two arguments.
// The largest multiple of b not larger than a is returned in b.
// (I.e.: the product of the original b with the returned divisor.)
// The arguments should consist of digits only
// and the first one should be numerically larger than the second one.
std::string vnl_decnum::div(std::string const& a, std::string& b)
{
#ifdef DEBUG
  std::cerr << "Entering vnl_decnum::div with " << a << " and " << b << '\n';
#endif
  int na=int(a.length()), nb=int(b.length());
  assert(na >= nb);
  if (comp(a,b)) ++nb;
  std::string u = "1";
  while (nb<na) { b.push_back('0'), u.push_back('0'); ++nb; }
  std::string c = b;
  for (; u[0]<'9'; u[0]++) {
    vnl_decnum d = plus(c,b,0L);
    if (vnl_decnum(a) < d) { b=c; return u; }
    c=d.data();
  }
  // if we end up here, the quotient must start with 9:
  b=c; return u;
}

vnl_decnum vnl_decnum::operator/(vnl_decnum const& r) const
{
#ifdef DEBUG
  std::cerr << "Entering vnl_decnum::operator/ with "
           << sign_ << data_ << 'e' << exp_ << " and "
           << r.sign() << r.data() << 'e' << r.exp() << '\n';
#endif
  if (data_ == "NaN") return *this;
  else if (r.data() == "NaN") return r;
  else if (data_ == "Inf" && r.data() == "Inf") return vnl_decnum("NaN");
  else if (r.data() == "Inf")                   return vnl_decnum(0L);
  else if (data_ == "Inf")
    return sign_ == r.sign() ? vnl_decnum("+Inf")
         :                     vnl_decnum("-Inf");
  else if (r == 0L)
    return sign_==' ' ? vnl_decnum("NaN")
         : sign_=='+' ? vnl_decnum("+Inf")
         :              vnl_decnum("-Inf");

  if (r == 1L) return *this;
  if (operator==(r)) return vnl_decnum('+',"1",0L);
  std::string a = data_, b = r.data();
  int na=int(a.length()), nb=int(b.length());
  vnl_decnum result(0L);
  while (na > nb || (na == nb && !comp(a,b))) {
    std::string c = b;
    std::string d = div(a, c);
#ifdef DEBUG
    std::cerr << "vnl_decnum::div returns " << d << '\n';
#endif
    result += vnl_decnum(d);
    vnl_decnum m = vnl_decnum(a) - vnl_decnum(c);
    a = m.data(); na=a.length();
  }
  result <<= (exp_ - r.exp());
  int sign = (sign_=='-'?-1:1) * (r.sign()=='-'?-1:1);
  return sign==-1 ? -result : result;
}

vnl_decnum vnl_decnum::operator%(vnl_decnum const& r) const
{
#ifdef DEBUG
  std::cerr << "Entering vnl_decnum::operator% with "
           << sign_ << data_ << 'e' << exp_ << " and "
           << r.sign() << r.data() << 'e' << r.exp() << '\n';
#endif
  if (r == 0L) return *this;
  else if (data_ == "NaN") return *this;
  else if (r.data() == "NaN") return r;
  else if (r.data() == "Inf") return vnl_decnum("NaN");
  else if (data_ == "Inf")    return *this;

  if (r == vnl_decnum("1")) return vnl_decnum("0");
  if (operator==(r)) return vnl_decnum("0");
  std::string a = data_, b = r.data();
  int na=int(a.length()), nb=int(b.length());
  while (na > nb || (na == nb && !comp(a,b))) {
    std::string c = b;
    std::string d = div(a, c);
#ifdef DEBUG
    std::cerr << "vnl_decnum::div returns " << d << '\n';
#endif
    vnl_decnum m = vnl_decnum(a) - vnl_decnum(c);
    a = m.data(); na=a.length();
  }
  if (na==0) return vnl_decnum(0L);
  else       return vnl_decnum(sign_,a,exp_);
}

// See also the constructor from std::string.
std::istream& operator>>(std::istream& s, vnl_decnum& r)
{
#ifdef DEBUG
  std::cerr << "Entering operator>>(istream,vnl_decnum) with " << r << '\n';
#endif
  std::string data = "";
  int c = ' ';
  while (c == ' ' || c == '\t' || c == '\r') c=s.get(); // blank skipping
  if (c == -1 || c == '\n') { r = vnl_decnum(0L); return s; } // stop parsing at EOLN or EOF
  if (c == '-') { data = "-"; c=s.get(); }
  else if (c == '+') c=s.get();
  if (c == 'I' && s.get() == 'n' && s.get() == 'f') { data += "Inf"; }
  else if (c == 'N' && s.get() == 'a' && s.get() == 'N') { data = "NaN"; }
  else {
    while (c == '0') c=s.get();
    while ((c >= '0' && c <= '9') || c == '.') { data.push_back(c); c=s.get(); }
    if (c == 'e') {
      data.push_back(c); c=s.get();
      if (c == '-' || c == '+') { data.push_back(c); c=s.get(); }
      while (c >= '0' && c <= '9') { data.push_back(c); c=s.get(); }
    }
  }
  r = vnl_decnum(data);
  if (c > 0) s.putback(c);
  return s;
}


// Remove all trailing zeros from the mantissa, and  increase the exponent accordingly.
// Return the (thus modified) *this.
vnl_decnum& vnl_decnum::compactify()
{
  if (sign_ == ' ' || data_ == "Inf") { exp_ = 0L; return *this; }
  unsigned long n = data_.find_last_not_of('0') + 1;
  unsigned long l = data_.length();
  if (n < l) { // at least one trailing zero is found
    exp_ += l-n; data_.erase(n);
  }
  else if (n > l) { // there are no non-zeros, i.e.: this number is zero
    exp_ = 0; data_.clear(); sign_ = ' ';
  }
  // if n == l, the mantissa did not end in 0, so it is returned unmodified.
  return *this;
}
