// This is core/vnl/vnl_finite.h
#ifndef vnl_finite_h_
#define vnl_finite_h_
//:
// \file
// \brief modulo-N arithmetic (finite ring Z_N and Z_N[X])
//
// The templated vnl_finite_int<N> provides arithmetic "modulo N", i.e.,
// arithmetic in the finite (Galois) field GF(N) in case N is a prime
// or just in the finite ring (or semi-field) of integers modulo N otherwise.
// In that case division makes no sense (unless no zero divisor is involved),
// but all other operations remain valid.
//
// Note that this does not cover finite fields with non-prime sizes (4,8,9,...).
// These are covered by the vnl_finite_int_poly<N,M> class, which implements
// arithmetic with polynomials of degree < M over vnl_finite_int<N>.
// Multiplication is defined modulo a degree M polynomial.
//
// For N prime, and when the "modulo" polynomial is irreducible,
// vnl_finite_int_poly<N,M> implements the finite field GF(N^M).
//
// \author
//  Peter Vanroose, K.U.Leuven, ESAT/PSI.
// \date 5 May 2002.
//
// \verbatim
// Modifications
//  1 June 2002 - Peter Vanroose - added totient(), decompose(), is_unit(), order(), log(), exp().
//  4 June 2002 - Peter Vanroose - renamed class and file name
//  8 June 2002 - Peter Vanroose - added vnl_finite_int_poly<N,M>
// \endverbatim

#include <vcl_iostream.h>
#include <vcl_cassert.h>
#include <vcl_vector.h>

//: finite modulo-N arithmetic
//
// The templated vnl_finite_int<N> provides arithmetic "modulo N", i.e.,
// arithmetic in the finite (Galois) field GF(N) in case N is a prime
// or just in the finite ring (or semi-field) of integers modulo N otherwise.
// In that case division makes no sense (unless no zero divisor is involved),
// but all other operations remain valid.
//
template <int N>
class vnl_finite_int
{
  int val_; //!< value of this number (smallest nonnegative representation)

  typedef vnl_finite_int<N> Base;
 public:
  //: The number of different finite_int numbers of this type
  static unsigned int cardinality() { return N; }

  //: Creates a finite int element.
  //  Default constructor gives 0.
  //  Also serves as automatic cast from int to vnl_finite_int.
  inline vnl_finite_int(int x = 0) : val_((x%=N)<0?N+x:x), mo_(0), lp1_(0) {assert(N>1);}
  //  Copy constructor
  inline vnl_finite_int(Base const& x) : val_(int(x)), mo_(x.mo_), lp1_(x.lp1_) {}
  //  Destructor
  inline ~vnl_finite_int() {}
  // Implicit conversions
  inline operator int() const { return val_; }
  inline operator int() { return val_; }
  inline operator long() const { return val_; }
  inline operator long() { return val_; }
  inline operator short() const { short r = (short)val_; assert(r == val_); return r; }
  inline operator short() { short r = (short)val_; assert(r == val_); return r; }

  //: Assignment
  inline Base& operator=(Base const& x) { val_ = int(x); mo_=x.mo_; lp1_=x.lp1_; return *this; }
  inline Base& operator=(int x) { x%=N; val_ = x<0 ? N+x : x; mo_=lp1_=0; return *this; }

  //: Comparison of finite int numbers.
  // Note that finite ints have no order, so < and > make no sense.
  inline bool operator==(Base const& x) const { return val_ == int(x); }
  inline bool operator!=(Base const& x) const { return val_ != int(x); }
  inline bool operator==(int x) const { return operator==(Base(x)); }
  inline bool operator!=(int x) const { return !operator==(x); }

  //: Unary minus - returns the additive inverse
  inline Base operator-() const { return Base(N-val_); }
  //: Unary plus - returns the current number
  inline Base operator+() const { return *this; }
  //: Unary not - returns true if finite int number is equal to zero.
  inline bool operator!() const { return val_ == 0; }

  //: Plus&assign: replace lhs by lhs + rhs
  inline Base& operator+=(Base const& r) { mo_=lp1_=0; val_ += int(r); if (val_ >= int(N)) val_ -= N; return *this; }
  inline Base& operator+=(int r) { return operator=(val_+r); }
  //: Minus&assign: replace lhs by lhs - rhs
  inline Base& operator-=(Base const& r) { mo_=lp1_=0; val_ -= int(r); if (val_ < 0) val_ += N; return *this; }
  inline Base& operator-=(int r) { return operator=(val_-r); }
  //: Multiply&assign: replace lhs by lhs * rhs
  inline Base& operator*=(int r) {
    r %=N; if (r<0) r=N+r;
    // This rather complicated implementation is necessary to avoid integer overflow
    if (N<=0x7fff || (val_<=0x7fff && r<=0x7fff)) { val_ *= r; val_ %= N; return *this; }
    else { int v=val_; operator+=(v); operator*=(r/2); if (r%2) operator+=(v); return *this; }
  }
  //: Multiply&assign: replace lhs by lhs * rhs
  inline Base& operator*=(Base const& r) {
    mo_=0;
    if (lp1_!=0 && r.lp1_!=0) set_log(lp1_+r.lp1_-2); else lp1_=0;
    // This rather complicated implementation is necessary to avoid integer overflow
    unsigned int s=int(r);
    if (N<=0x7fff || (val_<=0x7fff && s<=0x7fff)) { val_ *= s; val_ %= N; return *this; }
    else { int v=val_; operator+=(v); operator*=(s/2); if (s%2) operator+=(v); return *this; }
  }

  //: Return the Euler totient function, i.e., the number of units of this ring
  //  This number also equals the periodicity of the exponent: every unit,
  //  when raised to this power, yields 1.
  static inline unsigned int totient() {
    static unsigned int t_ = 0; // cached value
    if (t_ != 0) return t_;
    vcl_vector<unsigned int> d = decompose();
    t_ = 1; unsigned int p = 1;
    for (unsigned int i=0; i<d.size(); ++i)
    {
      if (p != d[i]) t_ *= d[i]-1;
      else           t_ *= d[i];
      p = d[i];
    }
    return t_;
  }

  //: Multiplicative inverse.
  //  Uses exp() and log() for efficient computation, unless log() is not defined.
  inline Base reciproc() const {
    assert(is_unit());
    if (val_==1) return *this;
    Base z = smallest_generator();
    if (z!=1) return exp(Base::totient()-log());
    for (unsigned int r=1; r<=N/2; ++r) {
      unsigned int t=int(*this*int(r));
      if (t==1) return r; else if (t==N-1) return N-r;
    }
    return 0; // This will never be executed
  }

  //: Divide&assign.  Uses r.reciproc() for efficient computation.
  inline Base& operator/=(Base const& r) {
    assert(r.is_unit());
    return val_ == 0 ? operator=(0) : operator*=(r.reciproc());
  }

  //: Pre-increment (++r).
  inline Base& operator++() { mo_=lp1_=0; ++val_; if (val_==N) val_=0; return *this; }
  //: Pre-decrement (--r).
  inline Base& operator--() { mo_=lp1_=0; if (val_==0) val_=N; --val_; return *this; }
  //: Post-increment (r++).
  inline Base operator++(int){Base b=*this; mo_=lp1_=0; ++val_; if (val_==N) val_=0; return b; }
  //: Post-decrement (r--).
  inline Base operator--(int){Base b=*this; mo_=lp1_=0; if (val_==0) val_=N; --val_; return b;}

  //: Write N as the unique product of prime factors.
  static vcl_vector<unsigned int> decompose() {
    static vcl_vector<unsigned int> decomposition_ = vcl_vector<unsigned int>(); // cached value
    if (decomposition_.size() > 0) return decomposition_;
    unsigned int r = N;
    for (unsigned int d=2; d*d<=r; ++d)
      while (r%d == 0) { decomposition_.push_back(d); r /= d; }
    if (r > 1) decomposition_.push_back(r);
    return decomposition_;
  }

  //: Return true when N is a prime number, i.e., when this ring is a field
  static inline bool is_field() {
    vcl_vector<unsigned int> d = Base::decompose();
    return d.size() == 1;
  }

  //: Return true only when x is a unit in this ring.
  //  In a field, all numbers except 0 are units.
  //  The total number of units is given by the Euler totient function.
  inline bool is_unit() const { return gcd(val_) == 1; }

  //: Return true only when x is a zero divisor, i.e., is not a unit.
  inline bool is_zero_divisor() const { return gcd(val_) != 1; }

  //: The additive order of x is the smallest nonnegative r such that r*x == 0.
  inline unsigned int additive_order() const { if (val_ == 0) return 1; return N/gcd(val_); }

  //: The multiplicative order of x is the smallest r (>0) such that x^r == 1.
  inline unsigned int multiplicative_order() const {
    if (mo_ != 0) return mo_;
    if (gcd(val_) != 1) return -1; // should actually return infinity
    Base y = val_;
    for (int r=1; r<N; ++r) { if (y==1) return mo_=r; y *= val_; }
    return 0; // this should not happen
  }

  //: Return the smallest multiplicative generator of the units in this ring.
  //  This is only possible if the units form a cyclic group for multiplication.
  //  If not, smallest_generator() returns 1 to indicate this fact.
  //  Note that the multiplication group of a finite *field* is always cyclic.
  static Base smallest_generator() {
    static Base gen_ = 0; // cached value
    if (gen_ != 0) return gen_;
    if (N==2) return gen_=1;
    unsigned int h = Base::totient() / 2; // note that totient() is always even
    for (gen_=2; gen_!=0; ++gen_) {
      // calculate gen_^h
      unsigned int g=h; Base y = 1, z = gen_; while (g>0) { if (g%2) y *= z; g/=2; z*=z; }
      // quick elimination of non-generator:
      if (y == 1) continue;
      // calculate gen_.multiplicative_order() only if really necessary:
      if (gen_.multiplicative_order() == Base::totient()) { gen_.set_log(1); return gen_; }
    }
    assert(!Base::is_field()); // can only reach this point when N is not prime
    return gen_=1;
  }

  //: Return the r-th power of this number.
  inline Base pow(int r) {
    r %= Base::totient(); if (r<0) r += Base::totient();
    if (r==0) return 1; if (r==1) return *this;
    Base y = 1, z = *this; int s=r; while (s!=0) { if (s%2) y*=z; s/=2; z*=z; }
    if (lp1_ != 0) y.set_log(r*(lp1_-1));
    return y;
  }

  //: Return the smallest nonnegative exponent r for which x=g^r, where g is the smallest generator.
  inline unsigned int log() const {
    if (is_zero_divisor()) return -1; // should actually return minus infinity
    if (lp1_ != 0) return lp1_-1;
    Base z = smallest_generator();
    assert(N==2||z!=1); // otherwise, the units of this ring do not form a cyclic group
    Base y = 1;
    for (lp1_=1; lp1_<=N; ++lp1_) {
      if (y == *this) return lp1_-1;
      y *= z;
    }
    return -1; // should never reach this point
  }

  //: Return the inverse of log(), i.e., return g^r where g is the smallest generator.
  static inline Base exp(int r) {
    Base z = smallest_generator();
    assert(N==2||z!=1); // otherwise, the units of this ring do not form a cyclic group
    return z.pow(r);
  }

  //: Calculate the greatest common divisor of l and N.
  static inline unsigned int gcd(unsigned int l, unsigned int n) {
    unsigned int l1 = n;
    while (l!=0) { unsigned int t = l; l = l1 % l; l1 = t; }
    return l1;
  }
  static inline unsigned int gcd(unsigned int l) { return gcd(l, N); }

 private:
  //: private function to set cached value of lp1_ when available
  void set_log(unsigned int r) const { r %= Base::totient(); lp1_ = r+1; }

  mutable unsigned int mo_; //!< cached value for multiplicative order
  mutable unsigned int lp1_; //!< cached value for 1+log()
};

//: formatted output
// \relates vnl_finite_int
template <int N>
inline vcl_ostream& operator<< (vcl_ostream& s, vnl_finite_int<N> const& r)
{
  return s << int(r);
}

//: simple input
// \relates vnl_finite_int
template <int N>
inline vcl_istream& operator>> (vcl_istream& s, vnl_finite_int<N>& r)
{
  int n; s >> n; r=n; return s;
}

//: Returns the sum of two finite int numbers.
// \relates vnl_finite_int
template <int N>
inline vnl_finite_int<N> operator+ (vnl_finite_int<N> const& r1, vnl_finite_int<N> const& r2)
{
  vnl_finite_int<N> result(r1); return result += r2;
}

template <int N>
inline vnl_finite_int<N> operator+ (vnl_finite_int<N> const& r1, int r2)
{
  vnl_finite_int<N> result(r1); return result += r2;
}

template <int N>
inline vnl_finite_int<N> operator+ (int r2, vnl_finite_int<N> const& r1)
{
  vnl_finite_int<N> result(r1); return result += r2;
}

//: Returns the difference of two finite int numbers.
// \relates vnl_finite_int
template <int N>
inline vnl_finite_int<N> operator- (vnl_finite_int<N> const& r1, vnl_finite_int<N> const& r2)
{
  vnl_finite_int<N> result(r1); return result -= r2;
}

template <int N>
inline vnl_finite_int<N> operator- (vnl_finite_int<N> const& r1, int r2)
{
  vnl_finite_int<N> result(r1); return result -= r2;
}

template <int N>
inline vnl_finite_int<N> operator- (int r2, vnl_finite_int<N> const& r1)
{
  vnl_finite_int<N> result(-r1); return result += r2;
}

//: Returns the product of two finite int numbers.
// \relates vnl_finite_int
template <int N>
inline vnl_finite_int<N> operator* (vnl_finite_int<N> const& r1, vnl_finite_int<N> const& r2)
{
  vnl_finite_int<N> result(r1); return result *= r2;
}

template <int N>
inline vnl_finite_int<N> operator* (vnl_finite_int<N> const& r1, int r2)
{
  vnl_finite_int<N> result(r1); return result *= r2;
}

template <int N>
inline vnl_finite_int<N> operator* (int r2, vnl_finite_int<N> const& r1)
{
  vnl_finite_int<N> result(r1); return result *= r2;
}

//: Returns the quotient of two finite int numbers.
//  Uses r2.reciproc() for efficient computation.
// \relates vnl_finite_int
template <int N>
inline vnl_finite_int<N> operator/(vnl_finite_int<N> const& r1, vnl_finite_int<N> const& r2)
{
  assert(r2.is_unit()); return r1 == 0 ? vnl_finite_int<N>(0) : r1*r2.reciproc();
}

template <int N>
inline vnl_finite_int<N> operator/ (vnl_finite_int<N> const& r1, int r2)
{
  vnl_finite_int<N> result(r1); return result /= r2;
}

template <int N>
inline vnl_finite_int<N> operator/ (int r1, vnl_finite_int<N> const& r2)
{
  vnl_finite_int<N> result(r1); return result /= r2;
}

template <int N>
inline bool operator== (int  r1, vnl_finite_int<N> const& r2) { return r2==r1; }
template <int N>
inline bool operator!= (int  r1, vnl_finite_int<N> const& r2) { return r2!=r1; }

//:
// \relates vnl_finite_int
template <int N>
inline vnl_finite_int<N> vnl_math_squared_magnitude(vnl_finite_int<N> const& x) { return x*x; }
template <int N>
inline vnl_finite_int<N> vnl_math_sqr(vnl_finite_int<N> const& x) { return x*x; }
template <int N>
inline bool vnl_math_isnan(vnl_finite_int<N> const& ){return false;}
template <int N>
inline bool vnl_math_isfinite(vnl_finite_int<N> const& x){return true;}

//: finite modulo-N arithmetic with polynomials of degree < M
//
// This class implements arithmetic with polynomials of degree < M over
// vnl_finite_int<N>. Multiplication is defined modulo a polynomial of degree M.
//
// For N prime, and when the "modulo" polynomial is irreducible,
// vnl_finite_int_poly<N,M> implements the finite field GF(N^M).
//
// Addition, subtraction and scalar multiplication are already defined without
// the presence of a "modulo" polynomial.  Restricted to these operations,
// vnl_finite_int_poly<N,M> forms an M-dimensional vector space over
// vnl_finite_int<N>.  The current implementation does not yet implement
// anything more than that.
//
template <int N, int M>
class vnl_finite_int_poly
{
  typedef vnl_finite_int_poly<N,M> Base;
  typedef vnl_finite_int<N> Scalar;

  vcl_vector<Scalar> val_; //!< M-tuple (or degree M-1 polynomial) representing this

  // This essentially implements std::pow(int,int) which is not always available
  static unsigned int Ntothe(unsigned int m) { return m==0?1:N*Ntothe(m-1); }
 public:
  //: The number of different finite_int polynomials of this type
  static unsigned int cardinality() { return Ntothe(M); }

  //: Creates a general finite_int_poly.
  inline vnl_finite_int_poly(vcl_vector<Scalar> const& p) : val_(p) { assert(N>1); assert(M>0); assert(p.size()<=M); }
  //: Creates a degree 0 finite_int_poly.
  inline vnl_finite_int_poly(Scalar const& n) : val_(vcl_vector<Scalar>(1)) { assert(N>1); assert(M>0); val_[0]=n; }
  //: Default constructor. Creates a degree 0 finite_int_poly equal to 0.
  inline vnl_finite_int_poly() : val_(vcl_vector<Scalar>(1)) { assert(N>1); assert(M>0); val_[0]=0; }
  //  Copy constructor
  inline vnl_finite_int_poly(Base const& x) : val_(x.val_) {}
  //  Destructor
  inline ~vnl_finite_int_poly() {}

  //: Formal degree of this polynomial
  inline unsigned int deg() const { return val_.size() - 1; }

  //: Effective degree of this polynomial; equals -1 when this polynomial is 0.
  inline int degree() const { for (int i=deg(); i>=0; --i) if (val_[i]!=0) return i; return -1; }

  //: Access to individual coefficients
  inline Scalar operator[](unsigned int i) const { assert(i<M); return i<=deg() ? val_[i] : Scalar(0); }

  //: Assignment
  inline Base& operator=(Base const& x) { val_ = x.val_; return *this; }
  inline Base& operator=(Scalar const& n) { val_ = vcl_vector<Scalar>(1); val_[0] = n; return *this; }

  //: Comparison of finite int polys.
  inline bool operator==(Base const& x) const {
    for (unsigned int i=0; i<=deg(); ++i)
      if (val_[i] != x[i]) return false;
    for (unsigned int i=deg()+1; i<=x.deg(); ++i)
      if (x[i] != 0) return false;
    return true;
  }
  inline bool operator!=(Base const& x) const { return !operator==(x); }
  inline bool operator==(Scalar const& x) const {
    if (x!=val_[0]) return false;
    for (unsigned int i=1; i<=deg(); ++i) if (val_[i] != 0) return false;
    return true;
  }
  inline bool operator!=(Scalar const& x) const { return !operator==(x); }

  //: Unary minus - returns the additive inverse
  inline Base operator-() const { vcl_vector<Scalar> p = val_; for (unsigned int i=0; i<p.size(); ++i) p[i]=-p[i]; return p; }
  //: Unary plus - returns the current polynomial
  inline Base operator+() const { return *this; }
  //: Unary not - returns true if finite int poly is equal to zero.
  inline bool operator!() const { for (unsigned int i=0; i<=deg(); ++i) if (val_[i] != 0) return false; return true; }

  //: Plus&assign: replace lhs by lhs + rhs
  inline Base& operator+=(Base const& r) {
    for (unsigned int i=0; i<=r.deg(); ++i)
      if (i<=deg()) val_[i] += r[i];
      else          val_.push_back(r[i]);
    return *this;
  }
  //: Minus&assign: replace lhs by lhs - rhs
  inline Base& operator-=(Base const& r) {
    for (unsigned int i=0; i<=r.deg(); ++i)
      if (i<=deg()) val_[i] -= r[i];
      else          val_.push_back(-r[i]);
    return *this;
  }

  //: Scalar multiple of this
  inline Base& operator*=(Scalar const& n) { for (unsigned int i=0; i<=deg(); ++i) val_[i] *= n; return *this; }

  //: The additive order of x is the smallest positive r such that r*x == 0.
  inline unsigned int additive_order() const {
    unsigned int r = N;
    for (unsigned int i=0; i<=deg(); ++i)
      if (val_[i] != 0) r=Scalar::gcd(val_[i],r);
    return N/r;
  }

  //: get/set the (irreducible) modulo polynomial of degree M
  //  Note that this polynomial has to be set only once, i.e., once set,
  //  it applies to all vnl_finite_int_polys with the same N and M.
  static vcl_vector<Scalar>& modulo_polynomial(vcl_vector<Scalar> p = vcl_vector<Scalar>())
  {
    static vcl_vector<Scalar> poly_(M+1, Scalar(0));
    if (p.size() == 0) { // retrieval
      assert(poly_[M] != 0); // cannot retrieve before having set
    }
    else
    {
      assert(p.size() == M+1 && p[M].is_unit());// must be of effective degree M
      // Now set poly_, thereby making the coefficient poly_[M] equal to -1.
      Scalar f = -1/p[M];
      for (int m=0; m<=M; ++m) poly_[m] = f*p[m];
    }
    return poly_;
  }

  //: Multiply&assign: replace lhs by lhs * rhs, modulo the "modulo" polynomial
  inline Base& operator*=(Base const& r) {
    Base x = *this; *this = r; *this *= x[0];
    while (val_.size() < M) val_.push_back(0);
    for (int i=1; i<=x.degree(); ++i)
      for (unsigned int j=0; j<=r.deg(); ++j)
        add_modulo_poly(i+j, x[i]*r[j]);
    return *this;
  }

  //: Return the multiplicative order of this polynomial.
  inline unsigned int multiplicative_order() const {
    Base t = Scalar(1);
    unsigned int order = 0;
    do t *= *this, ++order; while (t != Scalar(1) && t != Scalar(0));
    return t==Scalar(1) ? order : -1;
  }

  //: Return true when this ring is a field.
  //  Note that this requires that N is a prime, but that condition is not
  //  sufficient: also the "modulo" polynomial must be irreducible.
  static inline bool is_field() {
    if (!Scalar::is_field()) return false;

    vcl_vector<Scalar> mod_p = Base::modulo_polynomial();
    mod_p.pop_back(); // remove the "-1" coefficient of X^M
    return Base(mod_p).multiplicative_order()+1 == Base::cardinality();
  }

  //: Return the smallest multiplicative generator of the units in this ring.
  //  This is only possible if the units form a cyclic group for multiplication.
  //  If not, smallest_generator() returns 1 to indicate this fact.
  //  Note that the multiplication group of a finite *field* is always cyclic.
  static Base smallest_generator() {
    if (!Base::is_field()) return Scalar(1);
    vcl_vector<Scalar> mod_p = Base::modulo_polynomial();
    mod_p.pop_back(); // remove the "-1" coefficient of X^M
    return Base(mod_p);
  }

 private:
  //: Add x to the i-th degree coefficient of val_, possibly reducing modulo the "modulo" poly.
  inline void add_modulo_poly(unsigned int m, Scalar const& x)
  {
    if (m < M) val_[m] += x;
    else {
      vcl_vector<Scalar> p = modulo_polynomial(); // where p[M] == -1
      for (int k=0; k<M; ++k) add_modulo_poly(m-M+k, x*p[k]); // recursive call
    }
  }
};

//: Returns the sum of two finite int polynomials.
// \relates vnl_finite_int_poly
template <int N, int M>
inline vnl_finite_int_poly<N,M> operator+ (vnl_finite_int_poly<N,M> const& r1, vnl_finite_int_poly<N,M> const& r2)
{
  vnl_finite_int_poly<N,M> result=r1; return result += r2;
}

//: Returns the difference of two finite int polynomials.
// \relates vnl_finite_int_poly
template <int N, int M>
inline vnl_finite_int_poly<N,M> operator- (vnl_finite_int_poly<N,M> const& r1, vnl_finite_int_poly<N,M> const& r2)
{
  vnl_finite_int_poly<N,M> result=r1; return result -= r2;
}

//: Returns a scalar multiple of a finite int polynomial.
// \relates vnl_finite_int
// \relates vnl_finite_int_poly
template <int N, int M>
inline vnl_finite_int_poly<N,M> operator* (vnl_finite_int_poly<N,M> const& r1, vnl_finite_int<N> const& r2)
{
  vnl_finite_int_poly<N,M> result(r1); return result *= r2;
}

template <int N, int M>
inline vnl_finite_int_poly<N,M> operator* (vnl_finite_int<N> const& r2, vnl_finite_int_poly<N,M> const& r1)
{
  vnl_finite_int_poly<N,M> result(r1); return result *= r2;
}

//: Multiplies two finite int polynomials.
//  NOTE: this requires the "modulo" polynomial to be set.
//  Do this by calling modulo_polynomial(p), where p is a vector of length M+1.
// \relates vnl_finite_int_poly
template <int N, int M>
inline vnl_finite_int_poly<N,M> operator* (vnl_finite_int_poly<N,M> const& r1, vnl_finite_int_poly<N,M> const& r2)
{
  vnl_finite_int_poly<N,M> result(r1); return result *= r2;
}

//: formatted output
// \relates vnl_finite_int_poly
template <int N, int M>
inline vcl_ostream& operator<< (vcl_ostream& s, vnl_finite_int_poly<N,M> const& r)
{
  bool out = false;
  for (unsigned int i=0; i<=r.deg(); ++i) {
    if (r[i] == 0) continue;
    if (out) s << '+';
    out = true;
    if (r[i] != 1 || i==0) s << r[i];
    if (i>0) s << 'X';
    if (i>1) s << '^' << i;
  }
  if (!out) s << '0';
  return s;
}

#endif // vnl_finite_h_
