// This is core/vnl/vnl_polynomial.h
#ifndef vnl_polynomial_h_
#define vnl_polynomial_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Evaluation of univariate polynomials
// Templated class (on the data type of the coefficients),
// further very similar to the vnl_real_polynomial class,
// except that it uses std::vector instead of vnl_vector as data container,
// that the zero polynomial is represented by an empty vector,
// and that the coefficients go in the other direction.
//
// Important note on the implementation choice (reversed coefficient vector
// as opposed to the class vnl_real_npolynomial):
// The choice made here is definitely the more natural one, since it makes
// polynomials of different degrees much more naturally comparable, and hence
// simplifies the implementation of e.g. operator+(). Indeed: even if the
// degrees are different, the coefficients [i] of two polynomials are the ones
// to be considered together since they both refer to $X^i$. Also, normalizing
// the internal representation (in case the highest order coefficient is zero)
// now just needs to pop_back() instead of shifting the coefficients vector.
// In summary, the choice made here is both more natural and more performant.
//
// \author Peter Vanroose, ABIS Leuven.
// \date  August 2011
//
// \verbatim
//  Modifications
//   20 Aug 2011 - Peter Vanroose - internal repr change: coeff vector reversed
// \endverbatim

#include <vector>
#include <iosfwd>
#include <vcl_compiler.h>
#include <vcl_cassert.h>
#include "vnl/vnl_export.h"

//: Evaluation of polynomials.
//  vnl_polynomial<T> represents a univariate polynomial with
//  coefficients of datatype T, stored as a vector of values.
//  This allows evaluation of the polynomial $p(X)$ at given values of $X$,
//  and of its derivative $p'(X)$ or primitive function $\int p$.
//
//  The class also provides the common polynomial arithmetic, i.e.,
//  + - *, and even long division (through operators / and %).
//
//  The coefficients (coeffs_) are stored as a vector, starting with
//  the constant term. Hence coeffs_[n] is the coefficient of $X^n$,

template <class T>
class VNL_TEMPLATE_EXPORT vnl_polynomial
{
 public:
  //: Initialize the polynomial from its coefficients, lowest order first.
  // The polynomial is $ a[d] X^d + a[d-1] X^{d-1} + \cdots + a[0] = 0 $.
  // Note that this constructor expects the constant term coefficient first,
  // as opposed to the C array constructor!
  // An assertion makes sure that the input vector is in normalised form, i.e.,
  // that it is either empty or that the highest order coefficient is nonzero.
  vnl_polynomial(std::vector<T> const& a): coeffs_(a) { assert(a.begin()==a.end() || a.back() != T(0)); }

  //: Initialize polynomial from C array, highest order first.
  // The parameter \p len is the number of coefficients passed in,
  // which equals the degree plus one.
  // Note that this constructor expects the highest order coefficients first,
  // as opposed to the std::vector constructor!
  vnl_polynomial(T const* a, unsigned len) { assert(len==0 || *a != T(0)); while (len--) coeffs_.push_back(a[len]); }

  //: Initialize polynomial from single value, thus creating a monomial.
  // This is effectively an implicit cast from type T to class vnl_polynomial,
  // useful when adding or multiplying a polynomial with a number.
  vnl_polynomial(T const& a): coeffs_(1u, a) { if (a==T(0)) coeffs_.clear(); }

  //: Initialize polynomial of a given degree.
  // The default constructor initializes to the zero polynomial (which has degree -1).
  // but even with an explicit argument, the polynomial is the zero polynomial
  // (with non-compact storage) so it should always be used in conjunction with
  // operator[] for setting individual coefficients, at least coefficient [d].
  vnl_polynomial(int d=-1): coeffs_(d+1) { assert (d>=-1); }

  //: comparison operator
  bool operator==(vnl_polynomial<T> const& p) const { return p.coefficients() == coeffs_; }

  //: Returns negative of this polynomial
  vnl_polynomial<T> operator-() const;

  //: Returns polynomial which is sum of this with polynomial f
  vnl_polynomial<T> operator+(vnl_polynomial<T> const& f) const;

  //: Returns polynomial which is difference of this with polynomial f
  vnl_polynomial<T> operator-(vnl_polynomial<T> const& f) const { return operator+(-f); }

  //: Returns polynomial which is product of this with polynomial f
  vnl_polynomial<T> operator*(vnl_polynomial<T> const& f) const;

  //: Returns polynomial which is the result of the long division by polynomial f
  // Beware that this operation might not make sense for integral types T
  // if the highest order coefficient of f is not 1 or -1!
  vnl_polynomial<T> operator/(vnl_polynomial<T> const& f) const;

  //: Returns polynomial which is the remainder after a long division by polynomial f
  // Beware that this operation might not make sense for integral types T
  // if the highest order coefficient of f is not 1 or -1!
  vnl_polynomial<T> operator%(vnl_polynomial<T> const& f) const;

  vnl_polynomial<T>& operator+=(vnl_polynomial<T> const& f) { return *this = operator+(f); }
  vnl_polynomial<T>& operator-=(vnl_polynomial<T> const& f) { return *this = operator-(f); }
  vnl_polynomial<T>& operator*=(vnl_polynomial<T> const& f) { return *this = operator*(f); }
  vnl_polynomial<T>& operator/=(vnl_polynomial<T> const& f) { return *this = operator/(f); }
  vnl_polynomial<T>& operator%=(vnl_polynomial<T> const& f) { return *this = operator%(f); }

  //: Evaluate polynomial at value \p x
  T evaluate(T const& x) const;

  //: Return derivative of this polynomial
  vnl_polynomial<T> derivative() const;

  //: Evaluate derivative at value \p x
  T devaluate(T const& x) const { return derivative().evaluate(x); }

  //: Return primitive function (inverse derivative) of this polynomial
  // Since a primitive function is not unique, the one with constant term 0 is returned.
  // Beware that this operation might not make sense for integral types T!
  vnl_polynomial<T> primitive() const;

  //: Evaluate integral at \p x (assuming constant of integration is zero)
  // Beware that this operation might not make sense for integral types T!
  T evaluate_integral(T const& x) const { return primitive().evaluate(x); }

  //: Evaluate integral between \p x1 and \p x2
  // Beware that this operation might not make sense for integral types T!
  T evaluate_integral(T const& x1, T const& x2) const { return evaluate_integral(x2)-evaluate_integral(x1); }

  // Data Access---------------------------------------------------------------

  //: Return the degree (highest power of X) of the polynomial.
  // If the polynomial is zero, the degree is effectively -1.
  // Note that this method assumes a compactified representation, i.e., one
  // where the highest order coefficient is non-zero. Otherwise, the value
  // returned by degree() will be larger than the degree.
  int     degree() const { return int(coeffs_.size()) - 1; }

  //: Access to the polynomial coefficient of $X^i$
  T& operator [] (unsigned int i)       { assert(int(i)<=degree()); return coeffs_[i]; }
  //: Access to the polynomial coefficient of $X^i$
  T  operator [] (unsigned int i) const { assert(int(i)<=degree()); return coeffs_[i]; }

  //: Return the vector of coefficients
  const std::vector<T>& coefficients() const { return coeffs_; }
  //: Return the vector of coefficients
        std::vector<T>& coefficients()       { return coeffs_; }

  void set_coefficients(std::vector<T> const& coeffs) {coeffs_ = coeffs;}

  //: Print this polynomial to stream
  void print(std::ostream& os) const;

 protected:
  //: The coefficients of the polynomial.
  // coeffs_.front() is the const term.
  // coeffs_[n] is the coefficient of the $X^n$ term
  std::vector<T> coeffs_;
};

template <class T> VNL_TEMPLATE_EXPORT
std::ostream& operator<<(std::ostream& os, vnl_polynomial<T> const& p) { p.print(os); return os; }

#define VNL_POLYNOMIAL_INSTANTIATE(T) extern "please #include vnl/vnl_polynomial.hxx instead"

#endif // vnl_polynomial_h_
