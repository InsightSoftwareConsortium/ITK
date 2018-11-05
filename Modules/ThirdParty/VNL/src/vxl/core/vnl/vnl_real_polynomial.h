// This is core/vnl/vnl_real_polynomial.h
#ifndef vnl_real_polynomial_h_
#define vnl_real_polynomial_h_
//:
// \file
// \brief Evaluation of real polynomials
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   06 Aug 96
//
// \verbatim
// Modifications
// 23 may 97, Peter Vanroose - "NO_COMPLEX" option added (until "complex" type is standardised)
// 27/03/2001 Ian Scott and Tim Cootes - Added Binary IO
// 27/03/2001 Ian Scott - Comments tidied up
// 25/11/2001 Peter Vanroose - added operator==(), derivative(), primitive(), print()
// 12/22/2004 Kongbin Kang - add structured comment for operator==()
// \endverbatim

#include <complex>
#include <iosfwd>
#include <vnl/vnl_vector.h>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include <cassert>
#include "vnl/vnl_export.h"

//:Evaluation of real polynomials at real and complex points.
//    vnl_real_polynomial represents a univariate polynomial with real
//    coefficients, stored as a vector of doubles.  This allows
//    evaluation of the polynomial $p(x)$ at given values of $x$,
//    or of its derivative $p'(x)$.
//
//    The coefficients (coeffs_) are stored as a vnl_vector, where
//    coeffs_[n] is the coefficient of the x^(d-n) term,
//    where d is the degree of the polynomial.  Otherwise said,
//    the coefficients are stored starting with the highest degree term.
//
//    Roots may be extracted using the roots() method.
class VNL_EXPORT vnl_real_polynomial
{
 public:
  //: Initialize polynomial.
  // The polynomial is $ a[0] x^d + a[1] x^{d-1} + \cdots + a[d] = 0 $.
  vnl_real_polynomial(vnl_vector<double> const & a): coeffs_(a) {
    if (a.empty()) { coeffs_.set_size(1); coeffs_(0)=0.0; }
  }

  //: Initialize polynomial from C vector.
  // The parameter len is the number
  // of coefficients, one greater than the degree.
  vnl_real_polynomial(double const * a, unsigned len): coeffs_(a, len) {
    if (len==0) { coeffs_.set_size(1); coeffs_(0)=0.0; }
  }

  //: Initialize polynomial from double.
  // Useful when adding or multiplying a polynomial and a number.
  vnl_real_polynomial(double a): coeffs_(1u, a) {}

  //: Initialize polynomial of a given degree.
  vnl_real_polynomial(int d): coeffs_(static_cast<unsigned int>(d)+1u) { assert (d>=0); }
  vnl_real_polynomial(unsigned int d): coeffs_(d+1u) { }

  //: comparison operator
  bool operator==(vnl_real_polynomial const& p) const { return p.coefficients() == coeffs_; }

  //: Evaluate polynomial at value x
  double evaluate(double x) const;

  //: Evaluate integral at x (assuming constant of integration is zero)
  double evaluate_integral(double x) const;

  //: Evaluate integral between x1 and x2
  double evaluate_integral(double x1, double x2) const;

  //: Evaluate derivative at value x
  double devaluate(double x) const;

  //: Evaluate polynomial at complex value x
  std::complex<double> evaluate(std::complex<double> const& x) const;


  //: Evaluate derivative at complex value x
  std::complex<double> devaluate(std::complex<double> const& x) const;

  //: Return derivative of this polynomial
  vnl_real_polynomial derivative() const;

  //: Return primitive function (inverse derivative) of this polynomial
  // Since a primitive function is not unique, the one with constant = 0 is returned
  vnl_real_polynomial primitive() const;

  //: Add rhs to this and return *this
  vnl_real_polynomial& operator+=(vnl_real_polynomial const& rhs);

  //: Subtract rhs from this and return *this
  vnl_real_polynomial& operator-=(vnl_real_polynomial const& rhs);

  //: Multiply rhs with this and return *this
  vnl_real_polynomial& operator*=(vnl_real_polynomial const& rhs);

  // Data Access---------------------------------------------------------------

  //: Return the degree (highest power of x) of the polynomial.
  int     degree() const { return int(coeffs_.size()) - 1; }

  //: Access to the polynomial coefficients
  double& operator [] (int i)       { return coeffs_[i]; }
  //: Access to the polynomial coefficients
  double  operator [] (int i) const { return coeffs_[i]; }

  //: Return the vector of coefficients
  const vnl_vector<double>& coefficients() const { return coeffs_; }
  //: Return the vector of coefficients
        vnl_vector<double>& coefficients()       { return coeffs_; }

  void set_coefficients(vnl_vector<double> const& coeffs) {coeffs_ = coeffs;}

  //: Print this polynomial to stream
  void print(std::ostream& os) const;

 protected:
  //: The coefficients of the polynomial.
  // coeffs_.back() is the const term.
  // coeffs_[n] is the coefficient of the x^(d-n) term,
  //    where d=coeffs_.size()-1
  // \invariant coeffs_size() >= 1;
  vnl_vector<double> coeffs_;
};

//: Returns polynomial which is sum of two polynomials f1(x)+f2(x)
// \relatesalso vnl_real_polynomial
VNL_EXPORT vnl_real_polynomial operator+(const vnl_real_polynomial& f1, const vnl_real_polynomial& f2);

//: Returns polynomial which is different of two polynomials f1(x)-f2(x)
// \relatesalso vnl_real_polynomial
VNL_EXPORT vnl_real_polynomial operator-(const vnl_real_polynomial& f1, const vnl_real_polynomial& f2);

//: Returns polynomial which is product of two polynomials f1(x)*f2(x)
VNL_EXPORT vnl_real_polynomial operator*(const vnl_real_polynomial& f1, const vnl_real_polynomial& f2);

//: Returns RMS difference between f1 and f2 over range [x1,x2]
// $\frac1{\sqrt{|x_2-x_1|}}\,\sqrt{\int_{x_1}^{x_2}\left(f_1(x)-f_2(x)\right)^2\,dx}$
// \relatesalso vnl_real_polynomial
VNL_EXPORT double vnl_rms_difference(const vnl_real_polynomial& f1, const vnl_real_polynomial& f2,
                          double x1, double x2);

#endif // vnl_real_polynomial_h_
