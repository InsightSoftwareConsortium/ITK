#ifndef vnl_real_polynomial_h_
#define vnl_real_polynomial_h_
#ifdef __GNUC__
#pragma interface
#endif


// This is vxl/vnl/vnl_real_polynomial.h

//:
// \file
// \brief Evaluation of real polynomials
// \author Andrew W. Fitzgibbon, Oxford RRG, 06 Aug 96
//
// \warning 23 may 97, Peter Vanroose - "NO_COMPLEX" option added (until "complex" type is standardised)
//

// Modifications
// 27/03/2001 Ian Scott and Tim Cootes - Added Binary IO
// 27/03/2001 Ian Scott - Comments tidied up

#include <vnl/vnl_vector.h>
#include <vcl_complex.h>

//:Evaluation of real polynomials at real and complex points.
//    vnl_real_polynomial represents a univariate polynomial with real
//    coefficients, stored as a vector of doubles.  This allows
//    evaluation of the polynomial \f$p(x)\f$ at given values of \f$x\f$,
//    or of its derivative \f$p'(x)\f$.
//
//    Roots may be extracted using the roots() method.
class vnl_real_polynomial {
public:
  //: Initialize polynomial.
  // The polynomial is \f$ a[0] x^d + a[1] x^{d-1} + \cdots + a[d] = 0 \f$.
  vnl_real_polynomial(vnl_vector<double> const & a): coeffs_(a) {}

  //: Initialize polynomial from C vector.
  // The parameter len is the number
  // of coefficients, one greater than the degree.
  vnl_real_polynomial(double const * a, int len): coeffs_(a, len) {}

  //: Initialize polynomial of a given degree.
  vnl_real_polynomial(int d): coeffs_(d+1) {}


  //: Evaluate polynomial at value x
  double evaluate(double x) const;

  //: Evaluate derivative at value x
private: // not implemented
  double devaluate(double x) const;
public:

  //: Evaluate polynomial at complex value x
  vcl_complex<double> evaluate(vcl_complex<double> const& x) const;


  //: Evaluate derivative at complex value x
  vcl_complex<double> devaluate(vcl_complex<double> const& x) const;

  // Data Access---------------------------------------------------------------

  //: Return the degree (highest power of x) of the polynomial.
  int     degree() const { return coeffs_.size() - 1; }

  //: Access to the polynomial coefficients
  double& operator [] (int i)       { return coeffs_[i]; }
  //: Access to the polynomial coefficients
  double  operator [] (int i) const { return coeffs_[i]; }

  //: Return the vector of coefficients
  const vnl_vector<double>& coefficients() const { return coeffs_; }
  //: Return the vector of coefficients
        vnl_vector<double>& coefficients()       { return coeffs_; }

  void set_coefficients(const vnl_vector<double> & coeffs) {coeffs_ = coeffs;}



protected:
  //: The coefficients of the polynomial.
  // coeffs_[0] is the const term.
  // coeffs_[n] is the coefficient of the x^n term.
  vnl_vector<double> coeffs_;
};





#endif // vnl_real_polynomial_h_
