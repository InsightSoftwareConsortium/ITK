// This is core/vnl/vnl_real_npolynomial.h
#ifndef vnl_real_npolynomial_h_
#define vnl_real_npolynomial_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief contains class for polynomials with N variables
//
// Implements a polynomial with N variables
//
// \author Marc Pollefeys, ESAT-VISICS, K.U.Leuven
// \date   1997-08-12
//
// \verbatim
//  Modifications
//   Peter Vanroose   1999-10-10  added simplify();
//                                determine nterms_ nvar_ ideg_ automatically
//   Peter Vanroose   1999-10-20  Added operator+(), - * and std::ostream <<
//   dac, Manchester  2001-03-15  Tidied up the documentation + added binary_io
//   Lee Worden, Berkeley 2006-06-22 Minor fixes to simplify() and operator<<()
//   Peter Vanroose   2006-06-24  Added method asString implementing oper<<()
//   Peter Vanroose   2006-06-24  Bug fix in degree(), and added degrees() & maxdegree()
//   Marcus Brubaker  2007-10-15  Added deval() and deriv() functions
// \endverbatim


//-----------------------------------------------------------------------------

#include <vector>
#include <string>
#include <iosfwd>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vcl_compiler.h>
#include "vnl/vnl_export.h"

//: real polynomial in N variables.
//    vnl_real_npolynomial represents a polynomial in multiple variables.
//    Used by vnl_rnpoly_solve which solves systems of polynomial equations.
//    Representation:  an N-omial (N terms) is represented by (1) a vector
//    with the N coefficients (vnl_vector<double>), and (2) a matrix with
//    N rows, the i-th row representing the exponents of term i, as follows:
//    (vnl_matrix<int>) column k contains the (integer) exponent of variable
//    k.  Example: the polynomial $A X^3 + B XY + C Y^2 + D XY^2$ is
//    represented by the coefficients vector [A B C D] and the exponents
//    matrix
//  \verbatim
//    [3 0]
//    [1 1]
//    [0 2]
//    [1 2].
//  \endverbatim

class VNL_EXPORT vnl_real_npolynomial
{
 private:
  //: coefficients
  vnl_vector<double>       coeffs_;
  //: degrees of every term for every variable
  vnl_matrix<unsigned int> polyn_;
  //: number of variables = # columns of polyn_
  unsigned int             nvar_;
  //: number of terms of polynomial
  unsigned int             nterms_;
  //: max. degree of polynomial
  unsigned int             ideg_;

  friend class vnl_rnpoly_solve;

 public:

  // Constructor-----------------------------------------------------------------
  vnl_real_npolynomial() : coeffs_(), polyn_(), nvar_(0), nterms_(0), ideg_(0) {} // don't use this: only here for the STL vector class.

  //: Construct the polynomial with coefficients vector c and with exponents matrix p
  vnl_real_npolynomial(const vnl_vector<double>& c, const vnl_matrix<unsigned int>& p);

  // Computations--------------------------------------------------------------

  //: Evaluate the polynomial at x.
  double eval(const vnl_vector<double>& x);
  //: Evaluate the derivative of the polynomial at x with respect to the ith variable.
  double deval(const vnl_vector<double>& x, unsigned int i);
  //: Evaluate the gradient of the polynomial at x.
  vnl_vector<double> deval(const vnl_vector<double>& x);
  //: Differentiate the polynomial with respect to the ith variable.
  vnl_real_npolynomial deriv(unsigned int i);

  vnl_real_npolynomial operator-() const; // unary minus
  vnl_real_npolynomial operator+(vnl_real_npolynomial const& ) const;
  vnl_real_npolynomial operator-(vnl_real_npolynomial const& ) const;
  vnl_real_npolynomial operator*(vnl_real_npolynomial const& ) const;
  vnl_real_npolynomial& operator+=(vnl_real_npolynomial const& rhs);
  vnl_real_npolynomial& operator-=(vnl_real_npolynomial const& rhs);
  vnl_real_npolynomial& operator*=(vnl_real_npolynomial const& rhs);
  vnl_real_npolynomial operator+(double ) const;
  vnl_real_npolynomial operator-(double P) const { return operator+(-P); }
  vnl_real_npolynomial operator*(double ) const;
  vnl_real_npolynomial& operator*=(double P) { coeffs_ *= P; return *this; }
  vnl_real_npolynomial operator/(double P) const { return operator*(1.0/P); }
  vnl_real_npolynomial& operator/=(double P) { return operator*=(1.0/P); }
  friend VNL_EXPORT std::ostream& operator<<(std::ostream& , vnl_real_npolynomial const& );

  // nb also added functions to access the coeffs_ member variable

  //--- Data Access------------------------------------------------------------

  //: Return the degree (highest total power of all terms) of the polynomial.
  unsigned int degree() const;

  //: Return the highest degree of the polynomial in an individual variable.
  unsigned int maxdegree() const { return ideg_; }

  //: Return the degrees (highest power of all terms) in each of the variables.
  std::vector<unsigned int> degrees() const;

  //: Access to the polynomial coefficients
  double& operator [] (unsigned int i)       { return coeffs_[i]; }
  //: Access to the polynomial coefficients
  double  operator [] (unsigned int i) const { return coeffs_[i]; }

  //: Return the vector of coefficients
  const vnl_vector<double>& coefficients() const { return coeffs_; }
  //: Return the vector of coefficients
  vnl_vector<double>& coefficients()       { return coeffs_; }

  //: Set vector of coefficients of each product
  void set(const vnl_vector<double> & c, const vnl_matrix<unsigned int> & p);

  //: Return the polynomial matrix
  // (ie specifying the variables in each product)
  const vnl_matrix<unsigned int>& polyn() const { return polyn_; }

  //: Return the vector of coefficients
  vnl_matrix<unsigned int>& polyn() { return polyn_; }

  //: Return the textual representation of this polynomial
  std::string asString() const;

 private:
  void simplify();
  double eval(const vnl_matrix<double>& xn);
};

VNL_EXPORT std::ostream& operator<<(std::ostream& , vnl_real_npolynomial const& );

#endif // vnl_real_npolynomial_h_
