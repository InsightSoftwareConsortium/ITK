#ifndef vnl_real_npolynomial_h_
#define vnl_real_npolynomial_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME	vnl_real_npolynomial - real polynomial in N variables
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_real_npolynomial.h
// .FILE	vnl_real_npolynomial.cxx
//
// .SECTION Description
//    vnl_real_npolynomial represents a polynomial in multiple variables.
//    Used by vnl_rnpoly_solve which solves systems of polynomial equations.
//    Representation:  an N-omial (N terms) is represented by (1) a vector
//    with the N coefficients (vnl_vector<double>), and (2) a matrix with
//    N rows, the i-th row representing the exponents of term i, as follows:
//    (vnl_matrix<int>) column k contains the (integer) exponent of variable
//    k.  Example: the polynomial A*X^3 + B*X*Y + C*Y^2 + D*X*Y^2 is
//    represented by the coefficients vector [A B C D] and the exponents
//    matrix
//    [3 0]
//    [1 1]
//    [0 2]
//    [1 2].
//
// .SECTION Author
//    Marc Pollefeys, ESAT-VISICS, K.U.Leuven, 12-08-97
//
// .SECTION Modifications:
//    Peter Vanroose 10 Oct 1999 - added simplify();
//                                 determine nterms_ nvar_ ideg_ automatically
//    Peter Vanroose 20 Oct 1999 - Added operator+(), - * and ostream <<
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

//: real polynomial in N variables.

class vnl_real_npolynomial {
  friend class vnl_rnpoly_solve;
public:
// Constructor-----------------------------------------------------------------
  vnl_real_npolynomial() { } // don't use this. only here for the STL vector class.
  vnl_real_npolynomial(const vnl_vector<double>& c, const vnl_matrix<int>& p);

  // Computations--------------------------------------------------------------

  double eval(const vnl_vector<double>& x);
  int degree();
  vnl_real_npolynomial operator-() const; // unary minus
  vnl_real_npolynomial operator+(vnl_real_npolynomial const& ) const;
  vnl_real_npolynomial operator-(vnl_real_npolynomial const& ) const;
  vnl_real_npolynomial operator*(vnl_real_npolynomial const& ) const;
  vnl_real_npolynomial operator+(double ) const;
  vnl_real_npolynomial operator-(double P) const { return operator+(-P); }
  vnl_real_npolynomial operator*(double ) const;
  vnl_real_npolynomial& operator*=(double P) { coeffs_ *= P; return *this; }
  vnl_real_npolynomial operator/(double P) const { return operator*(1.0/P); }
  vnl_real_npolynomial& operator/=(double P) { return operator*=(1.0/P); }
  friend vcl_ostream& operator<<(vcl_ostream& , vnl_real_npolynomial const& );

private:
  void simplify();
  double eval(const vnl_matrix<double>& xn);

  // Data Members--------------------------------------------------------------
  vnl_vector<double> coeffs_; // coefficients
  vnl_matrix<int>    polyn_;  // degrees of every term for every variable
  int                nvar_;   // number of variables = # columns of polyn_
  int                nterms_; // number of terms of polynomial
  int                ideg_;   // max. degree of polynomial
};

#endif // vnl_real_npolynomial_h_
