// This is core/vnl/vnl_real_npolynomial.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
//  \file
//  \brief a degree n real polynomial
//  \author Marc Pollefeys, ESAT-VISICS, K.U.Leuven, 12-08-97
//
//  Implements a polynomial with N variables

#include "vnl_real_npolynomial.h"
#include <vcl_cassert.h>
#include <vcl_cmath.h>    // vcl_fabs()
#include <vcl_iostream.h>

//: Constructor
//<PRE>
// coeffs = vnl_vector<double>(nterms)
// polyn = vnl_matrix<int>(nterms,nvar)
// Example: A*x^3 + B*x*y + C*y^2 + D*x*y^2
// nvar = 2;
// nterms = 4;
// coeffs = [A B C D]';
// polyn = [3 0]
//         [1 1]
//         [0 2]
//         [1 2];
//</PRE>

vnl_real_npolynomial::vnl_real_npolynomial(const vnl_vector<double>& c, const vnl_matrix<unsigned int>& p)
  : coeffs_(c)
  , polyn_(p)
  , nvar_(p.cols())
  , nterms_(p.rows())
  , ideg_(p.max_value())
{
  assert(c.size() == p.rows());
  simplify();
}

//: Combine terms with identical exponents (i.e., identical rows in polyn_).
// Remove terms with zero coefficient.
void vnl_real_npolynomial::simplify()
{
  for (unsigned int row1=0; row1<nterms_; ++row1)
    for (unsigned int row2=row1+1; row2<nterms_; ++row2) {
      unsigned int col=0;
      while (col<nvar_ && polyn_(row1,col) == polyn_(row2,col)) ++col;
      if (col < nvar_) continue; // not all exponents are identical
      coeffs_(row1) += coeffs_(row2); coeffs_(row2) = 0;
    }
  for (unsigned int row=0; row<nterms_; ++row)
    if (coeffs_(row) == 0) {
      --nterms_; // decrement nterms, and move last element to vacant place:
      coeffs_(row) = coeffs_(nterms_);
      coeffs_(nterms_) = 0; // not really necessary; to keep coeffs_ consistent
      for (unsigned int i=0; i<nvar_; ++i)
        polyn_(row,i) = polyn_(nterms_,i);
    }
}

double vnl_real_npolynomial::eval(const vnl_matrix<double>& xn)
{
  double s=0;
  for (unsigned int i=0; i<nterms_; i++){
    double t=coeffs_(i);
    for (unsigned int j=0; j<nvar_; j++)
      t*=xn(j,polyn_(i,j));
    s+=t;
  }
  return s;
}

double vnl_real_npolynomial::eval(const vnl_vector<double>& x)
{
  vnl_matrix<double> xn(nvar_,ideg_+1);

  for (unsigned int j=0; j<nvar_; j++){
    xn(j,0)=1;
    for (unsigned int i=1; i<ideg_+1; i++)
      xn(j,i)=xn(j,i-1)*x(j);
  }
  return eval(xn);
}


//: Set the coefficients and degree of variable
void vnl_real_npolynomial::set(const vnl_vector<double>& c, const vnl_matrix<unsigned int>& p)
{
  coeffs_= c;
  polyn_ = p;
  nvar_ = p.cols();
  nterms_ = p.rows();
  ideg_ = p.max_value();
}


unsigned int vnl_real_npolynomial::degree()
{
  unsigned int d=0;
  for (unsigned int i=0; i<nterms_; i++)
  {
    unsigned int dt=0;
    for (unsigned int j=0; j<nvar_; j++)
      dt+=polyn_(i,j);
    if (dt>d) d=dt;
  }
  return d;
}

vnl_real_npolynomial vnl_real_npolynomial::operator-() const
{
  vnl_vector<double> coef(nterms_);
  for (unsigned int i=0; i<nterms_; ++i) coef(i) = - coeffs_(i);

  vnl_matrix<unsigned int> poly = polyn_;

  return vnl_real_npolynomial(coef, poly);
}

vnl_real_npolynomial vnl_real_npolynomial::operator+(vnl_real_npolynomial const& P) const
{
  assert(nvar_ == P.nvar_); // both polynomials must have the same variables

  vnl_vector<double> coef(nterms_+P.nterms_);
  unsigned int i = 0; for (; i<nterms_; ++i) coef(i) = coeffs_(i);
  for (unsigned int j=0; j<P.nterms_; ++i,++j) coef(i) = P.coeffs_(j);

  vnl_matrix<unsigned int> poly(nterms_+P.nterms_,nvar_);
  for (i=0; i<nterms_; ++i)
    for (unsigned int k=0; k<nvar_; ++k)
      poly(i,k) = polyn_(i,k);
  for (unsigned int j=0; j<P.nterms_; ++i,++j)
    for (unsigned int k=0; k<nvar_; ++k)
      poly(i,k) = P.polyn_(j,k);

  return vnl_real_npolynomial(coef, poly);
}

vnl_real_npolynomial vnl_real_npolynomial::operator+(double P) const
{
  vnl_vector<double> coef(nterms_+1);
  for (unsigned int i=0; i<nterms_; ++i)
    coef(i) = coeffs_(i);
  coef(nterms_) = P;

  vnl_matrix<unsigned int> poly(nterms_+1,nvar_);
  for (unsigned int i=0; i<nterms_; ++i)
    for (unsigned int k=0; k<nvar_; ++k)
      poly(i,k) = polyn_(i,k);
  for (unsigned int k=0; k<nvar_; ++k)
    poly(nterms_,k) = 0;

  return vnl_real_npolynomial(coef, poly);
}

vnl_real_npolynomial vnl_real_npolynomial::operator-(vnl_real_npolynomial const& P) const
{
  assert(nvar_ == P.nvar_); // both polynomials must have the same variables

  vnl_vector<double> coef(nterms_+P.nterms_);
  unsigned int i = 0; for (; i<nterms_; ++i) coef(i) = coeffs_(i);
  for (unsigned int j=0; j<P.nterms_; ++i,++j) coef(i) = - P.coeffs_(j);

  vnl_matrix<unsigned int> poly(nterms_+P.nterms_,nvar_);
  for (i=0; i<nterms_; ++i)
    for (unsigned int k=0; k<nvar_; ++k)
      poly(i,k) = polyn_(i,k);
  for (unsigned int j=0; j<P.nterms_; ++i,++j)
    for (unsigned int k=0; k<nvar_; ++k)
      poly(i,k) = P.polyn_(j,k);

  return vnl_real_npolynomial(coef, poly);
}

vnl_real_npolynomial vnl_real_npolynomial::operator*(vnl_real_npolynomial const& P) const
{
  assert(nvar_ == P.nvar_); // both polynomials must have the same variables

  vnl_vector<double> coef(nterms_*P.nterms_);
  unsigned int k = 0;
  for (unsigned int i=0; i<nterms_; ++i)
    for (unsigned int j=0; j<P.nterms_; ++j,++k)
      coef(k) = coeffs_(i) * P.coeffs_(j);

  vnl_matrix<unsigned int> poly(nterms_*P.nterms_,nvar_);
  k = 0;
  for (unsigned int i=0; i<nterms_; ++i)
    for (unsigned int j=0; j<P.nterms_; ++j,++k)
      for (unsigned int l=0; l<nvar_; ++l)
        poly(k,l) = polyn_(i,l) + P.polyn_(j,l);

  return vnl_real_npolynomial(coef, poly);
}

vnl_real_npolynomial vnl_real_npolynomial::operator*(double P) const
{
  vnl_vector<double> coef(nterms_);
  for (unsigned int i=0; i<nterms_; ++i)
    coef(i) = coeffs_(i) * P;

  vnl_matrix<unsigned int> poly = polyn_;

  return vnl_real_npolynomial(coef, poly);
}

vcl_ostream& operator<<(vcl_ostream& os, vnl_real_npolynomial const& P)
{
  if (P.nvar_ <= 3)
    for (unsigned int i=0; i<P.nterms_; ++i)
    {
      os << ' ';
      if (i>0 && P.coeffs_(i) > 0) os << '+';
      if (vcl_fabs(P.coeffs_(i)) != 1) os << P.coeffs_(i) << ' ';
      unsigned int totaldeg = 0;
      if (P.nvar_ > 0 && P.polyn_(i,0) > 0)  { os << 'X'; totaldeg += P.polyn_(i,0); }
      if (P.nvar_ > 0 && P.polyn_(i,0) > 1)  os << '^' << P.polyn_(i,0);
      if (P.nvar_ > 1 && P.polyn_(i,1) > 0)  { os << 'Y'; totaldeg += P.polyn_(i,1); }
      if (P.nvar_ > 1 && P.polyn_(i,1) > 1)  os << '^' << P.polyn_(i,1);
      if (P.nvar_ > 2 && P.polyn_(i,2) > 0)  { os << 'Z'; totaldeg += P.polyn_(i,2); }
      if (P.nvar_ > 2 && P.polyn_(i,2) > 1)  os << '^' << P.polyn_(i,2);
      if (totaldeg == 0 && vcl_fabs(P.coeffs_(i)) == 1) os << P.coeffs_(i);
    }
  else
    for (unsigned int i=0; i<P.nterms_; ++i)
    {
      os << ' ';
      if (i>0 && P.coeffs_(i) > 0) os << '+';
      if (vcl_fabs(P.coeffs_(i)) != 1) os << P.coeffs_(i) << ' ';
      unsigned int totaldeg = 0;
      for (unsigned int j=0; j<P.nvar_; ++j) {
        if (P.polyn_(i,j) > 0)  os << 'X' << j;
        if (P.polyn_(i,j) > 1)  os << '^' << P.polyn_(i,j);
        totaldeg += P.polyn_(i,j);
      }
      if (totaldeg == 0 && vcl_fabs(P.coeffs_(i)) == 1) os << P.coeffs_(i);
    }
  os << vcl_endl; return os;
}
