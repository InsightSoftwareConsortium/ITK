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

#include <iostream>
#include <sstream>
#include "vnl_real_npolynomial.h"
#include <vcl_cassert.h>
#include <vcl_compiler.h>

//: Constructor
// \verbatim
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
// \endverbatim

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
// Remove terms with zero coefficient, also at the end of the vector.
void vnl_real_npolynomial::simplify()
{
  for (unsigned int row1=0; row1<nterms_; ++row1)
    for (unsigned int row2=row1+1; row2<nterms_; ++row2) {
      unsigned int col=0;
      while (col<nvar_ && polyn_(row1,col) == polyn_(row2,col)) ++col;
      if (col < nvar_) continue; // not all exponents are identical
      coeffs_(row1) += coeffs_(row2); coeffs_(row2) = 0;
    }
  while (nterms_ > 0 && coeffs_(nterms_-1)==0) --nterms_;
  for (unsigned int row=0; row<nterms_; ++row)
    if (coeffs_(row) == 0) {
      --nterms_; // decrement nterms, and move last element to vacant place:
      coeffs_(row) = coeffs_(nterms_);
      for (unsigned int i=0; i<nvar_; ++i)
        polyn_(row,i) = polyn_(nterms_,i);
    }
  // Now physically remove those rows that became "invisible":
  if (nterms_ < polyn_.rows())
    this->set(coeffs_.extract(nterms_), polyn_.extract(nterms_,nvar_));
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

double vnl_real_npolynomial::deval(const vnl_vector<double>& x, unsigned int var)
{
  return deriv(var).eval(x);
}

vnl_vector<double> vnl_real_npolynomial::deval(const vnl_vector<double>& x)
{
  vnl_vector<double> dx(nvar_);

  for (unsigned int j=0; j<nvar_; j++) {
    dx(j) = deriv(j).eval(x);
  }
  return dx;
}

vnl_real_npolynomial vnl_real_npolynomial::deriv(unsigned int unk)
{
  vnl_vector<double> c(nterms_);
  vnl_matrix<unsigned int> p = polyn_;

  for (unsigned int i = 0; i < nterms_; i++) {
    if (polyn_(i,unk) > 0) {
      p(i,unk) = p(i,unk) - 1;
      c(i) = coeffs_(i)*polyn_(i,unk);
    }
    else {
      c(i) = coeffs_(i);
    }
  }

  return vnl_real_npolynomial(c,p);
}

void vnl_real_npolynomial::set(const vnl_vector<double>& c, const vnl_matrix<unsigned int>& p)
{
  coeffs_= c;
  polyn_ = p;
  nvar_ = p.cols();
  nterms_ = p.rows();
  ideg_ = p.max_value();
}

unsigned int vnl_real_npolynomial::degree() const
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

std::vector<unsigned int> vnl_real_npolynomial::degrees() const
{
  std::vector<unsigned int> d(nvar_);
  for (unsigned int j=0; j<nvar_; ++j)
  {
    d[j]=0;
    for (unsigned int i=0; i<nterms_; ++i)
      if (polyn_(i,j)>d[j]) d[j]=polyn_(i,j);
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

std::ostream& operator<<(std::ostream& os, vnl_real_npolynomial const& P)
{
  return os << P.asString() << std::endl;
}
vnl_real_npolynomial& vnl_real_npolynomial::operator+=(vnl_real_npolynomial const& rhs){
  *this = (*this) + rhs;
  return *this;
}
vnl_real_npolynomial& vnl_real_npolynomial::operator-=(vnl_real_npolynomial const& rhs){
  *this = (*this) - rhs;
  return *this;
}
vnl_real_npolynomial& vnl_real_npolynomial::operator*=(vnl_real_npolynomial const& rhs){
  *this = (*this) * rhs;
  return *this;
}
std::string vnl_real_npolynomial::asString() const
{
  std::ostringstream os;
  if (nvar_ <= 3)
    for (unsigned int i=0; i<nterms_; ++i)
    {
      if (i>0) os << ' ';
      if (i>0 && coeffs_(i) >= 0) os << "+ ";
      double abs_coef = coeffs_(i);
      if (coeffs_(i) < 0) { abs_coef = -abs_coef; os << "- "; }
      if (abs_coef != 1) os << abs_coef << ' ';
      unsigned int totaldeg = 0;
      if (nvar_ > 0 && polyn_(i,0) > 0)  { os << 'X'; totaldeg += polyn_(i,0); }
      if (nvar_ > 0 && polyn_(i,0) > 1)  os << '^' << polyn_(i,0) << ' ';
      if (nvar_ > 1 && polyn_(i,1) > 0)  { os << 'Y'; totaldeg += polyn_(i,1); }
      if (nvar_ > 1 && polyn_(i,1) > 1)  os << '^' << polyn_(i,1) << ' ';
      if (nvar_ > 2 && polyn_(i,2) > 0)  { os << 'Z'; totaldeg += polyn_(i,2); }
      if (nvar_ > 2 && polyn_(i,2) > 1)  os << '^' << polyn_(i,2) << ' ';
      if (totaldeg == 0 && abs_coef == 1) os << abs_coef;
    }
  else
    for (unsigned int i=0; i<nterms_; ++i)
    {
      if (i>0) os << ' ';
      if (i>0 && coeffs_(i) >= 0) os << "+ ";
      double abs_coef = coeffs_(i);
      if (coeffs_(i) < 0) { abs_coef = -abs_coef; os << "- "; }
      if (abs_coef != 1) os << abs_coef << ' ';
      unsigned int totaldeg = 0;
      for (unsigned int j=0; j<nvar_; ++j) {
        if (polyn_(i,j) > 0)  os << 'X' << j;
        if (polyn_(i,j) > 1)  os << '^' << polyn_(i,j) << ' ';
        totaldeg += polyn_(i,j);
      }
      if (totaldeg == 0 && abs_coef == 1) os << abs_coef;
    }
  return os.str();
}
