// This is core/vnl/vnl_real_polynomial.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \brief Evaluation of real polynomials - the implementation
// \author Andrew W. Fitzgibbon, Oxford RRG 23 Aug 96
//
// Modifications
// IMS (Manchester) 14/03/2001: Added Manchester IO scheme

#include <iostream>
#include <complex>
#include <cmath>
#include "vnl_real_polynomial.h"
#include <vcl_compiler.h>

// This is replacing a member template...
template <class T>
T vnl_real_polynomial_evaluate(double const *a, int n, T const& x)
{
  --n;
  T acc = a[n];
  T xn = x;
  while (n) {
    acc += a[--n] * xn;
    xn *= x;
  } ;

  return acc;
}

// The following code confuses doxygen, causing it to link every
// mention of double to vnl_real_polynomial::evaluate
#ifndef DOXYGEN_SHOULD_SKIP_THIS
# ifdef VCL_WIN32
#  define SELECT(T) <T >
# else
#  define SELECT(T)
# endif

//: Instantiate templates before use
template double vnl_real_polynomial_evaluate SELECT(double )
      (double const*,int,double const&);
template std::complex<double> vnl_real_polynomial_evaluate SELECT(std::complex<double>)
      (double const*,int,std::complex<double> const&);

//: Evaluate polynomial at value x
double vnl_real_polynomial::evaluate(double x) const
{
  return vnl_real_polynomial_evaluate SELECT(double)(coeffs_.data_block(), coeffs_.size(), x);
}


//: Evaluate polynomial at complex value x
std::complex<double> vnl_real_polynomial::evaluate(std::complex<double> const& x) const
{
  return vnl_real_polynomial_evaluate SELECT(std::complex<double>)
     (coeffs_.data_block(), coeffs_.size(), x);
}
#endif // DOXYGEN_SHOULD_SKIP_THIS

//: Evaluate derivative at value x.
double vnl_real_polynomial::devaluate(double x) const
{
  return derivative().evaluate(x);
}


//: Evaluate derivative at complex value x. Not implemented.
std::complex<double> vnl_real_polynomial::devaluate(std::complex<double> const& x) const
{
  return derivative().evaluate(x);
}

//: Evaluate integral at x (assuming constant of integration is zero)
double vnl_real_polynomial::evaluate_integral(double x) const
{
  int d = coeffs_.size()-1;
  const double* f = coeffs_.data_block();
  double sum = 0.0;
  int di=1;
  double xi=x;
  for (int i=d;i>=0;--i)
  {
    sum += f[i]*xi/di;
    xi*=x;
    di++;
  }

  return sum;
}

//: Evaluate integral between x1 and x2
double vnl_real_polynomial::evaluate_integral(double x1, double x2) const
{
  return evaluate_integral(x2)-evaluate_integral(x1);
}

//: Returns sum of two polynomials f1(x)+f2(x)
vnl_real_polynomial operator+(const vnl_real_polynomial& f1, const vnl_real_polynomial& f2)
{
  // Degree of result is highest of the two inputs
  const unsigned int d1=f1.degree();
  const unsigned int d2=f2.degree();
  unsigned int d = d1;
  if (d2>d) d=d2;

  vnl_real_polynomial sum(d);

  // Coefficients are stored such that f(i) is coef. on x^(d-i)
  for (unsigned int i=0; i<=d; ++i)
    {
    sum[d-i]=0.0;
    if (i<=d1) sum[d-i]+=f1[d1-i];
    if (i<=d2) sum[d-i]+=f2[d2-i];
    }

  return sum;
}

//: Returns sum of two polynomials f1(x)-f2(x)
vnl_real_polynomial operator-(const vnl_real_polynomial& f1, const vnl_real_polynomial& f2)
{
  // Degree of result is highest of the two inputs
  const unsigned int d1=f1.degree();
  const unsigned int d2=f2.degree();
  unsigned int d = d1;
  if (d2>d) d=d2;

  vnl_real_polynomial diff(d);

  // Coefficients are stored such that f(i) is coef. on x^(d-i)
  for (unsigned int i=0; i<=d; ++i)
    {
    diff[d-i]=0.0;
    if (i<=d1) diff[d-i]+=f1[d1-i];
    if (i<=d2) diff[d-i]-=f2[d2-i];
    }

  return diff;
}

//: Returns polynomial which is product of two polynomials f1(x)*f2(x)
vnl_real_polynomial operator*(const vnl_real_polynomial& f1, const vnl_real_polynomial& f2)
{
  const unsigned int d1=f1.degree();
  const unsigned int d2=f2.degree();
  const unsigned int d = d1+d2;

  vnl_real_polynomial prod(d);
  prod.coefficients().fill(0.0);

  for (unsigned int i=0; i<=d1; ++i)
    for (unsigned int j=0; j<=d2; ++j)
      prod[d-(i+j)] += f1[d1-i]*f2[d2-j];

  return prod;
}
//: Add rhs to this and return *this
vnl_real_polynomial& vnl_real_polynomial::operator+=(vnl_real_polynomial const& rhs){
  *this = (*this) + rhs;
  return *this;
}

//: Subtract rhs from this and return *this
vnl_real_polynomial& vnl_real_polynomial::operator-=(vnl_real_polynomial const& rhs){
  *this = (*this) - rhs;
  return *this;
}

//: multiply rhs with this and return *this
vnl_real_polynomial& vnl_real_polynomial::operator*=(vnl_real_polynomial const& rhs){
  *this = (*this) * rhs;
  return *this;
}

//: Returns RMS difference between f1 and f2 over range [x1,x2]
// $\frac1{\sqrt{|x_2-x_1|}}\,\sqrt{\int_{x_1}^{x_2}\left(f_1(x)-f_2(x)\right)^2\,dx}$
double vnl_rms_difference(const vnl_real_polynomial& f1, const vnl_real_polynomial& f2,
                          double x1, double x2)
{
  double dx = std::fabs(x2-x1);
  if (dx==0.0) return 0;

  vnl_real_polynomial df = f2-f1;
  vnl_real_polynomial df2 = df*df;
  double area = std::fabs(df2.evaluate_integral(x1,x2));
  return std::sqrt(area/dx);
}

//: Return derivative of this polynomial
vnl_real_polynomial vnl_real_polynomial::derivative() const
{
  int d = degree();
  vnl_vector<double> cd (d);
  for (int i=d-1,di=1; i>=0; --i,++di)
    cd[i] = coeffs_[i] * di;
  return vnl_real_polynomial(cd);
}

//: Return primitive function (inverse derivative) of this polynomial
// Since a primitive function is not unique, the one with constant = 0 is returned
vnl_real_polynomial vnl_real_polynomial::primitive() const
{
  int d = coeffs_.size(); // degree+1
  vnl_vector<double> cd (d+1);
  cd[d] = 0.0; // constant term
  for (int i=d-1,di=1; i>=0; --i,++di)
    cd[i] = coeffs_[i] / di;
  return vnl_real_polynomial(cd);
}

void vnl_real_polynomial::print(std::ostream& os) const
{
  int d = degree();
  bool first_coeff = true; // to avoid '+' in front of equation

  for (int i = 0; i <= d; ++i) {
    if (coeffs_[i] == 0.0) continue;
    os << ' ';
    if (coeffs_[i] > 0.0 && !first_coeff) os << '+';
    if (i==d)                           os << coeffs_[i]; // the 0-degree coeff should always be output if not zero
    else if (coeffs_[i] == -1.0)        os << '-';
    else if (coeffs_[i] != 1.0)         os << coeffs_[i] << ' ';

    if (i < d-1)                        os << "X^" << d-i;
    else if (i == d-1)                  os << 'X';
    first_coeff = false;
  }
  if (first_coeff) os << " 0";
}
