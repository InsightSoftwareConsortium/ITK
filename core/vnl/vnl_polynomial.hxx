// This is core/vnl/vnl_polynomial.hxx
#ifndef vnl_polynomial_hxx_
#define vnl_polynomial_hxx_

#include <iostream>
#include "vnl_polynomial.h"
//:
// \file
// \brief Evaluation of polynomials - the implementation
// Templated class (on the data type of the coefficients),
// further very similar to the vnl_real_polynomial class.
// \author Peter Vanroose, ABIS Leuven.
// \date  August 2011

#include <vcl_compiler.h>
#include <vcl_cassert.h>

//: Evaluate polynomial at value x
template <class T>
T vnl_polynomial<T>::evaluate(T const& x) const
{
  typename std::vector<T>::const_iterator i = coeffs_.begin();
  if (i == coeffs_.end()) return T(0);
  T acc = *i;
  T xi = x; // will be x^i
  for (++i; i!=coeffs_.end(); ++i) {
    acc += *i * xi;
    xi *= x;
  }
  return acc;
}

//: Returns negative of this polynomial
template <class T>
vnl_polynomial<T> vnl_polynomial<T>::operator-() const
{
  std::vector<T> neg = coeffs_;
  typename std::vector<T>::iterator i = neg.begin();
  for (; i!=neg.end(); ++i) *i = - *i;
  return vnl_polynomial<T>(neg);
}

//: Returns polynomial which is sum of this with polynomial f
template <class T>
vnl_polynomial<T> vnl_polynomial<T>::operator+(vnl_polynomial<T> const& f) const
{
  // Degree of result is (at most) the maximum of the two input degrees:
  int d=degree(), d2=f.degree(); // any or both of these might be -1 !
  std::vector<T> sum = coeffs_;
  for (int i=0;i<=d&&i<=d2;++i) sum[i]+=f[i];
  for (int i=d+1;i<=d2;++i) sum.push_back(f[i]);
  // normalise the result, viz. such that the highest order coefficient is zero:
  while (sum.end() != sum.begin() && sum.back() == T(0)) sum.pop_back();
  return vnl_polynomial<T>(sum);
}

//: Returns polynomial which is product of this with polynomial f
template <class T>
vnl_polynomial<T> vnl_polynomial<T>::operator*(vnl_polynomial<T> const& f) const
{
  int d1=degree(), d2=f.degree(), d = d1+d2;
  if (d1<0 || d2<0) return vnl_polynomial<T>(); // one of the factors is zero
  std::vector<T> prod(d+1, T(0));
  for (int i=0;i<=d1;++i)
    for (int j=0;j<=d2;++j)
      prod[i+j] += coeffs_[i]*f[j];
  return vnl_polynomial<T>(prod);
}

//: Returns polynomial which is the result of a long division by f
// Beware that this operation might not make sense for integral types T
// if the highest order coefficient of f is not 1 or -1!
template <class T>
vnl_polynomial<T> vnl_polynomial<T>::operator/(vnl_polynomial<T> const& f) const
{
  int d1=degree(), d2=f.degree(), d=d1-d2; // d will be the degree of the quotient
  assert (d2 >= 0 && f[d2] != T(0)); // denominator should not be zero
  if (d<0) return vnl_polynomial<T>(); // nominator is zero, or denominator has higher degree than nominator
  std::vector<T> quot;
  for (int i=0;i<=d;++i) {
    T acc = coeffs_[d1-i];
    for (int j=0;j<d2&&j<i;++j) acc -= quot[j] * f[d2-j-1];
    quot.insert(quot.begin(), 1, acc/f[d2]);
  }
  return vnl_polynomial<T>(quot);
}

//: Returns polynomial which is the remainder after long division by f
// Beware that this operation might not make sense for integral types T
// if the highest order coefficient of f is not 1 or -1!
template <class T>
vnl_polynomial<T> vnl_polynomial<T>::operator%(vnl_polynomial<T> const& f) const
{
  vnl_polynomial<T> quot = operator/(f);
  if (quot.degree() < 0) return *this;
  vnl_polynomial<T> prod = f * quot;
  int n=f.degree(); // size of the result, i.e., one more than degree of the result
  std::vector<T> diff;
  for (int i=0; i<n; ++i) diff.push_back(coeffs_[i] - prod[i]);
  // normalise the result, viz. such that the highest order coefficient is zero:
  while (diff.end() != diff.begin() && diff.back() == T(0)) diff.pop_back();
  return vnl_polynomial<T>(diff);
}

//: Return derivative of this polynomial
template <class T>
vnl_polynomial<T> vnl_polynomial<T>::derivative() const
{
  std::vector<T> cd; // will be one shorter than coeffs_
  typename std::vector<T>::const_iterator i = coeffs_.begin();
  T n = T(1);
  for (++i; i!=coeffs_.end(); ++i,++n)
    cd.push_back(*i * n);
  return vnl_polynomial<T>(cd);
}

//: Return primitive function (inverse derivative) of this polynomial
// Since a primitive function is not unique, the one with constant = 0 is returned
// Beware that this operation might not make sense for integral types T!
template <class T>
vnl_polynomial<T> vnl_polynomial<T>::primitive() const
{
  std::vector<T> cd; // will be one longer than coeffs_
  T n = T(0);
  cd.push_back(n);
  typename std::vector<T>::const_iterator i = coeffs_.begin();
  for (++n; i!=coeffs_.end(); ++i,++n)
    cd.push_back(*i / n);
  return vnl_polynomial<T>(cd);
}

template <class T>
void vnl_polynomial<T>::print(std::ostream& os) const
{
  bool first_coeff = true;

  for (int i=degree(); i >= 0; --i) {
    if (coeffs_[i] == T(0)) continue;
    os << ' ';
    if (coeffs_[i] > T(0) && !first_coeff) os << '+';
    if (i==0)                     os << coeffs_[i];
    else if (coeffs_[i] == -T(1)) os << '-';
    else if (coeffs_[i] != T(1))  os << coeffs_[i] << ' ';
    if (i == 1)                   os << 'X';
    else if (i != 0)              os << "X^" << i;
    first_coeff = false;
  }
  if (first_coeff) os << " 0";
}

#undef VNL_POLYNOMIAL_INSTANTIATE
#define VNL_POLYNOMIAL_INSTANTIATE(T) \
template class VNL_EXPORT vnl_polynomial<T >; \
template VNL_EXPORT std::ostream& operator<<(std::ostream& os, vnl_polynomial<T > const&)

#endif // vnl_polynomial_hxx_
