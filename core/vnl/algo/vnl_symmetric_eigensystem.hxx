// This is core/vnl/algo/vnl_symmetric_eigensystem.hxx
#ifndef vnl_symmetric_eigensystem_hxx_
#define vnl_symmetric_eigensystem_hxx_
//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date Created: 29 Aug 96
// \endverbatim
//
//-----------------------------------------------------------------------------

#include <algorithm>
#include <cmath>
#include <iostream>
#include "vnl_symmetric_eigensystem.h"
#include <vcl_cassert.h>
#include <vcl_compiler.h>
#include <vnl/vnl_copy.h>
#include <vnl/vnl_math.h>
#include <vnl/algo/vnl_netlib.h> // rs_()

//: Find eigenvalues of a symmetric 3x3 matrix
// \verbatim
// Matrix is   M11  M12  M13
//             M12  M22  M23
//             M13  M23  M33
// \endverbatim
template <class T>
void vnl_symmetric_eigensystem_compute_eigenvals(
  T M11, T M12, T M13,
         T M22, T M23,
                T M33,
  T &l1, T &l2, T &l3)
{
  // Characteristic eqtn |M - xI| = 0
  // x^3 + b x^2 + c x + d = 0
  const T b = -M11-M22-M33;
  const T c =  M11*M22 +M11*M33 +M22*M33  -M12*M12 -M13*M13 -M23*M23;
  const T d = M11*M23*M23 +M12*M12*M33 +M13*M13*M22 -2*M12*M13*M23 -M11*M22*M33;

  // Using a numerically tweaked version of the real cubic solver http://www.1728.com/cubic2.htm
  const T b_3 = b/3;
  const T f = b_3*b_3 -  c/3 ;
  const T g = b*c/6 - b_3*b_3*b_3 - d/2;

  if (f == 0 && g == 0)
  {
    l1 = l2 = l3 = - b_3 ;
    return;
  }

  const T f3 = f*f*f;
  const T g2 = g*g;
  const T sqrt_f = -std::sqrt(f);

  // deal explicitly with repeated root and treat
  // complex conjugate roots as numerically inaccurate repeated roots.

  // first check we are not too numerically inaccurate
  assert((g2 - f3) / vnl_math::sqr(vnl_math::cube(b)) < 1e-8);

  if (g2 >= f3)
  {
    if (g < 0)
    {
      l1 = 2 * sqrt_f  - b_3;
      l2 = l3 = - sqrt_f - b_3;
    }
    else
    {
      l1 = l2 = sqrt_f  - b_3;
      l3 = -2 * sqrt_f - b_3;
    }
    return;
  }


  const T sqrt_f3 = sqrt_f * sqrt_f * sqrt_f;
  const T k = std::acos(g / sqrt_f3) / 3;
  const T j = 2 * sqrt_f;
  l1 = j * std::cos(k) - b_3;
  l2 = j * std::cos(k + T(vnl_math::twopi / 3.0)) - b_3;
  l3 = j * std::cos(k - T(vnl_math::twopi / 3.0)) - b_3;

  if (l2 < l1) std::swap(l2, l1);
  if (l3 < l2)
  {
    std::swap(l2, l3);
    if (l2 < l1) std::swap(l2, l1);
  }
}

template <class T>
bool vnl_symmetric_eigensystem_compute(vnl_matrix<T> const & A,
                                       vnl_matrix<T>       & V,
                                       vnl_vector<T>       & D)
{
  A.assert_finite();
  const long n = A.rows();

  // Set the size of the eigenvalue vector D (output) if it does not match the size of A:
  if (D.size() != A.rows())
    D.set_size(n);

  // convert to double
  vnl_matrix<double> Ad(A.rows(), A.cols()); vnl_copy(A, Ad);
  vnl_vector<double> Dd(D.size());
  vnl_vector<double> work1(n);
  vnl_vector<double> work2(n);
  vnl_vector<double> Vvec(n*n);

  long want_eigenvectors = 1;
  long ierr = 0;

  // No need to transpose A, 'cos it's symmetric...
  v3p_netlib_rs_(&n, &n, Ad.data_block(), &Dd[0], &want_eigenvectors, &Vvec[0], &work1[0], &work2[0], &ierr);
  vnl_copy(Dd, D);

  if (ierr) {
    std::cerr << "vnl_symmetric_eigensystem: ierr = " << ierr << '\n';
    return false;
  }

  // Transpose-copy into V, which is first resized if necessary
  if (V.rows() != A.rows() || V.cols() != A.rows())
    V.set_size(n,n);
  double *vptr = &Vvec[0];
  for (int c = 0; c < n; ++c)
    for (int r = 0; r < n; ++r)
      V(r,c) = T(*vptr++);

  return true;
}

//----------------------------------------------------------------------

// - @{ Solve real symmetric eigensystem $A x = \lambda x$ @}
template <class T>
vnl_symmetric_eigensystem<T>::vnl_symmetric_eigensystem(vnl_matrix<T> const& A)
  : n_(A.rows()), V(n_, n_), D(n_)
{
  vnl_vector<T> Dvec(n_);

  vnl_symmetric_eigensystem_compute(A, V, Dvec);

  // Copy Dvec into diagonal of D
  for (int i = 0; i < n_; ++i)
    D(i,i) = Dvec[i];
}

template <class T>
vnl_vector<T> vnl_symmetric_eigensystem<T>::get_eigenvector(int i) const
{
  return vnl_vector<T>(V.extract(n_,1,0,i).data_block(), n_);
}

template <class T>
T vnl_symmetric_eigensystem<T>::get_eigenvalue(int i) const
{
  return D(i, i);
}

template <class T>
vnl_vector<T> vnl_symmetric_eigensystem<T>::solve(vnl_vector<T> const& b)
{
  //vnl_vector<T> ret(b.length());
  //FastOps::AtB(V, b, &ret);
  vnl_vector<T> ret(b*V); // same as V.transpose()*b

  vnl_vector<T> tmp(b.size());
  D.solve(ret, &tmp);

  return V * tmp;
}

template <class T>
T vnl_symmetric_eigensystem<T>::determinant() const
{
  int const n = D.size();
  T det(1);
  for (int i=0; i<n; ++i)
    det *= D[i];
  return det;
}

template <class T>
vnl_matrix<T> vnl_symmetric_eigensystem<T>::pinverse() const
{
  unsigned n = D.rows();
  vnl_diag_matrix<T> invD(n);
  for (unsigned i=0; i<n; ++i)
    if (D(i, i) == 0) {
      std::cerr << __FILE__ ": pinverse(): eigenvalue " << i << " is zero.\n";
      invD(i, i) = 0;
    }
    else
      invD(i, i) = 1 / D(i, i);
  return V * invD * V.transpose();
}

template <class T>
vnl_matrix<T> vnl_symmetric_eigensystem<T>::square_root() const
{
  unsigned n = D.rows();
  vnl_diag_matrix<T> sqrtD(n);
  for (unsigned i=0; i<n; ++i)
    if (D(i, i) < 0) {
      std::cerr << __FILE__ ": square_root(): eigenvalue " << i << " is negative (" << D(i, i) << ").\n";
      sqrtD(i, i) = (T)std::sqrt((typename vnl_numeric_traits<T>::real_t)(-D(i, i)));
                    // gives square root of the absolute value of T.
    }
    else
      sqrtD(i, i) = (T)std::sqrt((typename vnl_numeric_traits<T>::real_t)(D(i, i)));
  return V * sqrtD * V.transpose();
}

template <class T>
vnl_matrix<T> vnl_symmetric_eigensystem<T>::inverse_square_root() const
{
  unsigned n = D.rows();
  vnl_diag_matrix<T> inv_sqrtD(n);
  for (unsigned i=0; i<n; ++i)
    if (D(i, i) <= 0) {
      std::cerr << __FILE__ ": square_root(): eigenvalue " << i << " is non-positive (" << D(i, i) << ").\n";
      inv_sqrtD(i, i) = (T)std::sqrt(-1.0/(typename vnl_numeric_traits<T>::real_t)(D(i, i))); // ??
    }
    else
      inv_sqrtD(i, i) = (T)std::sqrt(1.0/(typename vnl_numeric_traits<T>::real_t)(D(i, i)));
  return V * inv_sqrtD * V.transpose();
}

//--------------------------------------------------------------------------------

#undef VNL_SYMMETRIC_EIGENSYSTEM_INSTANTIATE
#define VNL_SYMMETRIC_EIGENSYSTEM_INSTANTIATE(T) \
template class VNL_ALGO_EXPORT vnl_symmetric_eigensystem<T >; \
template VNL_ALGO_EXPORT void vnl_symmetric_eigensystem_compute_eigenvals(T,T,T,T,T,T,T&,T&,T&); \
template VNL_ALGO_EXPORT bool vnl_symmetric_eigensystem_compute(vnl_matrix<T > const&, vnl_matrix<T > &, vnl_vector<T >&)

#endif // vnl_symmetric_eigensystem_hxx_
