// This is core/vnl/algo/vnl_symmetric_eigensystem.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG
// Created: 29 Aug 96
//
//-----------------------------------------------------------------------------

#include "vnl_symmetric_eigensystem.h"
#include <vcl_cassert.h>
#include <vcl_cmath.h> // for sqrt(double)
#include <vcl_iostream.h>
#include <vnl/vnl_copy.h>
#include "vnl_netlib.h" // rs_()

bool vnl_symmetric_eigensystem_compute(vnl_matrix<float> const & A,
                                       vnl_matrix<float>       & V,
                                       vnl_vector<float>       & D)
{
  vnl_matrix<double> Ad(A.rows(), A.cols());
  vnl_matrix<double> Vd(V.rows(), V.cols());
  vnl_vector<double> Dd(D.size());
  vnl_copy(A, Ad);
  bool f = vnl_symmetric_eigensystem_compute(Ad, Vd, Dd);
  vnl_copy(Vd, V);
  vnl_copy(Dd, D);
  return f;
}

bool vnl_symmetric_eigensystem_compute(vnl_matrix<double> const & A,
                                       vnl_matrix<double>       & V,
                                       vnl_vector<double>       & D)
{
  A.assert_finite();

  // The fortran code does not like it if V or D are
  // undersized. I expect they probably should not be
  // oversized either. - IMS
  assert(V.rows() == A.rows());
  assert(V.cols() == A.rows());
  assert(D.size() == A.rows());

  int n = A.rows();
  vnl_vector<double> work1(n);
  vnl_vector<double> work2(n);
  vnl_vector<double> Vvec(n*n);

  int want_eigenvectors = 1;
  int ierr = 0;

  // No need to transpose A, cos it's symmetric...
  vnl_matrix<double> B = A; // since A is read-only and rs_ might change its third argument...
  rs_(&n, &n, B.data_block(), &D[0], &want_eigenvectors, &Vvec[0], &work1[0], &work2[0], &ierr);

  if (ierr) {
    vcl_cerr << "vnl_symmetric_eigensystem: ierr = " << ierr << vcl_endl;
    return false;
  }

  // Transpose-copy into V
  double *vptr = &Vvec[0];
  for (int c = 0; c < n; ++c)
    for (int r = 0; r < n; ++r)
      V(r,c) = *vptr++;

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
  vnl_vector<T> ret(b*V); // same as V.tranpose()*b

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
      vcl_cerr << __FILE__ ": pinverse(): eigenvalue " << i << " is zero.\n";
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
      vcl_cerr << __FILE__ ": square_root(): eigenvalue " << i << " is negative (" << D(i, i) << ").\n";
      sqrtD(i, i) = (T)vcl_sqrt((typename vnl_numeric_traits<T>::real_t)(-D(i, i)));
                    // gives square root of the absolute value of T.
    }
    else
      sqrtD(i, i) = (T)vcl_sqrt((typename vnl_numeric_traits<T>::real_t)(D(i, i)));
  return V * sqrtD * V.transpose();
}

template <class T>
vnl_matrix<T> vnl_symmetric_eigensystem<T>::inverse_square_root() const
{
  unsigned n = D.rows();
  vnl_diag_matrix<T> inv_sqrtD(n);
  for (unsigned i=0; i<n; ++i)
    if (D(i, i) <= 0) {
      vcl_cerr << __FILE__ ": square_root(): eigenvalue " << i << " is non-positive (" << D(i, i) << ").\n";
      inv_sqrtD(i, i) = (T)vcl_sqrt(-1.0/(typename vnl_numeric_traits<T>::real_t)(D(i, i))); // ??
    }
    else
      inv_sqrtD(i, i) = (T)vcl_sqrt(1.0/(typename vnl_numeric_traits<T>::real_t)(D(i, i)));
  return V * inv_sqrtD * V.transpose();
}

//--------------------------------------------------------------------------------

template class vnl_symmetric_eigensystem<float>;
template class vnl_symmetric_eigensystem<double>;
