// This is core/vnl/vnl_complex_ops.hxx
#ifndef vnl_complex_ops_hxx_
#define vnl_complex_ops_hxx_
//:
// \file
// \author fsm
// This is the implementation file for the following three header files:
// vnl_complexify.h vnl_real.h vnl_imag.h

#include "vnl_complexify.h"
#include "vnl_real.h"
#include "vnl_imag.h"

#include <vcl_cassert.h>

//-----------------------------------------------------------------------

template <class T>
void
vnl_complexify(T const *src, vcl_complex<T> *dst, unsigned n)
{
  for (unsigned i=0; i<n; ++i)
    dst[i] = src[i];
}

template <class T>
void
vnl_complexify(T const *re, T const *im, vcl_complex<T> *dst, unsigned n)
{
  for (unsigned i=0; i<n; ++i)
    dst[i] = vcl_complex<T>(re[i], im[i]);
}

// Real Alone:
// - vnl_vector
// - vnl_vector_fixed -- in header
// - vnl_matrix
// - vnl_matrix_fixed -- in header
// - vnl_diag_matrix
// - vnl_diag_matrix_fixed -- in header
// - vnl_sym_matrix

template <class T>
vnl_vector<vcl_complex<T> >
  vnl_complexify(vnl_vector<T> const &R)
{
  vnl_vector<vcl_complex<T> > C(R.size());
  vnl_complexify(R.begin(), C.begin(), R.size());
  return C;
}

template <class T>
vnl_matrix<vcl_complex<T> >
  vnl_complexify(vnl_matrix<T> const &R)
{
  vnl_matrix<vcl_complex<T> > C(R.rows(), R.cols());
  vnl_complexify(R.begin(), C.begin(), R.size());
  return C;
}

template <class T>
vnl_diag_matrix<vcl_complex<T> >
  vnl_complexify(vnl_diag_matrix<T> const& R)
{
  vnl_diag_matrix<vcl_complex<T> > C(R.rows(), R.cols());
  vnl_complexify(R.begin(), C.begin(), R.size());
  return C;
}

template <class T>
vnl_sym_matrix<vcl_complex<T> >
  vnl_complexify(vnl_sym_matrix<T> const& R)
{
  vnl_sym_matrix<vcl_complex<T> > C(R.size());
  vnl_complexify(R.begin(), C.begin(), R.size());
  return C;
}

//----------------------------------------------------------------------

// Real + Imaginary:
// - vnl_vector
// - vnl_vector_fixed -- in header
// - vnl_matrix
// - vnl_matrix_fixed -- in header
// - vnl_diag_matrix
// - vnl_diag_matrix_fixed -- in header
// - vnl_sym_matrix

template <class T>
vnl_vector<vcl_complex<T> >
  vnl_complexify(vnl_vector<T> const &R, vnl_vector<T> const &I)
{
  assert(R.size() == I.size());
  vnl_vector<vcl_complex<T> > C(R.size());
  vnl_complexify(R.begin(), I.begin(), C.begin(), R.size());
  return C;
}

template <class T>
vnl_matrix<vcl_complex<T> >
  vnl_complexify(vnl_matrix<T> const &R, vnl_matrix<T> const &I)
{
  assert(R.rows() == I.rows());
  assert(R.cols() == I.cols());
  vnl_matrix<vcl_complex<T> > C(R.rows(), R.cols());
  vnl_complexify(R.begin(), I.begin(), C.begin(), R.size());
  return C;
}

template <class T>
vnl_diag_matrix<vcl_complex<T> >
  vnl_complexify(vnl_diag_matrix<T> const &R, vnl_diag_matrix<T> const &I)
{
  assert(R.rows() == I.rows());
  vnl_diag_matrix<vcl_complex<T> > C(R.rows());
  vnl_complexify(R.begin(), I.begin(), C.begin(), R.size());
  return C;
}

template <class T>
vnl_sym_matrix<vcl_complex<T> >
  vnl_complexify(vnl_sym_matrix<T> const &R, vnl_sym_matrix<T> const &I)
{
  assert(R.rows() == I.rows());
  vnl_sym_matrix<vcl_complex<T> > C(R.rows());
  vnl_complexify(R.begin(), I.begin(), C.begin(), R.size());
  return C;
}

//----------------------------------------------------------------------
// vnl_real()
// - vnl_vector
// - vnl_vector_fixed -- in header
// - vnl_matrix
// - vnl_matrix_fixed -- in header
// - vnl_diag_matrix
// - vnl_diag_matrix_fixed -- in header
// - vnl_sym_matrix

//: Return array of real parts of complex array.
template <class T>
void
  vnl_real(vcl_complex<T> const* C, T* R, unsigned int n)
{
  for (unsigned int i=0; i<n; ++i)
    R[i] = vcl_real(C[i]);
}

//: Vector of real parts of vnl_vector<vcl_complex<T> >.
template <class T>
vnl_vector<T>
  vnl_real(vnl_vector<vcl_complex<T> > const & C)
{
  vnl_vector<T> R(C.size());
  typename vnl_vector<vcl_complex<T> >::const_iterator cIt = C.begin();
  typename vnl_vector<T>::iterator rIt = R.begin();
  for (; cIt != C.end(); ++cIt, ++rIt)
    *rIt = vcl_real(*cIt);
  return R;
}

//: Matrix of real parts of vnl_matrix<vcl_complex<T> >.
template <class T>
vnl_matrix<T>
  vnl_real(vnl_matrix<vcl_complex<T> > const& C)
{
  vnl_matrix<T> R(C.rows(), C.columns());
  typename vnl_matrix<vcl_complex<T> >::const_iterator cIt = C.begin();
  typename vnl_matrix<T>::iterator rIt = R.begin();
  for (; cIt != C.end(); ++cIt, ++rIt)
    *rIt = vcl_real(*cIt);
  return R;
}

//: Matrix of real parts of vnl_diag_matrix<vcl_complex<T> >.
template <class T>
vnl_diag_matrix<T>
  vnl_real(vnl_diag_matrix<vcl_complex<T> > const& C)
{
  vnl_diag_matrix<T> R(C.rows());
  typename vnl_diag_matrix<vcl_complex<T> >::const_iterator cIt = C.begin();
  typename vnl_diag_matrix<T>::iterator rIt = R.begin();
  for (; cIt != C.end(); ++cIt, ++rIt)
    *rIt = vcl_real(*cIt);
  return R;
}

//: Matrix of real parts of vnl_sym_matrix<vcl_complex<T> >.
template <class T>
vnl_sym_matrix<T>
  vnl_real(vnl_sym_matrix<vcl_complex<T> > const& C)
{
  vnl_sym_matrix<T> R(C.rows());
  typename vnl_sym_matrix<vcl_complex<T> >::const_iterator cIt = C.begin();
  typename vnl_sym_matrix<T>::iterator rIt = R.begin();
  for (; cIt != C.end(); ++cIt, ++rIt)
    *rIt = vcl_real(*cIt);
  return R;
}

//----------------------------------------------------------------------
// vnl_imag()
// - vnl_vector
// - vnl_vector_fixed -- in header
// - vnl_matrix
// - vnl_matrix_fixed -- in header
// - vnl_diag_matrix
// - vnl_diag_matrix_fixed -- in header
// - vnl_sym_matrix

//: Return array of imaginary parts of complex array.
template <class T>
void
  vnl_imag(vcl_complex<T> const* C, T* I, unsigned int n)
{
  for (unsigned int i=0; i<n; ++i)
    I[i] = vcl_imag(C[i]);
}

//: Vector of imaginary parts of vnl_vector<vcl_complex<T> >.
template <class T>
vnl_vector<T>
  vnl_imag(vnl_vector<vcl_complex<T> > const & C)
{
  vnl_vector<T> R(C.size());
  typename vnl_vector<vcl_complex<T> >::const_iterator cIt = C.begin();
  typename vnl_vector<T>::iterator rIt = R.begin();
  for (; cIt != C.end(); ++cIt, ++rIt)
    *rIt = vcl_imag(*cIt);
  return R;
}

//: Matrix of imaginary parts of vnl_matrix<vcl_complex<T> >.
template <class T>
vnl_matrix<T>
  vnl_imag(vnl_matrix<vcl_complex<T> > const& C)
{
  vnl_matrix<T> R(C.rows(), C.columns());
  typename vnl_matrix<vcl_complex<T> >::const_iterator cIt = C.begin();
  typename vnl_matrix<T>::iterator rIt = R.begin();
  for (; cIt != C.end(); ++cIt, ++rIt)
    *rIt = vcl_imag(*cIt);
  return R;
}

//: Matrix of real parts of vnl_diag_matrix<vcl_complex<T> >.
template <class T>
vnl_diag_matrix<T>
  vnl_imag(vnl_diag_matrix<vcl_complex<T> > const& C)
{
  vnl_diag_matrix<T> R(C.rows());
  typename vnl_diag_matrix<vcl_complex<T> >::const_iterator cIt = C.begin();
  typename vnl_diag_matrix<T>::iterator rIt = R.begin();
  for (; cIt != C.end(); ++cIt, ++rIt)
    *rIt = vcl_imag(*cIt);
  return R;
}

//: Matrix of real parts of vnl_sym_matrix<vcl_complex<T> >.
template <class T>
vnl_sym_matrix<T>
  vnl_imag(vnl_sym_matrix<vcl_complex<T> > const& C)
{
  vnl_sym_matrix<T> R(C.rows());
  typename vnl_sym_matrix<vcl_complex<T> >::const_iterator cIt = C.begin();
  typename vnl_sym_matrix<T>::iterator rIt = R.begin();
  for (; cIt != C.end(); ++cIt, ++rIt)
    *rIt = vcl_imag(*cIt);
  return R;
}

//-------------------------------------------------------------------------

#define VNL_COMPLEX_OPS_INSTANTIATE(T) \
template void vnl_complexify(T const *, vcl_complex<T > *, unsigned); \
template void vnl_complexify(T const *, T const *, vcl_complex<T > *, unsigned); \
\
template vnl_vector<vcl_complex<T > > vnl_complexify(vnl_vector<T > const &); \
template vnl_vector<vcl_complex<T > > vnl_complexify(vnl_vector<T > const &, vnl_vector<T > const &); \
template vnl_matrix<vcl_complex<T > > vnl_complexify(vnl_matrix<T > const &); \
template vnl_matrix<vcl_complex<T > > vnl_complexify(vnl_matrix<T > const &, vnl_matrix<T > const &); \
template vnl_diag_matrix<vcl_complex<T > > vnl_complexify(vnl_diag_matrix<T > const &); \
template vnl_diag_matrix<vcl_complex<T > > vnl_complexify(vnl_diag_matrix<T > const &,vnl_diag_matrix<T > const&); \
template vnl_sym_matrix<vcl_complex<T > > vnl_complexify(vnl_sym_matrix<T > const &); \
template vnl_sym_matrix<vcl_complex<T > > vnl_complexify(vnl_sym_matrix<T > const &,vnl_sym_matrix<T > const&); \
\
template void vnl_real(vcl_complex<T > const*, T*, unsigned int); \
template void vnl_imag(vcl_complex<T > const*, T*, unsigned int); \
\
template vnl_vector<T > vnl_real(vnl_vector<vcl_complex<T > > const&); \
template vnl_vector<T > vnl_imag(vnl_vector<vcl_complex<T > > const&); \
\
template vnl_matrix<T > vnl_real(vnl_matrix<vcl_complex<T > > const&); \
template vnl_matrix<T > vnl_imag(vnl_matrix<vcl_complex<T > > const&); \
\
template vnl_diag_matrix<T > vnl_real(vnl_diag_matrix<vcl_complex<T > > const&); \
template vnl_diag_matrix<T > vnl_imag(vnl_diag_matrix<vcl_complex<T > > const&); \
\
template vnl_sym_matrix<T > vnl_real(vnl_sym_matrix<vcl_complex<T > > const&); \
template vnl_sym_matrix<T > vnl_imag(vnl_sym_matrix<vcl_complex<T > > const&)

#endif // vnl_complex_ops_hxx_
