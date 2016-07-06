// This is core/vnl/vnl_real.h
#ifndef vnl_real_h_
#define vnl_real_h_
//:
// \file
// \brief Functions to return the real parts of complex arrays, vectors, matrices
//
// \verbatim
// Modifications
// Peter Vanroose - 2 July 2002 - part of vnl_complex_ops.h moved here
// \endverbatim

#include <complex>
#include <vcl_compiler.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_diag_matrix.h>
#include <vnl/vnl_diag_matrix_fixed.h>
#include <vnl/vnl_sym_matrix.h>
#include "vnl/vnl_export.h"

//: Return array R of real parts of complex array C.
template <class T> VNL_TEMPLATE_EXPORT
void
vnl_real(std::complex<T> const* C, T* R, unsigned int n);

// - vnl_vector
// - vnl_vector_fixed
// - vnl_matrix
// - vnl_matrix_fixed
// - vnl_diag_matrix
// - vnl_diag_matrix_fixed
// - vnl_sym_matrix

//: Vector of real parts of vnl_vector<std::complex<T> >.
// \relatesalso vnl_vector
template <class T> VNL_TEMPLATE_EXPORT
vnl_vector<T>
vnl_real(vnl_vector<std::complex<T> > const& C);

//: Vector of real parts of vnl_vector_fixed<std::complex<T>, N >.
// \relatesalso vnl_vector_fixed
template <class T, unsigned int N> VNL_TEMPLATE_EXPORT
vnl_vector_fixed<T,N>
vnl_real(vnl_vector_fixed<std::complex<T>, N > const& C)
{
  vnl_vector_fixed<T,N> R;
  typename vnl_vector_fixed<std::complex<T>,N >::const_iterator cIt = C.begin();
  typename vnl_vector_fixed<T,N>::iterator rIt = R.begin();
  for (; cIt != C.end(); ++cIt, ++rIt)
    *rIt = std::real(*cIt);
  return R;
}

//: Matrix of real parts of vnl_matrix<std::complex<T> >.
// \relatesalso vnl_matrix
template <class T> VNL_TEMPLATE_EXPORT
vnl_matrix<T>
vnl_real(vnl_matrix<std::complex<T> > const& C);

//: Matrix of real parts of vnl_matrix_fixed<std::complex<T>,NRow,NCol >.
// \relatesalso vnl_matrix_fixed
template <class T, unsigned int NRow, unsigned int NCol> VNL_TEMPLATE_EXPORT
vnl_matrix_fixed<T,NRow,NCol>
vnl_real(vnl_matrix_fixed<std::complex<T>,NRow,NCol > const& C)
{
  vnl_matrix_fixed<T,NRow,NCol> R;
  typename vnl_matrix_fixed<std::complex<T>,NRow,NCol >::const_iterator cIt = C.begin();
  typename vnl_matrix_fixed<T,NRow,NCol>::iterator rIt = R.begin();
  for (; cIt != C.end(); ++cIt, ++rIt)
    *rIt = std::real(*cIt);
  return R;
}

//: Matrix of real parts of vnl_diag_matrix<std::complex<T> >.
// \relatesalso vnl_diag_matrix
template <class T> VNL_TEMPLATE_EXPORT
vnl_diag_matrix<T>
vnl_real(vnl_diag_matrix<std::complex<T> > const& C);

//: Matrix of real parts of vnl_diag_matrix_fixed<std::complex<T> >.
// \relatesalso vnl_diag_matrix_fixed
template <class T, unsigned int N> VNL_TEMPLATE_EXPORT
vnl_diag_matrix_fixed<T,N>
vnl_real(vnl_diag_matrix_fixed<std::complex<T>,N > const& C)
{
  vnl_diag_matrix_fixed<T,N> R;
  typename vnl_diag_matrix_fixed<std::complex<T>,N >::const_iterator cIt = C.begin();
  typename vnl_diag_matrix_fixed<T,N>::iterator rIt = R.begin();
  for (; cIt != C.end(); ++cIt, ++rIt)
    *rIt = std::real(*cIt);
  return R;
}

//: Matrix of real parts of vnl_sym_matrix<std::complex<T> >.
// \relatesalso vnl_sym_matrix
template <class T> VNL_TEMPLATE_EXPORT
vnl_sym_matrix<T>
vnl_real(vnl_sym_matrix<std::complex<T> > const& C);

#endif // vnl_real_h_
