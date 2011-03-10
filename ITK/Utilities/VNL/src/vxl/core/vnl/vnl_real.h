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

#include <vcl_complex.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

//: Return array R of real parts of complex array C.
template <class T> void vnl_real(vcl_complex<T> const* C, T* R, unsigned int n);

//: Vector of real parts of vnl_vector<vcl_complex<T> >.
// \relatesalso vnl_vector
template <class T> vnl_vector<T> vnl_real(vnl_vector<vcl_complex<T> > const& C);

//: Matrix of real parts of vnl_matrix<vcl_complex<T> >.
// \relatesalso vnl_matrix
template <class T> vnl_matrix<T> vnl_real(vnl_matrix<vcl_complex<T> > const& C);

#endif // vnl_real_h_
