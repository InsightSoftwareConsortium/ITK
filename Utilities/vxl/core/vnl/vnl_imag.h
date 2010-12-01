// This is core/vnl/vnl_imag.h
#ifndef vnl_imag_h_
#define vnl_imag_h_
//:
// \file
// \brief Functions to return the imaginary parts of complex arrays, vectors, matrices
//
// \verbatim
// Modifications
// Peter Vanroose - 2 July 2002 - part of vnl_complex_ops.h moved here
// \endverbatim

#include <vcl_complex.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

//: Return array I of imaginary parts of complex array C.
template <class T> void vnl_imag(vcl_complex<T> const* C, T* I, unsigned int n);

//: Vector of imaginary parts of vnl_vector<vcl_complex<T> >.
// \relatesalso vnl_vector
template <class T> vnl_vector<T> vnl_imag(vnl_vector<vcl_complex<T> > const& C);

//: Matrix of imaginary parts of vnl_matrix<vcl_complex<T> >.
// \relatesalso vnl_matrix
template <class T> vnl_matrix<T> vnl_imag(vnl_matrix<vcl_complex<T> > const& C);

#endif // vnl_imag_h_
