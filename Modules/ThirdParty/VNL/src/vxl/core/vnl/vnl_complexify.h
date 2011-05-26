// This is core/vnl/vnl_complexify.h
#ifndef vnl_complexify_h_
#define vnl_complexify_h_
//:
//  \file
//  \brief Functions to create complex vectors and matrices from real ones
//  \author fsm
//
// \verbatim
// Modifications
// Peter Vanroose - 2 July 2002 - part of vnl_complex_ops.h moved here
// \endverbatim

#include <vcl_complex.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

//: Overwrite complex array C (of length n) with pairs from real arrays R and I.
template <class T> void
  vnl_complexify(T const* R, T const* I, vcl_complex<T>* C, unsigned n);
//: Overwrite complex array C (sz n) with complexified version of real array R.
template <class T> void
  vnl_complexify(T const* R,             vcl_complex<T>* C, unsigned n);

//: Return complexified version of real vector R.
// \relatesalso vnl_vector
template <class T> vnl_vector<vcl_complex<T> >
  vnl_complexify(vnl_vector<T> const& R);
//: Return complex vector R+j*I from two real vectors R and I.
// \relatesalso vnl_vector
template <class T> vnl_vector<vcl_complex<T> >
  vnl_complexify(vnl_vector<T> const& R, vnl_vector<T> const& I);
//: Return complexified version of real matrix R.
// \relatesalso vnl_matrix
template <class T> vnl_matrix<vcl_complex<T> >
  vnl_complexify(vnl_matrix<T> const& R);
//: Return complex matrix R+j*I from two real matrices R and I.
// \relatesalso vnl_matrix
template <class T> vnl_matrix<vcl_complex<T> >
  vnl_complexify(vnl_matrix<T> const& R, vnl_matrix<T> const& I);

#endif // vnl_complexify_h_
