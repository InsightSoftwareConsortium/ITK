#ifndef vnl_complex_ops_h_
#define vnl_complex_ops_h_
#ifdef __GNUC__
#pragma interface
#endif
// This is vxl/vnl/vnl_complex_ops.h

//: \file
//  \brief Classes for complex numbers 
//  \author fsm@robots.ox.ac.uk

// Modifications
// LSB (Manchester) 26/3/01

#include <vcl_complex.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

//
template <class T> void vnl_complexify(T const *, T const *, vcl_complex<T> *, unsigned);
template <class T> void vnl_complexify(T const *, vcl_complex<T> *, unsigned);

//
template <class T> vnl_vector<vcl_complex<T> > vnl_complexify(vnl_vector<T> const &);
template <class T> vnl_matrix<vcl_complex<T> > vnl_complexify(vnl_matrix<T> const &);

//
template <class T> vnl_vector<T> abs  (vnl_vector<vcl_complex<T> > const &);
template <class T> vnl_vector<T> angle(vnl_vector<vcl_complex<T> > const &);
template <class T> vnl_vector<T> real (vnl_vector<vcl_complex<T> > const &);
template <class T> vnl_vector<T> imag (vnl_vector<vcl_complex<T> > const &);

//
template <class T> vnl_matrix<T> abs  (vnl_matrix<vcl_complex<T> > const &);
template <class T> vnl_matrix<T> angle(vnl_matrix<vcl_complex<T> > const &);
template <class T> vnl_matrix<T> real (vnl_matrix<vcl_complex<T> > const &);
template <class T> vnl_matrix<T> imag (vnl_matrix<vcl_complex<T> > const &);

#endif // vnl_complex_ops_h_
