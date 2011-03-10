// This is core/vnl/io/vnl_io_matrix.h
#ifndef vnl_io_matrix_h
#define vnl_io_matrix_h
//:
// \file
// \author Louise Butcher
// \date 20-Mar-2001

#include <vsl/vsl_fwd.h>
#include <vnl/vnl_matrix.h>
#include <vcl_iosfwd.h>

//: Binary save vnl_matrix to stream.
//  \relatesalso vnl_matrix
template <class T>
void vsl_b_write(vsl_b_ostream & os, const vnl_matrix<T> & v);

//: Binary load vnl_matrix from stream.
//  \relatesalso vnl_matrix
template <class T>
void vsl_b_read(vsl_b_istream & is, vnl_matrix<T> & v);

//: Print human readable summary of object to a stream
//  \relatesalso vnl_matrix
template <class T>
void vsl_print_summary(vcl_ostream & os,const vnl_matrix<T> & b);

#endif // vnl_io_matrix_h
