// This is core/vnl/io/vnl_io_sparse_matrix.h
#ifndef vnl_io_sparse_matrix_h
#define vnl_io_sparse_matrix_h
//:
// \file
// \author Louise Bucther
// \date 20-Mar-2001

#include <vnl/vnl_sparse_matrix.h>
#include <vsl/vsl_binary_io.h>

//: Binary save vnl_sparse_matrix to stream.
//  \relatesalso vnl_sparse_matrix
template <class T>
void vsl_b_write(vsl_b_ostream & os, const vnl_sparse_matrix<T> & v);

//: Binary load vnl_sparse_matrix from stream.
//  \relatesalso vnl_sparse_matrix
template <class T>
void vsl_b_read(vsl_b_istream & is, vnl_sparse_matrix<T> & v);

//: Print human readable summary of object to a stream
//  \relatesalso vnl_sparse_matrix
template <class T>
void vsl_print_summary(vcl_ostream & os,const vnl_sparse_matrix<T> & b);

#endif // vnl_io_sparse_matrix_h
