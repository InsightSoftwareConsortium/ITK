// This is core/vnl/io/vnl_io_diag_matrix.h
#ifndef vnl_io_diag_matrix_h
#define vnl_io_diag_matrix_h
//:
// \file
// \author dac
// \date 21-Mar-2001

#include <vsl/vsl_binary_io.h>
#include <vnl/vnl_diag_matrix.h>

//: Binary save vnl_diag_matrix to stream.
//  \relatesalso vnl_diag_matrix
template <class T>
void vsl_b_write(vsl_b_ostream &os, const vnl_diag_matrix<T> & v);

//: Binary load vnl_diag_matrix from stream.
//  \relatesalso vnl_diag_matrix
template <class T>
void vsl_b_read(vsl_b_istream &is, vnl_diag_matrix<T> & v);

//: Print human readable summary of object to a stream
//  \relatesalso vnl_diag_matrix
template <class T>
void vsl_print_summary(vcl_ostream& os,const vnl_diag_matrix<T> & b);

#endif // vnl_io_diag_matrix_h
