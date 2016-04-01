// This is core/vnl/io/vnl_io_matrix_fixed.h
#ifndef vnl_io_matrix_fixed_h
#define vnl_io_matrix_fixed_h
//:
// \file
// \author Louise Butcher
// \date 20-Mar-2001

#include <vsl/vsl_binary_io.h>
#include <vnl/vnl_matrix_fixed.h>

//: Binary save vnl_matrix_fixed to stream.
//  \relatesalso vnl_matrix_fixed
template <class T, unsigned m, unsigned n>
void vsl_b_write(vsl_b_ostream & os, const vnl_matrix_fixed<T,m,n> & v);

//: Binary load vnl_matrix_fixed from stream.
//  \relatesalso vnl_matrix_fixed
template <class T, unsigned m, unsigned n>
void vsl_b_read(vsl_b_istream & is, vnl_matrix_fixed<T,m,n> & v);

//: Print human readable summary of object to a stream
//  \relatesalso vnl_matrix_fixed
template <class T, unsigned m, unsigned n>
void vsl_print_summary(std::ostream & os,const vnl_matrix_fixed<T,m,n> & b);

#endif // vnl_io_matrix_fixed_h
