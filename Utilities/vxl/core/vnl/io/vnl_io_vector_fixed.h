// This is core/vnl/io/vnl_io_vector_fixed.h
#ifndef vnl_io_vector_fixed_h
#define vnl_io_vector_fixed_h
//:
// \file
// \author Amitha Perera
// \date Oct 2002

#include <vsl/vsl_fwd.h>
#include <vnl/vnl_vector_fixed.h>
#include <vcl_iosfwd.h>

//: Binary save vnl_vector_fixed to stream.
//  \relatesalso vnl_vector_fixed
template <class T, unsigned n>
void vsl_b_write(vsl_b_ostream & os, const vnl_vector_fixed<T,n> & v);

//: Binary load vnl_vector_fixed from stream.
//  \relatesalso vnl_vector_fixed
template <class T, unsigned n>
void vsl_b_read(vsl_b_istream & is, vnl_vector_fixed<T,n> & v);

//: Print human readable summary of object to a stream
//  \relatesalso vnl_vector_fixed
template <class T, unsigned n>
void vsl_print_summary(vcl_ostream & os,const vnl_vector_fixed<T,n> & b);

#endif // vnl_io_vector_fixed_h
