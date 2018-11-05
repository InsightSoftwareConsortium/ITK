// This is core/vnl/io/vnl_io_vector.h
#ifndef vnl_io_vector_h
#define vnl_io_vector_h
//:
// \file
// \author Louise Butcher
// \date 20-Mar-2001

#include <iosfwd>
#include <vsl/vsl_fwd.h>
#include <vnl/vnl_vector.h>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif

//: Binary save vnl_vector to stream.
//  \relatesalso vnl_vector
template <class T>
void vsl_b_write(vsl_b_ostream & os, const vnl_vector<T> & v);

//: Binary load vnl_vector from stream.
//  \relatesalso vnl_vector
template <class T>
void vsl_b_read(vsl_b_istream & is, vnl_vector<T> & v);

//: Print human readable summary of object to a stream
//  \relatesalso vnl_vector
template <class T>
void vsl_print_summary(std::ostream & os,const vnl_vector<T> & b);

#endif // vnl_io_vector_h
