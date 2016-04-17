// This is core/vnl/io/vnl_io_real_npolynomial.h
#ifndef vnl_io_real_npolynomial_h
#define vnl_io_real_npolynomial_h
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \author dac
// \date 21-Mar-2001

#include <vsl/vsl_binary_io.h>
#include <vnl/vnl_real_npolynomial.h>

//: Binary save vnl_real_npolynomial to stream.
//  \relatesalso vnl_real_npolynomial
void vsl_b_write(vsl_b_ostream &os, const vnl_real_npolynomial & v);

//: Binary load vnl_real_polynomial from stream.
//  \relatesalso vnl_real_npolynomial
void vsl_b_read(vsl_b_istream &is, vnl_real_npolynomial & v);

//: Print human readable summary of object to a stream
//  \relatesalso vnl_real_npolynomial
void vsl_print_summary(std::ostream& os,const vnl_real_npolynomial & b);

#endif // vnl_io_real_npolynomial_h
