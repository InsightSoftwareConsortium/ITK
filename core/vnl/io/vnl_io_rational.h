// This is core/vnl/io/vnl_io_rational.h
#ifndef vnl_io_rational_h
#define vnl_io_rational_h
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \author Peter Vanroose
// \date 10-Oct-2001

#include <iosfwd>
#include <vsl/vsl_fwd.h>
#include <vnl/vnl_rational.h>
#include <vcl_compiler.h>

//: Binary save vnl_rational to stream.
//  \relatesalso vnl_rational
void vsl_b_write(vsl_b_ostream & os, vnl_rational const& v);

//: Binary load vnl_rational from stream.
//  \relatesalso vnl_rational
void vsl_b_read(vsl_b_istream & is, vnl_rational & v);

//: Print human readable summary of object to a stream
//  \relatesalso vnl_rational
void vsl_print_summary(std::ostream & os, vnl_rational const& b);

#endif // vnl_io_rational_h
