// This is core/vnl/xio/vnl_xio_vector_fixed.h
#ifndef vnl_xio_vector_fixed_h
#define vnl_xio_vector_fixed_h
//:
// \file
// \author Gamze Tunali
// \date  Dec 28, 2005

#include <vsl/vsl_fwd.h>
#include <vnl/vnl_vector_fixed.h>
#include <vcl_iosfwd.h>
#include <vcl_string.h>

//: XML save vnl_vector_fixed to stream.
// \relatesalso vnl_vector_fixed
template <class T, unsigned n>
void x_write(vcl_ostream & os, const vnl_vector_fixed<T,n> & v,
             vcl_string name="vnl_vector_fixed");

#endif // vnl_xio_vector_fixed_h
