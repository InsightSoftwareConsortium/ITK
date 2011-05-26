// This is core/vnl/xio/vnl_xio_vector.h
#ifndef vnl_xio_vector_h
#define vnl_xio_vector_h
//:
// \file
// \author Gamze Tunali
// \date 30-Dec-2005

#include <vnl/vnl_vector.h>
#include <vcl_string.h>
#include <vcl_iosfwd.h>

//: XML save vnl_vector_fixed to stream.
// \relatesalso vnl_vector
template <class T>
void x_write(vcl_ostream & os, const vnl_vector<T> & v,
             vcl_string name="vnl_vector");

#endif // vnl_xio_vector_h
