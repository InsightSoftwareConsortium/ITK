// This is core/vnl/xio/vnl_xio_quaternion.h
#ifndef vnl_xio_quaternion_h
#define vnl_xio_quaternion_h
//:
// \file
// \author Gamze D. Tunali
// \date 22-Dec-2005

//#include <vsl/vsl_binary_io.h>
#include <vnl/vnl_quaternion.h>
#include <vcl_string.h>
#include <vcl_iosfwd.h>

//: XML save vnl_quaternion to stream.
template <class T>
void x_write(vcl_ostream & os, const vnl_quaternion<T> & v, 
             vcl_string name="vnl_quaternion");

#endif // vnl_xio_quaternion_h
